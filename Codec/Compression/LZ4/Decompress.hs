module Codec.Compression.LZ4.Decompress
( decompress )
where

import Control.Monad
import Foreign
import Foreign.C
import System.IO.Unsafe
import Control.Exception.Base (throwIO)
import Data.Monoid
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy
import qualified Data.ByteString as Strict
import Data.ByteString.Unsafe

type LZ4FErrorCode = CSize

foreign import ccall unsafe
  "LZ4F_createDecompressionContext"
  c_LZ4F_createDecompressionContext
  :: Ptr Context -> CUInt -> IO LZ4FErrorCode

foreign import ccall unsafe
  "LZ4F_freeDecompressionContext"
  c_LZ4F_freeDecompressionContext
  :: Context -> IO LZ4FErrorCode

foreign import ccall unsafe
  "&wrap_free_dctx"
  c_wrap_free_dctx
  :: FunPtr (Ptr Context -> IO ())

foreign import ccall unsafe
  "LZ4F_decompress"
  c_LZ4F_decompress
  :: Context      -- lz4 internal state
  -> Ptr CChar    -- src buffer
  -> Ptr CSize    -- src buffer len
  -> Ptr CChar    -- dst buffer
  -> Ptr CSize    -- dst buffer len
  -> Ptr Options  -- options, null indicates none
  -> IO CSize     -- hint as to size of next chunk

foreign import ccall unsafe
  "LZ4F_isError"
  c_LZ4F_isError
  :: CSize -> CUInt

foreign import ccall unsafe
  "LZ4F_getErrorName"
  c_LZ4F_getErrorName
  :: CSize -> IO CString

data LZ4Dctx
type Context = Ptr LZ4Dctx
data Options
lz4Version = 100 -- lz4frame.h:158

defaultChunkSize = Lazy.defaultChunkSize
-- int is just alias for fromIntegral
int :: (Integral a, Integral b) => a -> b
int = fromIntegral

-- Tries an LZ4 action, returning the result if it is not an error.
-- Throws IO exception if it is an error.
tryLZ4 :: String -> IO CSize -> IO CSize
tryLZ4 userMsg action = do

  ret <- action
  if (not . toBool . c_LZ4F_isError) ret then
    return ret

  else c_LZ4F_getErrorName (int ret)
    >>= peekCString
    >>= (\msg -> throwIO . userError $ userMsg ++ " -- " ++ msg)

createCtx :: IO (ForeignPtr Context)
createCtx = do
  -- decompressioncontext is ptr to opaque struct:
  -- typedef struct LZ4F_dctx_s* LZ4F_decompressionContext_t;
  ctx <- mallocForeignPtrBytes (sizeOf (undefined :: Ptr LZ4Dctx))

  addForeignPtrFinalizer c_wrap_free_dctx ctx

  tryLZ4 "LZ4.createCtx"
    $ withForeignPtr ctx (flip c_LZ4F_createDecompressionContext lz4Version)

  return ctx

-- Takes lazy bytestring and decompresses it lazily.
-- Throws IO exception on failure. The reason it does not return
-- Either is because otherwise it would have to hold the whole
-- ByteString in memory to check the entire stream for corruption,
-- which is not appropriate for large operations
decompress :: Lazy.ByteString -> Lazy.ByteString
decompress input =
  decompressContinue (unsafePerformIO createCtx) input

decompressContinue :: ForeignPtr Context -> Lazy.ByteString -> Lazy.ByteString
decompressContinue ctx input = case input of
  Lazy.Empty       -> Lazy.empty
  Lazy.Chunk b bs  -> let

    -- even though this uses unsafePerformIO,
    -- this has to be sequenced because decompressContinue has a
    -- dependency on the output of decompressChunk
    (bytesRead, chunk) = unsafePerformIO $ decompressChunk ctx b

    rest = decompressContinue ctx
      (Lazy.drop (int bytesRead) input)

    in Lazy.Chunk chunk rest

-- Takes compressed chunk and opaque context as input.
-- Throws IO exception on failure.
-- Returns (number of bytes read, decompressed bytes)
decompressChunk
  :: ForeignPtr Context
  -> Strict.ByteString
  -> IO (Word64, Strict.ByteString)
decompressChunk ctx inp = do

  -- TODO figure out how to allocate without zeroing the whole chunk
  let out = Strict.replicate defaultChunkSize 0

  withForeignPtr ctx         $ \ctx              -> do
  unsafeUseAsCStringLen inp  $ \(srcPtr, lenInp) -> do
  with (int lenInp)          $ \lenInp           -> do
  unsafeUseAsCStringLen out  $ \(outPtr, lenOut) -> do
  with (int lenOut)          $ \lenOut           -> do

    derefCtx <- peek ctx

    let optsPtr = nullPtr
    tryLZ4 "LZ4.decompressChunk"
      $ c_LZ4F_decompress derefCtx outPtr lenOut srcPtr lenInp optsPtr

    -- c_LZ4F_decompress modifies the len pointers in place
    -- to tell us how much was read and written
    szWr <- peek lenOut
    szRd <- peek lenInp

    return (int szRd, Strict.take (int szWr) out)
