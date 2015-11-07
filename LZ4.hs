{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Data.Hex
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

foreign import ccall
  "LZ4F_createDecompressionContext"
  c_LZ4F_createDecompressionContext
  :: Ptr Context -> CUInt -> IO LZ4FErrorCode

foreign import ccall
  "LZ4F_freeDecompressionContext"
  c_LZ4F_freeDecompressionContext
  :: Context -> IO LZ4FErrorCode

foreign import ccall
  "&wrap_free_dctx"
  c_wrap_free_dctx
  :: FunPtr (Ptr Context -> IO ())

foreign import ccall
  "LZ4F_decompress"
  c_LZ4F_decompress
  :: Context -> Ptr CChar -> Ptr CSize -> Ptr CChar -> Ptr CSize -> Ptr Options -> IO CSize

foreign import ccall
  "LZ4F_isError"
  c_LZ4F_isError
  :: CSize -> CUInt

foreign import ccall
  "LZ4F_getErrorName"
  c_LZ4F_getErrorName
  :: CSize -> IO CString
foreign import ccall
  "print_ptr"
  c_print_ptr
  :: Ptr a -> IO ()

data LZ4Dctx
type Context = Ptr LZ4Dctx
data Options

version = 100 -- lz4frame.h:158

createCtx :: IO (ForeignPtr Context)
createCtx = do
  -- decompressioncontext is ptr to opaque struct:
  -- typedef struct LZ4F_dctx_s* LZ4F_decompressionContext_t;
  ctx <- mallocForeignPtrBytes (sizeOf (undefined :: CIntPtr))
  addForeignPtrFinalizer c_wrap_free_dctx ctx

  tryLZ4 "Create lz4 context"
    $ withForeignPtr ctx (flip c_LZ4F_createDecompressionContext version)
  return ctx

decompress :: Lazy.ByteString -> Lazy.ByteString
decompress input = unsafePerformIO $ do
  ctx <- createCtx
  decompressContinue input ctx

defaultChunkSize = Lazy.defaultChunkSize
int :: (Integral a, Integral b) => a -> b
int = fromIntegral

-- takes compressed chunk and opaque context as input
-- returns (number of bytes read, decompressed bytes)
decompressChunk :: Strict.ByteString -> ForeignPtr Context -> IO (Word64, Strict.ByteString)
decompressChunk inp ctx = do
  unsafeUseAsCStringLen inp $ \(srcPtr, len) -> do
    with (int len) $ \lenInp -> do
      with (int Lazy.defaultChunkSize) $ \lenOut -> do
        withForeignPtr ctx $ \ctx -> do
          let out = Strict.replicate defaultChunkSize 0
          (szout, bytesRead) <- unsafeUseAsCStringLen out $ \(outPtr, _lenOut) -> do
            derefCtx <- peek ctx
            -- LZ4F_decompress returns a hint as to next input buffer size, which we ignore
            _ <- tryLZ4 "Decompress continue"
              $ c_LZ4F_decompress
                  derefCtx
                  outPtr
                  lenOut
                  srcPtr
                  lenInp
                  nullPtr{-options-}
            (,) <$> peek lenOut <*> peek lenInp
          -- print $ "Decompressed " ++ show szout ++ " bytes"
          -- print $ Strict.take (int szout) out
          return (int bytesRead, Strict.take (int szout) out)

-- tries an action, returning the result if it is not an error
-- and throwing an IO exception if it is an error
tryLZ4 :: String -> IO CSize -> IO CSize
tryLZ4 userMsg action = do
  out <- action
  let prependUserMsg = ((userMsg++" -- ") ++)
  if toBool . c_LZ4F_isError $ out then
    c_LZ4F_getErrorName (int out)
      >>= peekCString
      >>= (\msg -> throwIO . userError $ prependUserMsg msg)
  else return out

decompressContinue :: Lazy.ByteString -> ForeignPtr Context -> IO Lazy.ByteString
decompressContinue inp ctx = case inp of
  Lazy.Empty       -> return Lazy.empty
  Lazy.Chunk b bs  -> do
    (bytesRead, chunk) <- decompressChunk b ctx
    rest <- decompressContinue (Lazy.drop (int bytesRead) inp) ctx
    return $ Lazy.Chunk chunk rest

main = do
  input <- Lazy.getContents
  let decompressed = decompress input
  -- print $ Lazy.length input
  print $ Lazy.length decompressed
  Lazy.putStr $ decompressed
