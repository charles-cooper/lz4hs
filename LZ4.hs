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

foreign import ccall
  "LZ4F_createDecompressionContext"
  c_LZ4F_createDecompressionContext
  :: Ptr Context -> IO LZ4FErrorCode

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
  :: Ptr Context -> Ptr CChar -> Ptr CSize -> Ptr CChar -> Ptr CSize -> Ptr Options -> IO CSize

foreign import ccall
  "LZ4F_isError"
  c_LZ4F_isError
  :: CSize -> CUInt

foreign import ccall
  "LZ4F_getErrorName"
  c_LZ4F_getErrorName
  :: CSize -> IO CString

data LZ4Dctx
type Context = Ptr LZ4Dctx
data Options

createCtx :: IO (ForeignPtr Context)
createCtx = do
  -- decompressioncontext is ptr to opaque struct
  -- typedef struct LZ4F_dctx_s* LZ4F_decompressionContext_t;
  ctx <- mallocBytes (sizeOf (undefined :: CIntPtr))
  c_LZ4F_createDecompressionContext ctx
  newForeignPtr (c_wrap_free_dctx) ctx

decompressInit :: Lazy.ByteString -> IO (ForeignPtr Context, Lazy.ByteString)
decompressInit = undefined -- TODO

decompress :: Lazy.ByteString -> Lazy.ByteString
decompress inp = unsafePerformIO $ do
  (ctx, input) <- decompressInit inp
  decompressContinue input ctx

defaultChunkSize = Lazy.defaultChunkSize
int :: (Integral a, Integral b) => a -> b
int = fromIntegral
decompressChunk :: CStringLen -> ForeignPtr Context -> IO (Word64, Strict.ByteString)
decompressChunk (inp, len) ctx = do
  with (int len) $ \lenInp -> do
    with (int Lazy.defaultChunkSize) $ \lenOut -> do
      let out = Strict.replicate defaultChunkSize 0
      szout <- unsafeUseAsCStringLen out $ \(out, _lenOut) -> do
        withForeignPtr ctx $ \ctx -> do
          tryLZ4 $ c_LZ4F_decompress ctx inp lenInp out lenOut nullPtr{-options-}
      return (int szout, Strict.take (int szout) out)

-- tries an action, returning the result if it is not an error
-- and throwing an IO exception if it is an error
tryLZ4 :: IO CSize -> IO CSize
tryLZ4 action = do
  out <- action
  if toBool . c_LZ4F_isError $ out then
    c_LZ4F_getErrorName (int out)
      >>= peekCString
      >>= throwIO . userError
  else return out

decompressContinue :: Lazy.ByteString -> ForeignPtr Context -> IO Lazy.ByteString
decompressContinue inp ctx = case inp of
  Lazy.Empty       -> return Lazy.empty
  Lazy.Chunk b bs  -> do
    unsafeUseAsCStringLen b $ \src -> do
      (bytesRead, chunk) <- decompressChunk src ctx
      rest <- decompressContinue (Lazy.drop (int bytesRead) inp) ctx
      return $ Lazy.Chunk chunk rest

main = do
  input <- Lazy.getContents
  Lazy.putStr $ decompress input
