import Codec.Compression.LZ4.Decompress
import qualified Data.ByteString.Lazy as Lazy
andThen :: (a -> b) -> (b -> c) -> a -> c
f `andThen` g = g . f
main = do
  -- input <- Lazy.getContents
  -- print $ Lazy.length input
  -- print $ Lazy.length decompressed
  -- Lazy.putStr $ input
  Lazy.getContents >>= (decompress `andThen` Lazy.putStr)
