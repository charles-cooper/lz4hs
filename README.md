# lz4hs
This exposes the streaming version of the LZ4 API in a Haskell interface. There will be two functions, `decompress` and `compress` which take lazy `ByteString`s and return `ByteString`s which are lazily evaluated.

lz4 r131

Still very much a work in progress. `decompress` is implemented, `compress` is in the works.
