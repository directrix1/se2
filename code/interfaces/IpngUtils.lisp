(interface IpngUtils
  (sig blowChunks (pngdata))
  (sig makeChunk (chunktype chunkdata))
  (sig makeNum (num flag))
  (sig parseNum (string flag))
  (sig calcCRC32 (string))
)
