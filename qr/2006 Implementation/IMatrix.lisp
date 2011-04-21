;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
(interface IMatrix
  (sig BitArray:makeArray (size))
  (sig BitArray ())
  (sig BitArray$1 (size))
  (sig BitArray:getSize (ba))
  (sig BitArray:getSizeInBytes (ba))
  (sig BitArray:ensureCapacity (ba size))
  (sig BitArray:get (ba i))
  (sig BitArray:set (ba i))
  (sig BitArray:flip (ba i))
  (sig BitArray:setBulk (ba i newBits))
  (sig BitArray:clear (ba))
  (sig BitArray:isRange (ba start end value))
  (sig BitArray:appendBit (ba bit))
  (sig BitArray:appendBits (ba value numBits))
  (sig BitArray:appendBitArray (ba other))
  (sig BitArray:xor (ba other))
  (sig BitArray:toBytes (ba bitOffset array offset numBytes))
  (sig BitArray:getBitArray (ba))
  (sig BitArray:reverse (ba))
  (sig BitMatrix$2 (width height))
  (sig BitMatrix$1 (dimension))
  (sig BitMatrix:get (bm x y))
  (sig BitMatrix:set (bm x y))
  (sig BitMatrix:flip (bm x y))
  (sig BitMatrix:clear (bm))
  (sig BitMatrix:setRegion (bm left top width height))
  (sig BitMatrix:getRow (bm y ba))
  (sig BitMatrix:getTopLeftOnBit (bm))
  (sig BitMatrix:getWidth (bm))
  (sig BitMatrix:getHeight (bm))
  (sig ByteMatrix$2 (width height))
  (sig ByteMatrix:getHeight (Bm))
  (sig ByteMatrix:getWidth (Bm))
  (sig ByteMatrix:get (Bm x y))
  (sig ByteMatrix:getArray (Bm))
  (sig ByteMatrix:set (Bm x y v))
  (sig ByteMatrix:clear (Bm value)))