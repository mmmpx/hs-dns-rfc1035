module Data.Attoparsec.Binary
  ( anyWord16be
  , anyWord16le
  , anyWord32be
  , anyWord32le
  , anyWord64be
  , anyWord64le
  , word16be
  , word16le
  , word32be
  , word32le
  , word64be
  , word64le
  ) where

import Data.Bits
import Data.Word
import Data.Attoparsec.ByteString
import qualified Data.ByteString as B


byteSize :: (FiniteBits b) => b -> Int
byteSize = (`div` 8) . finiteBitSize

pack :: (FiniteBits b, Num b) => B.ByteString -> b
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

anyWordN :: (FiniteBits b) => (B.ByteString -> b) -> Parser b
anyWordN = anyWordN' undefined
  where
    anyWordN' :: (FiniteBits b) => b -> (B.ByteString -> b) -> Parser b
    anyWordN' d = flip fmap $ Data.Attoparsec.ByteString.take $ byteSize d

anyWord16be :: Parser Word16
anyWord16be = anyWordN pack

anyWord16le :: Parser Word16
anyWord16le = anyWordN $ pack . B.reverse

anyWord32be :: Parser Word32
anyWord32be = anyWordN pack

anyWord32le :: Parser Word32
anyWord32le = anyWordN $ pack . B.reverse

anyWord64be :: Parser Word64
anyWord64be = anyWordN pack

anyWord64le :: Parser Word64
anyWord64le = anyWordN $ pack . B.reverse

unpack :: (FiniteBits b, Integral b) => b -> B.ByteString
unpack x = B.pack $ map f $ reverse [0..byteSize x - 1]
  where
    f :: Int -> Word8
    f s = fromIntegral $ shiftR x (8 * s)

wordN :: (FiniteBits b) => (b -> B.ByteString) -> b -> Parser b
wordN u w = string (u w) >> return w

word16be :: Word16 -> Parser Word16
word16be = wordN unpack

word16le :: Word16 -> Parser Word16
word16le = wordN $ B.reverse . unpack

word32be :: Word32 -> Parser Word32
word32be = wordN unpack

word32le :: Word32 -> Parser Word32
word32le = wordN $ B.reverse . unpack

word64be :: Word64 -> Parser Word64
word64be = wordN unpack

word64le :: Word64 -> Parser Word64
word64le = wordN $ B.reverse . unpack

