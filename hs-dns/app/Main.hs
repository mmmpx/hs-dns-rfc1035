module Main where

import Data.Word
import Data.Attoparsec.Binary
import qualified Data.Attoparsec.ByteString as AP
import Control.Monad
import Network.Socket
import Network.Socket.ByteString (recvFrom)
import qualified Data.ByteString as B


data Header = Header
  { id :: Word16
  , flags :: Word16
  , qdcount :: Word16
  , ancount :: Word16
  , nscount :: Word16
  , arcount :: Word16
  } deriving Show

data Question = Question
  { qname :: [B.ByteString]
  , qtype :: Word16
  , qclass :: Word16
  } deriving Show

data Record = Record
  { rname :: [B.ByteString]
  , rtype :: Word16
  , rclass :: Word16
  , ttl :: Word32
  , rdlength :: Word16
  , rdata :: B.ByteString
  } deriving Show

data Message = Message
 { header :: Header
 , question :: [Question]
 , answer :: [Record]
 , authority :: [Record]
 , additional :: [Record]
 } deriving Show


pHeader :: AP.Parser Header
pHeader =
  Header <$>
        anyWord16be
    <*> anyWord16be
    <*> anyWord16be
    <*> anyWord16be
    <*> anyWord16be
    <*> anyWord16be

pLabel :: AP.Parser B.ByteString
pLabel =
  fromIntegral <$> AP.anyWord8
    >>= AP.take

pLseq :: AP.Parser [B.ByteString]
pLseq = AP.manyTill pLabel $ AP.word8 0

pQuestion :: AP.Parser Question
pQuestion =
  Question <$>
        pLseq
    <*> anyWord16be
    <*> anyWord16be

pRecord :: AP.Parser Record
pRecord = do
  name     <- pLseq
  rtype    <- anyWord16be
  rclass   <- anyWord16be
  ttl      <- anyWord32be
  rdlength <- anyWord16be
  rdata    <- AP.take $ fromIntegral rdlength
  return $ Record name rtype rclass ttl rdlength rdata

pMessage :: AP.Parser Message
pMessage = do
  h <- pHeader
  let count f = AP.count (fromIntegral $ f h)
  Message h <$>
        count qdcount pQuestion
    <*> count ancount pRecord
    <*> count nscount pRecord
    <*> count arcount pRecord


awaitQ :: Socket -> IO (B.ByteString, SockAddr)
awaitQ sock = recvFrom sock 1232

main :: IO ()
main = do
  addr <- head <$> getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  sock <- socket (addrFamily addr) Datagram defaultProtocol
  bind sock (addrAddress addr)
  forever $ (awaitQ sock >>= (return . fst) >>= (return . AP.parse pMessage) >>= print)
  return ()

