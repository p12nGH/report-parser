{-# LANGUAGE OverloadedStrings #-}
import Text.Parsing.Report
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP


--space separated record
ss p = AP.skipSpace *> p <* AP.skipSpace

rn = ss $ AP.string "0x" *> AP.hexadecimal

t1 :: Parser [Integer]
t1 = do
    parseFullLine $ AP.string "ELF Header:"
    skipUntilMatch $ parseLine $ AP.string "Dynamic section at offset"
    skip 1
    many $ parseLine rn

parseElfHeaderRecord = do
    AP.skipSpace
    k <- AP.takeTill ((==) ':')
    AP.char ':'
    AP.skipSpace
    v <- AP.takeByteString
    return $ (BS.unpack k, BS.unpack v)

main = do
    x <- parseFile "log.txt" t1
    print x

