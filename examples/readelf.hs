{-# LANGUAGE OverloadedStrings #-}
import Text.Parsing.Report
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Char (isSpace)
import Data.ByteString as BS

parseSectionHeadersEntry :: Parser BS.ByteString
parseSectionHeadersEntry = do
    let
        ss = AP.skipSpace
        dec = AP.decimal :: AP.Parser Int
        word = AP.takeWhile (not . isSpace)
        ssword = ss *> word <* ss

    parseA $ ss *> "[" *> ss *> dec *> "]" *> ssword
    <* skip 1

-- run: readelf <any ELF binary> | readelf-example
-- process stdin, print all section names
main :: IO ()
main = do
    x <- processStdIn $ do
        skipUntil $ startsWith "Section Headers:"
        skip 3
        many parseSectionHeadersEntry
    print x
