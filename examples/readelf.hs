{-# LANGUAGE OverloadedStrings #-}
import Text.Parsing.Report
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.Char (isSpace)

ss = AP.skipSpace
dec = AP.decimal
word = AP.takeWhile (not . isSpace)
ssword = ss *> word <* ss

parseSectionHeadersEntry = do
    parseA $ ss *> "[" *> ss *> dec *> "]" *> ssword
    <* skip 1

-- run: readelf <any ELF binary> | readelf-example
-- process stdin, print all section names
main = do
    x <- processStdIn $ do
        skipUntil $ startsWith "Section Headers:"
        skip 3
        many parseSectionHeadersEntry
    print x
