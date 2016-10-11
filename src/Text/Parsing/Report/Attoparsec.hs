module Text.Parsing.Report.Attoparsec (
    parseA,
    parseFullLine
) where

import Text.Parsing.Report.Combinator
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString.Char8 as BS

-- use attoparsec parser on current line
parseA :: AP.Parser a -> Parser a
parseA p = Parser f where
    f (_, []) = Nothing
    f (i, x:xs) = case AP.parseOnly p (BS.pack x) of
        (Right d) -> Just (i + 1, xs, d)
        _ -> Nothing


parseFullLine :: AP.Parser a -> Parser a
parseFullLine p = parseA $ p <* AP.endOfInput
