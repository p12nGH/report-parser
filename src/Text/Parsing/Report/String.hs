module Text.Parsing.Report.String (
    spaces,
    emtpy,
    contains,
    startsWith
) where

import Text.Parsing.Report.Combinator
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)

-- match empty line
emtpy :: Parser ()
emtpy = predicate null

-- match line with spaces only or empty line
spaces :: Parser ()
spaces = predicate $ all isSpace

--
contains :: String -> Parser ()
contains s = predicate $ isInfixOf s

startsWith :: String -> Parser ()
startsWith s = predicate $ isPrefixOf s
