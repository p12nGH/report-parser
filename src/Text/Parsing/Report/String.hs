{-|
This module contains simple string predicates. They advance
 line position if condition is satisfied, otherwise they
 generate failure.
-}

module Text.Parsing.Report.String (
    spaces,
    emtpy,
    contains,
    startsWith,
    startsWithSpace
) where

import Text.Parsing.Report.Combinator
import Data.Char (isSpace)
import Data.List (isInfixOf, isPrefixOf)

-- | Matches  empty line.
emtpy :: Parser ()
emtpy = predicate null

-- | Matches line with spaces only or empty line.
spaces :: Parser ()
spaces = predicate $ all isSpace

-- | Matches line that contains specified string.
contains :: String -> Parser ()
contains s = predicate $ isInfixOf s

-- | Matches line starts with specified string.
startsWith :: String -> Parser ()
startsWith s = predicate $ isPrefixOf s

-- | Matches line starts with specified string. Ignorest spaces at beginning
startsWithSpace :: String -> Parser ()
startsWithSpace s = startsWith $ dropWhile isSpace s
