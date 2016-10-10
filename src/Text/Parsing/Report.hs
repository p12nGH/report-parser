module Text.Parsing.Report (
    Parser(..),
    many,
    skip,
    parseFullLine,
    skipUntil,
    parseA,
    parseFile,
    predicate,
    currentLine,
    processStdIn,
    startsWith
) where

import Text.Parsing.Report.String
import Text.Parsing.Report.Combinator
