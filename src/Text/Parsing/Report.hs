module Text.Parsing.Report (
    Parser(..),
    many,
    skip,
    parseFullLine,
    skipUntilMatch,
    parseLine,
    parseFile,
    predicate
) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP

type Line = BS.ByteString

-- takes line number and array of remaining lines,
-- returns new line number, remaining lines and parser result
data Parser a = Parser {runParser :: (Int, [Line]) -> Maybe (Int, [Line], a)}

instance Functor Parser where
    fmap f (Parser d) = Parser $ \(p, s) -> do
        (i', l, a) <- d (p, s)
        return (i', l, f a)

instance Applicative Parser where
    pure x = Parser $ \(p, s) -> Just $ (p, s, x)
    (<*>) (Parser f) (Parser d) = Parser $ \(p, s) -> do
        (i0, l0, f') <- f (p, s)
        (i1, l1, d') <- d (i0, l0)
        return $ (i1, l1, f' d')

instance Monad Parser where
    return = pure
    (>>=) (Parser d) b = Parser $ \(p, s) -> do
        (i, l, a) <- d (p, s)
        let (Parser f) = b a
        f (i, l)

parse :: Parser a -> BS.ByteString -> Maybe a
parse p d = (\(_, _, a) -> a) <$> runParser p (0, BS.lines d) 

getString :: Parser String
getString = Parser f where
    f (_, []) = Nothing
    f (i, x:xs) = Just (i + 1, xs, BS.unpack x)

parseLine :: AP.Parser a -> Parser a
parseLine p = Parser f where
    f (_, []) = Nothing
    f (i, x:xs) = case AP.parseOnly p x of
        (Right d) -> Just (i + 1, xs, d)
        _ -> Nothing

-- try to apply parser until it succeeds or fail at the end of input
skipUntilMatch :: Parser a -> Parser a
skipUntilMatch (Parser f) = Parser f' where
    f' (_, []) = Nothing
    f' (i, r@(x:xs)) = case f (i, r) of
        Nothing -> f' (i + 1, xs)
        m -> m -- match found

skip n = Parser f where
    f (i, r) = Just (i + n, drop n r, ()) 

-- take all matches from current position, never fails
many :: Parser a -> Parser [a]
many (Parser pat) = Parser (Just . f') where
    f' (i, r) = case pat (i, r) of
        Nothing -> (i, r, [])
        Just (i', r', a) -> (i'', r'', a:a') where
            (i'', r'', a') = f' (i', r')

parseFullLine p = parseLine $ p <* AP.endOfInput

predicate :: (String -> Bool) -> Parser ()
predicate p = Parser f where
    f (_, []) = Nothing
    f (i, r@(x:xs)) = case p (BS.unpack x) of
        True -> Just (i, r, ())
        _ -> Nothing

parseFile :: String -> Parser a -> IO (Maybe a)
parseFile f p = do
    lines <- BS.lines <$> BS.readFile f
    return $ do
        r <- (runParser p) (0, lines)
        let r' = \(_, _, x) -> x
        return $ r' r

