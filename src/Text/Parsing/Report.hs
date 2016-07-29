{-# LANGUAGE OverloadedStrings #-}
module Text.Parsing.Report (
    LogP(..),
    many,
    skip

) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP

data NTree a  = NTree a [NTree a]

data XNode = XNode

type LogTree  = NTree XNode
type LogTrees = [LogTree]
type LogFilter = LogTree -> [LogTree]

type Line = BS.ByteString
data LogP a = LogP {runLogP :: (Int, [Line]) -> Maybe (Int, [Line], a)}

instance Functor LogP where
    fmap f (LogP d) = LogP $ \(p, s) -> do
        (i', l, a) <- d (p, s)
        return (i', l, f a)

instance Applicative LogP where
    pure x = LogP $ \(p, s) -> Just $ (p, s, x)
    (<*>) (LogP f) (LogP d) = LogP $ \(p, s) -> do
        (i0, l0, f') <- f (p, s)
        (i1, l1, d') <- d (i0, l0)
        return $ (i1, l1, f' d')

instance Monad LogP where
    return = pure
    (>>=) (LogP d) b = LogP $ \(p, s) -> do
        (i, l, a) <- d (p, s)
        let (LogP f) = b a
        f (i, l)

parse :: LogP a -> BS.ByteString -> Maybe a
parse p d = (\(_, _, a) -> a) <$>runLogP p (0, BS.lines d) 

getString :: LogP String
getString = LogP f where
    f (_, []) = Nothing
    f (i, x:xs) = Just (i + 1, xs, BS.unpack x)

parseLine :: AP.Parser a -> LogP a
parseLine p = LogP f where
    f (_, []) = Nothing
    f (i, x:xs) = case AP.parseOnly p x of
        (Right d) -> Just (i + 1, xs, d)
        _ -> Nothing

-- try to apply parser until it succeeds or fail at the end of input
skipUntilMatch :: LogP a -> LogP a
skipUntilMatch (LogP f) = LogP f' where
    f' (_, []) = Nothing
    f' (i, r@(x:xs)) = case f (i, r) of
        Nothing -> f' (i + 1, xs)
        m -> m -- match found

skip n = LogP f where
    f (i, r) = Just (i + n, drop n r, ()) 

-- take all matches from current position, never fails
many :: LogP a -> LogP [a]
many (LogP pat) = LogP (Just . f') where
    f' (i, r) = case pat (i, r) of
        Nothing -> (i, r, [])
        Just (i', r', a) -> (i'', r'', a:a') where
            (i'', r'', a') = f' (i', r')

parseFullLine p = parseLine $ p <* AP.endOfInput

-- tests
rn = ss $ AP.string "0x" *> AP.hexadecimal

t1 :: LogP [Integer]
t1 = do
    parseFullLine $ AP.string "ELF Header:"
    skipUntilMatch $ parseLine $ AP.string "Dynamic section at offset"
    skip 1
    many $ parseLine rn

--space separated record
ss p = AP.skipSpace *> p <* AP.skipSpace  

parseElfHeaderRecord = do
    AP.skipSpace
    k <- AP.takeTill ((==) ':')
    AP.char ':'
    AP.skipSpace
    v <- AP.takeByteString
    return $ (BS.unpack k, BS.unpack v)

main = do
    log <- BS.lines <$> BS.readFile "log.txt"
    let (Just (n, _, x)) = runLogP t1 (0, log)
    print x
    print n
