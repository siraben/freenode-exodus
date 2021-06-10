{-# LANGUAGE TupleSections #-}

import Control.Arrow (Arrow ((***)))
import Control.Exception ()
import Control.Monad (unless)
import Data.Char (toLower)
import Data.Function ()
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import System.Exit (die)
import Text.ParserCombinators.Parsec
  ( Parser,
    between,
    char,
    choice,
    many1,
    noneOf,
    parse,
    spaces,
  )

projectName :: Parser String
projectName = many1 (noneOf "]")

cited :: Parser a -> Parser a
cited = between (char '[') (char ']')

parseChanName :: Parser String
parseChanName = do
  char '-' <* spaces
  map toLower . dropWhile (== '#') <$> choice parsers
  where
    parsers :: [Parser String]
    parsers = [cited, id] <*> [projectName]

freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)

main :: IO ()
main = do
  f <- lines <$> readFile "README.md"
  let (Right a, Right b) = (g *** g) (break ("##" `isPrefixOf`) f)
      g = traverse (parse parseChanName "") . filter ("-" `isPrefixOf`)
      ma = freqs a
      mb = freqs b
      (as, bs) = (M.keys ma, M.keys mb)
      dupsa = M.filter (/= 1) ma
      dupsb = M.filter (/= 1) mb
  unless (M.null dupsa) (die $ "The following channels are duplicated in the leave list: " ++ show dupsa)
  unless (M.null dupsb) (die $ "The following channels are duplicated in the stay list: " ++ show dupsb)
  writeFile "a.txt" (unlines a)
  writeFile "a-sorted.txt" (unlines as)
  writeFile "b.txt" (unlines b)
  writeFile "b-sorted.txt" (unlines bs)
  putStrLn $ "Channels in leave list: " ++ show (M.size ma)
  putStrLn $ "Channels in stay list: " ++ show (M.size mb)
