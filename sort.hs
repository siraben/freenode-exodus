{-# LANGUAGE TupleSections #-}

import Control.Arrow (Arrow ((***)))
import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as M
import System.Exit (die)
import Text.ParserCombinators.Parsec

-- | Parse a project name until the first right square bracket
projectName :: Parser String
projectName = many1 (noneOf "]")

-- | Parse a list item
parseItem :: Parser String
parseItem = do
  char '-' <* spaces
  map toLower . dropWhile (== '#') <$> choice parsers
  where
    -- All ways to parse a project name, cited or not.
    parsers :: [Parser String]
    parsers = [cited, id] <*> [projectName]
    cited :: Parser a -> Parser a
    cited = between (char '[') (char ']')

-- | Build a frequency table from a list
freqs :: Ord a => [a] -> Map a Int
freqs = M.fromListWith (+) . map (,1)

main :: IO ()
main = do
  f <- lines <$> readFile "README.md"
  let (Right a, Right b) = (g *** g) (break ("##" `isPrefixOf`) f)
      g = traverse (parse parseItem "") . filter ("-" `isPrefixOf`)
      (ma, mb) = (freqs a, freqs b)
      -- as and bs are sorted since M.keys returns them in ascending order
      (as, bs) = (M.keys ma, M.keys mb)
      (dupsa, dupsb) = (M.filter (/= 1) ma, M.filter (/= 1) mb)
  -- Check duplication
  unless (M.null dupsa) (die $ "The following channels are duplicated in the leave list: " ++ show dupsa)
  unless (M.null dupsb) (die $ "The following channels are duplicated in the stay list: " ++ show dupsb)
  -- Write lists to files (to be checked with git diff later)
  writeFile "a.txt" (unlines a)
  writeFile "a-sorted.txt" (unlines as)
  writeFile "b.txt" (unlines b)
  writeFile "b-sorted.txt" (unlines bs)
  -- Report statistics
  putStrLn $ "Channels in leave list: " ++ show (M.size ma)
  putStrLn $ "Channels in stay list: " ++ show (M.size mb)
