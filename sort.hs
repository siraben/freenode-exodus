import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit
import System.IO
import Text.ParserCombinators.Parsec (Parser)
import qualified Text.ParserCombinators.Parsec as P

parseChanName :: Parser String
parseChanName = do
  P.char '-' <* P.spaces
  map toLower . dropWhile (== '#') <$> P.choice parsers

-- ircNoCite = P.skipMany1 (P.char '#') *> P.many1 P.alphaNum

projectName :: Parser String
projectName = P.many1 (P.noneOf "]")

cited :: Parser a -> Parser a
cited = P.between (P.char '[') (P.char ']')

parsers = [cited, id] <*> [projectName]

main = do
  f <- lines <$> readFile "README.md"
  let (Right a, Right b) = (first init >>> g *** g) (break ("##" `isPrefixOf`) f)
      g = traverse (P.parse parseChanName "") . dropWhile (not . ("-" `isPrefixOf`))
  -- isSorted = and . (zipWith (<) `ap` tail)

  -- l' = map (dropWhile (/= '-')) [a,b]
  -- print l'
  -- print $ length l'
  -- map (P.parse parseChanName "") .
  -- print (unlines a)
  -- print (isSorted a, isSorted b)
  writeFile "a.txt" (unlines a)
  writeFile "a-sorted.txt" (unlines (sort a))
  pure ()
