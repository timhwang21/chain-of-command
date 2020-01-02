module Main where

import           Data.Char                      ( isSpace )
import           Data.Function                  ( on )
import           Data.List                      ( intercalate
                                                , sortBy
                                                )
import           Data.Map                       ( Map
                                                , empty
                                                , insertWith
                                                , toList
                                                )
import           Data.Ord                       ( Down(..) )
import           System.Console.GetOpt
import           System.Directory               ( canonicalizePath )
import           System.Environment             ( getArgs )
import           System.Exit                    ( die
                                                , exitSuccess
                                                )

data Options = Options { optFile      :: String
                       , optMinCount  :: Int
                       , optRunLength :: Int
                       , optWordCount :: Int
                       }

defaultOptions :: Options
defaultOptions =
  Options { optFile = "", optMinCount = 5, optRunLength = 2, optWordCount = 2 }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['f']
           ["file"]
           (ReqArg (\arg opt -> opt { optFile = arg }) "PATH")
           "Path to history file to analyze"
  , Option ['m']
           ["minCount"]
           (ReqArg (\arg opt -> opt { optMinCount = read arg }) "N")
           "Minimum number of matches needed to be displayed"
  , Option ['r']
           ["runLength"]
           (ReqArg (\arg opt -> opt { optRunLength = read arg }) "N")
           "Number of commands to use for sequences"
  , Option ['w']
           ["wordCount"]
           (ReqArg (\arg opt -> opt { optWordCount = read arg }) "N")
           "Number of words taken from commands"
  ]

exitWithError :: String -> IO a
exitWithError m = die (m ++ usageInfo "Usage: coc [options] [input]" options)

-- | Translates argv into an `Option` record
parseOptions :: [String] -> IO Options
parseOptions argv = case getOpt RequireOrder options argv of
  (actions, _, []    ) -> return (foldl (flip id) defaultOptions actions)
  (_      , _, errors) -> exitWithError $ concat errors

-- | Converts .bash_history or similar to list of commands
-- Commands are retrieved by taking first n words of a line
parseHistory :: Int -> String -> [String]
parseHistory n = map (unwords . take n . words) . lines

-- | Remove blank entries from history
filterWhitespace :: [String] -> [String]
filterWhitespace = filter (not . all isSpace)

-- | Map commands list into list of sequences
sequencify :: Int -> [a] -> [[a]]
sequencify _ [] = []
sequencify n list@(_ : xs) =
  if n <= length list then take n list : sequencify n xs else []

-- | Remove sequences where all elements are the same command
-- We don't care about these; they are noise
-- Don't filter single-length sequences
removeDupeSequences :: Eq a => [[a]] -> [[a]]
removeDupeSequences = filter (not . allEqual)
 where
  allEqual []       = True
  allEqual [_     ] = False
  allEqual (x : xs) = all (== x) xs

-- | Counts occurrences of sequences
sequencesToCountMap :: Ord a => [[a]] -> Map [a] Int
sequencesToCountMap = foldr (\s m -> insertWith (+) s 1 m) empty

-- | Transform count to list sorted by occurrence
countMapToSortedList :: Map [a] Int -> [([a], Int)]
countMapToSortedList = sortBy (compare `on` Down . snd) . toList

-- | Remove sequences that occur less than n times
-- These are also noise
filterLowCounts :: Int -> [([a], Int)] -> [([a], Int)]
filterLowCounts count = filter ((>= count) . snd)

-- | Pretty-prints sorted list of sequences
prettyPrint :: [([String], Int)] -> String
prettyPrint = intercalate "\n"
  . map (\(s, n) -> intercalate " → " (map padRight s) ++ ": " ++ show n)
 where
  padRight s = if length s >= maxLen
    then take (maxLen - 1) s ++ "…"
    else take maxLen $ s ++ repeat ' '
    where maxLen = 16

-- | Final composed function
countCommonSequences :: Options -> String -> String
countCommonSequences Options { optMinCount = minCount, optRunLength = runLength, optWordCount = wordCount }
  = prettyPrint
    . filterLowCounts minCount
    . countMapToSortedList
    . sequencesToCountMap
    . removeDupeSequences
    . sequencify runLength
    . filterWhitespace
    . parseHistory wordCount

main :: IO ()
main = do
  opts <- getArgs >>= parseOptions
  file <- if null $ optFile opts
    then getContents
    else (canonicalizePath . optFile) opts >>= readFile
  putStrLn $ countCommonSequences opts file
  exitSuccess
