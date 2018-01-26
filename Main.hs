module Main where

import           Data.List          (intercalate, isPrefixOf, sortBy)
import           Data.Map           (Map, empty, insertWith, toList)
import           System.Directory   (canonicalizePath, getHomeDirectory)
import           System.Environment (getArgs)
import           System.FilePath    (joinPath)

-- | Converts .bash_history or similar to list of commands
-- Commands are retrieved by taking first n words of a line
parseHistory :: Int -> String -> [String]
parseHistory n = map (unwords . take n . words) . lines

-- | Map commands list into list of sequences
sequencify :: Int -> [a] -> [[a]]
sequencify _ []          = []
sequencify n list@(_:xs) = if n <= length list
                                       then take n list : sequencify n xs
                                       else []

-- | Remove sequences where all elements are the same command
-- We don't care about these; they are noise
-- Don't filter single-length sequences
removeDupeSequences :: Eq a => [[a]] -> [[a]]
removeDupeSequences = filter (not . allEqual)
    where
        allEqual []     = True
        allEqual [_]    = False
        allEqual (x:xs) = all (== x) xs

-- | Counts occurrences of sequences
sequencesToCountMap :: Ord a => [[a]] -> Map [a] Int
sequencesToCountMap = foldr (\s m -> insertWith (+) s 1 m) empty

-- | Transform count to list sorted by occurrence
countMapToSortedList :: Map [a] Int -> [([a], Int)]
countMapToSortedList = sortBy(\(_, a) (_, b) -> compare b a) . toList

-- | Remove sequences that occur less than n times
-- These are also noise
filterLowCounts :: Int -> [([a], Int)] -> [([a], Int)]
filterLowCounts count = filter (\(_, n) -> n > count)

-- | Pretty-prints sorted list of sequences
prettyPrint :: [([String], Int)] -> String
prettyPrint = intercalate "\n" . map (\(s, n) -> intercalate " > " s ++ ": " ++ show n)

-- | Final composed function
countCommonSequences :: Int -> String -> String
countCommonSequences runLength = prettyPrint . filterLowCounts 1 . countMapToSortedList . sequencesToCountMap . removeDupeSequences . sequencify runLength . parseHistory 2

-- | Helper to resolve paths with tildes
-- Is there really not a library for this?
makeAbsolute :: String -> IO FilePath
makeAbsolute path = do
    homeDir <- getHomeDirectory
    if "~/" `isPrefixOf` path
        then return (joinPath [homeDir, drop 2 path])
        else return path

main :: IO ()
main = do
    -- TODO replace this with command line flag
    -- TODO make filterLowCounts and parseHistory also configurable via flags
    [path, runLength] <- getArgs
    file <- readFile =<< canonicalizePath =<< makeAbsolute path
    putStrLn $ countCommonSequences (read runLength) file
