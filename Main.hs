module Main where

  ----------------------------------------------------------------------------------------
  -- Specification:                                                                     --
  -- 1. Input format is displayed UTF-8 encoded text.                                   --
  -- 2. All punctuation that is not part of a word should be diregarded                 --
  -- 3. Frequency bar for each program should start on the same column                  --
  -- 4. A line should not be longer than 80 characters (size your bars appropriately)   --
  -- 5. A linear scale should be used                                                   --
  -- 6. A word with a bar length of 0 should not be printed                             --
  ----------------------------------------------------------------------------------------

import Data.Char    
import Data.Map as M hiding (map, filter)
import Data.List as L
import Control.Exception as E (catch)
import Control.Monad
import System.IO
import System.IO.Error        
import System.Directory        
import Control.Applicative        
import System.Environment

-- |Checks the first and last characters of a string for punctuation and removes them
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['\'']

clean :: [String] -> [String] 
clean [] = []
clean ([]:xs) = clean xs
clean ([x]:xs) = case x `elem` alphabet of
                   True -> [x] : clean xs
                   False -> clean xs
clean (x:xs) 
    | badhead && badtail = (tail . init) x : clean xs
    | badhead = tail x : clean xs
    | badtail = init x : clean xs
    | otherwise = x : clean xs
    where badhead = head x `notElem` alphabet
          badtail = last x `notElem` alphabet

-- |Maps to lower across a list of strings (frequencies are case-insensitive)                    
makeLower :: [String] -> [String]
makeLower = map (map toLower)
                     
-- |Passes in an empty map to be populated
toMap :: [String] -> Map String Int
toMap xs = trans xs M.empty 

-- |Wrapper function for addCount
trans :: [String] -> Map String Int -> Map String Int
trans [] list = list
trans (x:xs) list = trans xs $ addCount ((clean . makeLower . words) x) list 

-- |Adds occurrences of words to an immutable map
addCount :: [String] -> Map String Int -> Map String Int
addCount [] list = list
addCount (x:xs) list = case M.lookup x list of
    Nothing -> addCount xs $ M.insert x 1 list
    Just (val) -> addCount xs $ M.update inc x list where
      inc num = Just num >>= \x -> Just (x+1)

-- |Builds string output from map, calculates proper spacing
printMap :: Map String Int -> String
printMap m = M.foldWithKey f id m "" where
    longestkey key = ((getMaxKey m) - (length key))
    longestval = (maximum $ map snd $ M.toList m) + (getMaxKey m)
    linearscale = (longestval `div` 80) + 1
    f :: String -> Int -> (String -> String) -> (String -> String)
    f key val r = r . ((key ++ (concat $ replicate (longestkey key) " ") ++ "  " ++
                        (replicate (val `div` linearscale) '#') ++ "\n") ++)

-- |Helper function to retrieve the maximum key (largest word) in the map                  
getMaxKey:: Map String Int -> Int                  
getMaxKey m = maximum $ map (length . fst) (M.toList m)                  

-- |Output should be in descending order by highest frequency
sorter :: [Char] -> [Char] -> Ordering
sorter a b = compare (len b) (len a) where
    len = (length . filter (=='#')) 

-- |Main method wrapper
main :: IO()          
main = E.catch toTry handler
                    
-- |Main method body
toTry :: IO() 
toTry = do
  (file:xs) <- getArgs
  contents <- readFile file
  mapM_ putStrLn $ (sortBy sorter . filter ('#' `elem`) . lines . printMap . toMap) [contents]

-- |Error handler  
handler :: IOError -> IO ()
handler e = putStrLn "That file does not exist!"
