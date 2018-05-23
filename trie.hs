import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show, Eq)
data Action = Add | Search | Prefix | Print | Write | Exit deriving (Eq)
type Word = String

empty :: Trie
empty = Trie False M.empty

insert :: Word -> Trie -> Trie
insert []     (Trie _ m) = Trie True m
insert (c:cs) (Trie e m) = Trie e $ M.insert c (insert cs t) m
   where
   t :: Trie -- t is the trie where the rest of the characters will be added
             -- if the current character is not added before, we will create empty trie
   t = fromMaybe empty $ M.lookup c m
-- these statements are equal to the statement above
--   t = case M.lookup c m of
--      Just t' -> t'
--      Nothing -> empty

insertList :: [Word] -> Trie
insertList ss = foldr insert empty ss -- `ss` can be eliminated
-- foldl also can be used but we need lambda function for that

search :: Word -> Trie -> Bool
search []     (Trie e _) = e
search (c:cs) (Trie e m) = maybe False (search cs) $ M.lookup c m
-- these statements are equal to the statement above
--search (c:cs) (Trie e m) = case M.lookup c m of
--   Just m' -> search cs m'
--   Nothing -> False 

getWords :: Trie -> [Word]
getWords (Trie e m) = traverse (M.toList m) "" []
   where
   traverse :: [(Char, Trie)] -> String -> [String] -> [String]
                           -- traversing ->  horizontally                    vertically
   traverse [] _ acc = acc --              ---------------          --------------------------------
   traverse ((c, (Trie e m')):kvs) w acc = traverse kvs w (acc' ++ (traverse (M.toList m') (w ++ [c]) []))
      where
      -- this is the word accumulator
      acc' = if e then (w ++ [c]):acc else acc

prefix :: String -> Trie -> Maybe [Word]
prefix p t = prefix' p t -- we need this to preserve the actual prefix value
   where
   prefix' :: String -> Trie -> Maybe [String] 
   prefix' [] t' = Just $ map (\w -> p ++ w) $ getWords t'
   prefix' (c:cs) (Trie e m) = maybe Nothing (prefix' cs) $ M.lookup c m
-- these statements are equal to the statement above
--   prefix' (c:cs) (Trie e m) = case M.lookup c m of
--      Just m' -> prefix' cs m'
--      Nothing -> Nothing

searchIO :: Word -> Trie -> IO ()
searchIO w t
   | search w t = putStrLn "Exists in dictionary!\n"
   | otherwise  = putStrLn "NOT exist!\n"

prefixIO :: Word -> Trie -> IO ()
prefixIO w t = case prefix w t of
   Just ws -> putStrLn $ "Found words:\n" ++ (unlines ws)
   Nothing -> putStrLn $ "No words found with that prefix!\n"

getWordsIO :: Trie -> IO ()
getWordsIO t = putStrLn $ "List of words in dictionary:\n" ++ (unlines $ getWords t)

doAction :: Action -> Word -> Trie -> IO ()
doAction a w t = case a of
   Search -> searchIO w t
   Prefix -> prefixIO w t
   Print  -> getWordsIO t

-- we could include Add action to here but I think it is more suited to Haskell this way 
-- since it is not good practice to perform IO action and pure function evaluation

convertAction :: Char -> Action
convertAction s
   | s `elem` "aA" = Add
   | s `elem` "sS" = Search
   | s `elem` "fF" = Prefix
   | s `elem` "pP" = Print
   | s `elem` "eE" = Exit
   | otherwise     = error "Unknown input"

printMenu :: IO ()
printMenu = do
            putStrLn "a) Add Word"
            putStrLn "s) Search Word"
            putStrLn "f) Find words with prefix"
            putStrLn "p) Print all words"
            putStrLn "e) Exit"
            

getInput :: IO (Action, String)
getInput = do
           putStrLn "Enter the action: " -- if we use putStr, it won't flush to stdout
           choice <- getLine
           let action = convertAction (choice !! 0) -- no need to put deep checks
           if action /= Print && action /= Exit
              then do putStrLn "Enter word/prefix: "
                      word <- getLine
                      return (action, word)
              else do
                 return (action, "")

mainLoop :: Trie -> IO ()
mainLoop trie = do 
                printMenu
                (action, word) <- getInput
                if action == Add
                   then do 
                        let trie' = insert word trie -- `let trie = insert word trie` won't work
                        putStrLn "New word is added!"
                        mainLoop trie'
                   else if action == Exit
                   then do 
                        return ()
                   else do
                        doAction action word trie  
                        mainLoop trie

       

main = do
       args <- getArgs 
       content <- readFile (args !! 0) -- no need to put deep checks
       let words = lines content
       let trie = insertList words -- can be merged with line above, but I think this is more readable
       mainLoop trie
