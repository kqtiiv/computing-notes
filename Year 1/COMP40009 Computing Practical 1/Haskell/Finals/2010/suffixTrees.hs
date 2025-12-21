module SuffixTree where 
import Data.List (isPrefixOf, find)

data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix = isPrefixOf

removePrefix :: String -> String -> String
-- removePrefix "" s = s 
-- removePrefix (p:ps) (s:ss) = removePrefix ps ss 

removePrefix p s = drop (length p) s

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s = s:(suffixes (tail s))

isSubstring :: String -> String -> Bool
isSubstring sub s = any (isPrefix sub) (suffixes s)

findSubstrings :: String -> String -> [Int]
findSubstrings sub s = [ i | (i, suf) <- zip [0..] (suffixes s), isPrefix sub suf]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf x) = [x]
getIndices (Node xs) = concatMap (getIndices.snd) xs

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
-- partition s@(c:cs) s'@(c':cs') 
--     | c == c' = (c:pre, rem1, rem2)
--     | otherwise = ([], s, s')
--         where 
--             (pre, rem1, rem2) = partition cs cs'
-- partition s s' = ([],s,s')

partition xs ys = (pre, drop n xs, drop n ys)
    where 
        pre = map fst $ takeWhile (uncurry (==)) (zip xs ys)
        n = length pre 

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" t = getIndices t 
findSubstrings' sub (Node xs) = findSubstrings' rem1 st
    where 
        [(rem1, st)] = [ (rem1, st) | (suf, st) <- xs, let (pre, rem1, _) = partition sub suf, pre /= ""]    
findSubstrings' _ _ = []

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node []) = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t): ts)) 
    | pre == "" = Node ((a, t):ts')
    | pre == a = Node ((a, insert (rem1, n) t):ts)
    | otherwise = Node ((pre, Node [(rem1, Leaf n), (rem2, t)]):ts)
    where 
        (pre, rem1, rem2) = partition s a 
        Node ts' = insert (s, n) (Node ts)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring t = dfs t "" ""

dfs :: SuffixTree -> String -> String -> String 
dfs (Leaf _) acc sub = sub
dfs (Node ns) acc sub = longestStr $ map (\(s, n) -> dfs n (acc ++ s) acc) ns 

longestStr :: [String] -> String 
longestStr [] = ""
longestStr (x:xs) = if length x >= length x' then x else x'
    where x' = longestStr xs

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]