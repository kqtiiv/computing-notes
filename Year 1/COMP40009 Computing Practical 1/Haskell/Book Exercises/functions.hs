module Functions where 

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

safetail :: [a] -> [a]
-- safetail xs = if null xs then [] else tail xs
-- safetail xs 
--     | null xs = []
--     | otherwise = tail xs 
safetail [] = []
safetail xs = tail xs 

mult :: Int -> Int -> Int -> Int 
mult = \x y z -> x*y*z
