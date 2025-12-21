module UsefulFuncs where 

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S 
import Data.List 

groupBy' :: forall a k. Ord k => (a -> k) -> [a] -> Map k [a]
-- groupBy' f [] = M.empty
-- groupBy' f (x:xs) = M.insertWith (++) (f x) [x] m
--     where m = groupBy' f xs 

groupBy' f = foldr go (M.empty) 
    where 
        go :: a -> Map k [a] -> Map k [a] 
        go x m = M.insertWith (++) (f x) [x] m
