module One(change) where
import Data.Array 

type Pence = Int


change :: Pence -> Int 
change g = memo ! g
    where
        memo = tabulate (0, g) change'
        change' :: Pence -> Int
        change' 0 = 0
        change' g = minimum [memo | (g-coin) + 1 | coin <- coins, coin <= g]