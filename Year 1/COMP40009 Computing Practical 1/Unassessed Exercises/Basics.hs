module Basics(twelveTimesTable, secondsToHMS, pairOfNum) where

-- :i, :browse, :docs in ghci for more info

{-
 Using div and mod (infix or prefix) write an expression which converts time (an integer called s,
 say), representing the number of seconds since midnight, into a triple of integers representing the
 number of (whole) hours, minutes and seconds since midnight. Use a let expression for the specific
 case where s=8473, viz. let s = 8473 in ....
-}

secondsToHMS :: Int -> (Int, Int, Int)
secondsToHMS s =
    let hours = s `div` 3600
        minutes = s `mod` 3600 `div` 60
        seconds = s `mod` 60
    in (hours, minutes, seconds)

{-
The “times tables” for the numbers 2 to 12. The list should comprise triples of the form
 (𝑚,𝑛,𝑚 ∗𝑛),2 ≤ 𝑚,𝑛 ≤ 12.
-}

twelveTimesTable :: [(Int, Int, Int)]
twelveTimesTable = [(m, n, m * n) | m <- [2..12], n <- [2..12]]

{-
The list of all pairs of numbers (𝑚,𝑛),𝑚 < 𝑛, between 1 and 100 inclusive, whose sum is the
 same as the square of their absolute difference.
-}

pairOfNum :: [(Int, Int)]
pairOfNum =  [(m, n) | m <- [1..100]
                    , n <- [1..100]
                    , m < n
                    , (m + n) == (m- n) ^ 2]