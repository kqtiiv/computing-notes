{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
import Test.Tasty.Bench ( defaultMain, Benchmark, bgroup, bench, nf, env )
import GHC.Generics
import Control.DeepSeq

import Revision

main :: IO ()
main = defaultMain [benchmarks]

benchmarks :: Benchmark
benchmarks =  env @([[Int]]) (pure ()) (\ns ->
    bgroup "Solutions" [
        bench "1st" $ nf (unions''' ns) ,
        bench "2nd" $ nf (unions' ns) , 
        bench "3rd" $ nf (unions'' ns) 
    ])


deriving instance Generic Expr
deriving instance NFData Expr
deriving instance Generic Op
deriving instance NFData Op