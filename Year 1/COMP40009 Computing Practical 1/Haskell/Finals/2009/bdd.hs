module BDD where 
import Data.List 
import Data.Maybe (fromJust)
import qualified Data.Map as M 
import Control.Monad.State 

type Index = Int

data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)

type Env = [(Index, Bool)]

type NodeId = Int

type BDDNode =  (NodeId, (Index, NodeId, NodeId))

type BDD = (NodeId, [BDDNode])

------------------------------------------------------
-- PART I

-- Pre: The item is in the given table
lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x t = head [y | (x', y) <- t, x==x']
-- lookUp x = fromJust . lookup x

checkSat :: BDD -> Env -> Bool
checkSat (root, ns) env = go root
    where 
        go 0 = False 
        go 1 = True
        go x = if lookUp x' env then go r else go l 
            where 
                (x', l, r) = lookUp x ns 


sat :: BDD -> [[(Index, Bool)]]
sat (root, ns) = go root
    where 
        go :: Index -> [[(Index, Bool)]]
        go 0 = []
        go 1 = [[]]
        go x = (map ((x', False):) (go l)) ++ (map ((x', True):) (go r))
            where 
                (x', l, r) = lookUp x ns 

------------------------------------------------------
-- PART II

simplify :: BExp -> BExp
simplify (Not (Prim x)) = Prim (not x)
simplify (Or (Prim x) (Prim y)) = Prim (x||y)
simplify (And (Prim x) (Prim y)) = Prim (x&&y)
simplify x = x

restrict :: BExp -> Index -> Bool -> BExp
restrict (Or l r) i b = simplify (Or (restrict l i b) (restrict r i b))
restrict (And l r) i b = simplify (And (restrict l i b) (restrict r i b))
restrict (Not x) i b = simplify (Not (restrict x i b))
restrict (IdRef x) i b = if x==i then Prim b else IdRef x
restrict x i b = x 

------------------------------------------------------
-- PART III

-- Pre: Each variable index in the BExp appears exactly once
--     in the Index list; there are no other elements
-- The question suggests the following definition (in terms of buildBDD')
-- but you are free to implement the function differently if you wish.
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs

-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' (Prim b) _ [] = (if b then 1 else 0, [])
buildBDD' e id (x:xs) = (id, (id, (x, i, i')):ns++ns')
    where 
        (i, ns) = buildBDD' (restrict e x False) (2*id) xs
        (i', ns') = buildBDD' (restrict e x True) (2*id + 1) xs

------------------------------------------------------
-- PART IV

type UniqueKey = (Index, NodeId, NodeId)

data BDDState = BDDState 
    { nextId :: NodeId
    , unique :: M.Map UniqueKey NodeId 
    , nodes :: [BDDNode]
    }

initState :: BDDState 
initState = BDDState 
    {
        nextId = 2,
        unique = M.empty,
        nodes = []
    }

-- make next node
mk :: Index -> NodeId -> NodeId -> State BDDState NodeId 
mk x l r 
    | l == r = return l
    | otherwise = do 
        st <- get 
        let key = (x, l, r) 
        case M.lookup key (unique st) of 
            Just nid -> return nid 
            Nothing -> do 
                let nid = nextId st 
                put st 
                    {
                        nextId = nid + 1,
                        unique = M.insert key nid (unique st),
                        nodes = (nid, (x, l, r)) : nodes st 
                    }
                return nid 

-- Pre: Each variable index in the BExp appears exactly once
--      in the Index list; there are no other elements
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD e xs = (root, nodes st)
    where
        (root, st) = runState (buildROBDD' e xs) initState


-- Potential helper function for buildBDD which you are free
-- to define/modify/ignore/delete/embed as you see fit.
buildROBDD' :: BExp -> [Index] -> State BDDState NodeId
buildROBDD' (Prim b) [] = return (if b then 1 else 0)
buildROBDD' e (x:xs) = do 
        l <- buildROBDD' (restrict e x False) xs -- left
        r <- buildROBDD' (restrict e x True) xs -- right
        mk x l r  

------------------------------------------------------
-- Examples for testing...

b1, b2, b3, b4, b5, b6, b7, b8 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)),
           (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])

