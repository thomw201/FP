{-# LANGUAGE OverloadedStrings #-}
import Data.Set as Set
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Text.Printf
import Control.Exception
import Control.Monad.ST
import Control.Monad (foldM)
import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock


dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v,w)] -> (Array v w, Array v v)
dijkstra startNode invalid_index adj_list = runST $ do

  minimumDistance <- newNodesArray nodes maxBound
  writeArray minimumDistance startNode 0
  prevList <- newNodesArray nodes invalid_index
  let aux vertList =
        case Set.minView vertList of
          Nothing -> return ()
          Just ((dist, curNode), vertList') ->
            let pathes = adj_list ! curNode
                f vertList (nextNode, weight) = do
                  let newDist = dist + weight
                  old_dist <- readArray minimumDistance nextNode
                  if newDist >= old_dist then
                    return vertList
                  else do
                    let vertList' = Set.delete (old_dist, nextNode) vertList
                    writeArray minimumDistance nextNode newDist
                    writeArray prevList nextNode curNode
                    return $ Set.insert (newDist, nextNode) vertList'
            in
            foldM f vertList' pathes >>= aux
  aux (Set.singleton (0, startNode))
  m <- freeze minimumDistance
  p <- freeze prevList
  return (m, p)
  where nodes = bounds adj_list
        newNodesArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newNodesArray = newArray

getPath :: (Ix v) => v -> v -> Array v v -> [v]
getPath target invalid_index prevList =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (prevList ! vertex) (vertex : acc)

graph1 :: Array Char [(Char, Int)]
graph1 = listArray ('a', 'n') [ [('b',2), ('e',2), ('h',2), ('k',2)], --A
                                  [('a',2), ('c',4), ('h',1), ('i',2)], --B
                                  [('b',4), ('d',3), ('i',2), ('j',4)], --C
                                  [('c',3), ('j',2), ('n',1)], --D
                                  [('a',2), ('f',3), ('k',2)], --E
                                  [('e',3), ('g',4), ('k',2), ('l',1), ('m',3)], --F
                                  [('f',4), ('m',1), ('n',1)], --G
                                  [('a',2), ('b',1), ('i',4), ('k',2)], --H
                                  [('b',2), ('c',2), ('h',4), ('j',5), ('k',3), ('l',1)], --I
                                  [('c',4), ('d',2), ('i',5), ('l',2), ('m',2), ('n',1)], --J
                                  [('a',2), ('e',3), ('f',2), ('h',2), ('i',3), ('l',4)], --K
                                  [('f',1), ('i',1), ('j',2), ('k',4), ('m',3)], --L								 
                                  [('f',3), ('g',1), ('j',2), ('l',3), ('n',3)], --M
                                  [('d',1), ('g',1), ('j',1), ('m',3)] ] --N

graph2 :: Array Char [(Char, Int)]
graph2 = listArray ('a', 'f') [[('b',2),('c',3)], --A
                               [('a',2),('c',3),('d',2),('e',2)], --B
                               [('a',2),('b',3),('e',2)], --C
                               [('b',2),('e',1),('f',4)], --D	
                               [('b',2),('c',2),('d',1),('f',3)], --E
                               [('d',4),('e',3)]] --F							  

main :: IO ()
main = do
  start <- getTime Monotonic
  let (minimumDistance, prevList) = dijkstra 'a' ' ' graph2
  putStrLn $ "Distance from a to f: " ++ show (minimumDistance ! 'f')
  let path = getPath 'f' ' ' prevList
  putStrLn $ "Path: " ++ show path
  end   <- getTime Monotonic
  fprint (timeSpecs % "\n") start end
  start <- getTime Monotonic
  let (minimumDistance, prevList) = dijkstra 'a' ' ' graph1
  putStrLn $ "Distance from a to n: " ++ show (minimumDistance ! 'n')
  let path = getPath 'n' ' ' prevList
  putStrLn $ "Path: " ++ show path
  end   <- getTime Monotonic
  fprint (timeSpecs % "\n") start end