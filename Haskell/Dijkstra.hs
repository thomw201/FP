import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Set as S
 
patternMatch :: String -> String
patternMatch "Lol" = "Kek"
patternMatch x = "Bleh"

--shortestpad start end graph path = findpath [(start,0)] end graph
--findpath visited '?' []

dijkstra :: (Ix v, Num w, Ord w, Bounded w) => v -> v -> Array v [(v,w)] -> (Array v w, Array v v)
dijkstra src invalid_index adj_list = runST $ do

  min_distance <- newSTArray b maxBound
  writeArray min_distance src 0
  previous <- newSTArray b invalid_index
  let aux vertex_queue =
        case S.minView vertex_queue of
          Nothing -> return ()
          Just ((dist, u), vertex_queue') ->
            let edges = adj_list ! u
                f vertex_queue (v, weight) = do
                  let dist_thru_u = dist + weight
                  old_dist <- readArray min_distance v
                  if dist_thru_u >= old_dist then
                    return vertex_queue
                  else do
                    let vertex_queue' = S.delete (old_dist, v) vertex_queue
                    writeArray min_distance v dist_thru_u
                    writeArray previous v u
                    return $ S.insert (dist_thru_u, v) vertex_queue'
            in
            foldM f vertex_queue' edges >>= aux
  aux (S.singleton (0, src))
  m <- freeze min_distance
  p <- freeze previous
  return (m, p)
  where b = bounds adj_list
        newSTArray :: Ix i => (i,i) -> e -> ST s (STArray s i e)
        newSTArray = newArray

shortest_path_to :: (Ix v) => v -> v -> Array v v -> [v]
shortest_path_to target invalid_index previous =
  aux target [] where
    aux vertex acc | vertex == invalid_index = acc
                   | otherwise = aux (previous ! vertex) (vertex : acc)

adj_list :: Array Char [(Char, Int)]
adj_list = listArray ('a', 'n') [ [('b',2), ('e',2), ('h',2), ('k',2)], --A
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

