import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Control.Monad (foldM)
import Data.Set as S



dijkstra src graphnds [] finalweights = finalweights
dijkstra src graphnds tovisit finalweights = finalweights


gethead (x:xs) = x
gettail (x:xs) = xs

removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

henk d = x * y + c^d 
       where 
       x = 5 * y
       y = c * c
       c = d - 1

graphnds :: Array Char [(Char, Int)]
graphnds = listArray ('a', 'n') [ [('b',2), ('e',2), ('h',2), ('k',2)], --A
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