import Debug.Trace
import Control.Monad.ST
import Control.Monad (foldM)

shortestpath start end graph = enhance [(start,0)] end graph

enhance collection end graph = if not (null enddistances)
                                 then head enddistances
                                 else enhance collection1 end graph
                                     where {enddistances = [a | (x,a) <- collection1, x == end];
                                           neighbours = [(y,a1+a2) | (x,a1) <- collection, 
                                                         (x1,list) <- graph, 
                                                         x1 == x, 
                                                         (y,a2) <- list, 
                                                         notElem y [a|(a,b) <- collection] ]; 
                                           collection1 = collection ++ [(x,a)|(x,a)<-neighbours, a == minimum [b| (x,b) <- neighbours]]}         

--shortestpath 'a' 'c' [('a', [('b', 3), ('c', 6)]), ('b', [('a', 3), ('c', 1)]), ('c', [('a', 6), ('b', 1)])]