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

graph1 = [('a', [('b',2), ('e',2), ('h',2), ('k',2)]), 
         ('b', [('a',2), ('c',4), ('h',1), ('i',2)]), 
         ('c', [('b',4), ('d',3), ('i',2), ('j',4)]), 
         ('d', [('c',3), ('j',2), ('n',1)]), 
         ('e', [('a',2), ('f',3), ('k',2)]), 
         ('f', [('e',3), ('g',4), ('k',2), ('l',1), ('m',3)]), 
         ('g', [('f',4), ('m',1), ('n',1)]), 
         ('h', [('a',2), ('b',1), ('i',4), ('k',2)]), 
         ('i', [('b',2), ('c',2), ('h',4), ('j',5), ('k',3), ('l',1)]), 
         ('j', [('c',4), ('d',2), ('i',5), ('l',2), ('m',2), ('n',1)]), 
         ('k', [('a',2), ('e',3), ('f',2), ('h',2), ('i',3), ('l',4)]), 
         ('l', [('f',1), ('i',1), ('j',2), ('k',4), ('m',3)]), 
         ('m', [('f',3), ('g',1), ('j',2), ('l',3), ('n',3)]), 
         ('n', [('d',1), ('g',1), ('j',1), ('m',3)])]

--[('a',0),('b',2),('e',2),('h',2),('k',2),('i',4),('f',4),('l',5),('l',5),('c',6),('c',6),('m',7),('j',7),('j',7),('g',8),('g',8),('n',8),('n',8)]
--a k f l j n

-- :: IO ()
--main = do
  --let distance = getpath 'a' 'n' graph1
  --putStrLn $ "Distance from a to n: " ++ show distance