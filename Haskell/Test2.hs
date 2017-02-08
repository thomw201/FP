shortestpad start end graph = enhance [(start,0)] end graph

enhance collection end graph = if enddistances /= []
							     then head enddistances
                                 else enhance collection1 end graph
                                     where 
									 enddistances = [a | (x,a) <- collection, let x = end]
	                                 neighbours = [(y,a1+a2) | (x,a1) <- collection, (x1,list) <- graph,  x1 = x, (y,a2) <- list, notElem y collection ]
	                                 collection1 = collection ++ [(x,a)|(x,a)<-neighbours; a = shortestdistance]
	                                 shortestdistance = minimum [a| (x,a) <- neighbours]