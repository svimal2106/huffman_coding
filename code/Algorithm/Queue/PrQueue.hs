module Algorithm.Queue.PrQueue 
(Priorityqueue(..)
,empty
,singleton
,inspectmin
,merge
,insert
,minViewWithKey
,test
,fromList
) where

 --priority queue data type

data Ord k => Priorityqueue k a = Null
                                | Node k a (Priorityqueue k a) (Priorityqueue k a) 
                                
--show instance for priority queue

instance (Ord k,Show k,Show a)=>Show (Priorityqueue k a) where
  show t = show' t
  
--show'::(Ord k,Show k,Show a)=>(Priorityqueue k a)->String

show' Null = "Null"
show' (Node p v Null rt) = "Node " ++ show p ++ " " ++ show v ++ " (Null"++") (" ++ (show' rt)++")"
show' (Node p v lt Null)= "Node " ++ show p ++ " " ++ show v ++ " (" ++ (show' lt) ++ ") (Null)" 
show' (Node p v lt rt) = "Node " ++ show p ++ " " ++ show v ++ " (" ++ (show' lt) ++ ") (" ++ (show' rt)++")"


--return an empty priority queue
empty::Ord k => Priorityqueue k a
empty = Null


--return a singleton priority queue
singleton::Ord k =>(k,a)->Priorityqueue k a
singleton (p,v) = Node p v Null Null

--inspect the minpriority element
inspectmin::Ord k => (Priorityqueue k a)->(k,a)
inspectmin Null = error "queue is empty"
inspectmin (Node p v _ _) = (p,v)

--helper merge function
--helpermerge::Ord k  => (Priorityqueue k a)->(Priorityqueue k a)->(Priorityqueue k a)
--helpermerge (Node p v Null rt) t = Node p v t rt
--helpermerge (Node p v lt rt) t = Node p v rt (merge rt t)

--merge function to merge two priority queues
merge::Ord k => (Priorityqueue k a)->(Priorityqueue k a)->(Priorityqueue k a)
merge l Null = l
merge Null r = r
merge l@(Node p1 v1 lt rt) r@(Node p2 v2 lt' rt') | p1<=p2 = Node p1 v1 (merge r rt) lt
					    | otherwise = Node p2 v2 (merge l rt') lt'
			    

--insert a (key,value) pair 
insert::Ord k =>(k,a)->(Priorityqueue k a)->(Priorityqueue k a)
insert (p,v) q = merge (singleton (p,v)) q 


--Extract the minimum priority element along with the key
minViewWithKey::(Ord k)=>Priorityqueue k a->Maybe ((k,a),Priorityqueue k a)
minViewWithKey Null = Nothing
minViewWithKey (Node p v lt rt) = Just ((p,v),(merge lt rt)) 


--Initialize a priority queue from a list
fromList::Ord k=>[(k,a)]->Priorityqueue k a
fromList xs = foldl (\acc x -> merge acc x) Null nodes
  where nodes = map (\(x,y) -> Node x y Null Null) xs
	 

--test function 
test = foldl (\acc x->merge acc x) Null nodes
  where nodes = map (\(x,y)-> Node x y  Null Null) [(3,'a'),(5,'b'),(1,'c'),(9,'d'),(7,'e'),(2,'f')]
	
-- check queue@(Node p v _ _) = do (node,q) <- minViewWithKey queue
-- 		                   case node of
-- 	                                Nothing -> "Nothing"
-- 	                                Just (a,b) -> "(" ++ show a ++ show b ++ ")" ++ check q
-- 
