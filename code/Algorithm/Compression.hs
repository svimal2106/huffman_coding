module Algorithm.Compression
   (HuffmanTree(..)
   ,Bit(..)
   ,Code
   ,huffman
   ,codewords
   ) where



import Control.Arrow (first,second)
import qualified Algorithm.Queue.PrQueue as PQ


data Bit = Zero | One
	deriving Eq

instance Show Bit where
  show Zero = "0"
  show One  = "1"

data HuffmanTree a = Empty
                   | Node (HuffmanTree a) (HuffmanTree a)
                   | Leaf a
                   
show' Empty = " Empty "
show' (Leaf t) = " Leaf " ++ (show t)
show' (Node l r) = " Node (" ++ (show' l) ++") (" ++ (show' r) ++ ")"


instance (Show a)=>Show (HuffmanTree a)where
  show tree = show' tree
  


type Code a = [(a,[Bit])]

--prepare a prority queue from the input list
--preparequeue::(Ord w)=>[(a,w)] -> Priorityqueue w a
preparequeue = PQ.fromList. map (\(x,k) -> (k, Leaf x))

--build a huffman tree from the given priority queue
--buildtree::(Ord w,Num w)=>PQ.Priorityqueue w a->HuffmanTree a
buildtree pq = case PQ.minViewWithKey pq of
		    Nothing -> Empty
		    Just ((w,v),pq') -> case PQ.minViewWithKey pq' of
			                     Nothing -> v
			                     Just ((w',u),pq'') -> buildtree $PQ.insert ((w+w'),Node v u) pq''


--function  to build huffman tree
--huffman :: (Ord w, Num w) => [(a,w)] -> HuffmanTree a
huffman xs = buildtree  $preparequeue xs 


-- Derive the prefix-free binary code from a huffman tree.
codewords :: HuffmanTree a -> Code a
codewords = code' []
  where code' _    Empty      = []
        code' bits (Leaf x)   = [(x,bits)]
        code' bits (Node l r) = map (second (Zero:)) (code' bits l) ++
                                map (second (One:)) (code' bits r)

