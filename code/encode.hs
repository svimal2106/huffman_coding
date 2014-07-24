import System.IO
import Algorithm.Compression(huffman,codewords,Bit(..),HuffmanTree(..))
import Maybe
import Data.Bits
import Data.Binary
import Data.Char
import qualified Data.ByteString as B
import System( getArgs )


list = [(' ',0)] 

main :: IO ()
main = do
	arg <- getArgs
	inh <- openFile (head arg) ReadMode
--tree is the huffman tree generated and codelist is list of the encoded bit pattern of each character used in the input file
	treefile<-openFile (head arg++".key") WriteMode
	(codelist,tree) <- huffmanencode inh list treefile 
	hClose inh
	inh<- openFile (head arg) ReadMode
	outh <- openFile (head arg++".cmp") WriteMode
	fileencode inh outh codelist
	putStrLn "Encoding complete."
	hClose outh
	hClose inh
--	f<-openFile "tree"WriteMode
--	writetree tree f		      

--writetree tree file = 
--	do  

fileencode :: Handle -> Handle -> [(Char, [Bit])] -> IO ()
fileencode inh outh codelist =
	do iseof <- hIsEOF inh
	   if iseof then return()
	            else do inp<-hGetContents inh
		            enc (encode1 codelist inp) outh
		   

enc :: [Bit] -> Handle -> IO ()
enc line outh |(length line)==0 = return()
                      |(length line) < 8= do let line' = line++[Zero]
                                             enc line' outh
                      | otherwise = do let word = take 8 line
                                       B.hPut outh (B.singleton $toByte word)
				       --B.appendFile outfile (B.singleton $toByte word)
                                       enc (drop 8 line) outh


huffmanencode inh list treeFile= 
	do iseof <- hIsEOF inh
	   if iseof
	    then do putStrLn "Frequency list:\n"
                    putStrLn $show list
                    putStrLn "\nCode list :\n"
                    putStrLn (show (codewords $huffman list))
                    putlist treeFile list
                    hClose treeFile
                    return ((codewords (huffman list)),(huffman list))
		    --return()
	    else do 
		    inp <-hGetChar inh
	    	    let list' = (insert1 inp list)
		    huffmanencode inh list' treeFile
		    --putStrLn (show list2)

putlist file xs | null xs = hPutStrLn file ""
            | otherwise = do hPrint file $(fst $head xs)
                             --hPutChar file ' '
                             hPrint file (snd $head xs)
                             putlist file $tail xs


insert1 :: (Num t, Eq a) => a -> [(a, t)] -> [(a, t)]
insert1 inp (x:xs) | fst x == inp = (inp,snd x + 1):xs
		   | null xs =  x:(inp,1):[]
	           | otherwise = x:(insert1 inp xs) 

toByte :: [Bit] -> Word8
toByte list = toByteh list 0 0

toByteh :: [Bit] -> Int -> Word8 -> Word8
toByteh list 8 result = result
toByteh list i result 
    | head list == One = toByteh (tail list) (i + 1) (result .|. (2^i :: Word8))
    | otherwise = toByteh (tail list) (i + 1) result
encode1 :: Eq a1 => [(a1, [a])] -> [a1] -> [a]
encode1 _ [] = []
encode1 code1 (x:xs) = fromJust (Prelude.lookup x code1 ) ++ (encode1 code1 xs)
