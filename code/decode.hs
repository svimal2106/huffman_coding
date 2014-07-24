import System.IO
import Algorithm.Compression(huffman,codewords,Bit(..),HuffmanTree(..))
import Maybe
import Data.Bits
import Data.Binary
import Data.Char
import qualified Data.ByteString as B
import System( getArgs )

main = do
	arg <- getArgs
	let filein = head arg
	let filetree = head $tail arg
	let fileout = head $drop 2 arg
	inh<-openBinaryFile filein ReadMode
        outh<-openFile fileout  WriteMode
	treeFile<-openFile filetree ReadMode
	str<-hGetContents treeFile
	let newlist = map digit $getlist $lines str
	putStrLn $show newlist
	
        huffmandecode inh outh $huffman newlist
        hClose outh
        return ()

digit (x,str) = (x, read str::Integer)

getlist [] = []
getlist (x:[]) = []
getlist list = (fst $head $readLitChar $snd (head $readLitChar $head list),head $tail list): (getlist $drop 2 list)

huffmandecode inh2 outh2 tree=
        do content <- B.hGetContents inh2
           --putStrLn (show (content))
           let bytewords = B.unpack content
           --putStrLn (show (bytewords))
           let foo = concat $map decToBin bytewords
           --putStrLn (show foo)
           --putStrLn (show tree)
           decode2 tree foo outh2--['0','1']
           --fileput outh2 res
           --putStrLn res 
decode2 t code outh= decode2' t code outh
      where decode2' (Node lt rt) (c:cs) outh=
                case c of
                  0 -> decode2' lt cs outh
                  1 -> decode2' rt cs outh
                  _ -> return ()-- "otherwise" pronuces a warning about unused variable
            decode2' (Leaf s) cs outh = do hPutChar outh s
                                           decode2 t cs outh-- we need the whole tree again
            decode2' _ [] _ = return ()



decToBin x = binarybyte (decToBin' x)
    where
        decToBin' 0 = []
        decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

binarybyte list = let rep = 8 - (length list)
                   in (list ++ (take rep (repeat 0)))

