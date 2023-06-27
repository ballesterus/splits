module Newick
  (getleaves
  , sameTaxa
  , sharedTaxa
  , getBS_BL
  , split
  , antisplit
  , isSplitshared
  , split2Taxlist
  , splitDecomposition
  , asymmDiffbipart
  , asymmDiffnum
  , symmDiffnum
  , breakString)  where

import qualified Data.List as List



type Newick = [Char] -- a newick formated tree
type Split = String -- a split is a string of characters ".@"
type SplitColl = [Split] 
type SplitPlus = (Split, String) -- tuple grouping splits and branch values for later stats
type SplitPlusColl = [SplitPlus]

-- test tree-
-- t1 = "(Anopheles_farauti|99519at6656_69004_0:0.1413328525,Anopheles_funestus|99519at6656_62324_0:0.0692975708,(Anopheles_gambiae|99519at6656_7165_0:0.0692013967,(((Polistes_canadensis|99519at6656_91411_0:0.1268125212,(Acromyrmex_echinatior|99519at6656_103372_0:0.0862551699,Camponotus_floridanus|99519at6656_104421_0:0.0733313879)96:0.0619881320)94:0.0992356293,((Limulus_polyphemus|99519at6656_6850_0:0.1428255009,Centruroides_sculpturatus|99519at6656_218467_0:0.2054161465)100:0.1338858310,(Strigamia_maritima|99519at6656_126957_0:0.3442130683,Ixodes_scapularis|99519at6656_6945_0:0.3746577413)73:0.0672978964)100:0.1885627264)100:0.3047859790,Drosophila_rhopaloa|99519at6656_1041015_0:0.2392636631)100:0.3083073499)69:0.0303864936);"
-- t2 = "(Anopheles_farauti|99519at6656_69004_0:0.1112751761,Anopheles_gambiae|99519at6656_7165_0:0.0669750076,(((Polistes_canadensis|99519at6656_91411_0:0.0956377555,(Acromyrmex_echinatior|99519at6656_103372_0:0.0631983942,Camponotus_floridanus|99519at6656_104421_0:0.0607591357)98:0.0554551816)98:0.0731861993,((Limulus_polyphemus|99519at6656_6850_0:0.0953208848,Centruroides_sculpturatus|99519at6656_218467_0:0.1318312746)99:0.0848592731,(Strigamia_maritima|99519at6656_126957_0:0.2042150393,Ixodes_scapularis|99519at6656_6945_0:0.2280850266)53:0.0502968700)100:0.1112316922)100:0.1589646650,Drosophila_rhopaloa|99519at6656_1041015_0:0.1565317693)100:0.1765924470);"





-- I Making sense of the newick string
openParent :: Newick -> [Int] -- Takes a newick string and returns a tuple of two list with the indexes of opening and closing parentheses.
openParent xs = '(' `List.elemIndices` xs


closeParent :: Newick ->  [Int] -- Takes a newick string and returns a list with the indexes of closing parentheses.
closeParent xs =  ')' `List.elemIndices` xs 


pairParent :: [Int] -> [Int] ->[(Int, Int)] -- This function matches paiired parentheses
pairParent [] _  = []
pairParent _ [] = []
pairParent o (e:c) = (a,e): pairParent b c
  where a = lastofLess o e
        b = List.delete a o

lastofLess :: [Int] -> Int -> Int -- Function for extracting the largest index of of list that is less than a given values.
lastofLess xs l =  last $ List.takeWhile (< l) xs

withinBound :: (Int,Int) -> [Char] -> [Char]
withinBound (a,b) xs = drop a $ take b xs

-- II Leaves and Support
getleaves ::  Newick -> [String] -- good taxa name start  after "(," and end with "):,"
getleaves xs = case dropWhile (`elem` ":),;[]" ) xs of
                 "" -> []
                 xs'  -> filter (isLeaf) $ leaf : getleaves xs''
                   where leaf = [x | x <- a, x `notElem` "("]
                         (a,xs'') =
                           break (`elem` ":),;[]") xs'

isLeaf :: String -> Bool
isLeaf xs
  | all (`elem` "0123456789,./") xs = False
  | otherwise = True


sameTaxa :: String -> String -> Bool 
sameTaxa xs ys
  | x == y = True
  | List.sort x == List.sort y = True
  | otherwise = False
  where x = getleaves xs
        y = getleaves ys
                         
sharedTaxa :: [String] -> [String] -> [String] -- Take two list of taxa and returns a list with leaves shared in both sets
sharedTaxa xs ys =[x | x <- xs, x `elem` ys]


sharedTaxa' :: [String] -> [String] -> [String] -- Take two list of taxa and returns a list with leaves shared in both sets
sharedTaxa' xs ys =[if x `elem` ys then x else "miss"| x <- xs] 

getBS_BL :: Newick -> [String]
getBS_BL tree
  | rest == ";" = [] 
  | otherwise = (x ++ "  " ++  tail (y)): getBS_BL rest 
  where
    (x,y) =  break (==':') (takeWhile (`notElem` ")(,") rest)
    rest =  tail (dropWhile( /= ')') tree) 

-- Split Functions
split :: [String] -> [String] -> Split -- makes a split representation as a string with presence/absence taxa. The list of strings are list of reference taxa and an arbitrary list of leaves
split ref [] = [] 
split ref sample = [taxPresent x sample | x <- ref]


taxPresent ::  String -> [String] -> Char -- is a taxon represented in the split?
taxPresent tax split
  | tax == "miss" = '-'
  | tax `elem` split = '@'
  | otherwise = '.'

cleanSplit :: Split -> Split -- mark for removal
cleanSplit s = [a | a <- s, a /= '-']  

antisplit :: Split -> Split
antisplit xs = [if a == '.' then '@' else '.' | a <- xs]

  
issameSplit :: Split -> Split -> Bool
issameSplit x y
  | x == y = True
  | x == antisplit y = True
  | otherwise = False


isSplitshared :: Split -> SplitColl -> Bool
isSplitshared x xs = any (issameSplit x) xs 



isSplitNonTrivial :: Split -> Bool
isSplitNonTrivial split
  | all (== '@') split = False 
  | all (== '.') split = False
  | countChar '@' split == 1 = False
  | countChar '.' split == 1 = False
  | otherwise = True


countChar :: Char -> String ->  Int
countChar y xs  = foldr (\x acc -> if x == y then acc + 1 else acc + 0) 0 xs


lookupSplit :: Split -> SplitPlusColl -> String
lookupSplit s xs = foldr (\(k,v) acc -> if (issameSplit s k) then yay (k,v) else acc) nay  xs
  where yay (k,v) = "1  " ++ v
        nay =  "0   -  -"


asymmDiffbipart :: Newick -> Newick -> [String] -- where the list of input strings are list of bipartitions from a reference tree (x) and a sample tree (y)
asymmDiffbipart rtree atree = [a ++ "\t" ++ c ++ "\t" ++ b ++ "\t" ++ lookupSplit (b) asc  | (a,b,c) <- mas ]
  where ftax = getleaves rtree
        stax = sharedTaxa  ftax (getleaves atree)
        sref = splitDecomposition rtree ftax
        refv = getBS_BL rtree 
        rref = splitDecomposition rtree stax
        aval = getBS_BL atree
        aspl = splitDecomposition atree stax
        asc = splitswBval aspl aval
        mas = zip3 sref rref refv


asymmDiffnum :: [Split] -> [Split] -> Int -- where the list of input strings are list of bipartitions. x is reference.
asymmDiffnum xs ys = sum [if isSplitshared x fys then 0 else 1| x <- fxs]
  where fys = filter (isSplitNonTrivial) ys
        fxs = filter (isSplitNonTrivial) xs



symmDiffnum :: [Split] -> [Split] -> Int -- where the list of input strings are list of bipartitions
symmDiffnum xs ys = a + b
                    where a = asymmDiffnum xs ys
                          b = asymmDiffnum ys xs


splitswBval :: [Split] -> [String] -> [SplitPlus]
splitswBval splits bvals =  zip splits bvals


split2Taxlist :: String -> [String] -> [String] --Takes a split string, a list of taxa and returns a collection of taxa represented in the split
split2Taxlist [] _ = []
split2Taxlist _ [] = []
split2Taxlist (s:plit) (t:axa)
  | s == '@' = t : split2Taxlist plit axa
  | s == '.' = split2Taxlist plit axa


splitDecomposition :: Newick -> [String] -> [String] --A tree is a collection of splits
splitDecomposition newick taxa = [split taxa (getleaves (withinBound a newick)) | a <- fromParentheses]
  where open = openParent newick
        close = closeParent newick
        fromParentheses = pairParent open close


-- Miscellany


breakString :: String -> [String]
breakString "" = []
breakString "," = []
breakString xs = a : breakString rest
  where (a,b) = break (==',') xs
        rest = tail b



--- Math and stats

mean :: [Float] -> Float
mean [] = 0
mean xs = ss /fromIntegral n
  where ss = sum xs
        n = length xs

variance :: [Float] -> Float
variance xs = ssdiff / fromIntegral n
  where ssdiff = sum [(x - m)^2 | x <- xs]
        m = mean xs
        n = (length xs) - 1

sd :: [Float] -> Float
sd xs = sqrt $ variance xs
