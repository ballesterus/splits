import qualified Data.List as List

-- test tree-  let s = (U_glomosus|016215c55186_g4_i7:0.00055,U_glomosus|016210c55186_g4_i2:0.00288,((N_sinensis|002764c32854_g1_i1:0.04246,((((N_clavipes|005148c24804_g1_i1:0.23844,((N_arabesca|008677c46582_g1_i2:0.06151,(N_arabesca|008678c46582_g1_i3:0.00055,N_arabesca|008676c46582_g1_i1:0.00055)0.858:0.00055)0.958:0.03397,(G_hasselti|021872c46388_g1_i1:0.00587,(G_arcuata|000220c11540_g1_i1:0.00055,G_arcuata|000221c11540_g1_i2:0.00055)0.978:0.00885)0.998:0.06477)0.786:0.02409)0.584:0.03585,((E_leonina|022467c55244_g1_i1:0.04891,(M_guttata|030435c44002_g2_i3:0.00055,(M_guttata|030437c44002_g2_i5:0.00987,M_guttata|030436c44002_g2_i4:0.00055)0.983:0.01318)1.000:0.18070)0.742:0.02883,(L_venusta|028454c57950_g3_i1:0.05356,(T_perreira|012195c15901_g1_i1:0.00055,T_polychromata|011228c37131_g1_i1:0.02380)0.997:0.09783)0.961:0.04252)0.955:0.02889)0.868:0.01330,(F_communis|030772c61053_g3_i1:0.01551,F_communis|030773c61053_g3_i2:0.00055)1.000:0.09954)0.892:0.03202,((H_pococki|005985c27614_g1_i1:0.00053,H_pococki|006005c27653_g1_i1:0.00055)0.998:0.32956,(D_longipes|008825c32992_g1_i1:0.00055,D_longipes|009202c33745_g1_i1:0.00054)0.973:0.25748)0.273:0.09471)0.985:0.07772)0.999:0.04988,(U_glomosus|016209c55186_g4_i1:0.00055,U_glomosus|016217c55186_g4_i9:0.00055)0.996:0.00054)1.000:0.00375);

splitOn ::  [Char] ->[Char] -> [[Char]]
splitOn x xs = case dropWhile (`elem` x) xs of
                 "" -> []
                 xs'  -> a : splitOn x xs''
                   where (a,xs'') =
                           break (`elem` x) xs'


parent :: [Char] -> ([Int], [Int]) -- Takes a newick string and returns a tuple of two list with the indexes of opening and closing parentheses.
parent xs = (open,close)
  where
    open =  '(' `List.elemIndices` xs
    close = ')' `List.elemIndices` xs 

pairParent :: ([Int],[Int]) ->[(Int,Int)] -- This function matches paiired parentheses  
pairParent ([],[])  = []
pairParent (o,(e:c)) = (a,e): pairParent (b,c)
  where a = lastofLess o e
        b = List.delete a o

lastofLess :: [Int] -> Int -> Int -- Function for extracting the largest index of of list that is less than a given values.
lastofLess xs l =  last $ List.takeWhile (< l) xs

withinBound :: (Int,Int) -> [Char] -> [Char]
withinBound (a,b) xs = drop a $ take b xs

getleaves ::  String -> [String] -- good taxa name start with go after "(," and end with "):,"
getleaves xs = case dropWhile (`elem` ":),;[]" ) xs of
                 "" -> []
                 xs'  -> filter (isLeaf) $ leaf : getleaves xs''
                   where leaf = [x | x <- a, x `notElem` "("]
                         (a,xs'') =
                           break (`elem` ":),;[]") xs'

isLeaf :: String -> Bool
isLeaf xs
  | all (`elem` "0123456789,.") xs = False
  | otherwise = True


-- Split Functions
split :: [String] -> [String] -> String
split tax [] = [] 
split tax sample = [taxPresent  l sample | l <- tax]


antisplit :: String -> String
antisplit xs = [if a == '.' then '@' else '.' | a <- xs]


taxPresent ::  String -> [String] -> Char
taxPresent q tax 
  | q `elem` tax = '@'
  | otherwise = '.'

  
issameSplit :: String -> String -> Bool
issameSplit x y
  | x == y = True
  | x == antisplit y = True
  | otherwise = False

split2Taxlist :: String -> [String] -> [String] --Takes a split string, a list of taxa and returns a collection of taxa represented in the split
split2Taxlist [] _ = []
split2Taxlist _ [] = []
split2Taxlist (s:plit) (t:axa)
  | s == '@' = t : split2Taxlist plit axa
  | s == '.' = split2Taxlist plit axa


splitDecomposition :: String -> [String] --A tree is a collection of splits
splitDecomposition newick = [split taxa (getleaves (withinBound a newick)) | a <- fromParentheses]
  where taxa = getleaves newick
        fromParentheses = pairParent $ parent newick


sameTaxa :: String -> String -> Bool
sameTaxa x y
  | x == y = True
  | List.sort x == List.sort y = True
  | otherwise = False

                         
