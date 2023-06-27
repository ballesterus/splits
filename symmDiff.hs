import System.IO
import System.Environment 
import Newick


--ASCII art from https://patorjk.com/software/taag/#p=display&f=Big&t=asymmDiff%0A

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "\n                                _____  _  __  __ \n                               |  __ \\(_)/ _|/ _|\n  ___ _   _ _ __ ___  _ __ ___ | |  | |_| |_| |_ \n / __| | | | '_ ` _ \\| '_ ` _ \\| |  | | |  _|  _|\n \\__ \\ |_| | | | | | | | | | | | |__| | | | | |  \n |___/\\__, |_| |_| |_|_| |_| |_|_____/|_|_| |_|  \n       __/ |                                     \n      |___/                                      \n\nThis is a naive implementation of the symmetric difference between phylogenetic trees; a.k.a Robinson-Foulds\n\n\t Usage: symmDiff <NEWICk_FILE> <NEWICk_FILE>\n\nNOTE: If the OTUs are different, computation will be made on the shared leaves set.\n"
    
    else
    do
      handle1 <- openFile (args!!0) ReadMode
      f1 <- hGetLine handle1
      handle2 <- openFile (args!!1) ReadMode
      f2 <- hGetLine handle2
      let lx = getleaves f1
          ly = getleaves f2
          tset = sharedTaxa lx ly
          xss = splitDecomposition f1 tset
          yss = splitDecomposition f2 tset
          res = symmDiffnum xss yss
          n =   length tset
          nres = fromIntegral res / fromIntegral (2 * (n-3))
      if sameTaxa f1 f2 then  putStrLn "The two trees have same leaves" else putStrLn "Warning: The trees have different set of leaves. The different taxa will be dropped from all bipartitions.." 
      putStrLn $ "Symmetric (RF) distances: "  ++ (args!!0) ++ "\t" ++ (args!!1) ++ "\t" ++ show res ++ "\t" ++ show nres
      hClose handle1
      hClose handle2
