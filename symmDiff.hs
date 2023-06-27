import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
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
