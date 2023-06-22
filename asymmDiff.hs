import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
  h1 <- openFile (args !! 0) ReadMode
  f1 <- hGetLine h1
  h2 <- openFile (args !! 1) ReadMode
  f2 <- hGetLine h2
  let
    base1 = (takeWhile (/='.') (args !! 0))
    base2 =(takeWhile (/='.') (args !! 1))
    tset = getleaves (f2)
    xss = splitDecomposition f1 tset
    yss = splitDecomposition f2 tset
    resnum = asymmDiffnum xss yss
    res = asymmDiffbipart f1 f2
    logfile  =  "diff_" ++ base1 ++ "_" ++ base2 ++ ".log"
    reftaxa = getleaves (f1)

    keyref = "key_" ++ base1 ++ ".txt"
    keysha = "key_"++ base1 ++ "_" ++ base2 ++ ".txt"
  putStrLn $ "A-symmetric distances: "  ++ (args!!0) ++ "\t" ++ (args!!1) ++ "\t" ++ show resnum
  writeFile logfile (unlines  res)
  writeFile keyref (concatMap (++",") reftaxa)
  writeFile keysha (concatMap (++",") tset)
  hClose h1
  hClose h2
