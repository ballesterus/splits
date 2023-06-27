
import System.IO
import System.Environment 
import Newick

--ASCII art from https://patorjk.com/software/taag/#p=display&f=Big&t=asymmDiff%0A


main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then putStrLn "\n                                      _____  _  __  __ \n                                     |  __ \\(_)/ _|/ _|\n   __ _ ___ _   _ _ __ ___  _ __ ___ | |  | |_| |_| |_ \n  / _` / __| | | | '_ ` _ \\| '_ ` _ \\| |  | | |  _|  _|\n | (_| \\__ \\ |_| | | | | | | | | | | | |__| | | | | |  \n  \\__,_|___/\\__, |_| |_| |_|_| |_| |_|_____/|_|_| |_|  \n             __/ |                                     \n            |___/                                      \n\nThis is a naive implementation an asymmetric difference between phylogenetic trees, where the 1st tree is used as the sole reference and it returns the number of bipartitions present in the reference tree but missing in another tree. A detailed log is produced for further inspection of each bipartition.\n\n\t Usage: asymmDiff <NEWICk_FILE> <NEWICk_FILE>\n\nNOTE: If the OTUs are different, computation will be made on the shared leaves set.\n"
    else
    do
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
        rnum = asymmDiffnum xss yss
        n=length tset
        rstd = fromIntegral rnum / fromIntegral (n-3)
        res = asymmDiffbipart f1 f2
        logfile  =  "diff_" ++ base1 ++ "_" ++ base2 ++ ".log"
        reftaxa = getleaves (f1)
        keyref = "key_" ++ base1 ++ ".txt"
        keysha = "key_"++ base1 ++ "_" ++ base2 ++ ".txt"
      putStrLn $ "A-symmetric distances: "  ++ (args!!0) ++ "\t" ++ (args!!1) ++ "\t" ++ show rnum ++ "\t" ++ show rstd
      writeFile logfile (unlines  res)
      writeFile keyref (concatMap (++",") reftaxa)
      writeFile keysha (concatMap (++",") tset)
      hClose h1
      hClose h2






