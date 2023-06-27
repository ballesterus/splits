import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then
    putStrLn "A simple tool for converting a\"split\" character string into a list of taxa.\n\tUsage: split2Taxa \"....@@@...\" <RefFile>\n Where RefFile is a text file with the reference taxon set separated by commas>\n\n"
  
    else
    do
      h1 <- openFile (args !! 1) ReadMode
      f1 <- hGetLine h1
      let s = args !! 0
          tset = breakString f1
          res = split2Taxlist s tset
      putStrLn $ concatMap (++",") res
      hClose h1
