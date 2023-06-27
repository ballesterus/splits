import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
  if length args /= 2
    then
    putStrLn "A simple tool for converting a list of taxa into a \"split\"character string.\n\tUsage: taxa2Split \"TaxA,TaxB,TaxC\" <RefFile>\n Where RefFile is a text file with the ordered reference taxon in a single line with each taxon separated by a comma>\n\n"
    
    else
    do
      h1 <- openFile (args !! 1) ReadMode
      f1 <- hGetLine h1
      let query =  breakString $ args !! 0
          tset = breakString f1
          res1 = split tset query
          res2 = antisplit res1
      putStrLn res1
      putStrLn res2
      hClose h1
