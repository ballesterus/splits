import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
  h1 <- openFile (args !! 1) ReadMode
  f1 <- hGetLine h1
  let query =  breakString $ args !! 0
      tset = breakString f1
      res1 = split tset query
      res2 = antisplit res1
  putStrLn res1
  putStrLn res2
  
  hClose h1

