import System.IO
import System.Environment 
import Newick

main :: IO ()
main = do
  args <- getArgs
  h1 <- openFile (args !! 1) ReadMode
  f1 <- hGetLine h1
  let s = args !! 0
      tset = breakString f1
      res = split2Taxlist s tset
  putStrLn $ concatMap (++",") res
  hClose h1
