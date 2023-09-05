module Main (main) where

import qualified Data.ByteString as B
import Ftable
import Lib

main :: IO ()
main = do
  fname <- getLine
  contents <- readFile fname
  let (table, encoded) = encode contents
  writeFile "out.hfm" $ formatTable table
  B.appendFile "out.hfm" $ toByteString encoded
  print "done"
