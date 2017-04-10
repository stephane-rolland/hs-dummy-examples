module Main where

import qualified Server as S
import qualified Client as C
import qualified System.Environment as SE
 
main :: IO ()
main = do
  [f] <- SE.getArgs
  if f == "server"
    then S.main
    else C.main

