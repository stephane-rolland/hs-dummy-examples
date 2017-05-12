-- | A library to do stuff.
module Lib
    (
      main
    ) where
import qualified MaybeTIO as MTIO


main :: IO ()
main = do
  MTIO.mainT
  
