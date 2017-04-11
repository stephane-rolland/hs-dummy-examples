
{-# LANGUAGE DeriveGeneric #-}
module Command where
import Data.Binary
import GHC.Generics (Generic)

data Command = FirstMessage
               | DoNothing
               | DoSomething Int
               deriving (Show,Generic)

data Output = NoResult
              | SomeResult Int
              deriving (Show,Generic)


instance Binary Command
instance Binary Output
