
module MyState where

import Data.Functor
import Control.Applicative
import Control.Monad

data MyState s a = MyState (s -> (a, s))

get :: MyState s s
get = MyState $ \s -> (s, s)

put :: s -> MyState s ()
put s' = MyState $ \s -> ((), s') 

modify :: (s -> s) -> MyState s ()
modify f = do
  val <- get
  let newVal = f val
  put newVal
  return ()

instance Functor (MyState s) where
  -- fmap :: (a -> b) -> (MyState s) a -> (MyState s) b
  fmap f (MyState fa) = MyState $ \s -> let (a,s) = fa s in (f a,s)

instance Applicative (MyState s) where
  -- pure :: a -> (MyState s) a
  pure a = MyState $ \s -> (a, s)

  -- (<*>) :: (MyState s) (a -> b) -> (MyState s) a -> (MyState s) b
  (<*>) (MyState fab) (MyState fsa)  = MyState f'
    where
      f' s = (b, s)
         where
           (f,x') = fab s
           (a,x'') = fsa s
           b = f a

instance Monad (MyState s) where
  -- return :: a -> (MyState s) a
  return = pure

  -- (>>=) :: (MyState s) a -> (a -> (MyState s) b) -> (MyState s) b
  (>>=) (MyState fsa) (faMSsb)  = MyState $ f'
    where
      f' s = (b ,s)
        where
          (a,x) = fsa s
          MyState f'' = faMSsb a
          (b,x') = f'' s
      -- fsa s = (a,s)
      -- faMSsb a = MyState $ \s -> (b,s)
