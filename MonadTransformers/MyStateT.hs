
-- keep in my that the actual state always come late
module MyStateT where

import Data.Functor
import Data.Bifunctor
import Control.Applicative
import Control.Monad


data MyStateT s m a = MyStateT {runMyStateT :: s -> m (a, s)}

get :: Monad m => MyStateT s m s
get = MyStateT $ \s -> Control.Monad.return (s ,s)

put :: Monad m => s -> MyStateT s m ()
put s' = MyStateT $ \s -> Control.Monad.return ((),s')

modify :: (Monad m,Functor m) => (s -> s) -> MyStateT s m ()
modify f = fmap f get >>= put
  
instance (Monad m, Functor m) => Functor (MyStateT s m) where
  -- fmap :: (a -> b) -> (MyStateT s m) a -> (MyStateT s m) b
  fmap fab (MyStateT fsMas) = MyStateT $ \s -> bimap fab id <$> fsMas s

instance (Monad m, Functor m) => Applicative (MyStateT s m) where
  -- pure :: a -> (MyStateT s m) a
  pure a = MyStateT $ \s -> Control.Monad.return (a,s)

  MyStateT fsMfabs <*> MyStateT fsMas = MyStateT $ \s -> Control.Monad.ap (getMasbs s) (fsMas s)
    where
      getMasbs s = Control.Monad.ap mg (fsMfabs s)
      g (f,s) = bimap f id 
      mg = Control.Monad.return g

instance (Monad m,Functor m) => Monad (MyStateT s m) where
  -- return :: a -> (MyStateT s m) a
  return = pure

  -- (>>=) :: (MyStateT s m) a -> (a -> (MyStateT s m) b) -> (MyStateT s m) b
  MyStateT fsMas >>= faMSsb = MyStateT $ \s -> Control.Monad.join $ do
    (a,_) <- fsMas s
    let  fsMbs' = runMyStateT $ faMSsb a
    return $ fsMbs' s
                   
          
          
          
          
