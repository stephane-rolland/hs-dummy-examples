import Control.Monad.State

type StateVal = Int

modifyValue :: StateVal -> StateVal
modifyValue v | v < 50 = v + 1
modifyValue v = 50

simpleStateStep :: State StateVal ()
simpleStateStep = do
        s <- get
        put $ modifyValue s

unrollState :: StateVal -> StateVal
unrollState s = res
    where
    s' =  execState simpleStateStep s
    isSame = s == s'
    res = if isSame then s else unrollState s' 

main = do
    putStrLn $ show $ unrollState 1
