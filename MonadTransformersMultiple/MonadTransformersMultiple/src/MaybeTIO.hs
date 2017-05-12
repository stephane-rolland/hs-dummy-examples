module MaybeTIO where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad


main :: IO ()
main = do
    maybeUserName <- readUserName
    case maybeUserName of
      Nothing -> print "Invalid user name!"
      Just (uName) -> do
        maybeEmail <- readEmail
        case maybeEmail of
          Nothing -> print "Invalid email!"
          Just (email) -> do
            maybePassword <- readPassword
            case maybePassword of
              Nothing -> print "Invalid Password"
              Just password -> login uName email password

readUserName :: IO (Maybe String)
readUserName = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing
    
readEmail :: IO (Maybe String)
readEmail = do
  str <- getLine
  if any ('@' ==) str
    then return $ Just str
    else return Nothing

readPassword :: IO (Maybe String)
readPassword = do
  str <- getLine
  if length str > 5
    then return $ Just str
    else return Nothing

login :: String -> String -> String -> IO ()
login s s' s'' = do
  putStrLn $ "connection user = " ++ s ++ " " ++ s' ++ " " ++ s'' ++ " "

-- now using MonadTransformers
mainT :: IO ()
mainT = do
  maybeCreds <- runMaybeT $ do
    usr <- readUserNameT
    email <- readEmailT
    pass <- readPasswordT
    return ( usr, email, pass)
  case maybeCreds of
    Nothing -> print "could not login"
    Just (u,e,p) -> login u e p

readUserNameT :: MaybeT IO String
readUserNameT = do
  str <- liftIO getLine
  guard $ length str > 5
  return $ str

readEmailT :: MaybeT IO String
readEmailT = do
  str <- liftIO getLine
  guard $ any ('@'==) str 
  return str

readPasswordT :: MaybeT IO String
readPasswordT = do
  str <- liftIO getLine
  guard $ length str > 5
  return str

