{-# LANGUAGE OverloadedStrings #-}
-- | A library to do stuff.
module Lib where

import qualified Database.PostgreSQL.Simple as DPS
import qualified Data.ByteString as DB
import qualified Control.Monad as CM

connectionInfo :: DPS.ConnectInfo
connectionInfo = DPS.ConnectInfo
                   "localhost"
                   5432
                   "user_dummy"
                   "put_the_correct_password_here"
                   "dummy"

performSqlActions :: IO()
performSqlActions = do
  conn <- DPS.connect connectionInfo
  deleteTables conn
  fillTables conn
  selectTables conn
  updateTables conn

deleteTables :: DPS.Connection -> IO ()
deleteTables con = do

  i64 <- DPS.execute_ con "delete from info"
  i64 <- DPS.execute_ con "delete from price"

  putStrLn $ "tables are deleted"
  
  return ()

data Price = Price
  {
      price :: Double
    , label :: String
  }

fillTables :: DPS.Connection -> IO ()
fillTables con = do
  let lst = [ Price 5 "aaa", Price 6 "bbb", Price 7.5 "deded", Price 8.9 "ff" ]
  CM.mapM_ insertPrice lst
  return ()
  where
    insertPrice :: Price -> IO ()
    insertPrice (Price d l) = do
      i64 <- DPS.execute con "insert into price (price,label) values (?,?)" (d,l) 
      putStrLn $ "insert exit code is = " ++ (show i64)

selectTables :: DPS.Connection -> IO ()
selectTables con = do

  let query1 = "select price from price where price > ? "
  let param1 = (DPS.Only (7::Double)) 
  xs1 <- DPS.query
             con
             query1 
             param1
  CM.forM_ xs1 $ \x -> do
    putStrLn $ "res1:" ++ (show $ readPrice x)

  let query2 = "select label from price where price > ? "
  let param2 = (DPS.Only (7::Double)) 
  xs2 <- DPS.query
             con
             query2 
             param2
  CM.forM_ xs2 $ \x -> do
    putStrLn $ "res2:" ++ (show $ readLabel x)

  let query3 = "select price, label from price where price > ? "
  let param3 = (DPS.Only (7::Double)) 
  xs3 <- DPS.query
             con
             query3 
             param3
  CM.forM_ xs3 $ \x -> do
    putStrLn $ "res3:" ++ (show $ readPriceLabel x)

  let query4 = "select * from price where price > ? "
  let param4 = (DPS.Only (7::Double)) 
  xs4 <- DPS.query
             con
             query4 
             param4
  CM.forM_ xs4 $ \x -> do
    putStrLn $ "res4:" ++ (show $ readAll x)

  return ()
  where
    readPrice :: DPS.Only Double -> Double
    readPrice (DPS.Only x) = x :: Double

    readLabel :: DPS.Only String -> String
    readLabel (DPS.Only x) = x

    readPriceLabel :: (Double,String) -> (Double,String)
    readPriceLabel x = x

    readAll :: (Integer,Double,String) -> (Integer,Double,String)
    readAll x = x

updateTables :: DPS.Connection -> IO ()
updateTables con = do
  let listLabel = (DPS.In ["aaa","bbb"]) :: DPS.In [DB.ByteString]

  let query1 = "update price set price = ?, label = ? where label in ?"
  let param1 = (1.1::Double,"no more"::DB.ByteString,listLabel)

  i64 <- DPS.execute con query1 param1

  putStrLn $ "update exit code = " ++ (show i64)

  let query4 = "select * from price"
  xs4 <- DPS.query_
             con
             query4 
  CM.forM_ xs4 $ \x -> do
    putStrLn $ "res5:" ++ (show $ readAll x)

  return ()
  where
    readAll :: (Integer,Double,String) -> (Integer,Double,String)
    readAll x = x

