import Network
import Control.Concurrent
import System.IO
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (h,_,_) <- accept sock
   forkIO $ body h
   loop sock
  where
   body h = do
       hPutStr h msg
       hFlush h
       hClose h
 
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"







{-# LANGUAGE OverloadedStrings #-}
 
import Data.ByteString.Char8
 
import Network hiding (accept)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Control.Concurrent
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    loop sock
 
loop sock = do
   (conn, _) <- accept sock
   forkIO $ body conn
   loop sock
  where
   body c = do sendAll c msg
               sClose c
 
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"








{-# LANGUAGE OverloadedStrings #-}
 
-- A simple example of an epoll based http server in Haskell.
--
-- Uses two libraries:
--   * network-bytestring, bytestring-based socket IO.
--      - cabal install network-bytestring: 
--
--   * haskell-event, epoll-based scalable IO events
--      - git clone git://github.com/tibbe/event.git
--      - autoreconf ; then cabal install
 
import Network hiding (accept)
import Network.Socket (fdSocket, accept)
import Network.Socket.ByteString
import Data.ByteString.Char8
import System.Event
import System.Posix
import System.Posix.IO
 
main = withSocketsDo $ do
    sock <- listenOn $ PortNumber 5002
    let fd = fromIntegral (fdSocket sock)
    mgr <- new
    registerFd mgr (client sock) fd evtRead
    loop mgr
 
client sock _ _ = do
    (c,_) <- accept sock
    sendAll c msg
    sClose c
 
msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\nPong!\r\n"

