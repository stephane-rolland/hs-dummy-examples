
module Client where

import Pipes
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PipesPrelude
import qualified Pipes.Parse as PP
import qualified Command as C
--import qualified Network.Simple.TCP as NST
--import qualified Data.Binary as DB

pageSize :: Int
pageSize = 4096

-- A input, B ouput, to be used as is, connected with (>->) operator 
--handler :: (Monad m) => Pipe A B m r
handler :: (Monad m) => Pipe C.Command C.Output m r
handler = undefined -- should use await and yield 

-- pure handler, to be used with PipesPrelude.amp
pureHandler :: C.Command -> C.Command 
pureHandler c = c

-- inpure handler, to be used with PipesPremude.mapM
sideffectHandler :: MonadIO m => C.Command -> m C.Command
sideffectHandler c = do
  liftIO $ putStrLn $ "received = " ++ (show c)
  return C.DoNothing

main :: IO ()
main = PNT.connect ("127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
    putStrLn $ "Connected to distant server ip = " ++ (show remoteAddress)
    sendFirstMessage connectionSocket
    _ <- runEffect $ do
      let bytesReceiver = PNT.fromSocket connectionSocket pageSize
      let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
      commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket 
    return ()

sendFirstMessage :: PNT.Socket -> IO ()
sendFirstMessage s = do
  _ <- runEffect $ do
    let encodedProducer = PipesBinary.encode C.FirstMessage 
    encodedProducer >-> PNT.toSocket s  
  return ()
