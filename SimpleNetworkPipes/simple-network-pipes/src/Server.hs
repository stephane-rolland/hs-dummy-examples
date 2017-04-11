module Server where

import Pipes 
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Command as C
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as PipesPrelude

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
  liftIO $ putStrLn $ "received message = " ++ (show c)
  return $ C.DoSomething 0

main :: IO ()
main = PNT.serve (PNT.Host "127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
                     putStrLn $ "Remote connection from ip = " ++ (show remoteAddress)
                     _ <- runEffect $ do
                       let bytesReceiver = PNT.fromSocket connectionSocket pageSize
                       let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
                       commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket
                     return ()
