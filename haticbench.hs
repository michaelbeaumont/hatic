import System.IO
import System.Environment
import Network.Socket
import Control.Monad
import Control.Exception
import Control.Concurrent

import Hatic.Types


sendUntil requester n connhdl = sendWhile n
    where sendWhile 0 = return ()
          sendWhile x = do
            requester connhdl x
            resp <- hGetLine connhdl
            putStrLn $ '<':resp
            sendWhile (x-1)

getKey connhdl n = do
    hPutStrLn connhdl "g 1"
    hFlush connhdl

getKey2 connhdl n = do
    hPutStrLn connhdl "g 2"
    hFlush connhdl

setKey connhdl n = do
    hPutStrLn connhdl "s 2 3"
    hFlush connhdl

hashMake connhdl n = do
    hPutStrLn connhdl ("hs hash"++show n++" 1 blahblah")
    hPutStrLn connhdl ("hs hash"++show n++" 2 blahblah")
    hFlush connhdl

openClientSock hostname port = do
        addr <- liftM head $ getAddrInfo Nothing (Just hostname) (Just port)
        sock <- socket (addrFamily addr) Stream defaultProtocol
        setSocketOption sock KeepAlive 1
        connect sock (addrAddress addr)
        connhdl <- socketToHandle sock ReadWriteMode
        hSetBuffering connhdl NoBuffering
        return connhdl

        
main = bench

bench = do
    sett <- liftM (parseArgs defaultConfig) getArgs 
    print "gonnpua benchmark"
    h <- try $ openClientSock "localhost" "7900" 
            :: IO (Either SomeException Handle)
    case h of
        Left e -> print "Couldn't connect to server"
        Right m -> do 
                forkIO $ sendUntil getKey 10000 m
                forkIO $ sendUntil setKey 10000 m
                forkIO $ sendUntil getKey2 10000 m
                forkIO $ sendUntil hashMake 10000 m
                return ()



parseArgs :: ClientSettings -> [String] -> ClientSettings
parseArgs config ("-p":value:ss) = ClientSettings (settAddress config) value
parseArgs config (value:ss) = ClientSettings value (settPort config)
parseArgs config _ = config
