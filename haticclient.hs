import System.IO
import System.Environment
import Network.Socket
import Control.Monad
import Control.Exception

import Hatic.Types

openClientSock hostname port = do
        addr <- liftM head $ getAddrInfo Nothing (Just hostname) (Just port)
        sock <- socket (addrFamily addr) Stream defaultProtocol
        setSocketOption sock KeepAlive 1
        connect sock (addrAddress addr)
        connhdl <- socketToHandle sock ReadWriteMode
        hSetBuffering connhdl NoBuffering
        return connhdl

repl h = do
    putStr ">"
    hFlush stdout
    inp <- getLine
    hPutStrLn h inp
    hFlush h
    resp <- hGetLine h
    putStrLn $ '<':resp
    repl h

main = do
    sett <- liftM (parseArgs defaultConfig) getArgs 
    putStrLn $ "Connecting to server at "++ settAddress sett ++":"++ settPort sett
    h <- try $ openClientSock "localhost" "7900" :: IO (Either SomeException Handle)
    case h of
        Left e -> print "Couldn't connect to server"
        Right m -> repl m

handleErr (SomeException e) = putStrLn "Couldn't connect to server, mate."
        

parseArgs :: ClientSettings -> [String] -> ClientSettings
parseArgs config ("-p":value:ss) = ClientSettings (settAddress config) value
parseArgs config (value:ss) = ClientSettings value (settPort config)
parseArgs config _ = config
