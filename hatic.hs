{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Time (getCurrentTime)
import System.Environment
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString (recvFrom)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8 (hPutStrLn, pack)
import Text.Regex.PCRE ((=~))
import qualified Data.HashTable.IO as H
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as M

import Hatic.Types


--parse Command and return type
parseComm :: B.ByteString -> DBComm
parseComm (flip (=~) getMatch -> (_,_,_,[key]) :: RegexGroups ) = Get key
parseComm (flip (=~) hGetMatch -> (_,_,_,(okey:ikey:[])) :: RegexGroups ) = HGet okey ikey
parseComm (flip (=~) setMatch -> (_,_,_,ms@(key:val:[])) :: RegexGroups ) = Set key val
parseComm (flip (=~) hSetMatch -> (_,_,_,ms@(okey:ikey:val:[])) :: RegexGroups ) = HSet okey ikey val
parseComm _ = CommError "Invalid command"

getMatch = "^g ([a-zA-Z0-9_]*)" :: B.ByteString
hGetMatch = "^hg ([a-zA-Z0-9_]*) ([a-zA-Z0-9_]*)" :: B.ByteString
setMatch = "^s ([a-zA-Z0-9_]*) ([a-zA-Z0-9_]*)" :: B.ByteString
hSetMatch = "^hs ([a-zA-Z0-9_]*) ([a-zA-Z0-9_]*) ([a-zA-Z0-9_]*)" :: B.ByteString

--execute command
execComm :: DBComm -> Hatic (DBRes)
execComm (Get key) = do
        memState <- gets getMemState
        rawHash <- liftIO . takeMVar $ getKV memState
        res <- liftIO $ H.lookup rawHash key
        liftIO $ putMVar (getKV memState) rawHash
        case res of 
            (Just ans) -> return $ Value ans
            Nothing -> return . Msg $ B.concat [key," not found"]

execComm (Set key val) = do
        memState <- gets getMemState
        rawHash <- liftIO . takeMVar $ getKV memState
        liftIO $ H.insert rawHash key val
        liftIO $ putMVar (getKV memState) rawHash
        return . Msg $ B.concat [key, "=", val]

execComm (HGet okey ikey) = do
        memState <- gets getMemState
        res <- liftIO $ do
                rawHashes <- takeMVar $ getHH memState
                res <- H.lookup rawHashes okey >>= 
                       return . (fmap $ M.lookup ikey)
                putMVar (getHH memState) rawHashes
                return res
        let innerCheck = maybe (Msg "key not found in hash") Value
        return $ maybe (Msg "hash not found") innerCheck res

execComm (HSet okey ikey val) = do
        memState <- gets getMemState
        res <- liftIO $ do
                rawHashes <- takeMVar $ getHH memState
                res <- H.lookup rawHashes okey 
                let newMap = case res of
                        Nothing -> M.singleton ikey val
                        (Just oldMap) -> M.insert ikey val oldMap
                H.insert rawHashes okey newMap
                putMVar (getHH memState) rawHashes
                return res
        return . Msg $ B.concat [ikey, " in ", okey, "=", val]

execComm _ = return $ Msg "Not a valid command"


openSock :: HostName -> ServiceName -> IO ConnInfo
openSock hostname port = do
    addr <- liftM head $ getAddrInfo Nothing (Just hostname) (Just port)
    sock <- socket (addrFamily addr) Stream defaultProtocol
    bind sock (addrAddress addr)
    listen sock 150 -- why 150 ?
    return $ ConnInfo sock (addrAddress addr)

initServer :: Settings -> IO (ConnInfo, MemoryState)
initServer config = do
    conn <- liftIO $ openSock (address config) (port config)
    store <- initMemStore
    return (conn, store)
    where
        initMemStore = do
            kvHash <- H.new :: IO KVHash
            hashHash <- H.new :: IO HHash
            H.insert kvHash "1" "1"
            H.insert hashHash "hash1" $ M.singleton "1" "inside the hash!"
            kvVar <- newMVar kvHash
            hVar <- newMVar hashHash
            return $ MemoryState kvVar hVar

main = do
    config <- liftM (parseArgs defConfig) getArgs
    (conn, store) <- initServer config
    let accLog = defaultAccLog
    let errLog = defaultErrLog
    acceptLoop conn store accLog errLog

--loop and accept
acceptLoop conninfo store accLog errLog = loop
    where
        takeAndFork = do
            (connsock, clientaddr) <- accept . ciSocket $ conninfo
            logWithStamp accLog $ show clientaddr ++ " client connected"
            connhdl <- socketToHandle connsock ReadWriteMode
            hSetBuffering connhdl NoBuffering
            let cleanup _ = hClose connhdl >> 
                    logWithStamp accLog (show clientaddr ++ " disconnected")
            let st = HState connhdl store accLog errLog
            forkFinally (runStateT handleConn st) cleanup
        loop = do
            takeAndFork
            loop

--handle single connection
handleConn :: Hatic ()
handleConn = do
        connhdl <- gets getConn
        isEOF <- liftIO $ hIsEOF connhdl
        unless isEOF consumeLine
        where 
            consumeLine = do
                connhdl <- gets getConn
                res <- handleInp <=< grabInp $ connhdl
                makeResp connhdl res
                handleConn

-- input functions
grabInp :: Handle -> Hatic B.ByteString
grabInp = liftIO . B.hGetLine

handleInp :: B.ByteString -> Hatic DBRes
handleInp inp = do
    execComm (parseComm inp)
    
-- create response
makeResp :: Handle -> DBRes -> Hatic ()
makeResp connhdl result = liftIO $ do
    case result of
        (Error val) -> C8.hPutStrLn connhdl val
        (Msg val) -> C8.hPutStrLn connhdl val
        (Value val) -> C8.hPutStrLn connhdl val

-- args parsing
parseArgs :: Settings -> [String] -> Settings
parseArgs config ("-a":value:ss) = Settings value (port config) 
                                    (persistFile config)
parseArgs config ("-f":value:ss) = Settings (address config) (port config)
                                    value
parseArgs config ("-p":value:ss) = Settings (address config) value
                                    (persistFile config)
parseArgs config _ = config

--Logging functions
mkErrLogger :: FilePath -> String -> IO ()
mkErrLogger path msg = withFile path AppendMode (flip hPutStrLn msg)
            
defaultAccLog :: String -> IO ()
defaultAccLog = mkErrLogger "access.log"

defaultErrLog :: String -> IO ()
defaultErrLog = mkErrLogger "error.log"

withStamp :: String -> IO String
withStamp msg = getCurrentTime >>= return . (++ (" | " ++ msg)) . show

logWithStamp :: (String -> IO ()) -> String -> IO ()
logWithStamp = (withStamp >=>)
