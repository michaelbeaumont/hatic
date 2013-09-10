module Hatic.Types where
import Control.Monad.State
import Control.Concurrent

import System.IO
import Network.Socket

import qualified Data.HashTable.IO as H
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M

-- Server settings
data Settings = Settings
    {
        address :: String,
        port :: String,
        persistFile :: FilePath
    } deriving (Show)

-- default Config
defConfig = Settings "localhost" "7900" "."

-- Connection Info
data ConnInfo = ConnInfo 
    {
        ciSocket :: Socket,
        ciAddress :: SockAddr
    } deriving (Show)


-- Stored objects
data MemoryState = MemoryState {
                                getKV :: MVar KVHash,
                                getHH :: MVar HHash
                               }

-- State for Monad
data HState = HState { getConn :: Handle,
                       getMemState :: MemoryState, 
                       getAccLogger :: String -> IO (),
                       getErrLogger :: String -> IO () 
                     }

-- Server monad
type Hatic a = StateT HState IO a

-- Commands
data DBComm = Get B.ByteString | Set B.ByteString B.ByteString | HGet B.ByteString B.ByteString | HSet B.ByteString B.ByteString B.ByteString | CommError B.ByteString deriving (Show)

-- Result
data DBRes' a b = Value a | Msg b | Error b
type DBRes = DBRes' B.ByteString B.ByteString

-- Key/Value Hash
type KVHash = H.BasicHashTable B.ByteString B.ByteString

-- Hash of Maps
type HHash = H.BasicHashTable B.ByteString (M.Map B.ByteString B.ByteString)

-- Regex result
type RegexGroups = (B.ByteString,B.ByteString,B.ByteString,[B.ByteString])

--CLIENT TYPES
newtype ClientInfo = ClientInfo Handle

data ClientSettings = ClientSettings { settAddress :: String, settPort ::String}

defaultConfig :: ClientSettings
defaultConfig = ClientSettings "localhost" "7900"

