{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Aeson
import System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as U


-- Define execution result data type.
-- Map type cannot contain heterogeneous data.
data ExecResult = ExecResult {
    _stdout :: String,
    _stderr :: String,
    _exceptions :: [String]
} deriving (Generic)
instance ToJSON ExecResult
instance FromJSON ExecResult

main = do
    let
    putStrLn "serving at port 2001..."
    runZMQ $ do
        skt <- socket Rep
        bind skt port
        loop skt
    putStrLn "exit."

    where
    port = "tcp://*:2001"
    loop skt = do
        code_id <- receive skt
        code <- receive skt

        let (out, err, exceptions) = executeCode (show code_id) (U.toString code)
        let result = lazyToStrict . encode $ ExecResult out err exceptions

        send skt [] result
        loop skt

-- Convert Lazy ByteString to Strict ByteString
lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks

-- Execute code and give results
executeCode :: String -> String -> (String, String, [String])
executeCode code_id code = ("fake out", "fake err", ["fake exceptions"])
-- TODO: real code execution routine
