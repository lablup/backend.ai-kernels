{-# LANGUAGE DeriveGeneric #-}

import Debug.Trace
import GHC.Generics
import qualified Data.Text.Lazy.Encoding as T

import Data.Aeson
import System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as U


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

        -- TODO: execute code routine
        let (out, err, exceptions) = executeCode (show code_id) (U.toString code)

        let result = encode $ ExecResult out err exceptions

        send skt [] result
        loop skt

executeCode :: String -> String -> (String, String, [String])
executeCode code_id code = ("fake out", "fake err", ["fake exceptions"])
