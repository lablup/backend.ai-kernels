{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------
-- This code is not working. Just keep this for possible future reference.
--------------------------------------------------------------------------

import Control.Monad
import GHC.Generics
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Aeson
import System.ZMQ4.Monadic
import qualified Data.ByteString.UTF8 as U
import Language.Haskell.Interpreter


-- Define execution result data type.
-- Map type cannot contain heterogeneous data.
data ExecResult = ExecResult {
    stdout :: String,
    stderr :: String,
    exceptions :: [Maybe String]
} deriving (Generic)
instance ToJSON ExecResult
instance FromJSON ExecResult

-- Exception information
data ExceptionInfo = ExceptionInfo {
    exc :: String,
    args :: [String],
    raisedBeforeExec :: Bool,
    traceback :: Maybe String
}
-- TODO: Serializer for ExceptionInfo type

main :: IO ()
main = do
    let
    runZMQ $ do
        skt <- socket Rep
        bind skt port
        printInZMQ "serving at port 2001..."
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

-- Print string inside ZMQ monad
printInZMQ :: String -> ZMQ z ()
printInZMQ = liftIO . putStrLn

-- Convert Lazy ByteString to Strict ByteString
lazyToStrict :: BL.ByteString -> B.ByteString
lazyToStrict = B.concat . BL.toChunks

-- Execute code and give results
executeCode :: String -> String -> (String, String, [Maybe String])
executeCode code_id code = ("fake out", "fake err", [])
-- TODO: real code execution routine
