import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as Map
import Debug.Trace

import Data.Aeson
import System.ZMQ4.Monadic


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

        let result = Map.fromList [("stdout", out), ("stderr", err), ("exceptions", exceptions)]
        send skt [] code_id
        loop skt

executeCode :: String -> String -> (String, String, String)
executeCode code_id code = ("fake out", "fake err", "fake exceptions")
