import System.ZMQ4.Monadic

main :: IO ()
main = do
    let port = "tcp://*:2001"
    putStrLn "Started..."
    runZMQ $ do
        skt <- socket Rep
        bind skt port
        loop skt
    where
        loop skt = do
            msg <- receive skt
            send skt [] msg
            loop skt
