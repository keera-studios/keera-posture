module MyIO where

myPutStrLn :: String -> IO()
myPutStrLn s = appendFile "log" $ s ++ "\r\n"
