module MyIO where

-- import System.IO

myPutStrLn :: String -> IO()
myPutStrLn s = do
 appendFile "log" $ s ++ "\r\n"
 -- putStrLn s
 -- hFlush stdout
 -- hFlush stderr
