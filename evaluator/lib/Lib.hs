module Lib where

import System.Exit
import System.Process

apiKey = "19afd493c81c4b22b67556852d3d6740"

runCurl :: [String] -> IO String
runCurl args = do
    (status, stdout, stderr) <- readProcessWithExitCode "curl" args ""
    case status of
        ExitSuccess      -> return stdout
        ExitFailure code -> fail $ "Can't run curl with args" ++ (show args) ++ "\nstderr:" ++ stderr ++ "\nexit code:" ++ (show code)

send :: String -> IO String
send s = do
    runCurl [ "-X"
              , "POST"
             , "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=" ++ apiKey
             , "-H"
             , "\"accept: */*\""
             , "-H"
             , "\"Content-Type: text/plain\""
             , "-d"
             , s
             ]
