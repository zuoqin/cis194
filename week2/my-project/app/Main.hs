module Main where

import LogAnalysis
import Log

x = testParse parse 10 "E:\\DEV\\Haskell\\CIS194\\week2\\my-project\\resources\\error.log"
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "E 2 3151 fond of beheading people here; the great wonder is, that there's any one"

-- show
baz :: LogMessage -> String
baz p@(LogMessage _ _ c) = "The content field of (" ++ show p ++ ") is " ++ c


convertmsgtostr :: LogMessage -> IO()
convertmsgtostr msg = putStrLn (baz msg)

main :: IO ()
main = do
	list <- x
	(mapM_ print) list
