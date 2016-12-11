module Main where

import LogAnalysis
import Log


--Exercise 1
x = testParse parse 10 "E:\\DEV\\Haskell\\CIS194\\week2\\my-project\\resources\\error.log"
-- parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- parseMessage "E 2 3151 fond of beheading people here; the great wonder is, that there's any one"


--Exercise 5: Just run main in REPL to see results
main :: IO ()
main = do
  list <- parse <$> readFile "E:\\DEV\\Haskell\\CIS194\\week2\\my-project\\resources\\sample.log"
  let result = whatWentWrong list
  (mapM_ print) result


