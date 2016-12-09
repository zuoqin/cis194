-- Log file parsing

module LogAnalysis
    ( parseMessage,
      parse,
	  insert
    ) where

import Log


insert :: LogMessage -> MessageTree -> MessageTree
insert msg tr = tr

testmsg :: String -> String
testmsg msg = ((splitMessage msg) !!2)

concatenatemsg :: [String] -> String
concatenatemsg ls = foldr (++) " " ls

splitMessage :: String -> [String]
splitMessage msg = words msg

asciitonum :: (Char,Int) -> Int
asciitonum (x,y) = ((fromEnum x) - 48) * (10 ^y)

errorstrtonum :: String -> Int
errorstrtonum str = sum(map asciitonum (zip (reverse str) [0..]))

integertoint :: Integer -> Int
integertoint x = fromInteger(x)

removespace :: String -> String
removespace x = take ((length x) - 1) x

parseMessage :: String -> LogMessage
parseMessage msg = if head msg == 'E' 
		then LogMessage (Error  (errorstrtonum ((splitMessage msg) !!1))) (errorstrtonum ((splitMessage msg) !!2)) (removespace (concatMap (++" ") (drop 3 (splitMessage msg))))
		else if head msg == 'I'
			then LogMessage Info (errorstrtonum ((splitMessage msg) !!1)) (removespace (concatMap (++" ") (drop 2 (splitMessage msg))))
			else LogMessage Warning (errorstrtonum ((splitMessage msg) !!1)) (removespace (concatMap (++" ") (drop 2 (splitMessage msg))))


parse :: String -> [LogMessage]
parse contents = (map parseMessage (lines contents))