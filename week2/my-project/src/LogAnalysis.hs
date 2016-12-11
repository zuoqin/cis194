-- Log file parsing

module LogAnalysis
    ( parseMessage,
      parse,
      insert,
      build,
      inOrder,
      isErrorSeverity50,
      whatWentWrong
    ) where

import Log

isunknownmsg :: LogMessage -> Bool
isunknownmsg (Unknown _) = True
isunknownmsg _ = False

gettimestamp :: LogMessage -> Int
gettimestamp (LogMessage _ t _) = t

get_titem_time :: MessageTree -> Int
get_titem_time (Node l (LogMessage _ nt _) r) = nt

get_titem_l :: MessageTree -> MessageTree
get_titem_l (Node l m r) = l

get_titem_r :: MessageTree -> MessageTree
get_titem_r (Node l m r) = r
               
get_titem_msg :: MessageTree -> LogMessage
get_titem_msg (Node l m r) = m
               
insert :: LogMessage -> MessageTree -> MessageTree
insert msg tree
  | tree == Leaf = Node Leaf msg Leaf
  | isunknownmsg(msg) == True = tree
  | otherwise = if (get_titem_time tree) > (gettimestamp msg)
    then Node (insert msg (get_titem_l tree)) (get_titem_msg tree) (get_titem_r tree)
    else Node (get_titem_l tree) (get_titem_msg tree) (insert msg (get_titem_r tree)) 
    
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


myfunc :: String -> String
myfunc s = s

loadfile :: String -> IO MessageTree
loadfile file =  do
  x <- take 5 . parse <$> readFile file
  return $ build x

sortlog :: String -> IO [String]
sortlog file = do
  x <- parse <$> readFile file
  -- Instead of: map getMessage (filter isErrorSeverity50 (inOrder (build x)))
  -- use one function:
  return $ whatWentWrong x


parse :: String -> [LogMessage]
parse contents = (map parseMessage (lines contents))

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

isErrorSeverity50 :: LogMessage -> Bool
isErrorSeverity50 (LogMessage (Error s) _ _) = s >= 50
isErrorSeverity50 _                          = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m
getMessage (Unknown m)        = m


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMessage . inOrder . build . filter isErrorSeverity50
