{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage line =
  case typeCode of
    "E" -> parseDetails (Error (read (head rest))) (tail rest)
    "I" -> parseDetails Info rest
    "W" -> parseDetails Warning rest
    _ -> Unknown $ unwords rest
  where
    typeCode = head tokens
    rest = tail tokens
    tokens = words line

parseDetails :: MessageType -> [String] -> LogMessage
parseDetails messageType tokens =
  LogMessage messageType timeStamp message
    where
      (timeStampStr:rest) = tokens
      timeStamp = read timeStampStr
      message = unwords rest


parse :: String -> [LogMessage]
parse str = map parseMessage $ lines str


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMessage tree =
  case tree of
    Leaf -> Node Leaf logMessage Leaf
    Node leftTree otherLogMessage rightTree ->
       case compare (getTimeStamp logMessage) (getTimeStamp otherLogMessage) of
         LT -> Node (insert logMessage leftTree) otherLogMessage rightTree
         _ -> Node leftTree otherLogMessage (insert logMessage rightTree)

getTimeStamp :: LogMessage -> TimeStamp
getTimeStamp (LogMessage _ timeStamp _) = timeStamp
getTimeStamp (Unknown _) = error "No time stamp for unknown logs"

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder tree =
  case tree of
    (Leaf) -> []
    (Node left logMessage right) -> inOrder left ++ [logMessage] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages = extractMessages importantErrors
  where
    importantErrors = filter isImportantError $ inOrder (build logMessages)
    isImportantError (LogMessage (Error severity) _ _) = severity >= 50
    isImportantError _ = False

extractMessages :: [LogMessage] -> [String]
extractMessages = map extractMessage

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ msg) = msg
extractMessage (Unknown msg) = msg





