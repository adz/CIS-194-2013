{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
{-parseMessage "E 2 562 help help"-}
  {-== LogMessage (Error 2) 562 "help help"-}
{-parseMessage "I 29 la la la"-}
  {-== LogMessage Info 29 "la la la"-}
{-parseMessage "This is not in the right format"-}
  {-== Unknown "This is not in the right format"-}
-- Info /warning / error-ing
-- time
-- string
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

