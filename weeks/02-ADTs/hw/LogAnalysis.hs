-- CIS 194, Fall 2010
--
-- Sample solution for HW 2.

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Data.Char (toUpper)
import Data.List (isInfixOf)
import Log

-- | Parse a single line into a LogMessage object.
parseMessage :: String -> LogMessage
parseMessage m = parseAux . words $ m
  where parseAux ("I":txt)     = buildMsg Info txt
        parseAux ("W":txt)     = buildMsg Warning txt
        parseAux ("E":lvl:txt) = buildMsg (Error (read lvl)) txt
        parseAux _             = Unknown m
        buildMsg tag (ts:rst)  = LogMessage tag (read ts) (unwords rst)
        buildMsg _ _           = Unknown m

-- | Parse an entire file full of log messages.
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- | Split a list of log messages into the messages which happened
--   before a major (severity > 50) error, and those which happened
--   after (the error itself is included with the ones after).
findBigError :: [LogMessage] -> ([LogMessage], [LogMessage])
findBigError = break bigDeal
  where bigDeal (LogMessage (Error lvl) _ _) = lvl > 50
        bigDeal _ = False

-- | Extract the warning messages associated with the component that
--   failed.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = relevantWarnings (head $ words failTxt) prefix
  where (prefix, bigError : _) = findBigError msgs
        failTxt                = getMsg bigError

-- | Given the name of the component that failed, extract warnings
--   which mention that component.
relevantWarnings :: String -> [LogMessage] -> [String]
relevantWarnings keyword = filter relevant . map getMsg . filter isWarning
  where isWarning (LogMessage Warning _ _) = True
        isWarning _                        = False
        relevant txt                       = upperWord keyword `isInfixOf`
                                             upperWord txt

-- | Get the message payload of a LogMessage object.
getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg (Unknown s)          = s

-- | Convert a word to all uppercase.
upperWord :: String -> String
upperWord = map toUpper

