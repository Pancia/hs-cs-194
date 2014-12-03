import Data.Char
import Data.List
import Log

parseMessage :: String -> MaybeLogMessage
parseMessage string
        = case words string of
            ("E":sev:ts:msg) -> let (ValidInt sev') = (readInt sev)
                                    (ValidInt ts') = (readInt ts)
                                in ValidLM $ LogMessage (Error sev') ts' $ unwords msg
            ("W":ts:msg)     -> let (ValidInt ts') = (readInt ts)
                                in ValidLM $ LogMessage Warning ts' $ unwords msg
            ("I":ts:msg)     -> let (ValidInt ts') = (readInt ts)
                                in ValidLM $ LogMessage Info ts' $ unwords msg
            _                -> InvalidLM string

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly = (filter removeInvalidLM) . (map maybeLMToLM)
        where removeInvalidLM (LogMessage _ _ s) = "qqqq" /= s
              maybeLMToLM mlm = case mlm of
                                    InvalidLM _ -> LogMessage (Error 0) 0 "qqqq"
                                    ValidLM lm -> lm

parse :: String -> [LogMessage]
parse = validMessagesOnly . (map parseMessage) . lines

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ a _) (LogMessage _ b _) = compare a b

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages = sortBy compareMsgs

toListString :: [LogMessage] -> [String]
toListString = map (\(LogMessage _ _ m) -> m)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = toListString . keepRelevantErrors

keepRelevantErrors :: [LogMessage] -> [LogMessage]
keepRelevantErrors = (sortBy compareMsgs) . (filter isRelevant)
        where isRelevant (LogMessage (Error sev) _ _) = 50 <= sev
              isRelevant _ = False

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout patt list = filter (\(LogMessage _ _ msg) -> (isInfixOf patt (map toLower msg))) list

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced p = toListString . (messagesAbout p) . keepRelevantErrors
