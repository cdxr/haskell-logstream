module Data.LogStream where

import Prelude hiding ( log )
import Pipes

import Data.Maybe
import qualified Data.Text as T


-- | The label for a log, used to filter and route logs between handlers.
type LogLabel = T.Text

-- | The priority of a log, used to sort and filter logs by their
-- importance.
data LogLevel
    = DEBUG
    | NOTICE
    | WARNING
    | ERROR
    | CRITICAL
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | A @Log t@ is a unit of data @t@, usually `Text`, annotated with a
-- `LogLabel` and `LogLevel` and eventually passed to a @LogHandler t m@.
data Log t = Log
    { logLabel :: !LogLabel
    , logLevel :: !LogLevel
    , logValue :: t
    } deriving (Show, Read, Eq, Ord)


type Logger t = Producer (Log t)


-- | A @LogHandler t m@ is an action that will execute in response to a
-- log, or ignore the log based on its contents. A @LogHandler@ can be
-- matched against a log without performing the action.
newtype LogHandler t m = LogHandler (Log t -> Maybe (m ()))

-- | @processLog h l@ processes the log @l@ with the handler @h@. If @h@
-- does not handle @l@, it returns @False@.
processLog :: (Monad m) => LogHandler t m -> Log t -> m Bool
processLog (LogHandler f) log =
    case f log of
        Nothing -> return False
        Just m  -> m >> return True

-- | @handlesLog h l@ determines the output of @processLog h l@ without
-- performing the handler action.
handlesLog :: LogHandler t m -> Log t -> Bool
handlesLog (LogHandler f) = isJust . f


-- | @handler f@ is the LogHandler that matches all logs, performing
-- @f@ on their enclosed value.
handler :: (Monad m) => (t -> m ()) -> LogHandler t m
handler f = LogHandler $ Just . f . logValue

-- | @filterHandler p h@ is a handler that discards all logs that do not
-- match the predicate @p@. Otherwise, it behaves like @h@.
filterHandler :: (Log t -> Bool) -> LogHandler t m -> LogHandler t m
filterHandler p (LogHandler f) = LogHandler $ \log ->
    if p log then f log else Nothing

-- | @label p@ is a handler that filters based on a log's @LogLabel@.
label :: (LogLabel -> Bool) -> LogHandler t m -> LogHandler t m
label p = filterHandler $ p . logLabel

-- | @level p@ is a handler that filters based on a log's @LogLevel@.
level :: (LogLevel -> Bool) -> LogHandler t m -> LogHandler t m
level p = filterHandler $ p . logLevel
