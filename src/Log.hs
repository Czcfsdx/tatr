{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Log
  ( debugMsg,
    infoMsg,
    warnMsg,
    errorMsg,
    Logger,
    runLogger,
    LogLevel (Debug, Info, Warn, Error),
    showAvailableLogLevel,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Data.Text (Text)
import qualified Data.Text as T (intercalate, toLower)
import qualified Data.Text.IO as TIO (putStrLn)
import Utils (tshow)

data LogLevel = Debug | Info | Warn | Error
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

formatLogLevel :: LogLevel -> Text
formatLogLevel Debug = "[DEBUG] "
formatLogLevel Info = "[INFO] "
formatLogLevel Warn = "[WARN] "
formatLogLevel Error = "[ERROR] "

newtype Logger a = Logger {runLogger :: ReaderT LogLevel IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LogLevel)

logMsg :: LogLevel -> Text -> Logger ()
logMsg level message = do
  minLevel <- ask
  when (level >= minLevel) $ liftIO $ TIO.putStrLn $ (formatLogLevel level) <> message

debugMsg, infoMsg, warnMsg, errorMsg :: Text -> Logger ()
debugMsg = logMsg Debug
infoMsg = logMsg Info
warnMsg = logMsg Warn
errorMsg = logMsg Error

showAvailableLogLevel :: Text
showAvailableLogLevel =
  "available levels: "
    <> ( T.intercalate " | " $
           map (T.toLower . tshow) [minBound :: Log.LogLevel .. maxBound :: Log.LogLevel]
       )
