{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Log
  ( debugMsg,
    infoMsg,
    warnMsg,
    errorMsg,
    Logger,
    runLogger,
    LogLevel (Debug, Info, Warn, Error),
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask)

data LogLevel = Debug | Info | Warn | Error
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

formatLogLevel :: LogLevel -> String
formatLogLevel Debug = "[DEBUG] "
formatLogLevel Info = "[INFO] "
formatLogLevel Warn = "[WARN] "
formatLogLevel Error = "[ERROR] "

newtype Logger a = Logger {runLogger :: ReaderT LogLevel IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader LogLevel)

logMsg :: LogLevel -> String -> Logger ()
logMsg level message = do
  minLevel <- ask
  when (level >= minLevel) $ liftIO $ putStrLn $ (formatLogLevel level) ++ message

debugMsg, infoMsg, warnMsg, errorMsg :: String -> Logger ()
debugMsg = logMsg Debug
infoMsg = logMsg Info
warnMsg = logMsg Warn
errorMsg = logMsg Error
