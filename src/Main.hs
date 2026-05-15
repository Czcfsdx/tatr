module Main (main) where

import Control.Monad.Reader (runReaderT)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Log (LogLevel (Debug, Error, Info, Warn), runLogger)
import Numeric.Natural (Natural)
import Options.Applicative
import qualified Tatr

data Args = Args GlobalOpts Command
  deriving (Show)

data GlobalOpts = GlobalOpts
  { optDir :: FilePath,
    optLogLevel :: Log.LogLevel
  }
  deriving (Show)

-- TODO: Maybe We Don't need Find command
data Command
  = New NewArgs
  | List
  | Find
  | Summary
  deriving (Show)

data NewArgs = NewArgs
  { newArgsTitle :: String,
    newArgsPriority :: Natural,
    newArgsTags :: [String]
  }
  deriving (Show)

parser :: ParserInfo Args
parser =
  info
    (argsParser <**> helper)
    ( fullDesc
        <> progDesc "Track and manager tasks in DIRECTORY"
        <> header "tatr - a tasks tracker"
    )

argsParser :: Parser Args
argsParser =
  Args
    <$> globalOptsParser
    <*> hsubparser
      ( command "new" (info newParser (progDesc "Create a new task"))
          <> command "ls" (info (pure List) (progDesc "List the tasks"))
          <> command "find" (info (pure Find) (progDesc "Find the task with a given ID"))
          <> command "summary" (info (pure Find) (progDesc "Print the summary of the tasks"))
      )

globalOptsParser :: Parser GlobalOpts
globalOptsParser =
  GlobalOpts
    <$> strOption
      ( long "dir"
          <> short 'd'
          <> metavar "DIRECTORY"
          <> help "Path to the tasks directory"
          <> showDefault
          <> value "."
      )
    <*> option
      (eitherReader logLevelReader)
      ( long "log-level"
          <> metavar "LEVEL"
          <> help ("Set Log level, " ++ showAvailableLogLevel)
          <> showDefaultWith (map toLower . show)
          <> value Log.Info
      )

logLevelReader :: String -> Either String Log.LogLevel
logLevelReader s =
  case s of
    "debug" -> Right Log.Debug
    "info" -> Right Log.Info
    "warn" -> Right Log.Warn
    "error" -> Right Log.Error
    _ -> Left $ "Unknown log level: " ++ s ++ "\n    " ++ showAvailableLogLevel

showAvailableLogLevel :: String
showAvailableLogLevel =
  "available levels: "
    ++ ( intercalate " | " $
           map (map toLower . show) [minBound :: Log.LogLevel .. maxBound :: Log.LogLevel]
       )

newParser :: Parser Command
newParser = New <$> newArgsParser
  where
    newArgsParser =
      NewArgs
        <$> strArgument
          (metavar "TITLE")
        <*> option
          auto
          ( long "priority"
              <> short 'p'
              <> metavar "PRIORITY"
              <> help "Priority of the new task"
              <> showDefault
              <> value 30
          )
        <*> many
          ( strOption
              ( long "tag"
                  <> short 't'
                  <> metavar "TAG"
                  <> help "Add tag for the new task (can added multiple times)"
              )
          )

main :: IO ()
main = do
  Args opts subCommand <- execParser parser
  let level = optLogLevel opts
  result <-
    case subCommand of
      New args -> runReaderT (Log.runLogger newCommand) level
        where
          newCommand =
            Tatr.createTask (optDir opts) (newArgsTitle args) (newArgsPriority args) (newArgsTags args)
      List -> runReaderT (Log.runLogger listCommand) level
        where
          listCommand = Tatr.listTasks (optDir opts)
      Find -> Tatr.findTask
      Summary -> Tatr.summaryTasks
  return result
