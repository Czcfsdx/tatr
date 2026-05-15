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

data Command
  = New NewArgs
  | List ListArgs
  | Summary SummaryArgs
  deriving (Show)

data NewArgs = NewArgs
  { newArgsTitle :: String,
    newArgsPriority :: Natural,
    newArgsTags :: [String]
  }
  deriving (Show)

data ListArgs = ListArgs
  { listArgsStatus :: Tatr.StatusToShow,
    listArgsTags :: [String],
    listArgsSortByTime :: Bool
  }
  deriving (Show)

data SummaryArgs = SummaryArgs
  {summaryArgsStatus :: Tatr.StatusToShow}
  deriving (Show)

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
      List args -> runReaderT (Log.runLogger listCommand) level
        where
          listCommand = Tatr.listTasks (optDir opts) (listArgsStatus args) (listArgsTags args) (listArgsSortByTime args)
      Summary args -> runReaderT (Log.runLogger summaryCommand) level
        where
          summaryCommand = Tatr.summaryTasks (optDir opts) (summaryArgsStatus args)
  return result

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
          <> command "ls" (info listParser (progDesc "List the tasks"))
          <> command "summary" (info summaryParser (progDesc "Print the summary of the tasks"))
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
                  <> help "Add tag for the new task (can add multiple times)"
              )
          )

listParser :: Parser Command
listParser = List <$> listArgsParser
  where
    listArgsParser =
      ListArgs
        <$> statusToShowParser "List"
        <*> many
          ( strOption
              ( long "tag"
                  <> short 't'
                  <> metavar "TAG"
                  <> help "Show only tasks with given tags (can add multiple times)"
              )
          )
        <*> switch
          ( long "time"
              <> help "Sort the tasks by timestamp"
          )

summaryParser :: Parser Command
summaryParser = Summary <$> summaryArgsParser
  where
    summaryArgsParser =
      SummaryArgs
        <$> statusToShowParser "Summary"

statusToShowParser :: String -> Parser Tatr.StatusToShow
statusToShowParser verb =
  flag'
    (Tatr.Only Tatr.Closed)
    ( long "closed"
        <> short 'c'
        <> help (verb ++ " closed tasks")
    )
    <|> flag'
      Tatr.All
      ( long "all"
          <> short 'a'
          <> help (verb ++ " all tasks")
      )
    <|> pure (Tatr.Only Tatr.Open)

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
