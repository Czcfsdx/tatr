module Main (main) where

import Options.Applicative
import qualified Tatr

data Args = Args GlobalOpts Command
  deriving (Show)

data GlobalOpts = GlobalOpts
  {optDir :: FilePath}
  deriving (Show)

-- TODO: Maybe We Don't need Find command
data Command
  = New NewArgs
  | List
  | Find
  | Summary
  deriving (Show)

data NewArgs = NewArgs
  {title :: String}
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

newParser :: Parser Command
newParser = New <$> newArgsParser
  where
    newArgsParser =
      NewArgs
        <$> argument
          str
          (metavar "TITLE")

main :: IO ()
main = do
  Args opts subCommand <- execParser parser
  putStrLn $ "Tasks directory: " ++ optDir opts
  case subCommand of
    New args -> Tatr.createTask (title args)
    List -> Tatr.listTasks
    Find -> Tatr.findTask
    Summary -> Tatr.summaryTasks
