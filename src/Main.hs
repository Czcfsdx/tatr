module Main (main) where

import Numeric.Natural (Natural)
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
  case subCommand of
    New args -> Tatr.createTask (optDir opts) (newArgsTitle args) (newArgsPriority args) (newArgsTags args)
    List -> Tatr.listTasks
    Find -> Tatr.findTask
    Summary -> Tatr.summaryTasks
