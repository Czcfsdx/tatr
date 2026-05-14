module Tatr
  ( createTask,
    listTasks,
    findTask,
    summaryTasks,
  )
where

import Data.List (intercalate)
import Data.Time
  ( LocalTime,
    defaultTimeLocale,
    formatTime,
    getZonedTime,
    zonedTimeToLocalTime,
  )
import Numeric.Natural (Natural)
import System.Directory (createDirectoryIfMissing, makeAbsolute)
import System.FilePath ((<.>), (</>))

newtype Timestamp = Timestamp LocalTime
  deriving (Eq, Ord)

instance Show Timestamp where
  -- show in yyyymmdd-HHMMSS, like 20260514-164802
  show (Timestamp time) = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" time

data TaskStatus = Open | Closed
  deriving (Eq)

instance Show TaskStatus where
  show Open = "OPEN"
  show Closed = "Closed"

data Task = Task
  { taskID :: Timestamp,
    taskTitle :: String,
    taskStatus :: TaskStatus,
    taskPriority :: Natural,
    taskTAGS :: [String]
  }
  deriving (Eq, Show)

createTask :: FilePath -> String -> Natural -> [String] -> IO ()
createTask workdir title priority tags = do
  timestamp <- getCurrentTimestamp
  absDir <- makeAbsolute workdir
  let taskDir = absDir </> show timestamp
  createDirectoryIfMissing False taskDir
  let taskPath = taskDir </> "TASK" <.> "md"
  let newTask = Task timestamp title Open priority tags
  writeFile taskPath $ taskToHeader newTask
  putStrLn $ "[INFO] " ++ "Create Task in " ++ taskPath

listTasks :: IO ()
listTasks = do
  putStrLn "Listing the tasks!"

findTask :: IO ()
findTask = do
  putStrLn "Finding the task with a given ID"

summaryTasks :: IO ()
summaryTasks = do
  putStrLn "Printing the summary of the tasks!"

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp <$> zonedTimeToLocalTime <$> getZonedTime

taskToHeader :: Task -> String
taskToHeader (Task _ title status priority tags) =
  "# "
    ++ title
    ++ "\n\n- STATUS: "
    ++ show status
    ++ "\n- PRIORITY: "
    ++ show priority
    ++ "\n- TAGS: "
    ++ (intercalate "," tags)
