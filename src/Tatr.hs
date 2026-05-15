{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tatr
  ( createTask,
    listTasks,
    findTask,
    summaryTasks,
    TaskStatus (..),
    StatusToShow (..),
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower, toUpper)
import Data.List (dropWhileEnd, intercalate, sortOn, stripPrefix)
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Time
  ( LocalTime,
    ParseTime,
    defaultTimeLocale,
    formatTime,
    getZonedTime,
    parseTimeM,
    zonedTimeToLocalTime,
  )
import Log (Logger, debugMsg, errorMsg, infoMsg, warnMsg)
import Numeric.Natural (Natural)
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
    listDirectory,
    makeAbsolute,
  )
import System.FilePath ((<.>), (</>))
import Text.Read (readMaybe)

-- The number of the lines at the beginning of TASK.md
-- which will be parse as header
headerLinesNum :: Int
headerLinesNum = 5

newtype Timestamp = Timestamp LocalTime
  deriving (Eq, Ord, ParseTime)

instance Show Timestamp where
  -- show in yyyymmdd-HHMMSS, like 20260514-164802
  show (Timestamp time) = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" time

data TaskStatus = Open | Closed
  deriving (Eq, Read)

instance Show TaskStatus where
  show Open = "OPEN"
  show Closed = "CLOSED"

data StatusToShow = Only Tatr.TaskStatus | All
  deriving (Eq, Show)

data Task = Task
  { taskID :: Timestamp,
    taskTitle :: String,
    taskStatus :: TaskStatus,
    taskPriority :: Natural,
    taskTags :: [String]
  }
  deriving (Eq, Show)

instance Ord Task where
  compare x y = case compare (taskPriority x) (taskPriority y) of
    EQ -> compare (taskID x) (taskID y)
    rest -> rest

-- TODO: Better Error Handling

createTask :: FilePath -> String -> Natural -> [String] -> Logger ()
createTask workDir title priority tags = do
  timestamp <- liftIO getCurrentTimestamp
  absWorkDir <- liftIO $ makeAbsolute workDir
  let taskDir = absWorkDir </> show timestamp
  liftIO $ createDirectoryIfMissing False taskDir
  let taskPath = taskDir </> "TASK" <.> "md"
  let newTask = Task timestamp title Open priority tags
  debugMsg $ "Creating " ++ show newTask
  liftIO $ writeFile taskPath $ taskToHeader newTask
  infoMsg $ "Create Task in " ++ taskPath

listTasks :: FilePath -> StatusToShow -> Logger ()
listTasks workDir statusToShow = do
  absWorkDir <- liftIO $ makeAbsolute workDir
  entries <- liftIO $ listDirectory absWorkDir
  let timestamps = catMaybes $ map (\x -> parseTimeM True defaultTimeLocale "%Y%m%d-%H%M%S" x :: Maybe Timestamp) entries
  debugMsg $ "Found directories: " ++ show (map ((</>) absWorkDir . show) timestamps)
  tasks <- catMaybes <$> mapM (timestampToTask absWorkDir) timestamps
  let result = sortOn Down $ filter match tasks
  mapM_ (liftIO . putStrLn . formatTask absWorkDir) result
  where
    match = case statusToShow of
      Only Open -> (== Open) . taskStatus
      Only Closed -> (== Closed) . taskStatus
      All -> \_ -> True

findTask :: IO ()
findTask = do
  putStrLn "Finding the task with a given ID"

summaryTasks :: IO ()
summaryTasks = do
  putStrLn "Printing the summary of the tasks!"

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp <$> zonedTimeToLocalTime <$> getZonedTime

formatTask :: FilePath -> Task -> String
formatTask workDir task =
  workDir </> show timestamp </> "TASK.md:1: " ++ "[PRIORITY: " ++ show priority ++ formatTaskTags tags ++ "] " ++ title
  where
    timestamp = taskID task
    priority = taskPriority task
    title = taskTitle task
    tags = taskTags task
    formatTaskTags [] = ""
    formatTaskTags ts = ", TAGS: " ++ (intercalate ", " ts)

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

timestampToTask :: FilePath -> Timestamp -> Logger (Maybe Task)
timestampToTask workDir timestamp = do
  let taskDir = workDir </> show timestamp
  let path = taskDir </> "TASK.md"
  doesTaskMdExist <- liftIO $ doesFileExist path
  if doesTaskMdExist
    then do
      headerLines <- (take headerLinesNum . lines) <$> (liftIO $ readFile path)
      case headerToTask timestamp headerLines of
        Left msg -> (errorMsg $ path ++ ": " ++ msg) >> return Nothing
        Right task -> return $ Just task
    else (warnMsg $ "Fail to find TASK.md in " ++ taskDir ++ ". Ignore this path.") >> return Nothing

headerToTask :: Timestamp -> [String] -> Either String Task
headerToTask _ [] = Left "The header is empty"
headerToTask timestamp headerLines = do
  let hs = filter notBlank headerLines
  (titleLine, restLines) <- unconsHeader hs
  title <- parserTitle titleLine
  statusContent <- findField restLines "STATUS"
  priorityContent <- findField restLines "PRIORITY"
  tagsContent <- findField restLines "TAGS"
  status <- parseStatus statusContent
  priority <- parsePriority priorityContent
  tags <- parserTags tagsContent
  Right $ Task timestamp title status priority tags
  where
    parserTitle line
      | '#' : ' ' : afterHash <- dropWhile isSpace line = Right $ trim afterHash
      | otherwise = Left "Fail to find the title"

    findField [] field = Left $ "Fail to find field: " ++ field
    findField (line : rest) field
      | Just content <- stripPrefix ("- " ++ field ++ ":") line = Right $ trim content
      | otherwise = findField rest field

    parseStatus [] = Left "The value of STATUS is empty"
    parseStatus content
      | Just value <- readMaybe (capitalize content) :: Maybe TaskStatus = Right value
      | otherwise = Left $ "Fail to parse status from: " ++ content

    parsePriority [] = Left "The value of PRIORITY is empty"
    parsePriority content
      | Just value <- readMaybe content :: Maybe Natural = Right value
      | otherwise = Left $ "Fail to parse priority from: " ++ content

    parserTags content = Right $ filter notBlank $ map trim $ splitBy ',' content

    notBlank :: String -> Bool
    notBlank = not . all isSpace

    trim = (dropWhileEnd isSpace) . (dropWhile isSpace)

    unconsHeader [] = Left "The header is empty"
    unconsHeader (x : xs) = Right (x, xs)

    capitalize [] = []
    capitalize (x : xs) = toUpper x : map toLower xs

    splitBy _ [] = []
    splitBy c str =
      let (first, rest) = break (== c) str
       in first : case rest of
            [] -> []
            _ : rest' -> splitBy c rest'
