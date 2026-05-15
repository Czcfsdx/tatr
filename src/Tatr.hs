{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tatr
  ( createTask,
    listTasks,
    summaryTasks,
    TaskStatus (..),
    StatusToShow (..),
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isSpace, toLower, toUpper)
import qualified Data.HashMap.Strict as HM (HashMap, empty, insertWith, toList)
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
  )
import System.FilePath (takeDirectory, (<.>), (</>))
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

data StatusToShow = Only TaskStatus | All
  deriving (Eq)

instance Show StatusToShow where
  show (Only Open) = "OPEN"
  show (Only Closed) = "CLOSED"
  show All = "ALL"

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

createTask :: FilePath -> String -> Natural -> [String] -> Logger ()
createTask workDir title priority tags = do
  liftIO $ createDirectoryIfMissing False workDir
  timestamp <- liftIO getCurrentTimestamp
  let path = timestampToTaskPath workDir timestamp
  liftIO $ createDirectoryIfMissing False $ takeDirectory path
  let newTask = Task timestamp title Open priority tags
  debugMsg $ "Creating " ++ show newTask
  liftIO $ writeFile path $ taskToHeader newTask
  infoMsg $ "Create Task in " ++ path

listTasks :: FilePath -> StatusToShow -> Logger ()
listTasks workDir statusToShow = do
  tasks <- getAllTasks workDir
  let result = sortOn Down $ filter (matchTask statusToShow) tasks
  mapM_ (liftIO . putStrLn . formatTask workDir) result

summaryTasks :: FilePath -> StatusToShow -> Logger ()
summaryTasks workDir statusToShow = do
  tasks <- getAllTasks workDir
  let filteredTasks = filter (matchTask statusToShow) tasks
  let (total, untagged, tagMap) = foldl' collect (0, 0, HM.empty) filteredTasks
  let tagList = sortOn snd $ HM.toList tagMap
  liftIO $ putStrLn $ "STAUTS:   " ++ show statusToShow
  liftIO $ putStrLn $ "TOTAL:    " ++ show total
  liftIO $ putStrLn $ "UNTAGGED: " ++ show untagged
  liftIO $ putStrLn $ "TAGGED:"
  unless (tagList == []) $
    let maxTagLen = maximum $ map (length . fst) tagList
     in mapM_ (liftIO . putStrLn . formatTag maxTagLen) tagList
  where
    collect :: (Int, Int, HM.HashMap String Int) -> Task -> (Int, Int, HM.HashMap String Int)
    collect (total, untagged, tagMap) task =
      case taskTags task of
        [] -> (total + 1, untagged + 1, tagMap)
        tags -> (total + 1, untagged, go tags tagMap)

    go [] = id
    go (t : ts) = go ts . HM.insertWith (+) t 1

    formatTag len (tag, count) =
      replicate (len + 4 - length tag) ' ' ++ tag ++ " => " ++ show count

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp <$> zonedTimeToLocalTime <$> getZonedTime

matchTask :: StatusToShow -> Task -> Bool
matchTask (Only Open) = (== Open) . taskStatus
matchTask (Only Closed) = (== Closed) . taskStatus
matchTask All = \_ -> True

getAllTasks :: FilePath -> Logger [Task]
getAllTasks workDir = do
  entries <- liftIO $ listDirectory workDir
  let timestamps = catMaybes $ map (\x -> parseTimeM True defaultTimeLocale "%Y%m%d-%H%M%S" x :: Maybe Timestamp) entries
  debugMsg $ "Found directories: " ++ show (map ((</>) workDir . show) timestamps)
  tasks <- catMaybes <$> mapM (timestampToTask workDir) timestamps
  return tasks

timestampToTaskPath :: FilePath -> Timestamp -> FilePath
timestampToTaskPath workDir timestamp = workDir </> show timestamp </> "TASK" <.> "md"

timestampToTask :: FilePath -> Timestamp -> Logger (Maybe Task)
timestampToTask workDir timestamp = do
  let path = timestampToTaskPath workDir timestamp
  doesTaskMdExist <- liftIO $ doesFileExist path
  if doesTaskMdExist
    then do
      headerLines <- (take headerLinesNum . lines) <$> (liftIO $ readFile path)
      case headerToTask timestamp headerLines of
        Left msg -> (errorMsg $ path ++ ": " ++ msg) >> return Nothing
        Right task -> return $ Just task
    else (warnMsg $ "Fail to find " ++ path ++ " . Ignore this path.") >> return Nothing

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
