{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
import qualified Data.HashMap.Strict as HM (HashMap, empty, insertWith, toList)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
  ( intercalate,
    length,
    lines,
    pack,
    replicate,
    split,
    strip,
    stripPrefix,
  )
-- TODO: If you want to do I/O using the UTF-8 encoding, use Data.Text.IO.Utf8, which is faster than this module.
-- TODO: TIO.readFile read strictly
import qualified Data.Text.IO as TIO (putStrLn, readFile, writeFile)
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
import Utils (allIsSpace, capitalize, treadMaybe, tshow)

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
    taskTitle :: Text,
    taskStatus :: TaskStatus,
    taskPriority :: Natural,
    taskTags :: [Text]
  }
  deriving (Eq, Show)

instance Ord Task where
  compare x y = case compare (taskPriority x) (taskPriority y) of
    EQ -> compare (taskID x) (taskID y)
    rest -> rest

createTask :: FilePath -> Text -> Natural -> [Text] -> Logger ()
createTask workDir title priority tags = do
  liftIO $ createDirectoryIfMissing False workDir
  timestamp <- liftIO getCurrentTimestamp
  let path = timestampToTaskPath workDir timestamp
  liftIO $ createDirectoryIfMissing False $ takeDirectory path
  let newTask = Task timestamp title Open priority tags
  debugMsg $ "Creating " <> tshow newTask
  liftIO $ TIO.writeFile path $ taskToHeader newTask
  infoMsg $ "Create Task in " <> (T.pack path)

listTasks :: FilePath -> StatusToShow -> [Text] -> Bool -> Bool -> Logger ()
listTasks workDir status tags doesSortByTime doesSortReverse = do
  tasks <- getAllTasks workDir
  let result = sortTasks doesSortByTime doesSortReverse $ filter (matchTask status tags) tasks
  mapM_ (liftIO . TIO.putStrLn . formatTask workDir) result

summaryTasks :: FilePath -> StatusToShow -> Logger ()
summaryTasks workDir statusToShow = do
  tasks <- getAllTasks workDir
  let filteredTasks = filter (matchTask statusToShow []) tasks
  let (total, untagged, tagMap) = foldl' collect (0, 0, HM.empty) filteredTasks
  let tagList = sortOn snd $ HM.toList tagMap
  liftIO $ TIO.putStrLn $ "STAUTS:   " <> tshow statusToShow
  liftIO $ TIO.putStrLn $ "TOTAL:    " <> tshow total
  liftIO $ TIO.putStrLn $ "UNTAGGED: " <> tshow untagged
  liftIO $ TIO.putStrLn $ "TAGGED:"
  unless (tagList == []) $
    let maxTagLen = maximum $ map (T.length . fst) tagList
     in mapM_ (liftIO . TIO.putStrLn . formatTag maxTagLen) tagList
  where
    collect :: (Int, Int, HM.HashMap Text Int) -> Task -> (Int, Int, HM.HashMap Text Int)
    collect (total, untagged, tagMap) task =
      case taskTags task of
        [] -> (total + 1, untagged + 1, tagMap)
        tags -> (total + 1, untagged, go tags tagMap)

    go [] = id
    go (t : ts) = go ts . HM.insertWith (+) t 1

    formatTag :: Int -> (Text, Int) -> Text
    formatTag len (tag, count) =
      T.replicate (len + 4 - T.length tag) " " <> tag <> " => " <> tshow count

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = Timestamp <$> zonedTimeToLocalTime <$> getZonedTime

sortTasks :: Bool -> Bool -> [Task] -> [Task]
sortTasks False False = sortOn (Down . taskPriority)
sortTasks True False = sortOn (Down . taskID)
sortTasks False True = sortOn (taskPriority)
sortTasks True True = sortOn (taskID)

matchTask :: StatusToShow -> [Text] -> Task -> Bool
matchTask (Only s) tags task = s == taskStatus task && all (`elem` ts) tags
  where
    ts = taskTags task
matchTask All tags task = all (`elem` ts) tags
  where
    ts = taskTags task

getAllTasks :: FilePath -> Logger [Task]
getAllTasks workDir = do
  entries <- liftIO $ listDirectory workDir
  let timestamps = catMaybes $ map (\x -> parseTimeM True defaultTimeLocale "%Y%m%d-%H%M%S" x :: Maybe Timestamp) entries
  debugMsg $ "Found directories: " <> tshow (map ((</>) workDir . show) timestamps)
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
      headerLines <- (take headerLinesNum . T.lines) <$> (liftIO $ TIO.readFile path)
      case headerToTask timestamp headerLines of
        Left msg -> (errorMsg $ T.pack path <> ": " <> msg) >> return Nothing
        Right task -> return $ Just task
    else (warnMsg $ "Fail to find " <> T.pack path <> " . Ignore this path.") >> return Nothing

formatTask :: FilePath -> Task -> Text
formatTask workDir task =
  T.pack (timestampToTaskPath workDir timestamp) <> ":1: " <> "[PRIORITY: " <> tshow priority <> formatTaskTags tags <> "] " <> title
  where
    timestamp = taskID task
    priority = taskPriority task
    title = taskTitle task
    tags = taskTags task
    formatTaskTags [] = ""
    formatTaskTags ts = ", TAGS: " <> (T.intercalate ", " ts)

taskToHeader :: Task -> Text
taskToHeader (Task _ title status priority tags) =
  "# "
    <> title
    <> "\n\n- STATUS: "
    <> tshow status
    <> "\n- PRIORITY: "
    <> tshow priority
    <> "\n- TAGS: "
    <> (T.intercalate "," tags)

headerToTask :: Timestamp -> [Text] -> Either Text Task
headerToTask _ [] = Left "The header is empty"
headerToTask timestamp headerLines = do
  let hs = filter (not . allIsSpace) headerLines
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
      | Just content <- T.stripPrefix "# " line = Right $ T.strip content
      | otherwise = Left "Fail to find the title"

    findField [] field = Left $ "Fail to find field: " <> field
    findField (line : rest) field
      | Just content <- T.stripPrefix ("- " <> field <> ":") line = Right $ T.strip content
      | otherwise = findField rest field

    parseStatus "" = Left "The value of STATUS is empty"
    parseStatus content
      | Just value <- treadMaybe (capitalize content) :: Maybe TaskStatus = Right value
      | otherwise = Left $ "Fail to parse status from: " <> content

    parsePriority "" = Left "The value of PRIORITY is empty"
    parsePriority content
      | Just value <- treadMaybe content :: Maybe Natural = Right value
      | otherwise = Left $ "Fail to parse priority from: " <> content

    parserTags content = Right $ filter (not . allIsSpace) $ map T.strip $ T.split (== ',') content

    unconsHeader [] = Left "The header is empty"
    unconsHeader (x : xs) = Right (x, xs)
