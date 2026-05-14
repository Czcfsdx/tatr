module Tatr
  ( createTask,
    listTasks,
    findTask,
    summaryTasks,
  )
where

createTask :: String -> IO ()
createTask title = do
  putStrLn $ "Creating a new task with title: " ++ title

listTasks :: IO ()
listTasks = do
  putStrLn "Listing the tasks!"

findTask :: IO ()
findTask = do
  putStrLn "Finding the task with a given ID"

summaryTasks :: IO ()
summaryTasks = do
  putStrLn "Printing the summary of the tasks!"
