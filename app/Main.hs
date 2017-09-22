{-# LANGUAGE OverloadedStrings #-}

import Data.List
import SQLGenerator
import System.Environment
import TransXChangeParser
import Types ()

-- Main
main :: IO ()
main = do
  args <- getArgs
  let file = head args
  fileContents <- readFile $ "input/" ++ file
  let transXChangeData = parseTransXChangeXML fileContents
  let sqlStatements = generateSQLStatements transXChangeData
  let output = intercalate "\n" sqlStatements
  writeFile ("output/" ++ file ++ ".sql") output
