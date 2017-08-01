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
  fileContents <-
    readFile $
    "/Users/stefanchurch/Documents/Source/Haskell/parser/input/" ++ file
  let transXChangeData = parseTransXChangeXML fileContents
  let sqlStatements = generateSQLStatements transXChangeData
  let output = intercalate "\n" sqlStatements
  writeFile
    ("/Users/stefanchurch/Documents/Source/Haskell/parser/output/" ++
     file ++ ".sql")
    output
