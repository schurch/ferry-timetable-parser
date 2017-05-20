{-# LANGUAGE OverloadedStrings #-}

import Data.List
import SQLGenerator
import TransXChangeParser
import Types ()

-- Main
main :: IO ()
main = do
  fileContents <-
    readFile
      "/Users/stefanchurch/Documents/Source/Haskell/parser/input/SVRFSACM05.xml"
  let transXChangeData = parseTransXChangeXML fileContents
  let sqlStatements = generateSQLStatements transXChangeData
  let output = intercalate "\n" sqlStatements
  writeFile
    "/Users/stefanchurch/Documents/Source/Haskell/parser/output/SVRFSACM05.xml.sql"
    output
