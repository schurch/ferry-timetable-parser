{-# LANGUAGE OverloadedStrings #-}

import TransXChangeParser
import Types

-- Main
main :: IO ()
main = do
  fileContents <-
    readFile
      "/Users/stefanchurch/Documents/Source/Haskell/parser/SVRFSACM05.xml"
  let transXChangeData = parseTransXChangeXML fileContents
  print transXChangeData
  return ()
