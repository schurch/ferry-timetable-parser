{-# LANGUAGE OverloadedStrings #-}

module SQLGenerator
  ( generateSQLStatements
  ) where

import Data.List
import Text.Printf
import Types

generateSQLStatements :: TransXChangeData -> [String]
generateSQLStatements transXChangeData =
  concat [stopPointStatements, routeSectionStatements, routeStatements]
  where
    stopPointStatements =
      map stopPointToSQLStatement $ stopPoints transXChangeData
    routeSectionStatements =
      map routeSectionToSQLStatement $ routeSections transXChangeData
    routeStatements = map routeToSQLStatement $ routes transXChangeData

stopPointToSQLStatement :: AnnotatedStopPointRef -> String
stopPointToSQLStatement stopPoint =
  printf
    "INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('%s', '%s', 0, 0);"
    (stopPointRef stopPoint)
    (commonName stopPoint)

routeSectionToSQLStatement :: RouteSection -> String
routeSectionToSQLStatement routeSection =
  routeSectionStatement ++ "\n" ++ intercalate "\n" routeLinkStatements
  where
    routeSectionStatement =
      printf
        "INSERT INTO RouteSection (RouteSectionId) VALUES ('%s');"
        (routeSectionId routeSection)
    routeLinkToSQL = routeLinkToSQLStatement (routeSectionId routeSection)
    routeLinkStatements = map routeLinkToSQL $ routeLinks routeSection

routeLinkToSQLStatement :: String -> RouteLink -> String
routeLinkToSQLStatement routeSectionId routeLink =
  printf
    "INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, RouteDirection) VALUES ('%s', '%s', '%s', '%s', '%s');"
    (routeLinkId routeLink)
    routeSectionId
    (fromStopPointRef routeLink)
    (toStopPointRef routeLink)
    (routeDirection routeLink)

routeToSQLStatement :: Route -> String
routeToSQLStatement route =
  printf
    "INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('%s', '%s', '%s');"
    (routeId route)
    (routeDescription route)
    (routeSectionRef route)
