{-# LANGUAGE OverloadedStrings #-}

module SQLGenerator
  ( generateSQLStatements
  ) where

import Data.List
import Text.Printf
import Types

type RouteSectionStatement = (String, [String])

type JourneyPatternSectionStatement = (String, [String])

generateSQLStatements :: TransXChangeData -> [String]
generateSQLStatements transXChangeData =
  concat
    [ stopPointStatements
    , routeStatements
    , routeSectionStatements
    , journeyPatternSectionStatements
    , operatorStatements
    ]
  where
    stopPointStatements =
      map stopPointToSQLStatement $ stopPoints transXChangeData
    routeSectionStatements =
      concat $
      map flattenRouteSectionStatement $
      map routeSectionToSQLStatements $ routeSections transXChangeData
    routeStatements = map routeToSQLStatement $ routes transXChangeData
    journeyPatternSectionStatements =
      concat $
      map flattenJourneyPatternSectionStatement $
      map journeyPatternSectionToSQLStatement $
      journeyPatternSections transXChangeData
    operatorStatements = map operatorToSQLStatement $ operators transXChangeData

-- StopPoints
stopPointToSQLStatement :: AnnotatedStopPointRef -> String
stopPointToSQLStatement stopPoint =
  printf
    "INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('%s', '%s', 0, 0);"
    (stopPointRef stopPoint)
    (commonName stopPoint)

-- Route Sections
routeSectionToSQLStatements :: RouteSection -> RouteSectionStatement
routeSectionToSQLStatements routeSection =
  (sectionStatement, routeLinkStatements)
  where
    sectionStatement =
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

flattenRouteSectionStatement :: RouteSectionStatement -> [String]
flattenRouteSectionStatement (routeSectionStatement, routeLinksStatements) =
  [routeSectionStatement] ++ routeLinksStatements

-- Routes
routeToSQLStatement :: Route -> String
routeToSQLStatement route =
  printf
    "INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('%s', '%s', '%s');"
    (routeId route)
    (routeDescription route)
    (routeSectionRef route)

-- Journey Pattern Section
journeyPatternSectionToSQLStatement :: JourneyPatternSection
                                    -> JourneyPatternSectionStatement
journeyPatternSectionToSQLStatement journeyPatternSection =
  (journeyPatternSectionStatement, journeyPatternTimingStatements)
  where
    journeyPatternSectionStatement =
      printf
        "INSERT INTO JourneyPatternSection (JourneyPatterSectionId) VALUES ('%s');"
        (journeyPatterSectionId journeyPatternSection)
    journeyPatternTimingToSQL =
      journeyPatternTimingLinkToSQLStatement
        (journeyPatterSectionId journeyPatternSection)
    journeyPatternTimingStatements =
      map journeyPatternTimingToSQL $
      journeyPatternTimingLinks journeyPatternSection

journeyPatternTimingLinkToSQLStatement :: String
                                       -> JourneyPatternTimingLink
                                       -> String
journeyPatternTimingLinkToSQLStatement journeyPatternSectionId journeyPatternTimingLink =
  printf
    "INSERT INTO JourneyPatternTimingLink \
    \(JourneyPatternTimingLinkId, JourneyPatterSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime) \
    \VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');"
    (journeyPatternTimingLinkId journeyPatternTimingLink)
    journeyPatternSectionId
    (journeyPatternFromStopPointRef journeyPatternTimingLink)
    (journeyPatternFromTimingStatus journeyPatternTimingLink)
    (journeyPatternToStopPointsRef journeyPatternTimingLink)
    (journeyPatternToTimingStatus journeyPatternTimingLink)
    (routeLinkRef journeyPatternTimingLink)
    (journeyDirection journeyPatternTimingLink)
    (runTime journeyPatternTimingLink)

flattenJourneyPatternSectionStatement :: JourneyPatternSectionStatement
                                      -> [String]
flattenJourneyPatternSectionStatement (journeyPatternSectionStatement, journeyPatternSectionLinkStatement) =
  [journeyPatternSectionStatement] ++ journeyPatternSectionLinkStatement

-- Operators
operatorToSQLStatement :: Operator -> String
operatorToSQLStatement operator =
  printf
    "INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('%s', '%s', '%s', '%s');"
    (operatorId operator)
    (nationalOperatorCode operator)
    (operatorCode operator)
    (operatorShortName operator)
