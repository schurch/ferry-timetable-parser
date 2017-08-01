{-# LANGUAGE OverloadedStrings #-}

module SQLGenerator
  ( generateSQLStatements
  ) where

import Data.List
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Types

type RouteSectionStatement = (String, [String])

type JourneyPatternSectionStatement = (String, [String])

type ServiceSectionStatement = (String, [String], [String])

type VehicleJourneySectionStatement = (String, [String], [String])

generateSQLStatements :: TransXChangeData -> [String]
generateSQLStatements transXChangeData =
  concat
    [ stopPointStatements
    , routeStatements
    , routeSectionStatements
    , journeyPatternSectionStatements
    , operatorStatements
    , serviceStatements
    , vehicleJourneyStatements
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
    serviceStatements =
      concat $
      map flattenServiceStatements $
      map serviceToSQLStatement $ services transXChangeData
    vehicleJourneyStatements =
      concat $
      map flattenVehicleJourneyStatements $
      map vehicleJourneyToSQLStatement $ vehicleJourneys transXChangeData

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
    routeLinksWithIndex = zip [0 ..] (routeLinks routeSection)
    routeLinkStatements = map routeLinkToSQL routeLinksWithIndex

routeLinkToSQLStatement :: String -> (Int, RouteLink) -> String
routeLinkToSQLStatement routeSectionId (order, routeLink) =
  printf
    "INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('%s', '%s', '%s', '%s', %s, '%s');"
    (routeLinkId routeLink)
    routeSectionId
    (fromStopPointRef routeLink)
    (toStopPointRef routeLink)
    (show order)
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
        "INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('%s');"
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
    \(JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) \
    \VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s');"
    (journeyPatternTimingLinkId journeyPatternTimingLink)
    journeyPatternSectionId
    (journeyPatternFromStopPointRef journeyPatternTimingLink)
    (journeyPatternFromTimingStatus journeyPatternTimingLink)
    (journeyPatternToStopPointsRef journeyPatternTimingLink)
    (journeyPatternToTimingStatus journeyPatternTimingLink)
    (routeLinkRef journeyPatternTimingLink)
    (journeyDirection journeyPatternTimingLink)
    (runTime journeyPatternTimingLink)
    (journeyPatternFromWaitTime journeyPatternTimingLink)

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

-- Services
serviceToSQLStatement :: Service -> ServiceSectionStatement
serviceToSQLStatement service =
  (serviceStatement, lineStatements, journeyPatternStatements)
  where
    serviceStatement =
      printf
        "INSERT INTO Service \
      \(ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) \
      \VALUES ('%s', '%s', '%s', '%s', '%s', '%s', %s, %s);"
        (serviceCode service)
        (registeredOperatorRef service)
        (mode service)
        (description service)
        (origin (standardService service))
        (destination (standardService service))
        start
        end
    start =
      fromMaybe "0" $ show . dateToInt <$> (startDate (operatingPeriod service))
    end =
      fromMaybe "0" $
      show . epochTimeEndOfDay . dateToInt <$>
      (endDate (operatingPeriod service))
    lineStatements =
      map (lineToSQLStatement (serviceCode service)) (Types.lines service)
    journeyPatternStatements =
      map
        (journeyPatterToSQLStatement (serviceCode service))
        (journeyPatterns (standardService service))

lineToSQLStatement :: String -> Line -> String
lineToSQLStatement serviceCode line =
  printf
    "INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('%s', '%s', '%s');"
    (lineId line)
    serviceCode
    (lineName line)

journeyPatterToSQLStatement :: String -> JourneyPattern -> String
journeyPatterToSQLStatement serviceCode journeyPattern =
  printf
    "INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) \
    \VALUES ('%s', '%s', '%s', '%s');"
    (journeyPatternId journeyPattern)
    serviceCode
    (journeyPatternSectionRef journeyPattern)
    (journeyPatternDirection journeyPattern)

flattenServiceStatements :: ServiceSectionStatement -> [String]
flattenServiceStatements (serviceStatement, lineStatements, journeyPatternStatements) =
  [serviceStatement] ++ lineStatements ++ journeyPatternStatements

-- Vehicle Journeys
vehicleJourneyToSQLStatement :: VehicleJourney -> VehicleJourneySectionStatement
vehicleJourneyToSQLStatement vehicleJourney =
  ( vehicleJourneyStatement
  , daysOfOperationStatements
  , daysOfNonOperationStatements)
  where
    (hour, minute, second) = splitTime (departureTime vehicleJourney)
    vehicleJourneyStatement =
      printf
        "INSERT INTO VehicleJourney \
      \(VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES \
      \('%s', '%s', '%s', '%s', '%s', %s, %s, %s, %s, %s, %s, %s, %d, %d, %d);"
        (vehicleJourneyCode vehicleJourney)
        (serviceRef vehicleJourney)
        (lineRef vehicleJourney)
        (journeyPatternRef vehicleJourney)
        (operatorRef vehicleJourney)
        (boolToInt (elem Monday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Tuesday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Wednesday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Thursday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Friday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Saturday (daysOfWeek vehicleJourney)))
        (boolToInt (elem Sunday (daysOfWeek vehicleJourney)))
        hour
        minute
        second
    dateRangeToDayOfOperatation =
      vehicleJourneyOperationRangeToSQLStatement
        "DayOfOperation"
        (vehicleJourneyCode vehicleJourney)
    daysOfOperationStatements =
      map dateRangeToDayOfOperatation (specialDaysOfOperation vehicleJourney)
    dateRangeToDayOfNonOperatation =
      vehicleJourneyOperationRangeToSQLStatement
        "DayOfNonOperation"
        (vehicleJourneyCode vehicleJourney)
    daysOfNonOperationStatements =
      map
        dateRangeToDayOfNonOperatation
        (specialDaysOfNonOperation vehicleJourney)

splitTime :: String -> (Integer, Integer, Integer)
splitTime timeString =
  ( read $ T.unpack $ splitString !! 0 :: Integer
  , read $ T.unpack $ splitString !! 1 :: Integer
  , read $ T.unpack $ splitString !! 2 :: Integer)
  where
    splitString = T.splitOn (T.pack ":") (T.pack timeString)

vehicleJourneyOperationRangeToSQLStatement :: String
                                           -> String
                                           -> DateRange
                                           -> String
vehicleJourneyOperationRangeToSQLStatement rangeTableName vehicleJourneyCode dateRange =
  printf
    "INSERT INTO %s (VehicleJourneyRef, StartDate, EndDate) VALUES ('%s', %s, %s);"
    rangeTableName
    vehicleJourneyCode
    (fromMaybe "0" $ show . dateToInt <$> (startDate dateRange))
    (fromMaybe "0" $
     show . epochTimeEndOfDay . dateToInt <$> (endDate dateRange))

flattenVehicleJourneyStatements :: VehicleJourneySectionStatement -> [String]
flattenVehicleJourneyStatements (vehicleJourneyStatement, daysOfOperationStatements, daysOfNonOperationStatements) =
  [vehicleJourneyStatement] ++
  daysOfOperationStatements ++ daysOfNonOperationStatements

-- Helpers
dateToInt :: UTCTime -> Int
dateToInt date = (floor $ utcTimeToPOSIXSeconds date) :: Int

epochTimeEndOfDay :: Int -> Int
epochTimeEndOfDay epochTime = epochTime + 86399

boolToInt :: Bool -> String
boolToInt bool =
  if bool
    then "1"
    else "0"
