{-# LANGUAGE OverloadedStrings #-}

module TransXChangeParser
  ( parseTransXChangeXML
  ) where

import Data.Maybe
import Data.String
import Data.Time
import Text.XML.Light
import Types

parseTransXChangeXML :: String -> TransXChangeData
parseTransXChangeXML input =
  TransXChangeData
  { stopPoints = fromMaybe [] $ getStopPoints transXChangeElement
  , routeSections = fromMaybe [] $ getRouteSections transXChangeElement
  , routes = fromMaybe [] $ getRoutes transXChangeElement
  , journeyPatternSections =
      fromMaybe [] $ getJourneyPatternSections transXChangeElement
  , operators = fromMaybe [] $ getOperators transXChangeElement
  , services = fromMaybe [] $ getServices transXChangeElement
  , vehicleJourneys = fromMaybe [] $ getVehicleJourneys transXChangeElement
  }
  where
    xml = onlyElems $ parseXML input
    transXChangeElement = head $ drop 1 xml

-- Helpers
instance IsString QName where
  fromString name = QName name (Just "http://www.transxchange.org.uk/") Nothing

attr :: String -> QName
attr name = QName name Nothing Nothing

attrValue :: String -> Element -> String
attrValue name element = fromMaybe "" $ findAttr (attr name) element

parseXMLDate :: String -> UTCTime
parseXMLDate dateString =
  parseTimeOrError True defaultTimeLocale "%Y-%m-%d" dateString

-- Stop Points
getStopPoints :: Element -> Maybe [AnnotatedStopPointRef]
getStopPoints element = do
  stopPointElement <- findChild "StopPoints" element
  let stopPointElements = findChildren "AnnotatedStopPointRef" stopPointElement
  return $ map elementToStopPoint stopPointElements

elementToStopPoint :: Element -> AnnotatedStopPointRef
elementToStopPoint stopPointElement =
  AnnotatedStopPointRef
  { stopPointRef =
      fromMaybe "" $ strContent <$> findChild "StopPointRef" stopPointElement
  , commonName =
      fromMaybe "" $ strContent <$> findChild "CommonName" stopPointElement
  }

-- Route Sections
getRouteSections :: Element -> Maybe [RouteSection]
getRouteSections element = do
  routeSectionsElement <- findChild "RouteSections" element
  let routeSectionElements = findChildren "RouteSection" routeSectionsElement
  return $ map elementToRouteSection routeSectionElements

elementToRouteSection :: Element -> RouteSection
elementToRouteSection element =
  RouteSection
  { routeSectionId = attrValue "id" element
  , routeLinks = map elementToRouteLink (findChildren "RouteLink" element)
  }

elementToRouteLink :: Element -> RouteLink
elementToRouteLink element =
  RouteLink
  { routeLinkId = attrValue "id" element
  , fromStopPointRef =
      fromMaybe "" $
      strContent <$> (findChild "From" element >>= findChild "StopPointRef")
  , toStopPointRef =
      fromMaybe "" $
      strContent <$> (findChild "To" element >>= findChild "StopPointRef")
  , routeDirection = fromMaybe "" $ strContent <$> findChild "Direction" element
  }

-- Routes
getRoutes :: Element -> Maybe [Route]
getRoutes element = do
  routesElement <- findChild "Routes" element
  let routeElements = findChildren "Route" routesElement
  return $ map elementToRoute routeElements

elementToRoute :: Element -> Route
elementToRoute element =
  Route
  { routeId = attrValue "id" element
  , routeDescription =
      fromMaybe "" $ strContent <$> findChild "Description" element
  , routeSectionRef =
      fromMaybe "" $ strContent <$> findChild "RouteSectionRef" element
  }

-- JourneyPatternSection
getJourneyPatternSections :: Element -> Maybe [JourneyPatternSection]
getJourneyPatternSections element = do
  journeyPatternSectionsElement <- findChild "JourneyPatternSections" element
  let journeyPatternSectionsElements =
        findChildren "JourneyPatternSection" journeyPatternSectionsElement
  return $ map elementToJourneyPatternSection journeyPatternSectionsElements

elementToJourneyPatternSection :: Element -> JourneyPatternSection
elementToJourneyPatternSection element =
  JourneyPatternSection
  { journeyPatterSectionId = attrValue "id" element
  , journeyPatternTimingLinks =
      map elementToJourneyPatternTimingLink $
      findChildren "JourneyPatternTimingLink" element
  }

elementToJourneyPatternTimingLink :: Element -> JourneyPatternTimingLink
elementToJourneyPatternTimingLink element =
  JourneyPatternTimingLink
  { journeyPatternTimingLinkId = attrValue "id" element
  , journeyPatternFromWaitTime =
      fromMaybe "" $
      strContent <$> (findChild "From" element >>= findChild "WaitTime")
  , journeyPatternFromStopPointRef =
      fromMaybe "" $
      strContent <$> (findChild "From" element >>= findChild "StopPointRef")
  , journeyPatternFromTimingStatus =
      fromMaybe "" $
      strContent <$> (findChild "From" element >>= findChild "TimingStatus")
  , journeyPatternToStopPointsRef =
      fromMaybe "" $
      strContent <$> (findChild "To" element >>= findChild "StopPointRef")
  , journeyPatternToTimingStatus =
      fromMaybe "" $
      strContent <$> (findChild "To" element >>= findChild "TimingStatus")
  , routeLinkRef =
      fromMaybe "" $ strContent <$> findChild "RouteLinkRef" element
  , journeyDirection =
      fromMaybe "" $ strContent <$> findChild "Direction" element
  , runTime = fromMaybe "" $ strContent <$> findChild "RunTime" element
  }

-- Operators
getOperators :: Element -> Maybe [Operator]
getOperators element = do
  operatorsElement <- findChild "Operators" element
  let operatorsElements = findChildren "Operator" operatorsElement
  return $ map elementToOperator operatorsElements

elementToOperator :: Element -> Operator
elementToOperator element =
  Operator
  { operatorId = attrValue "id" element
  , nationalOperatorCode =
      fromMaybe "" $ strContent <$> findChild "NationalOperatorCode" element
  , operatorCode =
      fromMaybe "" $ strContent <$> findChild "OperatorCode" element
  , operatorShortName =
      fromMaybe "" $ strContent <$> findChild "OperatorShortName" element
  }

-- Services
getServices :: Element -> Maybe [Service]
getServices element = do
  servicesElement <- findChild "Services" element
  let serviceElements = findChildren "Service" servicesElement
  return $ map elementToService serviceElements

elementToService :: Element -> Service
elementToService element =
  Service
  { serviceCode = fromMaybe "" $ strContent <$> findChild "ServiceCode" element
  , Types.lines = fromMaybe [] $ getLines element
  , operatingPeriod =
      fromMaybe (DateRange Nothing Nothing) $
      elementToDateRange <$> findChild "OperatingPeriod" element
  , registeredOperatorRef =
      fromMaybe "" $ strContent <$> findChild "RegisteredOperatorRef" element
  , mode = fromMaybe "" $ strContent <$> findChild "Mode" element
  , description = fromMaybe "" $ strContent <$> findChild "Description" element
  , standardService =
      fromMaybe (StandardService "" "" []) $
      elementToStandardService <$> findChild "StandardService" element
  }

-- Lines
getLines :: Element -> Maybe [Types.Line]
getLines element = do
  linesElement <- findChild "Lines" element
  let lineElements = findChildren "Line" linesElement
  return $ map elementToLine lineElements

elementToLine :: Element -> Types.Line
elementToLine element =
  Types.Line
  { lineId = attrValue "id" element
  , lineName = fromMaybe "" $ strContent <$> findChild "LineName" element
  }

-- Standard Service
elementToStandardService :: Element -> StandardService
elementToStandardService element =
  StandardService
  { origin = fromMaybe "" $ strContent <$> findChild "Origin" element
  , destination = fromMaybe "" $ strContent <$> findChild "Destination" element
  , journeyPatterns =
      map elementToJourneyPattern $ findChildren "JourneyPattern" element
  }

-- Journey pattern
elementToJourneyPattern :: Element -> JourneyPattern
elementToJourneyPattern element =
  JourneyPattern
  { journeyPatternId = attrValue "id" element
  , journeyPatternDirection =
      fromMaybe "" $ strContent <$> findChild "Direction" element
  , journeyPatternSectionRef =
      fromMaybe "" $
      strContent <$> findChild "JourneyPatternSectionRefs" element
  }

-- Vehicle journeys
getVehicleJourneys :: Element -> Maybe [VehicleJourney]
getVehicleJourneys element = do
  vehicleJourneysElement <- findChild "VehicleJourneys" element
  let vehicleJourneyElements =
        findChildren "VehicleJourney" vehicleJourneysElement
  return $ map elementToVehicleJourney vehicleJourneyElements

elementToVehicleJourney :: Element -> VehicleJourney
elementToVehicleJourney element =
  VehicleJourney
  { operatorRef = fromMaybe "" $ strContent <$> findChild "OperatorRef" element
  , vehicleJourneyCode =
      fromMaybe "" $ strContent <$> findChild "VehicleJourneyCode" element
  , serviceRef = fromMaybe "" $ strContent <$> findChild "ServiceRef" element
  , lineRef = fromMaybe "" $ strContent <$> findChild "LineRef" element
  , journeyPatternRef =
      fromMaybe "" $ strContent <$> findChild "JourneyPatternRef" element
  , departureTime =
      fromMaybe "" $ strContent <$> findChild "DepartureTime" element
  , daysOfWeek = fromMaybe [] weekDays
  , specialDaysOfOperation = fromMaybe [] operationDays
  , specialDaysOfNonOperation = fromMaybe [] nonOperationDays
  }
  where
    nonOperationDays = do
      operatingProfile <- findChild "OperatingProfile" element
      specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
      daysOfNonOperation <- findChild "DaysOfNonOperation" specialDaysOperation
      return $
        map elementToDateRange $ findChildren "DateRange" daysOfNonOperation
    operationDays = do
      operatingProfile <- findChild "OperatingProfile" element
      specialDaysOperation <- findChild "SpecialDaysOperation" operatingProfile
      daysOfOperation <- findChild "DaysOfOperation" specialDaysOperation
      return $ map elementToDateRange $ findChildren "DateRange" daysOfOperation
    weekDays = do
      operatingProfile <- findChild "OperatingProfile" element
      regularDayType <- findChild "RegularDayType" operatingProfile
      daysOfWeekElement <- findChild "DaysOfWeek" regularDayType
      return $ map elementToDay $ elChildren daysOfWeekElement

elementToDay :: Element -> WeekDay
elementToDay element =
  case (qName $ elName element) of
    "Monday" -> Monday
    "Tuesday" -> Tuesday
    "Wednesday" -> Wednesday
    "Thursday" -> Thursday
    "Friday" -> Friday
    "Saturday" -> Saturday
    "Sunday" -> Sunday

elementToDateRange :: Element -> DateRange
elementToDateRange element =
  DateRange
  { startDate = parseXMLDate . strContent <$> findChild "StartDate" element
  , endDate = parseXMLDate . strContent <$> findChild "EndDate" element
  }
