{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.String
import Data.Time
import Debug.Trace
import Text.XML.Light
import Types

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

-- Main
main :: IO ()
main = do
  fileContents <-
    readFile
      "/Users/stefanchurch/Documents/Source/Haskell/parser/SVRFSACM05.xml"
  let xml = onlyElems $ parseXML fileContents
  let transXChangeElement = head $ drop 1 xml
  let stopPoints = fromMaybe [] $ getStopPoints transXChangeElement
  let routeSections = fromMaybe [] $ getRouteSections transXChangeElement
  let routes = fromMaybe [] $ getRoutes transXChangeElement
  let journeyPatternSections =
        fromMaybe [] $ getJourneyPatternSections transXChangeElement
  let operators = fromMaybe [] $ getOperators transXChangeElement
  let services = fromMaybe [] $ getServices transXChangeElement
  putStrLn (show stopPoints)
  putStrLn (show routeSections)
  putStrLn (show routes)
  putStrLn (show journeyPatternSections)
  putStrLn (show operators)
  putStrLn (show services)
  return ()

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
  routeSections <- findChild "RouteSections" element
  let routeSectionElements = findChildren "RouteSection" routeSections
  return $ map elementToRouteSection routeSectionElements

elementToRouteSection :: Element -> RouteSection
elementToRouteSection element =
  RouteSection
  { routeSectionId = attrValue "id" element
  , routeLink =
      fromMaybe (RouteLink "" "" "" "") $
      elementToRouteLink <$> (findChild "RouteLink" element)
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
  let routes = findChildren "Route" routesElement
  return $ map elementToRoute routes

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
  let journeyPatternSections =
        findChildren "JourneyPatternSection" journeyPatternSectionsElement
  return $ map elementToJourneyPatternSection journeyPatternSections

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
  , startDate =
      (parseXMLDate . strContent) <$>
      (findChild "OperatingPeriod" element >>= findChild "StartDate")
  , endDate =
      (parseXMLDate . strContent) <$>
      (findChild "OperatingPeriod" element >>= findChild "EndDate")
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
