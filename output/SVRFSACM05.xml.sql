INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300BRB', 'Brodick Isle of Arran Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ARD', 'Ardrossan Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_001', 'Brodick - Ardrossan
', 'FSACM05_001');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_002', 'Ardrossan - Brodick
', 'FSACM05_002');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, RouteDirection) VALUES ('FSACM05_001_276311', 'FSACM05_001', '9300BRB', '9300ARD', 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, RouteDirection) VALUES ('FSACM05_002_276313', 'FSACM05_002', '9300ARD', '9300BRB', 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatterSectionId) VALUES ('JPS_FSACM05-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatterSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime) VALUES ('JPS_FSACM05-3_276311', 'JPS_FSACM05-3', '9300BRB', 'PTP', '9300ARD', 'PTP', 'FSACM05_001_276311', 'inbound', 'PT3300S');
INSERT INTO JourneyPatternSection (JourneyPatterSectionId) VALUES ('JPS_FSACM05-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatterSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime) VALUES ('JPS_FSACM05-4_276313', 'JPS_FSACM05-4', '9300ARD', 'PTP', '9300BRB', 'PTP', 'FSACM05_002_276313', 'outbound', 'PT3300S');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');