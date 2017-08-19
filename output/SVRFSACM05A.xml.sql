INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300CPB', 'Campbeltown Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ARD', 'Ardrossan Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300BRB', 'Brodick Isle of Arran Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05A001', 'Campbeltown - Ardrossan
', 'FSACM05A001');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05A002', 'Campbeltown - Ardrossan
', 'FSACM05A002');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05A003', 'Ardrossan - Campbeltown
', 'FSACM05A003');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05A001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05A001_289687', 'FSACM05A001', '9300CPB', '9300ARD', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05A002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05A002_289689', 'FSACM05A002', '9300CPB', '9300BRB', 0, 'inbound');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05A002_289690', 'FSACM05A002', '9300BRB', '9300ARD', 1, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05A003');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05A003_289692', 'FSACM05A003', '9300ARD', '9300CPB', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM05A-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05A-4_289687', 'JPS_FSACM05A-4', '9300CPB', 'PTP', '9300ARD', 'PTP', 'FSACM05A001_289687', 'inbound', 'PT9600S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM05A-5');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05A-5_289689', 'JPS_FSACM05A-5', '9300CPB', 'PTP', '9300BRB', 'PTP', 'FSACM05A002_289689', 'inbound', 'PT8400S', '');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05A-5_289690', 'JPS_FSACM05A-5', '9300BRB', 'PTP', '9300ARD', 'PTP', 'FSACM05A002_289690', 'inbound', 'PT3300S', 'PT1500S');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM05A-6');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05A-6_289692', 'JPS_FSACM05A-6', '9300ARD', 'PTP', '9300CPB', 'PTP', 'FSACM05A003_289692', 'outbound', 'PT9600S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM05A', 'CAL', 'ferry', 'Ardrossan - Campbeltown', 'Ardrossan', 'Campbeltown', 1490918400, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM05A', 'CM5A');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM05A-4', 'FSACM05A', 'JPS_FSACM05A-4', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM05A-5', 'FSACM05A', 'JPS_FSACM05A-5', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM05A-6', 'FSACM05A', 'JPS_FSACM05A-6', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES ('96943', 'FSACM05A', '0', 'JPS_FSACM05A-4', 'CAL', 0, 0, 0, 0, 1, 0, 0, 7, 35, 0);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96943', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96943', 1506297600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES ('96944', 'FSACM05A', '0', 'JPS_FSACM05A-5', 'CAL', 0, 0, 0, 0, 0, 1, 0, 7, 0, 0);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1497657600, 1497743999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1498262400, 1498348799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1498867200, 1498953599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1499472000, 1499558399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1500076800, 1500163199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1500681600, 1500767999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1501286400, 1501372799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1501891200, 1501977599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1502496000, 1502582399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1503100800, 1503187199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1503705600, 1503791999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1504310400, 1504396799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1504915200, 1505001599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1505520000, 1505606399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1506124800, 1506211199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96944', 1506297600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES ('96945', 'FSACM05A', '0', 'JPS_FSACM05A-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 55, 0);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1497744000, 1497830399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1498348800, 1498435199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1498953600, 1499039999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1499558400, 1499644799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1500163200, 1500249599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1500768000, 1500854399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1501372800, 1501459199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1501977600, 1502063999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1502582400, 1502668799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1503187200, 1503273599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1503792000, 1503878399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1504396800, 1504483199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1505001600, 1505087999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1505606400, 1505692799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1506211200, 1506297599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96945', 1506297600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES ('96946', 'FSACM05A', '0', 'JPS_FSACM05A-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 50, 0);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1497744000, 1497830399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1498348800, 1498435199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1498953600, 1499039999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1499558400, 1499644799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1500163200, 1500249599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1500768000, 1500854399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1501372800, 1501459199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1501977600, 1502063999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1502582400, 1502668799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1503187200, 1503273599);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1503792000, 1503878399);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1504396800, 1504483199);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1505001600, 1505087999);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1505606400, 1505692799);
INSERT INTO DayOfOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1506211200, 1506297599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96946', 1506297600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond) VALUES ('96947', 'FSACM05A', '0', 'JPS_FSACM05A-6', 'CAL', 0, 0, 0, 1, 1, 0, 0, 18, 40, 0);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96947', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96947', 1506297600, 2524607999);