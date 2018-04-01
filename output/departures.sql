-- Table generation
DROP TABLE "AnnotatedStopPointRef";
DROP TABLE "RouteSection";
DROP TABLE "RouteLink";
DROP TABLE "Route";
DROP TABLE "JourneyPatternSection";
DROP TABLE "JourneyPatternTimingLink";
DROP TABLE "Operator";
DROP TABLE "Service";
DROP TABLE "Line";
DROP TABLE "JourneyPattern";
DROP TABLE "VehicleJourney";
DROP TABLE "DayOfOperation";
DROP TABLE "DayOfNonOperation";

CREATE TABLE "AnnotatedStopPointRef" (
	"StopPointRef" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"CommonName" TEXT NOT NULL,
	"Latitude" REAL,
	"Longitude" REAL
);

CREATE TABLE "RouteSection" (
	"RouteSectionId" TEXT PRIMARY KEY NOT NULL UNIQUE
);

CREATE TABLE "RouteLink" (
	"RouteLinkId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"RouteSectionId" TEXT NOT NULL REFERENCES "RouteSection"("RouteSectionId"),
	"FromStopPointRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"ToStopPointRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"Order" INTEGER NOT NULL,
	"RouteDirection" TEXT NOT NULL
);

CREATE TABLE "Route" (
	"RouteId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"RouteDescription" TEXT NOT NULL,
	"RouteSectionRef" TEXT NOT NULL REFERENCES "RouteSection"("RouteSectionId")
);

CREATE TABLE "JourneyPatternSection" (
	"JourneyPatternSectionId" TEXT PRIMARY KEY NOT NULL UNIQUE
);

CREATE TABLE "JourneyPatternTimingLink" (
	"JourneyPatternTimingLinkId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"JourneyPatternSectionId" TEXT NOT NULL REFERENCES "JourneyPatternSection"("JourneyPatterSectionId"),
	"JourneyPatternFromStopPointRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"JourneyPatternFromTimingStatus" TEXT NOT NULL,
	"JourneyPatternToStopPointsRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"JourneyPatternToTimingStatus" TEXT NOT NULL,
	"RouteLinkRef" TEXT NOT NULL REFERENCES "RouteLink"("RouteLinkId"),
	"JourneyDirection" TEXT NOT NULL,
	"RunTime" TEXT NOT NULL,
	"WaitTime" TEXT NULL
);

CREATE TABLE "Operator" (
	"OperatorId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"NationalOperatorCode" TEXT NOT NULL,
	"OperatorCode" TEXT NOT NULL,
	"OperatorShortName" TEXT NOT NULL
);

CREATE TABLE "Service" (
	"ServiceCode" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"RegisteredOperatorRef" TEXT NOT NULL,
	"Mode" TEXT NOT NULL,
	"Description" TEXT NOT NULL,
	"Origin" TEXT NOT NULL,
	"Destination" TEXT NOT NULL,
	"StartDate" INTEGER NOT NULL,
	"EndDate" INTEGER NOT NULL,
	"CalMacServiceId" INTEGER
);

CREATE TABLE "Line" (
	"LineId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"ServiceRef" TEXT NOT NULL REFERENCES "Service"("ServiceCode"),
	"LineName" TEXT NOT NULL
);

CREATE TABLE "JourneyPattern" (
	"JourneyPatternId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"ServiceRef" TEXT NOT NULL REFERENCES "Service"("ServiceCode"),
	"JourneyPatternSectionRef" TEXT NOT NULL REFERENCES "JourneyPatternSection"("JourneyPatterSectionId"),
	"JourneyPatternDirection" TEXT NOT NULL
);

CREATE TABLE "VehicleJourney" (
	"VehicleJourneyCode" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"ServiceRef" TEXT NOT NULL REFERENCES "Service"("ServiceCode"),
	"LineRef" TEXT NOT NULL REFERENCES "Line"("LineId"),
	"JourneyPatternRef" TEXT NOT NULL REFERENCES "JourneyPattern"("JourneyPatternId"),
	"OperatorRef" TEXT NOT NULL REFERENCES "Operator"("OperatorId"),
	"Monday" INTEGER NOT NULL,
	"Tuesday" INTEGER NOT NULL,
	"Wednesday" INTEGER NOT NULL,
	"Thursday" INTEGER NOT NULL,
	"Friday" INTEGER NOT NULL,
	"Saturday" INTEGER NOT NULL,
	"Sunday" INTEGER NOT NULL,
	"DepatureHour" INTEGER NOT NULL,
	"DepatureMinute" INTEGER NOT NULL,
	"DepatureSecond" INTEGER NOT NULL,
	"Note" TEXT NULL,
	"NoteCode" TEXT NULL
);

CREATE TABLE "DayOfOperation" (
	"VehicleJourneyRef" TEXT NOT NULL REFERENCES "VehicleJourney"("VehicleJourneyCode"),
	"StartDate" INTEGER NOT NULL,
	"EndDate" INTEGER NOT NULL
);

CREATE TABLE "DayOfNonOperation" (
	"VehicleJourneyRef" TEXT NOT NULL REFERENCES "VehicleJourney"("VehicleJourneyCode"),
	"StartDate" INTEGER NOT NULL,
	"EndDate" INTEGER NOT NULL
);

INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300RAY', 'Rothesay Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300WMB', 'Wemyss Bay Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM03_001', 'Rothesay - Wemyss Bay or Gourock
', 'FSACM03_001');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM03_002', 'Wemyss Bay or Gourock - Rothesay
', 'FSACM03_002');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM03_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM03_001_287428', 'FSACM03_001', '9300RAY', '9300WMB', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM03_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM03_002_287430', 'FSACM03_002', '9300WMB', '9300RAY', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM03-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM03-3_287428', 'JPS_FSACM03-3', '9300RAY', 'PTP', '9300WMB', 'PTP', 'FSACM03_001_287428', 'inbound', 'PT2100S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM03-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM03-4_287430', 'JPS_FSACM03-4', '9300WMB', 'PTP', '9300RAY', 'PTP', 'FSACM03_002_287430', 'outbound', 'PT2100S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM03', 'CAL', 'ferry', 'Wemyss Bay or Gourock - Rothesay', 'Wemyss Bay or Gourock', 'Rothesay', 1508716800, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM03', 'CM3');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM03-3', 'FSACM03', 'JPS_FSACM03-3', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM03-4', 'FSACM03', 'JPS_FSACM03-4', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87694', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 6, 25, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87695', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87696', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 8, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87697', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 8, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87698', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 9, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87699', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 10, 10, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87700', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 11, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87701', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87702', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 13, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87703', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 14, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87704', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87705', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87706', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87707', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87708', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 18, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87709', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87710', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 8, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87711', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 8, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87712', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 9, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87713', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 10, 10, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87714', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 11, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87715', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87716', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 13, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87717', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 14, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87718', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87719', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87720', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87721', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87722', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 18, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87723', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87724', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87725', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87726', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 10, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87727', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87728', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87729', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87730', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87731', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87732', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 17, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87733', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87734', 'FSACM03', '0', 'JPS_FSACM03-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87735', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87736', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 55, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87737', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 8, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87738', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 9, 25, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87739', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 10, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87740', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 11, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87741', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87742', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 13, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87743', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 14, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87744', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87745', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87746', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87747', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87748', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 18, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87749', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87750', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 19, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87751', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 8, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87752', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 9, 25, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87753', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 10, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87754', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 11, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87755', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87756', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 13, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87757', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 14, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87758', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87759', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87760', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87761', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87762', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 18, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87763', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87764', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 19, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87765', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87766', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 10, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87767', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 12, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87768', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87769', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87770', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 15, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87771', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87772', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 17, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87773', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87774', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 19, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87775', 'FSACM03', '0', 'JPS_FSACM03-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 19, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300BRB', 'Brodick Isle of Arran Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ARD', 'Ardrossan Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_001', 'Brodick - Ardrossan
', 'FSACM05_001');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_002', 'Ardrossan - Brodick
', 'FSACM05_002');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05_001_287432', 'FSACM05_001', '9300BRB', '9300ARD', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM05_002_287434', 'FSACM05_002', '9300ARD', '9300BRB', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM05-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05-3_287432', 'JPS_FSACM05-3', '9300BRB', 'PTP', '9300ARD', 'PTP', 'FSACM05_001_287432', 'inbound', 'PT3300S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM05-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM05-4_287434', 'JPS_FSACM05-4', '9300ARD', 'PTP', '9300BRB', 'PTP', 'FSACM05_002_287434', 'outbound', 'PT3300S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM05', 'CAL', 'ferry', 'Ardrossan - Brodick', 'Ardrossan', 'Brodick', 1508716800, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM05', 'CM5');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM05-3', 'FSACM05', 'JPS_FSACM05-3', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM05-4', 'FSACM05', 'JPS_FSACM05-4', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87776', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87777', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 11, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87778', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 13, 55, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87779', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87780', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 19, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87784', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87785', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 11, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87786', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 13, 55, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87787', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87791', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87792', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 11, 5, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87793', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 50, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87794', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87795', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 19, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87798', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87799', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87800', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 12, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87801', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 15, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87802', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87806', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87807', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87808', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 12, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87809', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 15, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87810', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87813', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87814', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 12, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87815', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 15, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87816', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87781', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87781', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87781', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87782', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87782', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87782', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87783', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87783', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87783', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87788', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87788', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87788', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87789', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87789', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87789', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87790', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87790', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87790', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87796', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87796', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87796', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87797', 'FSACM05', '0', 'JPS_FSACM05-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 18, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87797', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87797', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87803', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87803', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87803', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87804', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87804', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87804', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87805', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 19, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87805', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87805', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87811', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87811', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87811', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87812', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87812', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87812', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87817', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87817', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87817', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87818', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87818', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87818', 1516406400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('87819', 'FSACM05', '0', 'JPS_FSACM05-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 19, 20, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87819', 1508716800, 1514937599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('87819', 1516406400, 2524607999);INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300CPB', 'Campbeltown Ferry Terminal', 0, 0);
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
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('96943', 'FSACM05A', '0', 'JPS_FSACM05A-4', 'CAL', 0, 0, 0, 0, 1, 0, 0, 7, 35, 0, '', '');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96943', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96943', 1506297600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('96944', 'FSACM05A', '0', 'JPS_FSACM05A-5', 'CAL', 0, 0, 0, 0, 0, 1, 0, 7, 0, 0, '', '');
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
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('96945', 'FSACM05A', '0', 'JPS_FSACM05A-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 55, 0, '', '');
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
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('96946', 'FSACM05A', '0', 'JPS_FSACM05A-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 50, 0, '', '');
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
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('96947', 'FSACM05A', '0', 'JPS_FSACM05A-6', 'CAL', 0, 0, 0, 1, 1, 0, 0, 18, 40, 0, '', '');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96947', 1490918400, 1493251199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('96947', 1506297600, 2524607999);INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ARM', 'Armadale Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300MLG', 'Mallaig (CalMac) Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM18_002', 'Armadale (Skye) - Mallaig
', 'FSACM18_002');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM18_001', 'Mallaig - Armadale (Skye)
', 'FSACM18_001');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM18_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM18_002_287499', 'FSACM18_002', '9300ARM', '9300MLG', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM18_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM18_001_287501', 'FSACM18_001', '9300MLG', '9300ARM', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM18-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM18-3_287499', 'JPS_FSACM18-3', '9300ARM', 'PTP', '9300MLG', 'PTP', 'FSACM18_002_287499', 'inbound', 'PT1800S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM18-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM18-4_287501', 'JPS_FSACM18-4', '9300MLG', 'PTP', '9300ARM', 'PTP', 'FSACM18_001_287501', 'outbound', 'PT1800S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM18', 'CAL', 'ferry', 'Mallaig - Armadale (Skye)', 'Mallaig', 'Armadale (Skye)', 1508716800, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM18', 'CM18');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM18-3', 'FSACM18', 'JPS_FSACM18-3', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM18-4', 'FSACM18', 'JPS_FSACM18-4', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88134', 'FSACM18', '0', 'JPS_FSACM18-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 9, 25, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88135', 'FSACM18', '0', 'JPS_FSACM18-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88137', 'FSACM18', '0', 'JPS_FSACM18-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 45, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88138', 'FSACM18', '0', 'JPS_FSACM18-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 8, 40, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88139', 'FSACM18', '0', 'JPS_FSACM18-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88141', 'FSACM18', '0', 'JPS_FSACM18-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 16, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88136', 'FSACM18', '0', 'JPS_FSACM18-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 18, 15, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88136', 1508716800, 1521071999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88136', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88140', 'FSACM18', '0', 'JPS_FSACM18-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88140', 1508716800, 1521071999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88140', 1522368000, 2524607999);INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300LEV', 'Leverburgh Harris Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300BNY', 'Berneray Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM23_002', 'Leverburgh - Berneray
', 'FSACM23_002');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM23_001', 'Berneray - Leverburgh
', 'FSACM23_001');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM23_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM23_002_287551', 'FSACM23_002', '9300LEV', '9300BNY', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM23_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM23_001_287553', 'FSACM23_001', '9300BNY', '9300LEV', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM23-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM23-3_287551', 'JPS_FSACM23-3', '9300LEV', 'PTP', '9300BNY', 'PTP', 'FSACM23_002_287551', 'inbound', 'PT3600S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM23-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM23-4_287553', 'JPS_FSACM23-4', '9300BNY', 'PTP', '9300LEV', 'PTP', 'FSACM23_001_287553', 'outbound', 'PT3600S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM23-5');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM23-5_287551', 'JPS_FSACM23-5', '9300LEV', 'PTP', '9300BNY', 'PTP', 'FSACM23_002_287551', 'inbound', 'PT4200S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM23-6');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM23-6_287553', 'JPS_FSACM23-6', '9300BNY', 'PTP', '9300LEV', 'PTP', 'FSACM23_001_287553', 'outbound', 'PT4200S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM23', 'CAL', 'ferry', 'Berneray - Leverburgh', 'Berneray', 'Leverburgh', 1508716800, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM23', 'CM23');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM23-3', 'FSACM23', 'JPS_FSACM23-3', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM23-4', 'FSACM23', 'JPS_FSACM23-4', 'outbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM23-5', 'FSACM23', 'JPS_FSACM23-5', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM23-6', 'FSACM23', 'JPS_FSACM23-6', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88282', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88282', 1515888000, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88282', 1516492800, 1516579199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88282', 1517097600, 1517183999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88283', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 40, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88283', 1515888000, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88283', 1516492800, 1516579199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88283', 1517097600, 1517183999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88300', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88300', 1515888000, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88300', 1516492800, 1516579199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88300', 1517097600, 1517183999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88301', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88301', 1515888000, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88301', 1516492800, 1516579199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88301', 1517097600, 1517183999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88272', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 9, 25, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88272', 1508716800, 1515369599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88272', 1515888000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88273', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 14, 55, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88273', 1508716800, 1515369599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88273', 1515888000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88290', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 8, 15, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88290', 1508716800, 1515369599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88290', 1515888000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88291', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 13, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88291', 1508716800, 1515369599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88291', 1515888000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88284', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88284', 1508716800, 1515887999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88284', 1515974400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88285', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 40, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88285', 1508716800, 1515887999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88285', 1515974400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88302', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88302', 1508716800, 1515887999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88302', 1515974400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88303', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88303', 1508716800, 1515887999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88303', 1515974400, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88274', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 1, 1, 1, 1, 1, 1, 0, 10, 20, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88274', 1508716800, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88274', 1517097600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88275', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 1, 1, 1, 1, 1, 1, 0, 14, 50, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88275', 1508716800, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88275', 1517097600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88292', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 9, 0, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88292', 1508716800, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88292', 1517097600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88293', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 1, 1, 1, 1, 1, 1, 0, 13, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88293', 1508716800, 1515974399);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88293', 1517097600, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88286', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88286', 1508716800, 1516492799);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88286', 1516579200, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88287', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 40, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88287', 1508716800, 1516492799);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88287', 1516579200, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88304', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88304', 1508716800, 1516492799);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88304', 1516579200, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88305', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88305', 1508716800, 1516492799);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88305', 1516579200, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88288', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 9, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88288', 1508716800, 1517097599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88288', 1517184000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88289', 'FSACM23', '0', 'JPS_FSACM23-5', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 40, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88289', 1508716800, 1517097599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88289', 1517184000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88306', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 8, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88306', 1508716800, 1517097599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88306', 1517184000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88307', 'FSACM23', '0', 'JPS_FSACM23-6', 'CAL', 0, 0, 0, 0, 0, 0, 1, 13, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88307', 1508716800, 1517097599);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88307', 1517184000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88276', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 9, 10, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88276', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88276', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88277', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 13, 0, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88277', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88277', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88278', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 16, 5, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88278', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88278', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88294', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 8, 0, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88294', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88294', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88295', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 11, 50, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88295', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88295', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88296', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 14, 55, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88296', 1508716800, 1517183999);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88296', 1519516800, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88279', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 8, 35, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88279', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88279', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88280', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 12, 20, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88280', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88280', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88281', 'FSACM23', '0', 'JPS_FSACM23-3', 'CAL', 1, 1, 1, 1, 1, 1, 0, 16, 40, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88281', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88281', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88297', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 7, 25, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88297', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88297', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88298', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 11, 10, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88298', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88298', 1522368000, 2524607999);
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88299', 'FSACM23', '0', 'JPS_FSACM23-4', 'CAL', 1, 1, 1, 1, 1, 1, 0, 15, 30, 0, 'Service subject to change due to tidal conditions', 'TID');
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88299', 1508716800, 1519603199);
INSERT INTO DayOfNonOperation (VehicleJourneyRef, StartDate, EndDate) VALUES ('88299', 1522368000, 2524607999);INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ULL', 'Ullapool Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300STO', 'Stornoway Lewis Ferry Terminal', 0, 0);
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM25_002', 'Ullapool - Stornoway (Lewis)
', 'FSACM25_002');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM25_001', 'Stornoway (Lewis) - Ullapool
', 'FSACM25_001');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM25_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM25_002_287555', 'FSACM25_002', '9300ULL', '9300STO', 0, 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM25_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, `Order`, RouteDirection) VALUES ('FSACM25_001_287557', 'FSACM25_001', '9300STO', '9300ULL', 0, 'outbound');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM25-3');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM25-3_287555', 'JPS_FSACM25-3', '9300ULL', 'PTP', '9300STO', 'PTP', 'FSACM25_002_287555', 'inbound', 'PT9000S', '');
INSERT INTO JourneyPatternSection (JourneyPatternSectionId) VALUES ('JPS_FSACM25-4');
INSERT INTO JourneyPatternTimingLink (JourneyPatternTimingLinkId, JourneyPatternSectionId, JourneyPatternFromStopPointRef, JourneyPatternFromTimingStatus, JourneyPatternToStopPointsRef, JourneyPatternToTimingStatus, RouteLinkRef, JourneyDirection, RunTime, WaitTime) VALUES ('JPS_FSACM25-4_287557', 'JPS_FSACM25-4', '9300STO', 'PTP', '9300ULL', 'PTP', 'FSACM25_001_287557', 'outbound', 'PT9000S', '');
INSERT INTO Operator (OperatorId, NationalOperatorCode, OperatorCode, OperatorShortName) VALUES ('CAL', 'CALM', 'CAL', 'Caledonian MacBrayne');
INSERT INTO Service (ServiceCode, RegisteredOperatorRef, Mode, Description, Origin, Destination, StartDate, EndDate) VALUES ('FSACM25', 'CAL', 'ferry', 'Stornoway (Lewis) - Ullapool', 'Stornoway (Lewis)', 'Ullapool', 1508716800, 2524607999);
INSERT INTO Line (LineId, ServiceRef, LineName) VALUES ('0', 'FSACM25', 'CM25');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM25-3', 'FSACM25', 'JPS_FSACM25-3', 'inbound');
INSERT INTO JourneyPattern (JourneyPatternId, ServiceRef, JourneyPatternSectionRef, JourneyPatternDirection) VALUES ('JPS_FSACM25-4', 'FSACM25', 'JPS_FSACM25-4', 'outbound');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88308', 'FSACM25', '0', 'JPS_FSACM25-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 10, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88309', 'FSACM25', '0', 'JPS_FSACM25-3', 'CAL', 1, 1, 1, 1, 1, 0, 0, 17, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88310', 'FSACM25', '0', 'JPS_FSACM25-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 10, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88311', 'FSACM25', '0', 'JPS_FSACM25-3', 'CAL', 0, 0, 0, 0, 0, 1, 0, 18, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88312', 'FSACM25', '0', 'JPS_FSACM25-3', 'CAL', 0, 0, 0, 0, 0, 0, 1, 18, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88313', 'FSACM25', '0', 'JPS_FSACM25-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88314', 'FSACM25', '0', 'JPS_FSACM25-4', 'CAL', 1, 1, 1, 1, 1, 0, 0, 14, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88315', 'FSACM25', '0', 'JPS_FSACM25-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 7, 0, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88316', 'FSACM25', '0', 'JPS_FSACM25-4', 'CAL', 0, 0, 0, 0, 0, 1, 0, 14, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');
INSERT INTO VehicleJourney (VehicleJourneyCode, ServiceRef, LineRef, JourneyPatternRef, OperatorRef, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday, DepatureHour, DepatureMinute, DepatureSecond, Note, NoteCode) VALUES ('88317', 'FSACM25', '0', 'JPS_FSACM25-4', 'CAL', 0, 0, 0, 0, 0, 0, 1, 14, 30, 0, 'Passengers should check-in 10 mins before ferry departure time shown', 'FER');

UPDATE AnnotatedStopPointRef SET CommonName = REPLACE(CommonName, " Ferry Terminal", "");
UPDATE AnnotatedStopPointRef SET CommonName = "Brodick" WHERE StopPointRef = "9300BRB";

UPDATE Service SET CalMacServiceId = 5 WHERE ServiceCode = 'FSACM05';
UPDATE Service SET CalMacServiceId = 36 WHERE ServiceCode = 'FSACM05A';
UPDATE Service SET CalMacServiceId = 25 WHERE ServiceCode = 'FSACM25';
UPDATE Service SET CalMacServiceId = 23 WHERE ServiceCode = 'FSACM23';
UPDATE Service SET CalMacServiceId = 3 WHERE ServiceCode = 'FSACM03';
UPDATE Service SET CalMacServiceId = 18 WHERE ServiceCode = 'FSACM18';