-- Table generation
DROP TABLE "AnnotatedStopPointRef";
DROP TABLE "RouteSection";
DROP TABLE "RouteLink";
DROP TABLE "Route";
DROP TABLE "JourneyPatternSection";
DROP TABLE "JourneyPatternTimingLink";
DROP TABLE "Operator";

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
	"RouteDirection" TEXT NOT NULL
);

CREATE TABLE "Route" (
	"RouteId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"RouteDescription" TEXT NOT NULL,
	"RouteSectionRef" TEXT NOT NULL REFERENCES "RouteSection"("RouteSectionId")
);

CREATE TABLE "JourneyPatternSection" (
	"JourneyPatterSectionId" TEXT PRIMARY KEY NOT NULL UNIQUE
);

CREATE TABLE "JourneyPatternTimingLink" (
	"JourneyPatternTimingLinkId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"JourneyPatterSectionId" TEXT NOT NULL REFERENCES "JourneyPatternSection"("JourneyPatterSectionId"),
	"JourneyPatternFromStopPointRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"JourneyPatternFromTimingStatus" TEXT NOT NULL,
	"JourneyPatternToStopPointsRef" TEXT NOT NULL REFERENCES "AnnotatedStopPointRef"("StopPointRef"),
	"JourneyPatternToTimingStatus" TEXT NOT NULL,
	"RouteLinkRef" TEXT NOT NULL REFERENCES "RouteLink"("RouteLinkId"),
	"JourneyDirection" TEXT NOT NULL,
	"RunTime" TEXT NOT NULL
);

CREATE TABLE "Operator" (
	"OperatorId" TEXT PRIMARY KEY NOT NULL UNIQUE,
	"NationalOperatorCode" TEXT NOT NULL,
	"OperatorCode" TEXT NOT NULL,
	"OperatorShortName" TEXT NOT NULL
);

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