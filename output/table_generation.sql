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

