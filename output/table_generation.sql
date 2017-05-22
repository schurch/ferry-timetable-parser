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

