-- Table generation
DROP TABLE "AnnotatedStopPointRef";
DROP TABLE "RouteSection";
DROP TABLE "RouteLink";
DROP TABLE "Route";

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

