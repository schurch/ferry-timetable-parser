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

INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300BRB', 'Brodick Isle of Arran Ferry Terminal', 0, 0);
INSERT INTO AnnotatedStopPointRef (StopPointRef, CommonName, Latitude, Longitude) VALUES ('9300ARD', 'Ardrossan Ferry Terminal', 0, 0);
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_001');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, RouteDirection) VALUES ('FSACM05_001_276311', 'FSACM05_001', '9300BRB', '9300ARD', 'inbound');
INSERT INTO RouteSection (RouteSectionId) VALUES ('FSACM05_002');
INSERT INTO RouteLink (RouteLinkId, RouteSectionId, FromStopPointRef, ToStopPointRef, RouteDirection) VALUES ('FSACM05_002_276313', 'FSACM05_002', '9300ARD', '9300BRB', 'outbound');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_001', 'Brodick - Ardrossan
', 'FSACM05_001');
INSERT INTO Route (RouteId, RouteDescription, RouteSectionRef) VALUES ('FSACM05_002', 'Ardrossan - Brodick
', 'FSACM05_002');