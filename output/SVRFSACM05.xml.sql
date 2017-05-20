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