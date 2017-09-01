

UPDATE AnnotatedStopPointRef SET CommonName = 'Brodick' WHERE StopPointRef = '9300BRB';
UPDATE AnnotatedStopPointRef SET CommonName = 'Ardrossan' WHERE StopPointRef = '9300ARD';
UPDATE AnnotatedStopPointRef SET CommonName = 'Campbeltown' WHERE StopPointRef = '9300CPB';

UPDATE Service SET CalMacServiceId = 5 WHERE ServiceCode = 'FSACM05';
UPDATE Service SET CalMacServiceId = 36 WHERE ServiceCode = 'FSACM05A';