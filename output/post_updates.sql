

UPDATE AnnotatedStopPointRef SET CommonName = 'Brodick' WHERE StopPointRef = '9300BRB';
UPDATE AnnotatedStopPointRef SET CommonName = 'Ardrossan' WHERE StopPointRef = '9300ARD';
UPDATE AnnotatedStopPointRef SET CommonName = 'Campbeltown' WHERE StopPointRef = '9300CPB';
UPDATE AnnotatedStopPointRef SET CommonName = 'Ullapool' WHERE StopPointRef = '9300ULL';
UPDATE AnnotatedStopPointRef SET CommonName = 'Stornoway' WHERE StopPointRef = '9300STO';

UPDATE Service SET CalMacServiceId = 5 WHERE ServiceCode = 'FSACM05';
UPDATE Service SET CalMacServiceId = 36 WHERE ServiceCode = 'FSACM05A';
UPDATE Service SET CalMacServiceId = 25 WHERE ServiceCode = 'FSACM25';