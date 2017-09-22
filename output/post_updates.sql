

UPDATE AnnotatedStopPointRef SET CommonName = REPLACE(CommonName, " Ferry Terminal", "");
UPDATE AnnotatedStopPointRef SET CommonName = "Brodick" WHERE StopPointRef = "9300BRB";

UPDATE Service SET CalMacServiceId = 5 WHERE ServiceCode = 'FSACM05';
UPDATE Service SET CalMacServiceId = 36 WHERE ServiceCode = 'FSACM05A';
UPDATE Service SET CalMacServiceId = 25 WHERE ServiceCode = 'FSACM25';
UPDATE Service SET CalMacServiceId = 23 WHERE ServiceCode = 'FSACM23';
UPDATE Service SET CalMacServiceId = 3 WHERE ServiceCode = 'FSACM03';
UPDATE Service SET CalMacServiceId = 18 WHERE ServiceCode = 'FSACM18';