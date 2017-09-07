WITH VehicleJourneysOperatingToday AS 
(
    SELECT VehicleJourneyRef
    FROM DayOfOperation DOO
    WHERE strftime('%s','now') >= DOO.StartDate 
    AND strftime('%s','now') <= DOO.EndDate
),

VehicleJourneysNotOperatingToday AS 
(
    SELECT VehicleJourneyRef
    FROM DayOfNonOperation DONO
    WHERE strftime('%s','now') >= DONO.StartDate 
    AND strftime('%s','now') <= DONO.EndDate
),

FromStopPointRef AS 
(
    SELECT *
    FROM AnnotatedStopPointRef ASPR
),

ToStopPointRef AS 
(
    SELECT *
    FROM AnnotatedStopPointRef ASPR
),

MultiJourneyDepatures AS 
(
	SELECT JPTL.JourneyPatternTimingLinkId
    FROM RouteLink RL
    INNER JOIN JourneyPatternTimingLink JPTL ON RL.RouteLinkId = JPTL.RouteLinkRef
    WHERE
    RL.FromStopPointRef = "9300BRB" 
	AND RL.ToStopPointRef = "9300ARD"
    AND RL.RouteSectionId IN
    (
        SELECT RL.RouteSectionId 
        FROM RouteLink RL 
        GROUP BY RL.RouteSectionId HAVING COUNT(*) > 1
    )
)

SELECT FSPR.CommonName AS `From`, TSPR.CommonName AS `To`, VJ.DepatureHour AS Hour, VJ.DepatureMinute AS Minute, JPTL.RunTime AS RunTime, VJ.Note AS Note
FROM VehicleJourney VJ
INNER JOIN JourneyPattern JP ON JP.JourneyPatternId = VJ.JourneyPatternRef
INNER JOIN JourneyPatternSection JPS ON JPS.JourneyPatternSectionId = JP.JourneyPatternSectionRef
INNER JOIN JourneyPatternTimingLink JPTL ON JPTL.JourneyPatternSectionId = JPS.JourneyPatternSectionId
INNER JOIN Service S ON S.ServiceCode = JP.ServiceRef
INNER JOIN RouteLink RL ON RL.RouteLinkId = JPTL.RouteLinkRef
INNER JOIN FromStopPointRef FSPR ON FSPR.StopPointRef = JPTL.JourneyPatternFromStopPointRef
INNER JOIN ToStopPointRef TSPR ON TSPR.StopPointRef = JPTL.JourneyPatternToStopPointsRef
WHERE VJ.Saturday = 1 
AND JPTL.JourneyPatternFromStopPointRef = "9300BRB"
AND JPTL.JourneyPatternToStopPointsRef = "9300ARD"
AND S.CalMacServiceId = 5
AND 
(
    (strftime('%s','now') >= S.StartDate AND strftime('%s','now') <= S.EndDate)
    OR
    (VJ.VehicleJourneyCode IN VehicleJourneysOperatingToday)
)
AND VJ.VehicleJourneyCode NOT IN VehicleJourneysNotOperatingToday
AND JPTL.JourneyPatternTimingLinkId NOT IN MultiJourneyDepatures
ORDER BY VJ.DepatureHour, VJ.DepatureMinute
