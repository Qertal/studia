DECLARE @oracle_query NVARCHAR(MAX) = N'
SELECT
    CST_TRN_POINTS.tpt_id || '':'' || CST_TRANSACTIONS.trn_idn_id || '':'' || TO_CHAR( CST_TRN_POINTS.tpt_expiration_date, ''YYYYMMDD'') AS "CleanOrgID",
    TRN_IDN_NO as "EB_Number",
    TPT_STATUS as "Status",
    TPT_POINTS_TYPE_ID as "PointType"
FROM CST_TRN_POINTS
LEFT JOIN CST_TRANSACTIONS ON CST_TRANSACTIONS.TRN_ID = CST_TRN_POINTS.TPT_SRC_TRN_ID
LEFT JOIN CST_CUSTOMERS ON TPT_CUS_ID = CUS_ID
WHERE TPT_POINTS > 0
AND (
    (TPT_AUDIT_MD >= ? AND TPT_AUDIT_MD < ?) OR
    (TPT_AUDIT_CD >= ? AND TPT_AUDIT_CD < ?) OR
    (TRN_PROCESS_DATE >= ? AND TRN_PROCESS_DATE < ?)
)';

DECLARE @startyear int = 2017
DECLARE @limityear int = 2026

DROP TABLE IF EXISTS #temp_exp

CREATE TABLE #temp_exp (
    CleanOrgID VARCHAR(255),
    EB_Number VARCHAR(100),
    Status VARCHAR(50),
    PointType INT
);

DECLARE @startdate DATE = cast(@startyear as varchar) + '-01-01';
DECLARE @enddate DATE = DATEADD(MONTH, 3, @startdate);
DECLARE @curyear int = YEAR(@startdate)


WHILE @curyear < @limityear
BEGIN

	TRUNCATE TABLE #temp_exp

	INSERT INTO #temp_exp ("CleanOrgID", "EB_Number", "Status", "PointType")
	EXEC (
		@oracle_query,
		@startdate, @enddate, -- Parametry dla pierwszej części OR
		@startdate, @enddate, -- Parametry dla drugiej części OR
		@startdate, @enddate  -- Parametry dla trzeciej części OR
	) AT CLM_SAS_REF;

	UPDATE FCT_ExpirationForecast set FCT_ExpirationForecast.EXF_PTPCleanOrgID = PointType,
										FCT_ExpirationForecast.EXF_Status = [Status],
										FCT_ExpirationForecast.EXF_EBNumber = EB_Number
	FROM FCT_ExpirationForecast 
	JOIN #temp_exp
	ON FCT_ExpirationForecast.EXF_CleanOrgID = #temp_exp.CleanOrgID

	set @startdate = DATEADD(MONTH, 3, @startdate)
	set @enddate = DATEADD(MONTH, 3, @startdate);
	set @curyear = YEAR(@startdate)

END
