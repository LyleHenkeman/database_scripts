<Query Kind="SQL" />

--Counting events being passed over Interface (change table names, this was for Hermes)

--This should probably be made into a generic page, since pretty much all interface tables have the same sort of query.

SELECT		InterfaceState		,		count(*) 
FROM		tblPNTrackEvent 
GROUP BY	InterfaceState
see [TblPNTrackEvent#InterfaceState] for a list of interface states
To check on a backlog if it happens you can use:
SELECT 
		GETDATE()			AS CurrentDate
	,	COUNT(*)			AS Backlog
	,	MIN(RowTimestamp)	AS Oldest
	,	MAX(RowTimestamp)	AS Newest
FROM tblPNTrackEvent 
WHERE InterfaceState = 0