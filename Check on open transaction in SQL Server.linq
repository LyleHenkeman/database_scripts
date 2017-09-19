<Query Kind="SQL" />

--Check on open transaction in SQL Server

--This will allow you to see any current open transactions
 
  DBCC OPENTRAN
--This will show the current size that the transaction log is using.
  DBCC SQLPERF(logspace)

--This will show some extra information on the transaction log - i have no idea what. Please google it if your interested
  DBCC LOGINFO

--This will count open transactions
SELECT @@TRANCOUNT