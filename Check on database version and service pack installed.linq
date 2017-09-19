<Query Kind="SQL" />

--Check on database version and service pack installed

--To check the current version of the SQL Server installed
  SELECT @@VERSION
  GO
--To check the current Service Pack of the SQL Server installed
  SELECT SERVERPROPERTY('ProductLevel')
  GO
