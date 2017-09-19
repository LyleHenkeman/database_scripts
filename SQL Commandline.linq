<Query Kind="SQL" />

--SQL Commandline
--delete all jobs for a db

sqlcmd -S BUILD-SQL\SQL2K8 -E -i \\enterprise\development\infrastructure\Build\scripts\ext_DeleteDatabaseJobs.sql -v DatabaseName = "_dbname_"
 
 
 
sqlcmd -E -S BUILD-SQL\SQL2K8 -d "master" -Q "DROP DATABASE [_dbname_]"