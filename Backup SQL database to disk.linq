<Query Kind="SQL" />

--Backup SQL database to disk
--Reasons for this page

--To quickly be able to get a database off a server, and back to us (for BAU)
--The script

BACKUP DATABASE dbName TO disk = 'E:\dbName-yyyymmdd.bak'

--OR, 

Backup to multiple files (split backup)
BACKUP DATABASE dbName TO 
disk = 'E:\dbName-yyyymmdd(1).bak',
disk = 'E:\dbName-yyyymmdd(2).bak',
disk = 'E:\dbName-yyyymmdd(3).bak'


--ALSO, if there's not enough space on one disk, you can backup to more than one disk using this method
BACKUP DATABASE dbName TO 
disk = 'E:\dbName-yyyymmdd(1).bak',
disk = 'F:\dbName-yyyymmdd(2).bak',
disk = 'C:\dbName-yyyymmdd(3).bak'


--We can then:
--zip each file up separately
--Delete the original BAK files
--Copy via ftp back to us
--Delete the Zip files from the server
--Restore the backup to a server and then we have the database of a BAU environment almost set up.