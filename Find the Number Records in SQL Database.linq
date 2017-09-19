<Query Kind="SQL" />

--Find the Number Records in SQL Database
--SQL Table Size for a Given Database

--Following
SELECT OBJECT_NAME(id),rowcnt
FROM SYSINDEXES
WHERE OBJECTPROPERTY(id,'isUserTable')=1 AND indid < 2
ORDER BY rowcnt DESC

--This is only an approx number. It is updated when statistics are updated. The only way to get an accurate values is select count(*) from , but this takes longer and can lock the table.