<Query Kind="SQL" />

--Determine Object Dependencies using SQL
--You can look up the dependencies on an object in the Enterprise Manager / Management Studio GUI, but did you know there's also a query for this?

SELECT DISTINCT sysobj.name
FROM syscomments 
INNER JOIN sysobjects sysobj ON syscomments.id = sysobj.id
WHERE charindex('XXXXXX', text) > 0
Replace XXXXXX with the object name, for example a table name, sp, etc...