<Query Kind="SQL" />

--Grant Execute Permission on SPs

SELECT
         obj.Name AS SPName
 
FROM sys.sql_modules modu
 
INNER JOIN sys.objects obj
 
ON modu.object_id = obj.object_id
 
WHERE obj.type = 'P' AND obj.Name LIKE 'spWeb%'
--Then...

GRANT execute ON <Stored Proc' name> to [<the SQL login/user>]