<Query Kind="SQL" />

--SQL index fragmentation

--http://blogs.technet.com/josebda/archive/2009/03/20/sql-server-2008-fragmentation.aspx

SELECT a.index_id, name, avg_fragmentation_in_percent
FROM sys.dm_db_index_physical_stats (NULL, NULL, NULL, NULL, NULL) AS a
    JOIN sys.indexes AS b ON a.object_id = b.object_id AND a.index_id = b.index_id
WHERE avg_fragmentation_in_percent > 30
ORDER BY avg_fragmentation_in_percent DESC