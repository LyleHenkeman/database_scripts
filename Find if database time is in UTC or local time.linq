<Query Kind="SQL" />

--Find if database time is in UTC or local time
--Sometimes, it's hard to tell if a field in the database is in UTC time, or in the local server time. This is a (mostly) fool-proof method of determining what time a certain field is in.
--SQL used 
--Note, for these examples, I am finding the timezone for the created field in tblevent
--We use two inbuilt SQL functions, getutcdate() and getdate(). You can probably figure out that getdate() gets the local server time, and if you can't figure out getutcdate(), then what are you doing working here anyway?

--So, the code below gets the three date fields from tblevent, utc time, local server time, but it gets the last one inserted into the database, not finding the max of anything.
SELECT top 1 getdate() AS 'Local server time', getUTCdate() AS 'UTC time',
Created,HHSentTimestamp,ReceivedTimeStamp
FROM tblEvent ORDER BY EventId DESC

--Now, you can use this for any you want, changing COLUMN and TABLE for the values you want. Order it by the event id (or similar)
SELECT top 1 getdate() AS 'Local server time', getUTCdate() AS 'UTC time',
COLUMN
FROM TABLE ORDER BY EventId DESC