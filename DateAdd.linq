<Query Kind="SQL" />

--DateAdd is a SQL function to add hours, minutes or days to a time. 

--You can use it like follows:

SELECT dateadd(hh,1,created) FROM tblevent

--You can also do some funky things with it:
--get difference between UTC time and Local time
--Note this is negative (swap the two dates to get positives)
SELECT datediff(hh,getDate(),getUTCDate())
 
 
--Get start of day
SELECT dateadd(dd, 0, datediff(dd,0,getDate()))
 
 
 
--Get the start of the local day in UTC time, not local server time
SELECT dateadd(hh, datediff(hh,getDate(),getUTCDate()), dateadd(dd, 0, datediff(dd,0,getDate())))