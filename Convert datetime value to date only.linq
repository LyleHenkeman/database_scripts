<Query Kind="SQL" />

--Convert datetime value to date only

--We have three methods.
--Floor * Shouldn't use this since it's not accurate (see 1)
--Cast/Convert * slower than dateadd/datediff (see 2)
--Dateadd/Datediff *use this one

--Floor
SELECT CAST(FLOOR(CAST(getdate() AS FLOAT)) AS DATETIME)

--Cast/Convert
--the 112 is the formatting YYYY-MM-DD hh:mm:ss.MMM (MMM is milliseconds)
SELECT CAST(CONVERT(char(8), GETDATE(), 112) AS datetime)

--Dateadd/Datediff
SELECT dateadd(dd, 0, datediff(dd, 0, getdate()))

--Convert to date
--This will only work in SQL 2008.
SELECT convert(date,getdate())