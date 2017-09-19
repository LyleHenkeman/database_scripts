<Query Kind="SQL" />

--Generate SQL Insert Statements

/***[ Also see "Datamaker" in TFS @ $Platform\Dev\DataMaker ]
Add this to your DB, and then call the sp with the name of the table you want to script out. Assuming it works for you without too much tweaking, very convenient compared to the wizard.
Suspect some work could be done hardening this up - especially around column sizes - to avoid truncation. Looks like some arbitrary (admittedly large) varchar's have been used to hopefully be big enough.
Don't let the single-quoting beat you!
I've tested this on SQL2005 and large XML columns.

USE [DeliveryConnect_DPD]
GO
/****** Object:  StoredProcedure [dbo].[InsertGenerator]    Script Date: 06/20/2011 10:23:26 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER OFF
GO
 
IF EXISTS (SELECT * FROM sys.objects WHERE type = 'P' AND name = 'InsertGenerator')
DROP PROCEDURE InsertGenerator
GO
 
CREATE PROC [dbo].[InsertGenerator]
(@tableName varchar(100)) AS
 
BEGIN
 
SET NOCOUNT ON
 
--Declare a cursor to retrieve column specific information for the specified table
DECLARE cursCol CURSOR FAST_FORWARD FOR 
SELECT column_name,data_type FROM information_schema.COLUMNS WHERE table_name = @tableName
OPEN cursCol
DECLARE @string nvarchar(max) --for storing the first half of INSERT statement
DECLARE @stringData nvarchar(max) --for storing the data (VALUES) related statement
DECLARE @dataType nvarchar(max) --data types returned for respective columns
DECLARE @snippet nvarchar(max)
SET @string='INSERT '+@tableName+'('
SET @stringData=''
 
DECLARE @colName nvarchar(50)
 
FETCH NEXT FROM cursCol INTO @colName,@dataType
 
IF @@fetch_status<>0
	begin
	print 'Table '+@tableName+' not found, processing skipped.'
	close curscol
	deallocate curscol
	RETURN
END
 
WHILE @@FETCH_STATUS=0
BEGIN
 
	--PRINT 'Column ' + @colname + ' is ' + @dataType
 
IF @dataType IN ('varchar','char','nchar','nvarchar')
BEGIN
	SET @stringData=@stringData+''''+'''+isnull('''''+'''''+'+@colName+'+'''''+''''',''NULL'')+'',''+'
END
ELSE
IF @dataType IN ('text','ntext') --if the datatype is text or something else 
BEGIN
	SET @stringData=@stringData+'''''''''+isnull(cast('+@colName+' as varchar(2000)),'''')+'''''',''+'
END
ELSE
IF @dataType = 'money' --because money doesn't get converted from varchar implicitly
BEGIN
	SET @stringData=@stringData+'''convert(money,''''''+isnull(cast('+@colName+' as varchar(200)),''0.0000'')+''''''),''+'
END
ELSE 
IF @dataType='datetime'
BEGIN
	SET @snippet='''convert(datetime,'+'''+isnull('''''+'''''+convert(varchar(200),'+@colName+',121)+'''''+''''',''NULL'')+'',121),''+'
	--PRINT @snippet
	SET @stringData = @stringData+@snippet
END
ELSE 
IF @dataType='image' 
BEGIN
	SET @stringData=@stringData+'''''''''+isnull(cast(convert(varbinary,'+@colName+') as varchar(6)),''0'')+'''''',''+'
END
ELSE
IF @dataType='xml'
BEGIN
	SET @snippet='''''''''+REPLACE(cast('+@colName+' as varchar(max)),'''''''','''''''''''')+'''''''',+'
	--PRINT @snippet
	SET @stringData = @stringData+@snippet
END
ELSE --presuming the data type is int,bit,numeric,decimal 
BEGIN
	SET @stringData=@stringData+''''+'''+isnull('''''+'''''+convert(varchar(200),'+@colName+')+'''''+''''',''NULL'')+'',''+'
END
 
SET @string=@string+@colName+','
 
--PRINT @string
 
FETCH NEXT FROM cursCol INTO @colName,@dataType
END
DECLARE @Query nvarchar(max)
 
SET @query ='SELECT '''+substring(@string,0,len(@string)) + ') VALUES(''+ ' + substring(@stringData,0,len(@stringData)-2)+'''+'')'' FROM '+@tableName
--print @query
exec sp_executesql @query
--select @query
 
CLOSE cursCol
DEALLOCATE cursCol
 
END