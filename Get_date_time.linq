<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Get date
SELECT
CONVERT(TIME,GETDATE()) AS HourMinuteSecond,
CONVERT(DATE,GETDATE(),101) AS DateOnly
GO


--Change date and time
IF EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[axvw_get_datetime]')
    AND objectproperty( id, N'IsView' ) = 1)
DROP VIEW [dbo].[axvw_get_datetime]
GO

CREATE VIEW [dbo].[axvw_get_datetime]
AS
   --SELECT server_dt = getdate()
   SELECT server_dt = '01-Jul-2012'
GO