<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

SELECT SYSDATETIME()
    ,SYSDATETIMEOFFSET()
    ,SYSUTCDATETIME()
    ,CURRENT_TIMESTAMP
    ,GETDATE()
    ,GETUTCDATE();