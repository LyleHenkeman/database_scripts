<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- find database restore history
declare @DB sysname = 'MY_DB';
select * from msdb.dbo.restorehistory where destination_database_name = @DB;