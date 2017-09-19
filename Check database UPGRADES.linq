<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP 1000 [db_upgrade_step_id]
      ,[is_vo_step]
      ,[run_date]
      ,[stamp]
  FROM [MODSPACE_LATEST].[dbo].[db_upgrade_step]
  --where db_upgrade_step_id = 736873725
  order by run_date desc