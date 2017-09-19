<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Merge database tables from one DB to another on the same instance
Merge BDO_Poked.dbo.ax_user as tgt
USING BDO_Before.dbo.ax_user as src
ON src.ax_user_id = tgt.ax_user_id
WHEN MATCHED
THEN UPDATE SET tgt.Name = src.Name, tgt.ext_name = src.ext_name;