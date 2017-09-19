<Query Kind="SQL">
  <Connection>
    <ID>ee885e94-f75f-47bd-a8a3-102f30613e63</ID>
    <Server>.\LYLE</Server>
    <Database>AXIOMSEED</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Output>DataGrids</Output>
</Query>

select t.name as TableWithForeignKey, fk.constraint_column_id as FK_PartNo , c.name as ForeignKeyColumn 
from sys.foreign_key_columns as fk
inner join sys.tables as t on fk.parent_object_id = t.object_id
inner join sys.columns as c on fk.parent_object_id = c.object_id and fk.parent_column_id = c.column_id
where fk.referenced_object_id = (select object_id from sys.tables where name = 'TableOthersForeignKeyInto')
order by TableWithForeignKey, FK_PartNo