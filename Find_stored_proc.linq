<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

select * 
from 
   sys.procedures 

   where 
   name like '%sp_reset_connection %'