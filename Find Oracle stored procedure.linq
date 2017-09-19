<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Find oracle stored procedure

select * 
  from USER_SOURCE 
 where type='PROCEDURE' 
   and name='my_stored_procedure.' 
   
   --find object
   SELECT *
  FROM USER_OBJECTS
 WHERE object_type = 'PROCEDURE'
   AND object_name = 'MY_STORED_PROCEDURE'
   
   --drop sp
   DROP PROCEDURE hr.remove_emp; 