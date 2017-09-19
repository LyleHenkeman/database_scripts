<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

program.lu_name as Program, party.name as Business_Unit
from program_list 
inner join party on program_list.value_id = party.party_id
inner join program on program_list.program_id = program.program_id
where program_list.list_type = '27500'
order by program.program_id asc 
