<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

select *
from contract_custom_flow
where contract_id = 46 and link_no = 2


select link_no, count (1)
from contract_custom_flow
where contract_id = 46
and is_deleted = 0
group by link_no having COUNT (1) > (1)
