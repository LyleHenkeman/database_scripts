<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--match bankflow and flow

select *
from flow
where contract_id = 576 and flow_id = 154464


select *
from bank_flow
where contract_id = 576


select * 
from flow ae
	JOIN bank_flow a ON a.contract_id = ae.contract_id
	--where ae.flow_id = 154464
	where a.bank_flow_id = 71217 and ae.flow_id = 154464


	select *
	from lookupset
	where lookupset_id = 1003