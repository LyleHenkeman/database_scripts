<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Find Flow method 

select *
from lookupset
where set_name = 'PaymentMethod'

select *
from lookupset ae
	JOIN flow a ON a.flow_method_id = ae.lookupset_id
	where ae.lookupset_id = 5100 --Cheque receivable
	and a.contract_id = 384
	order by expected_dt asc