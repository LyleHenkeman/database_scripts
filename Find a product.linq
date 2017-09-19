<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Find a product

select *
from contract ae
	JOIN product a ON a.product_id = ae.product_id
	where a.name = 'Operating Lease' and contract_id >0 and ae.contract_state = '11140'