<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Check enhanced pricing

select *
from product
where is_tax_based_pricing = 1 and name = 'FL FLOAT QTLY ANNUITY'