<Query Kind="SQL">
  <Connection>
    <ID>ee885e94-f75f-47bd-a8a3-102f30613e63</ID>
    <Server>.\LYLE</Server>
    <Database>AXIOMSEED</Database>
    <ShowServer>true</ShowServer>
  </Connection>
  <Output>DataGrids</Output>
</Query>

-- Find Oracle Index
    SELECT * FROM user_indexes WHERE index_name like '%FLOW%';
	
-- Drop Index
DROP INDEX ord_customer_ix_demo;