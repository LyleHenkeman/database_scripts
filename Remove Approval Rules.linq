<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

UPDATE
approval_rule_hdr
set is_active = 0
where approval_rule_hdr_id = 1

--Then go in and remove the statechains in workflow