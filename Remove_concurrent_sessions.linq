<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

UPDATE
system_security_defs
set prevent_concurrent_sessions = 0
where system_security_defs_id = 1