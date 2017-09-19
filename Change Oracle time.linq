<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--To change:
CREATE OR REPLACE VIEW axvw_get_datetime (server_dt) AS SELECT to_date('09-Jun-2023','dd-mon-yyyy') AS server_dt FROM dual


--To reset:
CREATE OR REPLACE VIEW axvw_get_datetime (server_dt) AS SELECT SYSDATE AS server_dt FROM dual