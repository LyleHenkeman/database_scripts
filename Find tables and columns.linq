<Query Kind="ESQL">
  <Connection>
    <ID>ee885e94-f75f-47bd-a8a3-102f30613e63</ID>
    <Persist>true</Persist>
    <Server>.\LYLE</Server>
    <Database>AXIOMSEED</Database>
    <ShowServer>true</ShowServer>
  </Connection>
</Query>

SELECT COLUMN_NAME, TABLE_NAME 
FROM INFORMATION_SCHEMA.COLUMNS 
WHERE COLUMN_NAME LIKE '%User%'