<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- Find how many contracts 

select * from contract where currency_id = 99

-- Update currency

update currency
set code = 'RON'
where code = 'ROL'


/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:	Case 42240 Update Romanian currency code.sql
Client:			Tetra Laval
Case#:			FB 42240
Client ref:		TTP5905 Romanian currency code
42 Version:		4.60.X
Database Type:	MSSQL
Author:			Glendon Filer
Owner:			Glendon Filer
Site Owner:		Rose Harling
Summary:		Adhoc script to update the currency code on Romanian CCY;
				Currently set to ROL, must be RON.
				CCY's code has changed with Reuters in recent years.

History: 	Date		Author			Action
			20130628	Glendon Filer	Created
*/

/* Create a backup of old details and update mechanism for new code
	incase we need to rollback and for testing*/
select ccy.currency_id, ccy.name, ccy.code old_code, 'RON' new_code
into tli_c42240_ccy_bu
from currency ccy
where code = 'ROL';

/* Show details of current data in backup set*/
select ccy.currency_id, ccy.name, ccy.code old_code, bu.new_code
from currency ccy
inner join tli_c42240_ccy_bu bu on bu.currency_id = ccy.currency_id;  

/*Update CCY code*/
update ccy set ccy.code = bu.new_code
from currency ccy 
inner join tli_c42240_ccy_bu bu on ccy.currency_id = bu.currency_id;

/*Roll back*/
/*
update ccy set ccy.code = bu.old_code
from currency ccy 
inner join tli_c42240_ccy_bu bu on ccy.currency_id = bu.currency_id;
*/

/*Drop backup / mapping table*/
/*
drop table tli_c42240_ccy_bu;
*/