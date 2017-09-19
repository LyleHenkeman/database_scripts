<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- Leasetech case 38377

--The following is a query to find contracts they will have this issue.

select * from contract where contract_id in 
( 
    select distinct current_contract_id from asset_hdr ah where not exists (select * from asset a where asset_id = ah.asset_hdr_id) 
    union 
    select distinct original_contract_id from asset_hdr ah where not exists (select * from asset a where asset_id = ah.asset_hdr_id)
)
and mature_dt1 > GETDATE() 
and contract_state = 11130

--This returned 349 contracts on our copy of database.

--In order to workaround this issue with an old build:

--For example, lets say you are getting the following error message on contract 7109.
--An attempt was made to lookup an invalid value. The following value is invalid 9027 for the following table asset.

--1. Find contract alterations which are stopping the contract to be restructured.

select *
from contract_alteration ca
inner join contract_alteration_lookup cal
    on ca.fld_id = cal.contract_alteration_lookup_id
where ca.contract_id = 7109
and ca.contract_restructure_id = (select MAX(contract_restructure_id) from contract_restructure cr where cr.contract_id = ca.contract_id)
and ca.fld_id in (548,549,550,552,553)

--2. Make sure that all of them are not important for you.

3. Delete those alterations.
delete
from contract_alteration
where contract_id = 7109
and obj_id = 9027
and contract_restructure_id = (select MAX(contract_restructure_id) from contract_restructure cr where cr.contract_id = contract_id)
and fld_id in (548,549,550,552,553)




-- Alternate delete (by contract_alteration_id)

delete
from contract_alteration
where contract_alteration_id = (
select contract_alteration_id
from contract_alteration ca
inner join contract_alteration_lookup cal
    on ca.fld_id = cal.contract_alteration_lookup_id
where ca.contract_id = 7747
and ca.contract_restructure_id = (select MAX(contract_restructure_id) from contract_restructure cr where cr.contract_id = ca.contract_id)
and ca.fld_id in (548,549,550,552,553)
)