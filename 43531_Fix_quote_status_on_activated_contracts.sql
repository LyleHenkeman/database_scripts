/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:    43531_Fix_quote_status_on_activated_contracts.sql
Client:         ModSpace
Case#:			Case 43531
Client ref:		PGL267
AAF Version:    4.41.03
Rev:			$Rev: 43611 $
Database Type:  SQL Server
Author:         Alven Lee
Summary:        
				
History:		Date				Author				Action
				31-May-2013		Alven Lee			Created
				11-Jul-2013		Michael Carson		Amended
				 
Instructions:

Script should be run in its entirity.  Do not run statements individually.

This script must only be run once.

This script must be run before the script for case 42709.

*/

/***************************************************************************
0. Log this event
***************************************************************************/
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Initiated: 43531_Fix_quote_status_on_activated_contracts.sql', 0, 'Script' , 3 , ' ' , 0);

/***********************************************************************************/
--BK1 Begin: Backup affected records
/***********************************************************************************/

--find Complete Activated contracts with asset header quotes in the Quoting status
select distinct c.contract_id, c.calc_dt into #contract_ids
from contract c 
join asset_hdr hdr on hdr.current_contract_id = c.contract_id
join asset_hdr_quote ahq on ahq.contract_id = c.contract_id
where c.contract_state in (11130, 11140) --Complete Activated, Partially Terminated
and ahq.asset_hdr_id = hdr.asset_hdr_id
and hdr.asset_status = 43040 --Quoting
and ahq.quote_status = 44000 --Quoting
and product_style = 2014 --Rental
and exists (select * from event_queue eq 
			where  eq.contract_id = c.contract_id 
			and event_state_before = 11030); --Expired. Contracts which has moved to expired state. 
					
--create a backup record of the contract/asset_hdr that are in the incorrect state
select distinct temp.contract_id, temp.calc_dt, ahq.asset_hdr_quote_id, hdr.asset_hdr_id, ahq.quote_status, hdr.asset_status, hdr.delivery_address_id, hdr.current_address_id 
into dbo.mod_40627_asset_quote
	from #contract_ids temp 
	join asset_hdr hdr on hdr.current_contract_id = temp.contract_id
	join asset_hdr_quote ahq on ahq.contract_id = temp.contract_id	and ahq.asset_hdr_id = hdr.asset_hdr_id
	join asset a on a.asset_hdr_id in (hdr.asset_hdr_id, hdr.owner_id, hdr.parent_owner_id) and a.contract_id = temp.contract_id
	where a.is_terminated = 0;
	
--find the current addresses that need to be updated	
with address_cte(asset_hdr_id, address_id, effective_dt)
as
(
--if there is a delivery address then we will set the current address to that
select asset_hdr_id, delivery_address_id, calc_dt
from mod_40627_asset_quote 
where delivery_address_id > 0

union

--if there is no delivery address then we will set the current address to the contract default address
select quote_bk.asset_hdr_id, contract_det.default_asset_address_id, quote_bk.calc_dt 
from mod_40627_asset_quote quote_bk
join contract_det on contract_det.contract_id = quote_bk.contract_id
where quote_bk.delivery_address_id = 0
)
,
latest_current_address_cte(asset_hdr_id, address_id, effective_dt)
as
(
--find the latest current address on the asset_hdr
select distinct aha.asset_hdr_id, aha.address_id, aha.effective_dt
from asset_hdr_address aha
join (select aha2.asset_hdr_id, max(aha2.effective_dt) as effective_dt
		from asset_hdr_address aha2
		join mod_40627_asset_quote quote_bk on aha2.asset_hdr_id = quote_bk.asset_hdr_id 
		where aha2.asset_hdr_address_type = 45101 --Current
		group by aha2.asset_hdr_id
		) sub on sub.asset_hdr_id = aha.asset_hdr_id and sub.effective_dt = aha.effective_dt
)

--make a backup of the addresses we will be updating - also flag those records where the user has manually updated the address record
select asset_hdr_id, address_id, effective_dt, 
			case when exists(select * from latest_current_address_cte 
									where latest_current_address_cte.asset_hdr_id = address_cte.asset_hdr_id 
									and (latest_current_address_cte.address_id = address_cte.address_id
										or latest_current_address_cte.effective_dt >= address_cte.effective_dt)) then 0 else 1 end as update_current_address,
			case when exists(select * from latest_current_address_cte 
									where latest_current_address_cte.asset_hdr_id = address_cte.asset_hdr_id 
									and latest_current_address_cte.effective_dt = address_cte.effective_dt) then 0 else 1 end as insert_asset_hdr_address
into dbo.mod_40627_asset_hdr_address
from address_cte
	
--BK1 End:

/***********************************************************************************/
--UP1 Begin: Update records
/***********************************************************************************/

--update the asset_hdr status
update asset_hdr
	set asset_status = 43030 --Active
	from asset_hdr ah
	join dbo.mod_40627_asset_quote bk on ah.asset_hdr_id = bk.asset_hdr_id;

--update the asset_hdr_quote status
update asset_hdr_quote
	set quote_status = 44030 --ActiveOnContract
	from asset_hdr_quote ahq
	join dbo.mod_40627_asset_quote quote_bk on ahq.asset_hdr_quote_id = quote_bk.asset_hdr_quote_id;

--insert new current address records
insert into asset_hdr_address(asset_hdr_id, address_id, effective_dt, asset_hdr_address_type, stamp)
select asset_hdr_id, address_id, effective_dt, 45101, 0
from mod_40627_asset_hdr_address
where insert_asset_hdr_address = 1

--update the current address on the asset_hdr
update asset_hdr
set current_address_id = address_bk.address_id
from asset_hdr ah
join mod_40627_asset_hdr_address address_bk on address_bk.asset_hdr_id = ah.asset_hdr_id
where address_bk.update_current_address = 1

drop table #contract_ids

--UP1 End:

/***********************************************************************************
--R1 Begin: Revert changes, restore from backup.
 
update asset_hdr 
	set asset_status = quote_bk.asset_status 
	from asset_hdr 
	join dbo.mod_40627_asset_quote quote_bk on asset_hdr.asset_hdr_id = quote_bk.asset_hdr_id;
	
update asset_hdr_quote 
	set quote_status = quote_bk.quote_status 
	from asset_hdr_quote 
	join dbo.mod_40627_asset_quote quote_bk on asset_hdr_quote.asset_hdr_quote_id = quote_bk.asset_hdr_quote_id;

update asset_hdr
set current_address_id = quote_bk.current_address_id
from asset_hdr ah
join mod_40627_asset_hdr_address address_bk on address_bk.asset_hdr_id = ah.asset_hdr_id 
join mod_40627_asset_quote quote_bk on quote_bk.asset_hdr_id = address_bk.asset_hdr_id
where address_bk.update_current_address = 1
	
delete from asset_hdr_address
where asset_hdr_address_id in 
	(select asset_hdr_address_id 
	 from asset_hdr_address aha
	 join mod_40627_asset_hdr_address address_bk on aha.asset_hdr_id = address_bk.asset_hdr_id 
																and aha.address_id = address_bk.address_id 
																and aha.effective_dt = address_bk.effective_dt 
																and aha.asset_hdr_address_type = 45101
	 where address_bk.insert_asset_hdr_address = 1																)

drop table mod_40627_asset_hdr_address
drop table mod_40627_asset_quote

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted: 43531_Fix_quote_status_on_activated_contracts.sql', 0, 'Script' , 3 , ' ' , 0);

--R1 End:
***********************************************************************************/