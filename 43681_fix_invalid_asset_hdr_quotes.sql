/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:		43681_asset_hdr_quotes.sql
Client:				ModSpace
Case#:				Case 43681
Client ref:			
AAF Version:		4.41.03
Rev:					$Rev: 43651 $
Database Type:		SQL Server
Author:				Michael Carson
Summary:          This script fixes invalid asset_hdr_quote.quote_status and asset_hdr.asset_status. 
				
History:		Date				Author				Action
				15-Jul-2013		Michael Carson		Created			
				 
Instructions:

Script should be run in its entirity.  Do not run statements individually.

Run 40627_Fix_quote_status_on_activated_contracts.sql before running this script.

This script can be run multiple times.

*/

/***************************************************************************
0. Log this event
***************************************************************************/
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Initiated: 43681_asset_hdr_quotes.sql', 0, 'Script' , 3 , ' ' , 0);

/***************************************************************************
1. Archive and delete existing backups
***************************************************************************/
if exists(select * from dbo.sysobjects where id = object_id(N'[dbo].[mod_43681_asset_hdr_quote_bk]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
begin
	if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[mod_43681_asset_hdr_quote_bk_archive]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
	begin
		--create empty archive table if it doesn't exist
		select bu.*, getdate() save_dt into dbo.mod_43681_asset_hdr_quote_bk_archive
		from dbo.mod_43681_asset_hdr_quote_bk bu
		where 1 = 0;
	end
	insert into dbo.mod_43681_asset_hdr_quote_bk_archive select *, getdate() 
	from dbo.mod_43681_asset_hdr_quote_bk;
	
	drop table dbo.mod_43681_asset_hdr_quote_bk;
end

if exists(select * from dbo.sysobjects where id = object_id(N'[dbo].[mod_43681_asset_hdr_bk]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
begin
	if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[mod_43681_asset_hdr_bk_archive]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
	begin
		--create empty archive table if it doesn't exist
		select bu.*, getdate() save_dt into dbo.mod_43681_asset_hdr_bk_archive
		from dbo.mod_43681_asset_hdr_bk bu
		where 1 = 0;
	end
	insert into dbo.mod_43681_asset_hdr_bk_archive select *, getdate() 
	from dbo.mod_43681_asset_hdr_bk;
	
	drop table dbo.mod_43681_asset_hdr_bk;
end

/***************************************************************************
2. Create backup of the asset_hdr_quote records for which the quote_status is wrong
	(this needs to be done before updating asset_hdr.asset_status)
		- asset_hdr_quotes without an asset record on the contract should be in expired status		
		- asset_hdr_quotes on a terminated contract should be in terminated status		
***************************************************************************/
select ahq.asset_hdr_quote_id, 			
			ahq.quote_status, 			
			case when a.asset_id is null then 44010 --Expired				  
				  when a.is_terminated = 1 and c.contract_state = 11150 then 44040 --Terminated				  
				  else ahq.quote_status end as new_quote_status
into dbo.mod_43681_asset_hdr_quote_bk			
from asset_hdr_quote ahq
join asset_hdr ah on ah.asset_hdr_id = ahq.asset_hdr_id
join contract c on c.contract_id = ahq.contract_id
left join asset a on a.asset_hdr_id in (ah.asset_hdr_id, ah.owner_id, ah.parent_owner_id) and a.contract_id = ahq.contract_id
where (a.asset_id is null or c.contract_state = 11150)--Terminated
and ahq.quote_status in (44000, 44020)	--Quoting, Reserved	
	
/***************************************************************************
3. update the asset_hdr_quote.quote_status
***************************************************************************/
update asset_hdr_quote
set quote_status = bk.new_quote_status
from asset_hdr_quote ahq 
join mod_43681_asset_hdr_quote_bk bk on bk.asset_hdr_quote_id = ahq.asset_hdr_quote_id
		
/***************************************************************************
4. Create backup of the asset_hdr records for which the asset_status is wrong
***************************************************************************/	
/************************************************************************************
4.1. asset_hdr current on a contract but still in Quoting or Reserved status - there are no other active quotes
*************************************************************************************/
--update asset_hdr.asset_status = 43030 --Active
select ah.asset_hdr_id, ah.asset_status,43030 as new_asset_status
into dbo.mod_43681_asset_hdr_bk
from asset_hdr ah	
join contract c on c.contract_id = ah.current_contract_id
where ah.asset_status in (43040, 43050)	--Quoting, Reserved
and c.contract_state = 11130	--Complete Activated
and not exists 
(
	select 1
	from asset_hdr_quote ahq2 
	join contract c2 on c2.contract_id = ahq2.contract_id
	where ahq2.asset_hdr_id = ah.asset_hdr_id		
	and ahq2.quote_status in (44000, 44020) 	--Quoting, Reserved		
) 
	
/************************************************************************************
4.2. Active asset_status with Reserved quote - should be Reserved		
*************************************************************************************/
--update asset_hdr.asset_status = 43050 --Reserved
insert into dbo.mod_43681_asset_hdr_bk (asset_hdr_id, asset_status, new_asset_status)
select ah.asset_hdr_id, ah.asset_status, 43050 as new_asset_status
from asset_hdr ah		
join contract c on c.contract_id = ah.current_contract_id
join product p on p.product_id = c.product_id	
where ah.asset_status = 43030	--Active	
and c.contract_state = 11130	--Complete Activated	
and exists 
(
	select 1
	from asset_hdr_quote ahq2 
	join asset_hdr ah2 on ah2.asset_hdr_id = ahq2.asset_hdr_id
	join contract c2 on c2.contract_id = ahq2.contract_id
	join asset a on a.asset_hdr_id in (ah2.asset_hdr_id, ah2.owner_id, ah2.parent_owner_id) and a.contract_id = ahq2.contract_id
	where ahq2.asset_hdr_id = ah.asset_hdr_id				
	and ahq2.quote_status = 44020 	--Reserved			
) 
	
/************************************************************************************
4.3. Active asset_status with Quoting quote - should be Quoting
*************************************************************************************/
--update asset_hdr.asset_status = 43040 --Quoting
insert into dbo.mod_43681_asset_hdr_bk (asset_hdr_id, asset_status, new_asset_status)
select ah.asset_hdr_id, ah.asset_status, 43040 as new_asset_status
from asset_hdr ah		
join contract c on c.contract_id = ah.current_contract_id
join product p on p.product_id = c.product_id	
where ah.asset_status = 43030	--Active	
and c.contract_state = 11130	--Complete Activated	
and exists 
(
	select 1
	from asset_hdr_quote ahq2 
	join asset_hdr ah2 on ah2.asset_hdr_id = ahq2.asset_hdr_id
	join contract c2 on c2.contract_id = ahq2.contract_id
	join asset a on a.asset_hdr_id in (ah2.asset_hdr_id, ah2.owner_id, ah2.parent_owner_id) and a.contract_id = ahq2.contract_id
	where ahq2.asset_hdr_id = ah.asset_hdr_id				
	and ahq2.quote_status = 44000 	--Quoting
) 

/***************************************************************************
5. update the asset_hdr.asset_status
***************************************************************************/
update asset_hdr
set asset_status = bk.new_asset_status
from asset_hdr ah
join mod_43681_asset_hdr_bk bk on bk.asset_hdr_id = ah.asset_hdr_id

/***************************************************************************
6. Rollback
***************************************************************************/
/*

update asset_hdr
set asset_status = bk.asset_status
from asset_hdr ah
join mod_43681_asset_hdr_bk bk on bk.asset_hdr_id = ah.asset_hdr_id

update asset_hdr_quote
set quote_status = bk.quote_status
from asset_hdr_quote ahq 
join mod_43681_asset_hdr_quote_bk bk on bk.asset_hdr_quote_id = ahq.asset_hdr_quote_id

drop table mod_43681_asset_hdr_bk
drop table mod_43681_asset_hdr_quote_bk

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted: 43681_asset_hdr_quotes.sql', 0, 'Script' , 3 , ' ' , 0);

*/