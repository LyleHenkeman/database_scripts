/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:	42709_fix_asset_hdr_quote_on_terminated_contracts.sql
Client:			ModSpace
Case#:			42709
Client ref:		N/A
AAF Version:	4.41
Database Type:	SQL Server
Author:			Yoshiki Okawa
Owner:			Yoshiki Okawa
Site Owner:		Jason Depew
Revision:		$Rev: 42442 $
Summary:		This script covers all terminated assets where their statuses are incorrect, which is causing assets to move to other contracts.
				Originally caused by issue resolved by case 39341. Alven will work on active contracts in case 40621.
				This script MUST be run after 40627_Fix_quote_status_on_activated_contracts.sql.
				
History:		Date			Author				Action
				26-Jun-2013		Yoshiki OKawa		Created

PREREQUISITES:
40627_Fix_quote_status_on_activated_contracts.sql

Instructions:

This script should be run once only using 4.41.02 or later build.

*/

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Ran script 42709_fix_asset_hdr_quote_on_terminated_contracts.sql ($Rev: 42442 $)', 0, 'Script' , 3 , ' ' , 0);

-- Create backup table for inserted asset hdr address ids
select 0 asset_hdr_address_id
into dbo.mod_inserted_asset_hdr_address_ids_42709
where 0 = 1;

select distinct ahq.asset_hdr_quote_id, ah.asset_hdr_id, ahq.quote_status, ah.asset_status, ahq.contract_id
into dbo.mod_asset_status_backup_42709
from asset_hdr_quote ahq
inner join asset_hdr ah on ah.asset_hdr_id = ahq.asset_hdr_id
inner join asset a on a.contract_id = ahq.contract_id and a.asset_hdr_id in (ah.asset_hdr_id, ah.owner_id, ah.parent_owner_id)
where a.is_terminated = 1 and ahq.quote_status != 44040; -- not Terminated

-- Reset quote status to terminated.
update ahq
set ahq.quote_status = 44040 -- Terminated
from asset_hdr_quote ahq
inner join mod_asset_status_backup_42709 bu on bu.asset_hdr_quote_id = ahq.asset_hdr_quote_id;

-- Reset quote status to active when there isn't any quoting or reserved quote.
update ah
set ah.asset_status = 43030 -- Active
from asset_hdr ah
inner join mod_asset_status_backup_42709 bu on bu.asset_hdr_id = ah.asset_hdr_id
and not exists
(
	select 0
	from asset_hdr_quote ahq
	where ahq.asset_hdr_id = ah.asset_hdr_id
	and ahq.quote_status in (44000, 44020) -- Quoting or Reserved
);

-- Insert asset hdr current effective address based on contract start date and contract's default asset address
insert into asset_hdr_address(asset_hdr_id, address_id, effective_dt, asset_hdr_address_type, stamp)
output inserted.asset_hdr_address_id into dbo.mod_inserted_asset_hdr_address_ids_42709
select bu.asset_hdr_id, cd.default_asset_address_id, c.calc_dt, 45101, 0
from mod_asset_status_backup_42709 bu
inner join contract c on c.contract_id = bu.contract_id
inner join contract_det cd on cd.contract_id = bu.contract_id
where not exists
(
	select 0
	from asset_hdr_address aha
	where aha.asset_hdr_address_type = 45101
	and effective_dt = c.calc_dt
	and aha.asset_hdr_id = bu.asset_hdr_id
);

/* Restore:

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted script 42709_fix_asset_hdr_quote_on_terminated_contracts.sql ($Rev: 42442 $)', 0, 'Script' , 3 , ' ' , 0);

update ahq
set ahq.quote_status = bu.quote_status
from asset_hdr_quote ahq
inner join mod_asset_status_backup_42709 bu on bu.asset_hdr_quote_id = ahq.asset_hdr_quote_id;

update ah
set ah.asset_status = bu.asset_status
from asset_hdr ah
inner join mod_asset_status_backup_42709 bu on bu.asset_hdr_id = ah.asset_hdr_id;

delete from asset_hdr_address where asset_hdr_address_id in (select * from mod_inserted_asset_hdr_address_ids_42709);

drop table mod_inserted_asset_hdr_address_ids_42709;
drop table mod_asset_status_backup_42709;

*/