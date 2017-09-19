/* SunGard : Ambit Asset Finance : Database script : Copyright 2012

Script Name:	40320_duplicate tax_calc_flow with incorrect asset_hdr_id.sql
Client:			ModSpace
Case#:			40320
Client ref:		
AAF Version:	4.41.03
Database Type:	SQL Server
Author:			Yoshiki Okawa
Owner:			Yoshiki Okawa
Site Owner:		Jason Depew / Mark McFadden
Summary:		Deletes incorrect duplicate tax_calc_flow for incorrect asset hdr.
History:		Date			Author				Action
				8-Mar-2013		Yoshiki Okawa		Created
				30-May-2013		Yoshiki Okawa		Updated to do reversals of invoiced flows.
Revision:		$Rev: 43987 $

Instructions:
DBA:
1. Run this script in its entirity.  Do not run statements individually.
2. Make sure Q2 returns no records. Flows 1037623 and 1037624 are covered by another script for case 40131.

Business User:
3. After running this script on 4.41.03 build, open each bank flow in the result of the query Q4, and re-allocate to new current flows.
   The query Q3 lists new current flows which require re-allocation.

*/

-- Log event
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Initiated: $Id: 40320_duplicate tax_calc_flow with incorrect asset_hdr_id.sql 43987 2013-07-25 04:15:02Z Yoshiki.Okawa $', 0, 'Script' , 3 , ' ' , 0);

-- Create a table for new current flows to be inserted.
select f.flow_id, 0 original_flow_id, f.contract_id into dbo.mod_flow_inserted_40320
from flow f
inner join flow f2 on 1 = 0;

-- Create a backup table containing invalid tax_calc_flow to be deleted.
select tcf.*
into [dbo].[mod_tax_calc_flow_backup_40320]
from contract_custom_flow ccf
inner join flow f
	on f.custom_flow_hdr_id = ccf.custom_flow_hdr_id
	and f.contract_id = ccf.contract_id
	and f.custom_flow_link_no = ccf.link_no
inner join tax_calc_flow tcf
	on tcf.flow_id = f.flow_id
	and tcf.asset_hdr_id != ccf.destination_asset_hdr_id
	and tcf.asset_hdr_id not in (select asset_hdr_id from axsp_get_asset_hdr_child_tbl(ccf.destination_asset_hdr_id))
inner join asset_hdr ah on ah.asset_hdr_id = tcf.asset_hdr_id
left join asset a on a.contract_id = f.contract_id and a.asset_hdr_id in (ah.asset_hdr_id, ah.owner_id, ah.parent_owner_id)
/* too complicated...
left join contract_alteration ca on ca.contract_id = f.contract_id and ca.fld_id = 417 and ccf.contract_custom_flow_id = ca.obj_id
left join contract_restructure cr on ca.contract_restructure_id = cr.contract_restructure_id
left join note n on cr.contract_restructure_id = n.contract_restructure_id
*/
where
-- is custom flow
f.flow_type = 1010
-- is current since reversed / reversal flows can have tax_calc_flow with different asset_hdr_id than current destination asset hdr.
and f.reversal_status = 4200
-- has custom_flow_hdr_id
and f.custom_flow_hdr_id != 0
-- is active
and ccf.is_active = 1
-- is not deleted
and ccf.is_deleted = 0
-- is not restructure
and ccf.is_restructure = 0
-- does not have duplicate link no
and not exists
(
	select 0
	from contract_custom_flow ccf2
	where ccf2.contract_custom_flow_id != ccf.contract_custom_flow_id
	and ccf2.link_no = ccf.link_no
	and ccf2.contract_id = ccf.contract_id
	and ccf2.is_active = 1
	and ccf2.is_deleted = 0
)
-- correct tax_calc_flow's exist. Otherwise, asset structure is totally modified outside contract.
and exists
(
	select 0
	from tax_calc_flow tcf2
	where
	tcf2.flow_id = f.flow_id
	and
	(
		tcf2.asset_hdr_id = ccf.destination_asset_hdr_id
		or tcf2.asset_hdr_id in (select asset_hdr_id from axsp_get_asset_hdr_child_tbl(ccf.destination_asset_hdr_id))
	)
)
-- The following flows are OK because they have calc_dt < restructure date of custom flow's destination asset hdr id change.
and f.flow_id not in (3151477,3151500,2363401,3151480,3151503,3151543,3151526,3151486,3151529,3151506,3151512,2363393,3151472,3151535,3151518,2363399,3151478,
2654144,3151541,3151464,3151484,3151504,3151527,3151487,3151510,2654136,3151533,3151470,3876804,3151513,4058796,3151539,3151516,3151476,3151496,3151542,2363403,3151525,
3151502,3151488,3151468,3151471,3151514,3151474)
/* too complicated...
and
(
	n.hidden_links is null
	or
	(
		f.image_no = PARSENAME(replace(replace(n.hidden_links, ' ', '.'), ',', '.'), 1)
		and
		(
			tcf.asset_hdr_id = cast(ca.fld_value_new as int)
			or tcf.asset_hdr_id not in (select asset_hdr_id from axsp_get_asset_hdr_child_tbl(cast(ca.fld_value_new as int)))
		)
	)
)*/
and (a.asset_hdr_id is not null or tcf.asset_hdr_id = 0);

-- Delete invalid tax_calc_flow for non-invoiced flows
delete tcf
from tax_calc_flow tcf
inner join mod_tax_calc_flow_backup_40320 tcf2 on tcf.tax_calc_flow_id = tcf2.tax_calc_flow_id
inner join flow f on f.flow_id = tcf.flow_id
where f.status != 2101 and f.invoice_id = 0 and f.statement_id <= 0 and f.amt_matched = 0;

-- Backup flows which need to be reversed
select distinct f.*
into dbo.mod_flow_backup_40320
from flow f
inner join mod_tax_calc_flow_backup_40320 tcf on f.flow_id = tcf.flow_id
where f.status = 2101 or f.invoice_id != 0 or f.statement_id > 0 or f.amt_matched != 0;

-- Mark the flow as Reversed for invoiced flows
update f set 
f.reversal_status = 4201,
f.amt_matched = f2.amt_gross,
f.amt_matched_principal = f2.amt_principal,
f.amt_matched_interest = f2.amt_interest,
f.amt_matched_netted = f2.amt_gross_netted,
f.last_user_id = 3
from flow f
inner join mod_flow_backup_40320 f2 on f.flow_id = f2.flow_id;

-- Insert reversal flows
insert into flow (flow_id,contract_id,image_no,input_dt,calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,status,owner_id,beneficiary_id,nett_no,bank_account_id,is_set,amt_principal,amt_interest,installment_no,flow_method_id,payee_ref,payment_ref,party_account_id,amt_gross,reversal_status,flow_link_id,amt_matched,rate,invoice_id,amt_contingent_rental,bank_interface_run_id,custom_flow_hdr_id,rejected_reason,settlement_bank_info_id,amt_gross_netted,collection_state,late_fee_upto,is_overdue_interest_pending,last_overdue_interest_dt,statement_id,amt_rental,release_dt,settled_dt,rejected_dt,amt_matched_principal,penalty_grace_days,amt_matched_interest,amt_matched_tax,exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,last_user_id,in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,reserve_statement_id,gl_account_id,purchase_invoice_id,amt_invoice,can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,amt_matched_netted,pp_nett_no,is_funding_transfer,stamp)
select 
(select isnull(max(flow_id), 0)+1 from flow) + row_number() over (order by flow_id),
[contract_id],
[image_no],
dbo.axsp_get_datetime(),		--[input_dt]
[calc_dt],
dbo.axsp_dateonly(dbo.axsp_get_datetime()),	--[actual_dt], Set actual date to today
[expected_dt],
[currency_id],
-[amount], 					--Negate the Amount
[flow_type],
[is_cash],
2100, 							--[status], Set Status to Pending
[owner_id],
[beneficiary_id],
0,								--[nett_no],
[bank_account_id],
[is_set],
-[amt_principal], 				--Negate AmtPrincial
-[amt_interest],				--Negate Amt Interest
[installment_no],
0,								--[flow_method_id],
[payee_ref],
[payment_ref],
[party_account_id],
-[amt_gross],					--Negate AmtGross
4202,							--[reversal_status], Set Reversal Status to Reversal
flow_id,						--[flow_link_id],
-[amt_gross],					--[amt_matched], --Set Amount Matched to Amt gross
[rate],
0,								--[invoice_id],
-[amt_contingent_rental], 		--Negate AmtContingentRental
0,								--[bank_interface_run_id],
[custom_flow_hdr_id],
[rejected_reason],
0,								--[settlement_bank_info_id],
-[amt_gross_netted],			--Negate amt_gross_netted 
14800,							--[collection_state],
0,								--[late_fee_upto],
0,								--[is_overdue_interest_pending],
'01-Jan-1900',					--[last_overdue_interest_dt],
0,								--[statement_id],
[amt_rental],
[release_dt],
'01-Jan-1900',					--[settled_dt],
[rejected_dt],
-[amt_principal],				--[amt_matched_principal],
[penalty_grace_days],
-[amt_interest],				--[amt_matched_interest],
-([amt_gross]-[amount]),		--[amt_matched_tax],
[exclude_from_account_bal],
[settle_count],
[exclude_from_late_fees],
[exclude_from_overdue_interest],
[split_no],
3,								--[last_user_id], Set to system user
[in_recovery],
[split_flow_variation_id],
[gross_rec_amt],
[is_shadow_copy],
[grp_link_id],
[reserve_id],
0,								--[reserve_statement_id],
[gl_account_id],
[purchase_invoice_id],
- [amt_gross],					--[amt_invoice],
[can_process],
[custom_flow_link_no],
[leg_no],	
[asset_hdr_id],	
[tax_point_dt],
[is_first_settlement_bank_info],
[termination_quote_id],
[payment_confirmation_status],
[payment_confirmation_dt],
[payment_confirmation_user_id],
[payment_credited_dt],
-[amt_gross_netted],			--[amt_matched_netted],
[pp_nett_no],
[is_funding_transfer],
[stamp] from mod_flow_backup_40320;

-- Insert tax_flow for reversals
insert into tax_flow(amount,amt_financed,amt_invoice,amt_matched,contract_id,flow_id,image_no,stamp,tax_authority_id,tax_rate,tax_type_hdr_id)
select	
-tf.amount,			--Negate amount
-tf.amt_matched,		--Negate amt_matchd
-tf.amt_invoice,		--Negate amt_invoice
-tf.amt_financed,		--Negate amt_financed
tf.contract_id,
f2.flow_id,
tf.image_no,
tf.stamp,
tf.tax_authority_id,
tf.tax_rate,
tf.tax_type_hdr_id
from tax_flow tf
inner join mod_flow_backup_40320 f on tf.flow_id = f.flow_id
inner join flow f2 on f2.flow_link_id = f.flow_id;

-- Insert tax_calc_flow for reversals
insert into tax_calc_flow(flow_id, asset_hdr_id, contract_id, tax_authority_id, tax_type_id, amount, amt_tax, rate, stamp)
select
f2.flow_id,
tcf.asset_hdr_id,
tcf.contract_id,
tcf.tax_authority_id,
tcf.tax_type_id,
-tcf.amount,
-tcf.amt_tax,
tcf.rate,
tcf.stamp
from tax_calc_flow tcf
inner join mod_flow_backup_40320 f on tcf.flow_id = f.flow_id
inner join flow f2 on f2.flow_link_id = f.flow_id;

-- Insert current flows based on the reversed flows
merge flow
USING
(
	select 
	(select isnull(max(flow_id), 0)+1 from flow) + row_number() over (order by flow_id) flow_id, -- flow_id
	contract_id,
	image_no + 1 image_no, -- image_no + 1
	dbo.axsp_get_datetime() input_dt, -- input_dt = now
	calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,
	2100 status, -- status = Pending
	owner_id,beneficiary_id,
	0 nett_no, -- nett_no = 0
	bank_account_id,is_set,amt_principal,amt_interest,installment_no,
	0 flow_method_id, -- flow_method_id = 0
	payee_ref,payment_ref,party_account_id,amt_gross,
	4200 reversal_status, -- reversal_status = Current
	0 flow_link_id, -- flow_link_id = 0
	0 amt_matched, -- amt_matched = 0
	rate,
	0 invoice_id, -- invoice_id = 0
	amt_contingent_rental,
	0 bank_interface_run_id, -- bank_interface_run_id = 0
	custom_flow_hdr_id,rejected_reason,
	0 settlement_bank_info_id, -- settlement_bank_info_id = 0
	amt_gross_netted,
	14800 collection_state, -- collection_state = None
	0 late_fee_upto, -- late_fee_upto = 0
	0 is_overdue_interest_pending, -- is_overdue_interest_pending = 0
	'1900-01-01' last_overdue_interest_dt, -- last_overdue_interest_dt = '1900-01-01'
	0 statement_id, -- statement_id = 0
	amt_rental,release_dt,
	'1900-01-01' settled_dt, -- settled_dt = '1900-01-01'
	rejected_dt,amt_matched_principal,penalty_grace_days,
	0 amt_matched_interest, -- amt_matched_interest = 0
	0 amt_matched_tax, -- amt_matched_tax = 0
	exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,
	3 last_user_id, -- last_user_id = System
	in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,
	0 reserve_statement_id, -- reserve_statement_id = 0
	gl_account_id,purchase_invoice_id,
	amt_gross amt_invoice, -- amt_invoice = amt_gross
	can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,
	0 amt_matched_netted, -- amt_matched_netted = 0
	pp_nett_no,is_funding_transfer,stamp,
	flow_id original_flow_id
	from mod_flow_backup_40320 f
	where f.status = 2101 or f.invoice_id != 0 or f.statement_id > 0 or f.amt_Matched != 0
) AS bu on 1 = 0
WHEN NOT MATCHED THEN
	INSERT (flow_id,contract_id,image_no,input_dt,calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,status,owner_id,beneficiary_id,nett_no,bank_account_id,is_set,amt_principal,amt_interest,installment_no,flow_method_id,payee_ref,payment_ref,party_account_id,amt_gross,reversal_status,flow_link_id,amt_matched,rate,invoice_id,amt_contingent_rental,bank_interface_run_id,custom_flow_hdr_id,rejected_reason,settlement_bank_info_id,amt_gross_netted,collection_state,late_fee_upto,is_overdue_interest_pending,last_overdue_interest_dt,statement_id,amt_rental,release_dt,settled_dt,rejected_dt,amt_matched_principal,penalty_grace_days,amt_matched_interest,amt_matched_tax,exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,last_user_id,in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,reserve_statement_id,gl_account_id,purchase_invoice_id,amt_invoice,can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,amt_matched_netted,pp_nett_no,is_funding_transfer,stamp)
	VALUES (flow_id,contract_id,image_no,input_dt,calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,status,owner_id,beneficiary_id,nett_no,bank_account_id,is_set,amt_principal,amt_interest,installment_no,flow_method_id,payee_ref,payment_ref,party_account_id,amt_gross,reversal_status,flow_link_id,amt_matched,rate,invoice_id,amt_contingent_rental,bank_interface_run_id,custom_flow_hdr_id,rejected_reason,settlement_bank_info_id,amt_gross_netted,collection_state,late_fee_upto,is_overdue_interest_pending,last_overdue_interest_dt,statement_id,amt_rental,release_dt,settled_dt,rejected_dt,amt_matched_principal,penalty_grace_days,amt_matched_interest,amt_matched_tax,exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,last_user_id,in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,reserve_statement_id,gl_account_id,purchase_invoice_id,amt_invoice,can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,amt_matched_netted,pp_nett_no,is_funding_transfer,stamp)
output inserted.flow_id, bu.original_flow_id, inserted.contract_id
into dbo.mod_flow_inserted_40320;

-- Populate tax_calc_flow
insert into tax_calc_flow(flow_id, asset_hdr_id, contract_id, tax_authority_id, tax_type_id, amount, amt_tax, rate, stamp)
select f.flow_id, tcf.asset_hdr_id, tcf.contract_id, tcf.tax_authority_id, tcf.tax_type_id, tcf.amount, tcf.amt_tax, tcf.rate, tcf.stamp
from tax_calc_flow tcf
inner join mod_flow_inserted_40320 f on f.original_flow_id = tcf.flow_id
where tcf.tax_calc_flow_id not in (select tax_calc_flow_id from mod_tax_calc_flow_backup_40320); -- Exclude ones which should be deleted

-- Populate tax_flow based on tax_calc_flow
insert into tax_flow(amount,amt_financed,amt_invoice,amt_matched,contract_id,flow_id,image_no,stamp,tax_authority_id,tax_rate,tax_type_hdr_id)
select tcf.amt_tax,tcf.amt_tax,tcf.amt_tax,tcf.amt_tax,tcf.contract_id,tcf.flow_id,0,0,tcf.tax_authority_id,round(tcf.amt_tax / tcf.amount, 4),tcf.tax_type_id
from
(
	select SUM(amt_tax) amt_tax, tcf.amount, tcf.contract_id, tcf.flow_id, tcf.tax_authority_id, tcf.tax_type_id
	from tax_calc_flow tcf
	inner join mod_flow_inserted_40320 f on f.flow_id = tcf.flow_id
	group by tcf.amount, tcf.contract_id, tcf.flow_id, tcf.tax_authority_id, tcf.tax_type_id
) tcf;

-- Correct flow.amt_gross
update f
set f.amt_gross = f.amount + (select SUM(amt_tax) from tax_calc_flow tcf where tcf.flow_id = f.flow_id),
f.amt_gross_netted = f.amount + (select SUM(amt_tax) from tax_calc_flow tcf where tcf.flow_id = f.flow_id)
from flow f
inner join mod_flow_backup_40320 f2 on f.flow_id = f2.flow_id
where f.reversal_status = 4200;

update f
set f.amt_gross = f.amount + (select SUM(amt_tax) from tax_calc_flow tcf where tcf.flow_id = f.flow_id),
f.amt_gross_netted = f.amount + (select SUM(amt_tax) from tax_calc_flow tcf where tcf.flow_id = f.flow_id)
from flow f
inner join mod_flow_inserted_40320 f2 on f.flow_id = f2.flow_id;

-- Backup contract
select contract_id, save_status
into dbo.mod_contract_backup_40320
from contract
where contract_id in (select contract_id from mod_tax_calc_flow_backup_40320);

-- Reset save status on contracts to Pending GLs
update contract set save_status = 2201 where contract_id in
(select contract_id from mod_contract_backup_40320);

-- Update the next_no table with the most recent flow_id+1
update next_no set next_no = (select max(flow_id)+1 from flow)
where table_name = 'flow';

-- Backup bank_flow_match to be deleted
select *
into dbo.mod_bank_flow_match_backup_40320
from bank_flow_match
where flow_id in (select f.flow_id from flow f inner join mod_flow_backup_40320 f2 on f.flow_id = f2.flow_id where f.reversal_status = 4201);

-- Unallocate bankflows that were attached to these reversed flows
update bfm set is_deleted = 1, unallocated_dt = dbo.axsp_dateonly(dbo.axsp_get_datetime()), unallocated_user_id = 3
from bank_flow_match bfm
inner join mod_bank_flow_match_backup_40320 bfm2 on bfm2.bank_flow_match_id = bfm.bank_flow_match_id;

-- Q1: List of invoice numbers affected by this bug.
select i.invoice_no, sum(tcf.amt_tax) correct_total_tax
from [dbo].[mod_tax_calc_flow_backup_40320] tcf
inner join flow f on f.flow_id = tcf.flow_id
inner join invoice i on f.invoice_id = i.invoice_id and i.invoice_id != 0
group by i.invoice_no;

-- Q2: Check that all taxes on tax_calc_flow are correctly add up to tax_flow and flow's taxes.
-- i.e. this should return no records.
-- Flows 1037623 and 1037624 are covered by another script for case 40131.
select
*
from flow f
where
f.contract_id in (select contract_id from mod_tax_calc_flow_backup_40320 tcf)
and
ROUND(f.amt_gross - f.amount, 2, 0) != (select SUM(amt_tax) from tax_calc_flow tcf where tcf.flow_id = f.flow_id)
and f.reversal_status = 4200
and f.flow_id not in (1037623, 1037624);

-- Q3 : List of new flows need to be allocated.
select distinct f.flow_id, f.calc_dt, f.amount, f.contract_id, bf.cheque_no, bf.bank_flow_id, bf.batch_no, bf.invoice_no
from 
flow f
inner join mod_flow_inserted_40320 f2 on f2.flow_id = f.flow_id
inner join mod_bank_flow_match_backup_40320 bfm on f2.original_flow_id = bfm.flow_id
inner join bank_flow bf on bf.bank_flow_id = bfm.bank_flow_id;

-- Q4: List of bank flow deleted (Should be listed in unallocated bankflows view)
select distinct bf.contract_id, bf.bank_flow_id, bf.batch_no, bf.invoice_no, pa.account_no, bf.cheque_no
from bank_flow bf
inner join party_account pa on pa.party_account_id = bf.party_account_id
inner join mod_bank_flow_match_backup_40320 bfm on bfm.bank_flow_id = bf.bank_flow_id;

/*
-- Restore

set identity_insert tax_calc_flow on;

insert into tax_calc_flow(tax_calc_flow_id,amount,amt_tax,asset_hdr_id,contract_id,flow_id,rate,stamp,tax_authority_id,tax_type_id)
select distinct tcf.tax_calc_flow_id,tcf.amount,tcf.amt_tax,tcf.asset_hdr_id,tcf.contract_id,tcf.flow_id,tcf.rate,tcf.stamp,tcf.tax_authority_id,tcf.tax_type_id
from mod_tax_calc_flow_backup_40320 tcf
inner join flow f on f.flow_id = tcf.flow_id
where f.status != 2101 and f.invoice_id = 0 and f.statement_id <= 0 and f.amt_matched = 0;

set identity_insert tax_calc_flow off;

update f set 
f.reversal_status = 4200,
f.amt_matched = f2.amt_matched,
f.amt_matched_principal = f2.amt_matched_principal,
f.amt_matched_interest = f2.amt_matched_interest,
f.amt_matched_netted = f2.amt_matched_netted,
f.amt_gross = f2.amt_gross,
f.amt_gross_netted = f2.amt_gross_netted,
f.last_user_id = f2.last_user_id
from flow f
inner join mod_flow_backup_40320 f2 on f.flow_id = f2.flow_id;

delete from tax_flow
where flow_id in (select flow_id from flow where flow_link_id in (select flow_id from mod_flow_backup_40320) and reversal_status = 4202);

delete from tax_calc_flow
where flow_id in (select flow_id from flow where flow_link_id in (select flow_id from mod_flow_backup_40320) and reversal_status = 4202);

delete from flow
where flow_link_id in (select flow_id from mod_flow_backup_40320) and reversal_status = 4202;

delete from tax_flow
where flow_id in (select flow_id from mod_flow_inserted_40320);

delete from tax_calc_flow
where flow_id in (select flow_id from mod_flow_inserted_40320);

delete from flow
where flow_id in (select flow_id from mod_flow_inserted_40320);

-- Reset save status on contracts to Pending GLs
update c
set c.save_status = c2.save_status
from contract c
inner join mod_contract_backup_40320 c2 on c.contract_id = c2.contract_id;

-- Update the next_no table with the most recent flow_id+1
update next_no set next_no = (select max(flow_id)+1 from flow)
where table_name = 'flow';

update bfm set is_deleted = 0, unallocated_dt = '1900-01-01', unallocated_user_id = 0
from bank_flow_match bfm
inner join mod_bank_flow_match_backup_40320 bfm2 on bfm2.bank_flow_match_id = bfm.bank_flow_match_id;

-- Drop backup tables (Run only after R1)
drop table mod_tax_calc_flow_backup_40320;
drop table mod_flow_backup_40320;
drop table mod_flow_inserted_40320;
drop table mod_contract_backup_40320;
drop table mod_bank_flow_match_backup_40320;

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted: $Id: 40320_duplicate tax_calc_flow with incorrect asset_hdr_id.sql 43987 2013-07-25 04:15:02Z Yoshiki.Okawa $', 0, 'Script' , 3 , ' ' , 0);

*/