/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:	43429_fix_missing_tax_calc_flow_for_security_deposit.sql
Client:			ModSpace
Case#:			43429/42318
Client ref:		N/A
AAF Version:	4.41
Database Type:	SQL Server
Author:			Yoshiki Okawa
Owner:			Yoshiki Okawa
Site Owner:		Jason Depew
Revision:		$Rev: 42987 $
Summary:		This script reverses invoiced security deposit flows for re-invoicing with correct taxes.
				
History:		Date			Author				Action
				08-Jul-2013		Yoshiki OKawa		Created

Instructions:
DBA:
1.	This script should be run once only using 4.41.03 or later build.
	Script should be run in its entirity.  Do not run statements individually.
	
2.	Send results of Q1 and Q2 to Business User.

Business User:
3.	Open each contract listed in Q1, open flow schedule, confirm taxes are populated, and save the contract.

4.	If Q2 returned any record, please open each bank flow affected and re-allocate to new current flow created by this script.

Note: As of Jun 20, only 2 contracts 1024732 and 1043735 are affected and no bank flows are allocated against security deposits.

*/

-- Insert event log
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Ran script 42318_fix_missing_tax_calc_flow_for_security_deposit.sql ($Rev: 42987 $)', 0, 'Script' , 3 , ' ' , 0);

-- Create a table for new current flows to be inserted.
select f.flow_id, 0 original_flow_id, f.contract_id into dbo.mod_flow_inserted_42318
from flow f
where 1 = 0;

-- Backup flows which need to be reversed
select distinct f.*
into dbo.mod_flow_backup_42318
from flow f
where flow_type = 1014 -- Security Deposit
and reversal_status = 4200
and invoice_id != 0
and not exists
(
	select 0
	from tax_calc_flow tcf
	where tcf.flow_id = f.flow_id
);

-- Mark the flow as Reversed for invoiced flows
update f set 
f.reversal_status = 4201,
f.amt_matched = f2.amt_gross,
f.amt_matched_principal = f2.amt_principal,
f.amt_matched_interest = f2.amt_interest,
f.amt_matched_netted = f2.amt_gross_netted,
f.last_user_id = 3
from flow f
inner join mod_flow_backup_42318 f2 on f.flow_id = f2.flow_id;

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
[stamp] from mod_flow_backup_42318;

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
	from mod_flow_backup_42318 f
) AS bu on 1 = 0
WHEN NOT MATCHED THEN
	INSERT (flow_id,contract_id,image_no,input_dt,calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,status,owner_id,beneficiary_id,nett_no,bank_account_id,is_set,amt_principal,amt_interest,installment_no,flow_method_id,payee_ref,payment_ref,party_account_id,amt_gross,reversal_status,flow_link_id,amt_matched,rate,invoice_id,amt_contingent_rental,bank_interface_run_id,custom_flow_hdr_id,rejected_reason,settlement_bank_info_id,amt_gross_netted,collection_state,late_fee_upto,is_overdue_interest_pending,last_overdue_interest_dt,statement_id,amt_rental,release_dt,settled_dt,rejected_dt,amt_matched_principal,penalty_grace_days,amt_matched_interest,amt_matched_tax,exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,last_user_id,in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,reserve_statement_id,gl_account_id,purchase_invoice_id,amt_invoice,can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,amt_matched_netted,pp_nett_no,is_funding_transfer,stamp)
	VALUES (flow_id,contract_id,image_no,input_dt,calc_dt,actual_dt,expected_dt,currency_id,amount,flow_type,is_cash,status,owner_id,beneficiary_id,nett_no,bank_account_id,is_set,amt_principal,amt_interest,installment_no,flow_method_id,payee_ref,payment_ref,party_account_id,amt_gross,reversal_status,flow_link_id,amt_matched,rate,invoice_id,amt_contingent_rental,bank_interface_run_id,custom_flow_hdr_id,rejected_reason,settlement_bank_info_id,amt_gross_netted,collection_state,late_fee_upto,is_overdue_interest_pending,last_overdue_interest_dt,statement_id,amt_rental,release_dt,settled_dt,rejected_dt,amt_matched_principal,penalty_grace_days,amt_matched_interest,amt_matched_tax,exclude_from_account_bal,settle_count,exclude_from_late_fees,exclude_from_overdue_interest,split_no,last_user_id,in_recovery,split_flow_variation_id,gross_rec_amt,is_shadow_copy,grp_link_id,reserve_id,reserve_statement_id,gl_account_id,purchase_invoice_id,amt_invoice,can_process,custom_flow_link_no,leg_no,asset_hdr_id,tax_point_dt,is_first_settlement_bank_info,termination_quote_id,payment_confirmation_status,payment_confirmation_dt,payment_confirmation_user_id,payment_credited_dt,amt_matched_netted,pp_nett_no,is_funding_transfer,stamp)
output inserted.flow_id, bu.original_flow_id, inserted.contract_id
into dbo.mod_flow_inserted_42318;

-- Backup contract
select contract_id, save_status
into dbo.mod_contract_backup_42318
from contract
where contract_id in (select contract_id from mod_flow_inserted_42318);

-- Reset save status on contracts to Pending GLs
update contract set save_status = 2201 where contract_id in
(select contract_id from mod_contract_backup_42318);

-- Update the next_no table with the most recent flow_id+1
update next_no set next_no = (select max(flow_id)+1 from flow)
where table_name = 'flow';

-- Backup bank_flow_match to be deleted
select *
into dbo.mod_bank_flow_match_backup_42318
from bank_flow_match
where flow_id in (select f.flow_id from flow f inner join mod_flow_backup_42318 f2 on f.flow_id = f2.flow_id);

-- Unallocate bankflows that were attached to these reversed flows
update bfm set is_deleted = 1, unallocated_dt = dbo.axsp_dateonly(dbo.axsp_get_datetime()), unallocated_user_id = 3
from bank_flow_match bfm
inner join mod_bank_flow_match_backup_42318 bfm2 on bfm2.bank_flow_match_id = bfm.bank_flow_match_id;

-- Q1 : List of contracts affected.
select distinct f.contract_id
from flow f
where flow_type = 1014
and reversal_status = 4200
and not exists
(
	select 0
	from tax_calc_flow tcf
	where tcf.flow_id = f.flow_id
);

-- Q2: List of bank flow deleted (Should be listed in unallocated bankflows view)
select distinct bf.contract_id, bf.bank_flow_id, bf.batch_no, bf.invoice_no, pa.account_no, bf.cheque_no
from bank_flow bf
inner join party_account pa on pa.party_account_id = bf.party_account_id
inner join mod_bank_flow_match_backup_42318 bfm on bfm.bank_flow_id = bf.bank_flow_id;

/* Restore:

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
inner join mod_flow_backup_42318 f2 on f.flow_id = f2.flow_id;

delete from flow
where flow_link_id in (select flow_id from mod_flow_backup_42318);

delete from flow
where flow_id in (select flow_id from mod_flow_inserted_42318);

-- Reset save status
update c
set c.save_status = c2.save_status
from contract c
inner join mod_contract_backup_42318 c2 on c.contract_id = c2.contract_id;

-- Update the next_no table with the most recent flow_id+1
update next_no set next_no = (select max(flow_id)+1 from flow)
where table_name = 'flow';

update bfm set is_deleted = 0, unallocated_dt = '1900-01-01', unallocated_user_id = 0
from bank_flow_match bfm
inner join mod_bank_flow_match_backup_42318 bfm2 on bfm2.bank_flow_match_id = bfm.bank_flow_match_id;

-- Drop backup tables
drop table mod_flow_backup_42318;
drop table mod_flow_inserted_42318;
drop table mod_contract_backup_42318;
drop table mod_bank_flow_match_backup_42318;

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted script 42318_fix_missing_tax_calc_flow_for_security_deposit.sql ($Rev: 42987 $)', 0, 'Script' , 3 , ' ' , 0);

*/