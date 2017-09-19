<Query Kind="SQL" />

/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:    42425_release_incorrectly_allocated_loan_account_flows.sql
Client:         HSBC UK
Case#:			Case 42425
Client Ref:
AAF Version:    4.40.17
Revision:       $Rev: 42649 $
Database Type:  Oracle
Author:         Lyle Henkeman
Summary:        This script releases loan account flows for allocation where those flows are incorrectly allocated.

History:        Date            Author              Action
                02-Jul-2013     Yoshiki Okawa       Created
				
Instructions:

DBA:
1.  Script should be run once in its entirity.  Do not run statements individually.

2.  Send lists of contracts and flows affected returned by step 5 and 6 to business user. Both queries should return 5 contracts.

Business User:
3.  Open unallocated bank flow(s) for each contract listed in table returned by step 5 if there is any.
    If there is no unallocated bank flow, please wait for another bank flow or unallocate existing bank_flow and reallocate flows released.

4.  Allocate flows using table returned by step 6.

*/

-- 0. Log event
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', axsp_get_datetime(), 'Ran script 42425_release_incorrectly_allocated_loan_account_flows.sql ($Rev: 42649 $)', 0, 'Script' , 3 , ' ' , 0);

-- 1. Create a backup of flows to be updated.
create table c42425_flow_backup as
select f.*
from flow f
inner join contract c on c.contract_id = f.contract_id
where f.reversal_status = 4200
and f.flow_type != 1034 -- Not balance write off
and f.amt_matched != 0
and f.contract_id > 0
and f.is_cash = 1
and c.product_style = 2011 -- Only loan accounts
and f.nett_no = 0
and not exists
(select 0 from bank_flow_match bfm where bfm.flow_id = f.flow_id and is_deleted = 0);

-- 2. Create a backup of tax flows to be updated.
create table c42425_tax_flow_backup as
select *
from tax_flow
where flow_id in (select flow_id from c42425_flow_backup);

-- 3. Unmatch flows
update flow
set amt_matched = 0,
amt_matched_principal = 0,
amt_matched_interest = 0,
amt_matched_netted = 0,
amt_matched_tax = 0
where flow_id in (select flow_id from c42425_flow_backup);

-- 4. Unmatch tax flows
update tax_flow
set amt_matched = 0
where tax_flow_id in (select tax_flow_id from c42425_tax_flow_backup);

-- 5. List affected contracts
select distinct contract_id
from c42425_flow_backup
order by contract_id;

-- 6. List affected flows
select contract_id, flow_id, calc_dt, amount, amt_gross, l1.value flow_type, pa.account_no, p.party_no, i.invoice_no
from c42425_flow_backup bu
inner join lookupset l1 on l1.lookupset_id = bu.flow_type
inner join party_account pa on pa.party_account_id = bu.party_account_id
inner join party p on p.party_id = pa.party_id
inner join invoice i on i.invoice_id = bu.invoice_id
order by contract_id, calc_dt, amount;

/* Restore

update flow f
set (amt_matched, amt_matched_principal, amt_matched_interest, amt_matched_netted, amt_matched_tax) =
(select amt_matched, amt_matched_principal, amt_matched_interest, amt_matched_netted, amt_matched_tax from c42425_flow_backup bu where f.flow_id = bu.flow_id)
where flow_id in (select flow_id from c42425_flow_backup);

update tax_flow tf
set amt_matched = (select amt_matched from c42425_tax_flow_backup bu where tf.tax_flow_id = bu.tax_flow_id)
where tax_flow_id in (select tax_flow_id from c42425_tax_flow_backup);

drop table c42425_flow_backup;
drop table c42425_tax_flow_backup;

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', axsp_get_datetime(), 'Reverted script 42425_release_incorrectly_allocated_loan_account_flows.sql ($Rev: 42649 $)', 0, 'Script' , 3 , ' ' , 0);

*/