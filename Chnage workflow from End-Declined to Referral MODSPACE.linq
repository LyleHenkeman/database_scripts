<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

/* SunGard : Ambit Asset Finance : Database script : Copyright 2013

Script Name:    43110_Fix_credit_state_for_contract_244024.sql
Client:         Modspace
Case#:			Case 45994
Client Ref:		N/A
AAF Version:    4.41.03 / 4.41.04
Revision:		$Rev: 45441 $
Database Type:  SQL Server
Author:         Alven Lee
Summary:        The credit state for contract 244024 was moved to 'End Declined' by mistake preventing the deletion/activation of current restructure.
				It is a final state and the user cannot move it back to a prior state. 
				This script will change the credit state back to Decision Pending. 

History:    	Date      		Author       		Action
				27-Aug-2013		Alven Lee			Created
				04-Nov-2013		Lyle Henkeman		Modified - New contract
Instructions:

Script should be run in its entirity.  Do not run statements individually.

It MUST be run once.

*/

-- 0. Log event
INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Run', dbo.axsp_get_datetime(), 'Initiated: $Id: 45994_Fix_credit_state_for_contract_260287.sql 45441 2013-08-27 00:37:34Z Lyle.Henkeman $', 0, 'Script' , 3 , ' ' , 0);

-- 1. Backup tables
/* This is the statement that creates the first backup table */
SELECT	contract_id, 
		credit_state, 
		approval_status1, 
		approval_status2, 
		approver1_id, 
		approver2_id
INTO dbo.mod_43110_contract_backup
FROM contract c
WHERE contract_id = 260287;

SELECT * into dbo.mod_45994_event_queue_backup
FROM event_queue
WHERE contract_id = 260287 AND event_type = 24903 AND event_state_before in (12020, 12040); -- Decision Pending & Declined

-- 2. Update credit state on contract. Set credit state back to Decision Pending, and approval status to Pending
UPDATE contract set credit_state = 12020, approval_status1 = 6500, approval_status2 = 6500, approver1_id = 0, approver2_id = 0
FROM contract c
INNER JOIN mod_45994_contract_backup bu ON bu.contract_id = c.contract_id;

--Remove from event queue all the credit state transition from Decision Pending onwards. 
DELETE FROM event_queue  
WHERE event_queue_id IN (SELECT event_queue_id from mod_45994_event_queue_backup);

-- 3. Restore backups
/*
UPDATE contract SET credit_state = bu.credit_state, approval_status1 = bu.approval_status1, 
approval_status2 = bu.approval_status2, approver1_id = bu.approver1_id, approver2_id = bu.approver2_id
FROM contract c
INNER JOIN mod_45994_contract_backup bu ON bu.contract_id = c.contract_id;

BEGIN
SET IDENTITY_INSERT event_queue ON;
INSERT INTO event_queue 
			(event_queue_id
			,input_dt
			,user_id
			,event_type
			,party_id
			,contract_id
			,purchase_invoice_id
			,event_state_before
			,event_state_after
			,doc_gen_run_id
			,time_in_state_secs
			,inertia_id
			,task_id
			,termination_quote_id
			,settlement_bank_info_id
			,asset_hdr_id
			,workflow_id
			,wf_state_chain_hdr_id
			,bank_flow_batch_no
			,opportunity_id
			,stamp
			,secondary_id) 
	SELECT	event_queue_id
			,input_dt
			,user_id
			,event_type
			,party_id
			,contract_id
			,purchase_invoice_id
			,event_state_before
			,event_state_after
			,doc_gen_run_id
			,time_in_state_secs
			,inertia_id
			,task_id
			,termination_quote_id
			,settlement_bank_info_id
			,asset_hdr_id
			,workflow_id
			,wf_state_chain_hdr_id
			,bank_flow_batch_no
			,opportunity_id
			,stamp
			,secondary_id FROM mod_45994_event_queue_backup;
SET IDENTITY_INSERT event_queue OFF;
END

DROP TABLE dbo.mod_45994_contract_backup;
DROP TABLE dbo.mod_45994_event_queue_backup;

INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, stamp)
VALUES ('Script Revert', dbo.axsp_get_datetime(), 'Reverted: $Id: 45994_Fix_credit_state_for_contract_260287.sql 45441 2013-08-27 00:37:34Z Lyle.Henkeman $', 0, 'Script' , 3 , ' ' , 0);
*/