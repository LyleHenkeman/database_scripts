-- Script Name:         Case 31003
-- Client:              HSBC
-- Case#:               31003
-- 42 Version:          4.40
-- Author:              Peter Borodin/Rod Hardcastle 
-- Owner:               Sarah Marsh
-- Site Owner:          Rose Harling
-- Summary:             Promise to Pay incorrect bankflow allocations arising from Case 26003 
--
-- History:           	03-May-2012     pborodin         Created.
-- 		              	03-May-2012     rharcastle       Reviewed/Modified.
-- 		              	09-May-2012     gfiler2          Reviewed/Modified (Backup scripts).
--
-- Replace every 117572 with the actual contract ID that is being fixed by the script.
--
--Q1 :  Select the affected contract and show corupt Bankflows  
select f.flow_id, f.amt_Matched, bfm.amt_matched from flow f,
(select flow_id, sum(amt_matched) amt_matched from bank_flow_match where bank_flow_id in
(select bank_flow_id from bank_flow where contract_id = 117572 and is_deleted=0)
and is_deleted=0
and is_rejected=0 group by flow_id) bfm
where f.flow_id = bfm.flow_id
and f.contract_id = 117572
and f.reversal_status = 4200
and f.amt_Matched != bfm.amt_matched;
 
--Q2 :  Select the affected contract and show corupt Bankflows recovery status true
select f.flow_id, f.amt_Matched, bfm.amt_matched from flow f,
(select flow_id, sum(amt_matched) amt_matched from bank_flow_match where bank_flow_id in
(select bank_flow_id from bank_flow where contract_id = 117572 and is_deleted=0)
and is_deleted=0 
and flow_id != recovery_flow_id 
and is_rejected=0 group by flow_id) bfm
where f.flow_id = bfm.flow_id
and f.contract_id = 117572 
and f.reversal_status != 4200
and f.amt_Matched != bfm.amt_matched;

--CR1 : Create backup tables
create table c31003_backup_01_117572
as SELECT bfm.bank_flow_match_id, bfm.is_deleted
FROM flow f, bank_flow_match bfm, bank_flow bf
WHERE f.contract_id = 117572
AND bfm.is_deleted = 0 AND f.exclude_from_account_bal = 0 AND f.flow_id = bfm.flow_id
AND bfm.flow_id != bfm.recovery_flow_id AND bf.bank_flow_id = bfm.bank_flow_id;
--
create table c31003_backup_02_117572
as SELECT bfm.bank_flow_match_id, bfm.is_deleted
FROM flow f, bank_flow_match bfm, bank_flow bf
WHERE f.contract_id = 117572
AND bfm.is_deleted = 0 AND f.exclude_from_account_bal != 0
AND f.flow_id = bfm.flow_id AND bf.bank_flow_id = bfm.bank_flow_id;
--
create table c31003_backup_03_117572
as SELECT f.flow_id, f.amt_matched,
f.amt_matched_principal, f.amt_matched_interest, f.amt_matched_tax, f.status
FROM flow f
WHERE f.contract_id = 117572 AND f.is_cash = 1 AND f.reversal_status = 4200;
--END CR154

--U1 :  Unallocate in recovery and P2P flows and correct matched amount on flow for Contract.
BEGIN
-- A: Mark as deleted bank flow matches allocated against flows that are in_recovery
  UPDATE bank_flow_match SET is_deleted = 1
  WHERE bank_flow_match_id IN (SELECT bfm.bank_flow_match_id
                              FROM flow f, bank_flow_match bfm, bank_flow bf
                              WHERE f.contract_id = 117572
                              AND bfm.is_deleted = 0
                              AND f.exclude_from_account_bal = 0
                              AND f.flow_id = bfm.flow_id
                              AND bfm.flow_id != bfm.recovery_flow_id
                              AND bf.bank_flow_id = bfm.bank_flow_id);
--
-- B: Mark as deleted bank flow matches allocated against flows that are Collection Recovery flows (Promise to Pay)
  UPDATE bank_flow_match SET is_deleted = 1
  WHERE bank_flow_match_id IN (SELECT bfm.bank_flow_match_id
                              FROM flow f, bank_flow_match bfm, bank_flow bf
                              WHERE f.contract_id = 117572
                              AND bfm.is_deleted = 0
                              AND f.exclude_from_account_bal != 0
                              AND f.flow_id = bfm.flow_id
                              AND bf.bank_flow_id = bfm.bank_flow_id);
--							  
  -- C : Update matched amounts on flows using bfm's as source
  UPDATE flow SET
  amt_matched = Nvl((SELECT Sum(amt_matched) FROM bank_flow_match bfm WHERE bfm.is_deleted = 0 AND flow.flow_id = bfm.flow_id),0),
  amt_matched_principal = Nvl((SELECT Sum(amt_matched_principal) FROM bank_flow_match bfm WHERE bfm.is_deleted = 0 AND flow.flow_id = bfm.flow_id),0),
  amt_matched_interest = Nvl((SELECT Sum(amt_matched_interest) FROM bank_flow_match bfm WHERE bfm.is_deleted = 0 AND flow.flow_id = bfm.flow_id),0),
  amt_matched_tax = Nvl((SELECT Sum(amt_matched_tax) FROM bank_flow_match bfm WHERE bfm.is_deleted = 0 AND flow.flow_id = bfm.flow_id),0)
  WHERE contract_id = 117572 AND is_cash = 1 AND reversal_status = 4200;
  
  --D:  Update flow status
  UPDATE flow SET
  status = CASE WHEN amount = amt_matched THEN 2101 ELSE 2100 END
  WHERE contract_id = 117572 AND is_cash = 1 AND reversal_status = 4200;
 --
 --Commit
  COMMIT;
END;

--R1 : Revert data to pre update script.
begin
--Revert update A
merge into bank_flow_match bfm
  using (select * from c31003_backup_01_117572) bu
  on    (bfm.bank_flow_match_id = bu.bank_flow_match_id) 
  when 	matched then update set bfm.is_deleted = bu.is_deleted;
--Revert update B
merge into bank_flow_match bfm
  using (select * from c31003_backup_02_117572) bu
  on    (bfm.bank_flow_match_id = bu.bank_flow_match_id) 
  when 	matched then update set bfm.is_deleted = bu.is_deleted;
--Revert update C and D (Note restore can be done as one step since we are reverting to orignal value, which is held in c31003_backup_03)
merge into flow f
  using (select * from c31003_backup_03_117572) bu
  on    (f.flow_id = bu.flow_id) 
  when 	matched then update set f.amt_matched = bu.amt_matched, f.amt_matched_principal = bu.amt_matched_principal, 
  f.amt_matched_interest = bu.amt_matched_interest, f.amt_matched_tax = bu.amt_matched_tax, f.status = bu.status;
--Commit
commit;
end;
--END R1

--DBU1 : Drop backup tables
drop table c31003_backup_01_117572;
drop table c31003_backup_02_117572;
drop table c31003_backup_03_117572;
--END DBU1