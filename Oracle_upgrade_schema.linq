<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>


--REM ====== Global Packages ======
--REM
--REM Message : Created Package : globalPkg
CREATE OR REPLACE PACKAGE globalPkg AUTHID CURRENT_USER AS
/* The following are T/SQL specific global variables. */
  identity  INTEGER;
  trancount INTEGER := 0;
  TYPE RCT1 IS REF CURSOR;/*new weak cursor definition*/
  PROCEDURE incTrancount;
  PROCEDURE decTrancount;
  function lastIdentity return integer;
END globalPkg;
/
--REM
--REM Message : Created Package : utilities
--REM User :
CREATE OR REPLACE PACKAGE utilities AS
  DebugFile   VARCHAR2(20) DEFAULT 'trace.log';
  /* The following variable DebugDir should be edited to
     DEFAULT to a valid UTL_FILE_DIR entry within the
     destination databases init.ora initialization file. */
  DebugDir    VARCHAR2(50); /* DEFAULT ''; */
  DebugOut    INTEGER DEFAULT 3;
  PROCEDURE DEBUG(debug_statement VARCHAR2);
  PROCEDURE DEBUG_TO_TABLE(debug_statement VARCHAR2);
  PROCEDURE DEBUG_TO_DBMS(debug_statement VARCHAR2);
  PROCEDURE DEBUG_TO_FILE(debug_statement VARCHAR2);
  PROCEDURE RESET_DEBUG_TABLE;
  PROCEDURE RESET_DEBUG_FILE;
  FUNCTION HEX (n pls_integer)
    RETURN VARCHAR2;
  FUNCTION MDY (month_in pls_integer,
                day_in   pls_integer,
                year_in  pls_integer)
    RETURN DATE;
  FUNCTION WEEKDAY(date_in DATE)
    RETURN INTEGER;
END utilities;
/
--REM
--REM Message : Created Package : AXSP_JARO_WINKLER
CREATE OR REPLACE PACKAGE AXSP_JARO_WINKLER AS
  function jwrun (x varchar2, y varchar2, p_pref_length number default 4) return number;
END AXSP_JARO_WINKLER;
/
CREATE OR REPLACE PACKAGE BODY globalPkg AS
/* This is a dummy package body added by the migration
   workbench in order to emulate T/SQL specific global variables. */
PROCEDURE incTrancount IS
BEGIN
  trancount := trancount + 1;
END incTrancount;
PROCEDURE decTrancount IS
BEGIN
  trancount := trancount - 1;
END decTrancount;
function lastIdentity return integer
is
begin
	 return identity;
end lastIdentity;
END globalPkg;
/
--REM
--REM Message : Created Procedure : UTILITIES
--REM User :
CREATE OR REPLACE PACKAGE BODY utilities AS
PROCEDURE DEBUG (debug_statement IN VARCHAR2) IS
BEGIN
  /* Call the appropriate sub procedure depending on the
     value of the utilities.DebugOut variable.
     This variable should be set within the utilities
     package header. */
  IF(debug_statement IS NULL) THEN
    RETURN;
  END IF;
  IF    (utilities.DebugOut = 1) THEN
    DEBUG_TO_FILE(debug_statement);
  ELSIF (utilities.DebugOut = 2) THEN
    DEBUG_TO_DBMS(debug_statement);
  ELSE
    DEBUG_TO_TABLE(debug_statement);
  END IF;
END DEBUG;
PROCEDURE DEBUG_TO_TABLE (debug_statement IN VARCHAR2) IS
PRAGMA AUTONOMOUS_TRANSACTION;
BEGIN
  INSERT INTO debug_table
  VALUES(SYSDATE,
         USER,
         debug_statement);
  COMMIT;
EXCEPTION
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20108,'utilities.DEBUG_TO_TABLE : Error raised when attempting to insert row into debug_table table.');
END DEBUG_TO_TABLE;
PROCEDURE DEBUG_TO_DBMS(debug_statement VARCHAR2) IS
BEGIN
  DBMS_OUTPUT.PUT_LINE(debug_statement);
EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.ENABLE(1000000);
    DBMS_OUTPUT.PUT_LINE(debug_statement);
END DEBUG_TO_DBMS;
PROCEDURE DEBUG_TO_FILE(debug_statement VARCHAR2) IS
fileID         UTL_FILE.FILE_TYPE;
BEGIN
  fileID := UTL_FILE.FOPEN(utilities.DebugDir,
                           utilities.DebugFile,
                           'a');
  UTL_FILE.PUT_LINE(fileID,
                    SYSDATE
                    || ' '
                    || USER
                    || ' '
                    || debug_statement);
  UTL_FILE.FCLOSE(fileID);
EXCEPTION
  WHEN UTL_FILE.INVALID_OPERATION THEN
    RAISE_APPLICATION_ERROR(-20100,'utilities.DEBUG_TO_FILE raised : Invalid operation.');
  WHEN UTL_FILE.INVALID_FILEHANDLE THEN
    RAISE_APPLICATION_ERROR(-20101,'utilities.DEBUG_TO_FILE raised : Invalid file handle.');
  WHEN UTL_FILE.WRITE_ERROR THEN
    RAISE_APPLICATION_ERROR(-20102,'utilities.DEBUG_TO_FILE raised : Write Error.');
  WHEN UTL_FILE.INVALID_PATH THEN
    RAISE_APPLICATION_ERROR(-20103,'utilities.DEBUG_TO_FILE raised : Invalid path.');
  WHEN UTL_FILE.INVALID_MODE THEN
    RAISE_APPLICATION_ERROR(-20104,'utilities.DEBUG_TO_FILE raised : Invalid mode.');
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20105,'utilities.DEBUG_TO_FILE raised : Unhandled Exception.');
END DEBUG_TO_FILE;
PROCEDURE RESET_DEBUG_TABLE IS
BEGIN
  DELETE FROM debug_table;
EXCEPTION
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20107,'utilities.RESET_DEBUG_TABLE : Error raised when attempting to clear the debug_table table.');
END RESET_DEBUG_TABLE;
PROCEDURE RESET_DEBUG_FILE IS
fileID         UTL_FILE.FILE_TYPE;
BEGIN
  fileID := UTL_FILE.FOPEN(utilities.DebugDir,
                           utilities.DebugFile,
                           'w');
  UTL_FILE.PUT_LINE(fileid,
                    'Log file creation :'
                    || SYSDATE);
  UTL_FILE.FCLOSE(fileID);
EXCEPTION
  WHEN UTL_FILE.INVALID_OPERATION THEN
    RAISE_APPLICATION_ERROR(-20100,'utilities.RESET_DEBUG_FILE  raised : Invalid operation.');
  WHEN UTL_FILE.INVALID_FILEHANDLE THEN
    RAISE_APPLICATION_ERROR(-20101,'utilities.RESET_DEBUG_FILE  raised : Invalid file handle.');
  WHEN UTL_FILE.WRITE_ERROR THEN
    RAISE_APPLICATION_ERROR(-20102,'utilities.RESET_DEBUG_FILE  raised : Write Error.');
  WHEN UTL_FILE.INVALID_PATH THEN
    RAISE_APPLICATION_ERROR(-20103,'utilities.RESET_DEBUG_FILE  raised : Invalid path.');
  WHEN UTL_FILE.INVALID_MODE THEN
    RAISE_APPLICATION_ERROR(-20104,'utilities.RESET_DEBUG_FILE  raised : Invalid mode.');
  WHEN OTHERS THEN
    RAISE_APPLICATION_ERROR(-20105,'utilities.RESET_DEBUG_FILE  raised : Unhandled Exception.');
END RESET_DEBUG_FILE;
FUNCTION HEX(n pls_integer)
RETURN VARCHAR2 IS
BEGIN
  IF n > 0 THEN
    RETURN HEX (TRUNC (n / 16)) || SUBSTR ('0123456789ABCDEF', MOD (n, 16) + 1, 1);
  ELSE
    RETURN NULL;
  END IF;
END HEX;
FUNCTION MDY(month_in pls_integer,
             day_in   pls_integer,
             year_in  pls_integer)
RETURN DATE IS
bad_day   EXCEPTION;
bad_month EXCEPTION;
bad_year  EXCEPTION;
BEGIN
  IF month_in < 0 OR month_in > 12 THEN
    RAISE bad_month;
  END IF;
  IF day_in < 0 OR day_in > 31 THEN
    RAISE bad_day;
  END IF;
  IF year_in < 999 THEN
    RAISE bad_year;
  END IF;
  RETURN TO_DATE(TO_CHAR(month_in)
                 || '-'
                 || TO_CHAR(day_in)
                 || '-'
                 || TO_CHAR(year_in),
                 'MM-DD-YYYY');
EXCEPTION
  WHEN bad_day THEN
    RETURN NULL;
  WHEN bad_year THEN
    RETURN NULL;
  WHEN bad_month THEN
    RETURN NULL;
END MDY;
FUNCTION WEEKDAY(date_in DATE)
RETURN INTEGER IS
BEGIN
  IF date_in IS NULL THEN
    RETURN NULL;
  END IF;
  RETURN TO_NUMBER(TO_CHAR(date_in,'D'));
END WEEKDAY;
END utilities;
/
--REM
--REM Message : Created Procedure : AXSP_JARO_WINKLER
CREATE OR REPLACE PACKAGE BODY AXSP_JARO_WINKLER AS
  type t_is_assigned is table of boolean index by pls_integer;
  v_assigned1 t_is_assigned;
  v_assigned2 t_is_assigned;
--
  function PrefLength(p integer, x varchar2, y varchar2) return integer is
    prefixLen integer;
    maxLen integer;
  begin
    maxLen := least(p, length(x));
    prefixLen := 0;
    for i in 1..maxLen loop
      prefixLen := i;
      if substr(x,i,1) <> substr(y,i,1) then
        prefixLen := prefixLen - 1;
        exit;
      end if;
    end loop;
    return prefixLen;
  end PrefLength;
--
  function CommonChars(x varchar2, y varchar2) return number is
    m integer := length(x);
    n integer := length(y);
    v_left_limit number;
    v_right_limit number;
    maxCommonDistance number;
    commonCnt number := 0;
  begin
    maxCommonDistance := floor((m / 2) - 1);
    -- Initialize the boolean arrays for each string
    for i in 1..m loop
      v_assigned1(i) := false;
    end loop;
    for i in 1..n loop
      v_assigned2(i) := false;
    end loop;
    for i in 1..m loop
      v_left_limit  := greatest(1, i - maxCommonDistance);
      v_right_limit := least(n, i + maxCommonDistance + 1);
      for j in v_left_limit..v_right_limit loop
        if substr(x,i,1) = substr(y,j,1) AND NOT v_assigned2(j) then
          -- We have found a pair of common characters
          commonCnt := commonCnt + 1;
          v_assigned1(i) := true;
          v_assigned2(j) := true;
          exit;
        end if;
      end loop;
    end loop;
    return commonCnt;
  end CommonChars;
--
  function transpositions(x varchar2, y varchar2, p_common_cnt number) return number is
    v_transpositions number := 0;
    m number := length(x);
    n number := length(y);
    i number;
    j number;
  begin
    i := 0;
    j := 0;
    for k in 1..p_common_cnt loop
      loop
        i := i + 1;
        exit when v_assigned1(i) OR i > m;
      end loop;
      loop
        j := j + 1;
        exit when v_assigned2(j) OR j > n;
      end loop;
      if (substr(x,i,1) <> substr(y,j,1)) then
        v_transpositions := v_transpositions + 1;
      end if;
    end loop;
    v_transpositions := v_transpositions/2;
    return v_transpositions;
  end transpositions;
--
  function JaroMetric(x varchar2, y varchar2) return number is
    m integer := length(x);
    n integer := length(y);
    commonCnt integer;
    v_transpositions integer;
    v_retval number;
  begin
    -- Count common elements (defined as equal elements at
    -- indexes i and j such that abs(i-j) <= maxCommonDistance)
    commonCnt := CommonChars(x, y);
    -- A special case (otherwise there will be a divide by
    -- zero later, when the Jaro string comparator is computed)
    if commonCnt = 0 then
      v_retval := 0;
      goto exit_func;
    end if;
    -- Compute transpositions
    v_transpositions := transpositions(x, y, commonCnt);
    -- Compute the Jaro string comparator
    v_retval := (((commonCnt / m) + (commonCnt / n) + ((commonCnt - v_transpositions) / commonCnt ))) / 3;
    <<exit_func>>
    return v_retval;
  end JaroMetric;
--
  function jwrun (x varchar2, y varchar2, p_pref_length number default 4) return number is
    v_retval number;
    v_jaro   number;
    v_pref_length number;
    m integer := length(x);
    n integer := length(y);
    v_string1 varchar2(1000) := lower(x);
    v_string2 varchar2(1000) := lower(y);
  begin
  --If the strings are equal, there is no need to continue.
    if v_string1 = v_string2 then
      v_retval := 1;
    else
    -- IF second string longer than first, then flip them.
      if m > n then
        v_string1 := lower(y);
        v_string2 := lower(x);
        m := length(v_string1);
        n := length(v_string2);
      end if;
      v_jaro := jarometric(v_string1,v_string2);
      v_pref_length := PrefLength(p_pref_length, v_string1, v_string2);
      v_retval := v_jaro + v_pref_length * 0.1 * (1.0 -  v_jaro);
    end if;
    return v_retval;
  end jwrun;
END AXSP_JARO_WINKLER;
/
--REM ====== Views ======

CREATE OR REPLACE FORCE VIEW axvw_asset_hdr_asset_man
AS
 SELECT hdr.asset_hdr_id, hdr.original_purchase_dt, hdr.asset_type_id, hdr.name, hdr.serial_no,
		  hdr.year_of_manufacture,hdr.asset_condition,hdr.location_type,hdr.current_contract_id,
		  axsp_get_asset_hdr_address_id(hdr.asset_hdr_id, 45100, axsp_dateonly(axsp_get_datetime())) AS address_id,hdr.maturity_dt, isnull(dep.depreciation_rate_hdr_id,0) as depreciation_rate_hdr_id,
		  dep.effective_life,
		  CASE
			WHEN (hdr.parent_owner_id = 0) THEN dep.asset_id
			ELSE 0
			END as asset_id,
		  CASE
		    WHEN (hdr.current_contract_id = 0 OR hdr.asset_status in (43040, 43050)) THEN pending_amt_base_repayment
          ELSE amt_base_repayment
          END  as amt_base_repayment,
        hdr.asset_description, hdr.num_items, hdr.hdr_Type, hdr.owner_id, hdr.parent_owner_id, hdr.asset_status,
        (case ahq.quote_status when 1 then 44000 when 2 then 44020 else 44030 end) quote_status, axsp_get_asset_hdr_address_id(hdr.asset_hdr_id, 45101, axsp_dateonly(axsp_get_datetime())) AS current_address_id, axsp_get_asset_hdr_address_id(hdr.asset_hdr_id, 45102, axsp_dateonly(axsp_get_datetime())) AS delivery_address_id,
		  hdr.stamp
 FROM asset_hdr hdr
 LEFT JOIN asset_depr dep on hdr.asset_hdr_id = dep.asset_hdr_id and hdr.current_contract_id = dep.contract_id and dep.stop_depreciation_dt =  axsp_datemax()
 LEFT JOIN (select asset_hdr_id, max(case quote_status when 44000 then 1 when 44020 then 2 else 0 end) quote_status from asset_hdr_quote group by asset_hdr_id) ahq on ahq.asset_hdr_id = hdr.asset_hdr_id
GROUP BY  hdr.asset_hdr_id, hdr.original_purchase_dt, hdr.asset_type_id, hdr.name, hdr.serial_no, hdr.year_of_manufacture, hdr.asset_condition, hdr.location_type,
                      hdr.current_contract_id, hdr.maturity_dt, depreciation_rate_hdr_id, dep.effective_life, dep.asset_id, hdr.asset_description, hdr.num_items, hdr.hdr_type,
                      hdr.owner_id, hdr.parent_owner_id, hdr.asset_status, ahq.quote_status, hdr.stamp, amt_base_repayment, pending_amt_base_repayment, hdr.po_row_id
/

CREATE OR REPLACE FORCE VIEW axvw_contract_asset_list
AS
SELECT c.contract_id, c.amt_financed, pr.name program, pd.name as product, bu.name business_unit, br.name branch,
c.reference, cd.ext_reference, c.input_dt, c.calc_dt, c.credit_state, c.contract_state, c.opportunity_id,
cast(round(c.term/30.5,0) as NUMBER) term_months, c.last_updated_dt, c.stamp, c.mature_dt1, c.is_active,
ccy.code as currency_code,
l1.value as contract_state_value, l2.value as credit_state_value,
p.ext_name as party_ext_name,p.reference party_ref, p.party_id,
x.asset_id, y.amt_base_repayment, y.cost, y.residual_value, y.amt_balloon,
x.stock_code, x.asset_description, x.asset_type_ext_name,
cd.amt_profit_measure,
NVL((select 1 from dual where exists (select 1 from task
where task_type in (6200, 6204, 6226) --Approve Contract; Approve Credit; Approve Generic Workflow
and status NOT IN (6402, 6405) -- Completed; Not Required
and approval_status != 6501 -- Approved
AND contract_id = c.contract_id
)),0) as has_outstanding_approval_task,
NVL((select sum(f.amt_gross-f.amt_matched)
	from flow f
	where f.reversal_status = 4200
	and f.is_cash = 1
	and (f.collection_state = 14801 or f.collection_state = 14802)
	and f.status not in (2099, 2104, 2105)
	and	f.is_shadow_copy = 0
	and f.expected_dt < axsp_dateonly(axsp_get_datetime())
	and f.contract_id=x.contract_id),0) as amt_overdue
FROM
contract c
inner join contract_det cd ON cd.contract_id = c.contract_id
inner join currency ccy ON c.currency_id = ccy.currency_id
inner join program pr on pr.program_id=c.program_id
inner join product pd on pd.product_id = c.product_id
inner join party bu on bu.party_id=c.business_unit_id
inner join party br on br.party_id=c.branch_id
inner join party p on p.party_id=c.cparty_id
inner join lookupset l1 on l1.lookupset_id = c.contract_state
inner join lookupset l2 on l2.lookupset_id = c.credit_state
left join (select sum(amt_base_repayment) as amt_base_repayment, sum(residual_value) as residual_value, sum(cost) as cost, SUM(amt_balloon) as amt_balloon, contract_id
		   from asset
		   group by contract_id) y on y.contract_id = c.contract_id
left join (select a1.asset_id, a1.contract_id, ah.stock_code, ah.asset_description, at.ext_name as asset_type_ext_name,
		     RANK() OVER(PARTITION BY a1.contract_id ORDER BY ah.original_purchase_price DESC, asset_id desc) my_order
	        from asset a1 inner join asset_hdr ah on a1.asset_hdr_id=ah.asset_hdr_id
	        inner join asset_type at ON at.asset_type_id = ah.asset_type_id
) x on x.contract_id=c.contract_id
where NVL(x.my_order,1)=1
/

CREATE OR REPLACE VIEW axvw_contract_find_view
AS
 SELECT contract_id,product_style,contract_grp_style,reference,product_id,business_unit_id,branch_id,location_id,currency_id,calc_dt,mature_dt1,stamp
 FROM contract where is_active = 1
/

CREATE OR REPLACE VIEW axvw_todo_hdr
AS
   SELECT h.todo_hdr_id,
          h.NAME,
          h.DESCRIPTION,
          l.VALUE state_type,
          S.NAME state_chain,
          h.is_approval_pending,
          h.is_active,
          h.is_deleted,
          h.stamp
     FROM todo_hdr h
            JOIN lookupset l
             ON h.state_type = l.lookupset_id
            JOIN workflow_state_hdr S
             ON h.workflow_state_hdr_id = S.workflow_state_hdr_id
      WHERE ( h.workflow_state_hdr_id <> 0 )
   UNION ALL
   SELECT h.todo_hdr_id,
          h.NAME,
          h.DESCRIPTION,
          t.NAME state_type,
          c.NAME state_chain,
          h.is_approval_pending,
          h.is_active,
          h.is_deleted,
          h.stamp
     FROM todo_hdr h
            JOIN wf_state_chain_hdr c
             ON h.wf_state_chain_hdr_id = c.wf_state_chain_hdr_id
            JOIN wf_type t
             ON c.wf_type_id = t.wf_type_id
      WHERE ( h.workflow_state_hdr_id = 0 )
/
CREATE OR REPLACE VIEW axvw_state_chain_hdr
AS
   SELECT hdr.workflow_state_hdr_id hdr_id,
          hdr.NAME hdr_name,
          hdr.state_type TYPE_ID,
          L.VALUE TYPE_NAME,
          hdr.is_approval_pending,
          1 is_system_state_chain,
          1 as is_42_workflow,
          1 is_active,
          0 is_deleted,
          hdr.stamp
     FROM workflow_state_hdr hdr
            JOIN lookupset L
             ON hdr.state_type = L.lookupset_id
      WHERE hdr.workflow_state_hdr_id > 0
   UNION ALL
   SELECT hdr.wf_state_chain_hdr_id hdr_id,
          hdr.NAME hdr_name,
          L.wf_type_id TYPE_ID,
          L.NAME TYPE_NAME,
          hdr.is_approval_pending,
          0 is_system_state_chain,
          hdr.is_42_workflow as is_42_workflow,
          hdr.is_active is_active,
          hdr.is_deleted is_deleted,
          hdr.stamp
     FROM wf_state_chain_hdr hdr
            JOIN wf_type L
             ON hdr.wf_type_id = L.wf_type_id
/
CREATE OR REPLACE FORCE VIEW axvw_get_datetime
AS
  SELECT SYSDATE AS server_dt FROM dual
/
CREATE OR REPLACE FORCE VIEW axvw_party_ids_all
AS
SELECT    party_id
FROM   	 party
WHERE     (is_deleted = 0) AND (is_active = 1)
AND rownum <= 3000
ORDER BY party_id
/
CREATE OR REPLACE FORCE VIEW  axvw_asset_type_xml_view
AS
SELECT at.asset_type_id, at.name, at.code, owner_asset_type.name AS owner_name 
FROM asset_type at
  left outer join asset_type owner_asset_type ON 
    owner_asset_type.asset_type_id = at.owner_id
/
CREATE OR REPLACE FORCE VIEW  axvw_rate_basis_xml_view
AS
SELECT r.rate_basis_id, /* --> rate_basis.rate_basis_id */
       r.name, /* --> rate_basis.name */
       r.rate_basis_type /* --> rate_basis.rate_basis_type */
FROM rate_basis r
/
CREATE OR REPLACE FORCE VIEW  axvw_party_xml_view
AS
SELECT
          a.party_id, /* --> party.party_id */
          a.party_no, /* --> party.party_no */
          a.name, /* --> party.name */
          a.gender, /* --> party.gender */
          a.first_names, /* --> party.first_names */
          a.middle_name, /* --> party.middle_name */
          a.date_of_birth, /* --> party.date_of_birth */
          a.drivers_license_no, /* --> party.drivers_license_no */
          a.business_ref_no, /* --> party.business_ref_no */
          a.tax_no, /* --> party.tax_no */
          a.reference, /* --> party.reference */
          a.stamp /* --> party.stamp */
FROM   	   party a
/
CREATE OR REPLACE FORCE VIEW  axvw_ledger_xml_view
AS
SELECT l.ledger_id, 
	l.name, 
	l.code, 
	bu.party_id business_unit_id
FROM ledger l, party bu
WHERE
	(bu.party_id = l.business_unit_id 
	 OR bu.party_id in (select party_id from TABLE(CAST(axsp_get_party_child_tbl(l.business_unit_id) AS PARTY_CHILD_TBLTAB))))
	AND bu.is_business_unit = 1
	AND l.ledger_id > 0
	AND l.business_unit_id > 0
	AND l.is_active = 1
/
CREATE OR REPLACE FORCE VIEW axvw_pay_term
AS
SELECT
	pth.pay_term_hdr_id,
	pth.name,
	ptd.party_id,
	ptd.pay_term_party_role,
	NVL(ptdm.party_id, 0) manufacturer_id,
	pth.asset_type_id,
	pth.principal_party_id,
	pth.program_id,
	pth.location_id,
	pth.pay_term_state,
	pth.is_active,
	pth.is_deleted,
	pth.stamp
FROM
	pay_term_hdr pth INNER JOIN
	pay_term_det ptd ON pth.pay_term_hdr_id = ptd.pay_term_hdr_id LEFT JOIN
	pay_term_det ptdm ON pth.pay_term_hdr_id=ptdm.pay_term_hdr_id AND ptdm.pay_term_party_role = 47200
WHERE
	pth.is_deleted = 0
/
CREATE OR REPLACE FORCE VIEW axvw_portfolio_view AS
select p.portfolio_id, /* --> portfolio.portfolio_id */
p.code, /* --> portfolio.code */
p.name, /* --> portfolio.name */
p.portfolio_type, /* --> portfolio.portfolio_type */
l.value portfolio_type_value, /* --> lookupset.portfolio_type_value */
p.amt_financed_formula, /* --> lookupset.value */
l2.value amt_financed_formula_value, /* --> lookupset.value */
p.start_dt, /* --> portfolio.start_dt */
p.business_unit_id, /* --> portfolio.business_unit_id */
p.stamp /* --> portfolio.stamp */
from portfolio p, lookupset l, lookupset l2
where p.portfolio_type = l.lookupset_id and
p.portfolio_id > 0 and
p.amt_financed_formula = l2.lookupset_id and
p.is_deleted = 0
/
CREATE OR REPLACE FORCE VIEW axvw_tax_type AS
SELECT 	a.tax_type_hdr_id, /* --> tax_type_hdr.tax_type_hdr_id */
	a.name, /* --> tax_type_hdr.name */
	a.code, /* --> tax_type_hdr.code */
	a.ext_name, /* --> tax_type_hdr.ext_name */
	a.is_active, /* --> tax_type_hdr.is_active */
	a.is_deleted, /* --> tax_type_hdr.is_deleted */
	a.tax_table_hdr_id, /* --> tax_type_hdr.tax_table_hdr_id */
	a.tax_purpose, /* --> tax_type_hdr.tax_purpose */
	a.reference, /* --> tax_type_hdr.reference */
	a.is_approval_pending, /* tax_type_hdr.is_approval_pending */
	a.stamp, /* --> tax_type_hdr.stamp */
	b.effect_dt, /* --> tax_type_det.effect_dt */
	(SELECT MAX(effect_dt)	FROM tax_type_det f
				WHERE f.tax_type_hdr_id = a.tax_type_hdr_id) future_effect_dt, /* --> tax_type_det.max(effect_dt) */
	(SELECT MIN(effect_dt)	FROM tax_type_det g
				WHERE g.tax_type_hdr_id = a.tax_type_hdr_id) past_effect_dt, /* --> tax_type_det.min(effect_dt) */
        b.tax_authority_id, /* --> tax_type_det.tax_authority_id */
        a.owner_tax_type_hdr_id /* --> tax_type_det.owner_tax_type_hdr_id*/
FROM 	tax_type_det b right outer join tax_type_hdr a
	on b.tax_type_hdr_id = a.tax_type_hdr_id
WHERE   a.tax_type_hdr_id > 0
        AND (b.effect_dt is null or
		b.effect_dt = nvl(
			-- First, try to match against a past detail closest to todays date
			(SELECT MAX(e.effect_dt)
		    FROM tax_type_det e WHERE e.tax_type_hdr_id = a.tax_type_hdr_id and
				e.effect_dt <= axsp_get_datetime()) ,
			-- If Null, Take into account tax type_s with only future details (Case 8207)
			(SELECT MAX(e.effect_dt)
		    FROM tax_type_det e WHERE e.tax_type_hdr_id = a.tax_type_hdr_id)))
/
CREATE OR REPLACE FORCE VIEW  axvw_mortgage_view
AS
SELECT m.mortgage_id, /* --> mortgage.mortgage_id */
       m.expiry_dt, /* --> mortgage.expiry_dt */
       l1.value mortgage_type_value, /* --> lookupset.value */
       m.amt_application, /* --> mortgage.amt_application */
       m.amt_approved, /* --> mortgage.amt_approved */
       m.approve_by_dt, /* --> mortgage.approve_by_dt */
       m.approve_dt, /* --> mortgage.approve_dt */
       m.exchange_dt, /* --> mortgage.exchange_dt */
       pr.name product, /* --> product.name */
       l2.value loan_style_value, /* --> lookupset.value */
       m.review_months, /* --> mortgage.review_months */
       m.review_dt, /* --> mortgage.review_dt */
       m.term_years, /* --> mortgage.term_years */
       m.term_months, /* --> mortgage.term_months */
       m.maturity_dt, /* --> mortgage.maturity_dt */
       l3.value installment_freq_value, /* --> lookupset.value */
       m.installments, /* --> mortgage.installments */
       l4.value installment_dt_rule_value, /* --> lookupset.value */
       m.first_installment_dt, /* --> mortgage.first_installment_dt */
       m.base_rate, /* --> mortgage.base_rate */
       m.margin_rate, /* --> mortgage.margin_rate */
       m.interest_rate, /* --> mortgage.interest_rate */
       m.offer, /* --> mortgage.offer */
       m.rate_change_notice_months, /* --> mortgage.rate_change_notice_months */
       m.amt_borrowed, /* --> mortgage.amt_borrowed */
       m.amt_custom_flows, /* --> mortgage.amt_custom_flows */
       m.cost_of_credit, /* --> mortgage.cost_of_credit */
       m.loan_value_ratio, /* --> mortgage.loan_value_ratio */
       m.repay_income_ratio, /* --> mortgage.repay_income_ratio */
       p1.name cparty, /* --> party.name */
       p1.ext_name cparty_ext_name, /* --> party.ext_name */
       p1.first_names cparty_first_names, /* --> party.first_names */
       p1.middle_name cparty_middle_name, /* --> party.middle_name */
       m.amt_fixed_installment, /* --> mortgage.amt_fixed_installment */
       m.amt_variable_installment, /* --> mortgage.amt_variable_installment */
       m.amt_last_installment, /* --> mortgage.amt_last_installment */
       r.name base_rate_basis, /* --> rate_basis.base_rate_basis */
       p2.name business_unit, /* --> party.name */
       c.code ccy, /* --> currency.code */
       l.name location, /* --> location.name */
       t.name tax_profile_hdr, /* --> tax_profile_hdr.name */
       m.fixed_rate, /* --> mortgage.fixed_rate */
       l5.value contract_state_value, /* --> lookupset.value */
       l6.value credit_state_value, /* --> lookupset.value */
       m.stamp  /* --> mortgage.mortgage_id */
FROM   mortgage m,
       lookupset l1,
       lookupset l2,
       lookupset l3,
       lookupset l4,
       lookupset l5,
       lookupset l6,
       product pr,
       party p1,
       party p2,
       currency c,
       location l,
       tax_profile_hdr t,
       rate_basis r
WHERE  m.mortgage_type = l1.lookupset_id AND
       m.product_id = pr.product_id AND
       m.loan_style = l2.lookupset_id AND
       m.installment_freq = l3.lookupset_id AND
       m.installment_dt_rule = l4.lookupset_id AND
       m.cparty_id = p1.party_id AND
       m.business_unit_id = p2.party_id AND
       m.base_rate_basis_id = r.rate_basis_id AND
       m.currency_id = c.currency_id AND
       m.location_id = l.location_id AND
       m.contract_state = l5.lookupset_id AND
       m.credit_state = l6.lookupset_id AND
       m.tax_profile_hdr_id = t.tax_profile_hdr_id
/
CREATE OR REPLACE FORCE VIEW  axvw_pp_nett_flows AS
select
	f.pp_nett_no,
	f.flow_id,
	f.contract_id,
	f.flow_type,
	f.expected_dt,
	f.currency_id,
	f.amount,
	c.business_unit_id,
	c.product_id,
	f.bank_account_id,
	p.party_id,
	f.party_account_id,
	f.status,
	f.amt_matched,
	f.stamp
from
	flow f
	inner join contract c on (c.contract_id=f.contract_id)
	inner join party_account p on (p.party_account_id=f.party_account_id)
where
	f.contract_id > 0
	and f.flow_id > 0
	and f.pp_nett_no > 0
	and f.is_cash = 1
/
CREATE OR REPLACE FORCE VIEW axvw_gl_interface
AS
SELECT 	ge.gl_entry_id, /* --> gl_entry.gl_entry_id */
	ge.gl_interface_id, /* --> gl_entry.gl_interface_id */
	ge.input_dt, /* --> gl_entry.input_dt */
	axsp_dateonly(ge.post_dt) post_dt, /* --> gl_entry.post_dt */
	ge.amount, /* --> gl_entry.amount */
	ge.gl_tag_id, /* --> gl_entry.gl_tag_id */
	gt.name gl_tag_name, /* --> gl_tag.name */
	ge.currency_id, /* --> gl_entry.currency_id */
	ccy.code currency_code, /* --> currency.code */
	ge.base_amount, /* --> gl_entry.base_amount */
	ge.base_currency_id, /* --> gl_entry.base_currency_id */
	ccyb.code base_currency_code, /* --> currency.code */
	ge.fx_rate, /* --> gl_entry.fx_rate */
	ge.fx_rate_dt, /* --> gl_entry.fx_rate_dt */
	ge.ledger_id, /* --> gl_entry.ledger_id */
	lg.name ledger_name, /* --> ledger.name */
	ge.gl_account_id, /* --> gl_entry.gl_account_id */
	ga.name account_name, /* --> gl_account.account_name */
	ga.code account_code, /* --> gl_account.account_code */
	CASE WHEN ge.amount >= 0
		THEN CASE WHEN nvl(ga.xbrl_dr_ref, ' ') = ' ' THEN ga.code ELSE ga.xbrl_dr_ref END
		ELSE CASE WHEN nvl(ga.xbrl_cr_ref, ' ') = ' ' THEN ga.code ELSE ga.xbrl_cr_ref END
	END account_ref,
	ga.gl_interface_style gl_interface_style, /* --> gl_account.gl_interface_style */
	ge.narration, /* --> gl_entry.narration */
	ge.flow_id, /* gl_entry.flow_id */
	ge.flow_type, /* gl_entry.flow_type */
	l1.value flow_type_value, /* --> lookupset.value */
	l2.value account_class_value, /* --> lookupset.value */
	l3.value secondary_class_value, /* --> xt_lookupset.value */
	l4.value alternate_class_value, /* --> xt_lookupset.value */
	ge.contract_id, /* --> gl_entry.contract_id */
	c.reference contract_ref, /* --> contract.reference */
	ge.business_unit_id, /* --> gl_entry.business_unit_id */
	bu.name business_unit_name, /* --> business_unit.name */
	bu.reference business_unit_ref, /* --> business_unit.reference */
	N'C' entry_type,
	gl_entry_type, /* --> gl_entry.gl_entry_type */
	l5.value gl_entry_type_value, /* --> lookupset.value */
	ge.reversal_status, /* gl_entry.reversal_status */
	l6.value reversal_status_value, /* --> lookupset.value */
	p.code product_code, /* --> product.code */
	p.name product_name, /* --> product.name */
	l.name location_name, /* --> location.name */
	l7.value cost_centre_value, /* --> xt_lookupset.value */
	ge.branch_id, /* gl_entry.branch_id */
	b.name branch_name, /* branch.name */
	ge.gl_journal_id, /* gl_entry.gl_journal_id */
	ge.purchase_invoice_id, /* gl_entry.purchase_invoice_id */
	ge.asset_hdr_id, /* gl_entry.asset_hdr_id */
	ge.bank_flow_id, /* gl_entry.bank_flow_id */
	i.code industry_code, /* --> industry.code */
	bu.is_tax_exempt, /* --> business_unit.is_tax_exempt */
	l8.value asset_condition_value, /* --> lookupset.value */
	c.term contract_term, /* --> contract.term */
	d.party_no dealer_party_no, /* --> party.party_no */
	ge.contract_actg_cf_id, /* gl_entry.contract_actg_cf_id */
	l9.value external_class_value, /* --> xt_lookupset.value */
	ge.stamp /* --> gl_entry.stamp */
FROM 	gl_entry ge
INNER JOIN gl_tag gt ON ge.gl_tag_id = gt.gl_tag_id
INNER JOIN currency ccy ON ge.currency_id = ccy.currency_id
INNER JOIN currency ccyb ON ge.base_currency_id = ccyb.currency_id
INNER JOIN ledger lg ON ge.ledger_id = lg.ledger_id
INNER JOIN gl_account ga ON ge.gl_account_id = ga.gl_account_id
INNER JOIN lookupset l1 ON ge.flow_type = l1.lookupset_id
INNER JOIN lookupset l2 ON ga.account_class = l2.lookupset_id
INNER JOIN xt_lookupset l3 ON ga.secondary_class = l3.xt_lookupset_id
INNER JOIN xt_lookupset l4 ON ga.alternate_class = l4.xt_lookupset_id
INNER JOIN lookupset l5 ON ge.gl_entry_type = l5.lookupset_id
INNER JOIN lookupset l6 ON ge.reversal_status = l6.lookupset_id
INNER JOIN contract c ON ge.contract_id = c.contract_id
INNER JOIN party bu ON ge.business_unit_id = bu.party_id
INNER JOIN product p ON c.product_id = p.product_id
INNER JOIN location l ON c.location_id = l.location_id
INNER JOIN xt_lookupset l7 ON ge.cost_centre = l7.xt_lookupset_id
INNER JOIN party b ON ge.branch_id = b.party_id
INNER JOIN party cp ON c.cparty_id = cp.party_id
INNER JOIN industry i ON cp.industry_id = i.industry_id
INNER JOIN party d on c.dealer_id = d.party_id
INNER JOIN asset_hdr a ON ge.asset_hdr_id = a.asset_hdr_id
INNER JOIN lookupset l8 ON a.asset_condition = l8.lookupset_id
INNER JOIN xt_lookupset l9 ON ga.external_class = l9.xt_lookupset_id
WHERE ga.exclude_interface = 0
/
CREATE OR REPLACE FORCE VIEW axvw_gl_interface_platinum AS /* "system_defs_party_role_id = 123" IS FOR TOYOTA ONLY, IT WILL NEED TO BE CHANGE IF WE SELL IT TO SOME1ELSE */
SELECT	axvw_gl_interface.*, l.reference loan_officer_ref, d.reference dealer_ref  FROM axvw_gl_interface
	INNER JOIN Contract ON axvw_gl_interface.Contract_id = Contract.contract_id
	INNER JOIN Party d ON Contract.dealer_id = d.party_id
	LEFT OUTER JOIN Contract_Party ON axvw_gl_interface.Contract_id = Contract_Party.contract_id AND Contract_Party.system_defs_party_role_id = 123
	LEFT OUTER JOIN Party l ON Contract_Party.party_id = l.party_id
/
CREATE OR REPLACE FORCE VIEW  axvw_tax_profile AS
SELECT 	a.tax_profile_hdr_id, /* --> tax_profile_hdr.tax_profile_hdr_id */
	a.name, /* --> tax_profile_hdr.name */
	a.code, /* --> tax_profile_hdr.code */
	a.ext_name, /* --> tax_profile_hdr.ext_name */
	a.is_active,/* --> tax_profile_hdr.is_active */
	a.is_deleted,/* --> tax_profile_hdr.is_deleted */
	nvl(b.tax_default_id, 0) tax_default_id, /* --> tax_default.tax_default_id */
	nvl(b.location_id, 0) location_id, /* --> tax_default.location_id */
	nvl(b.business_unit_id, 0) business_unit_id,  /* --> tax_default.business_unit_id */
	nvl(b.product_id, 0) product_id, /* --> tax_default.product_id */
	nvl(b.priority, 0) priority, /* --> tax_default.priority */
	a.tax_method_id, /* --> tax_profile_hdr.tax_method_id */
	a.income_tax_type_id,/* --> tax_profile_hdr.income_tax_type_id */
	a.is_tax_interface_profile, /* --> tax_profile_hdr.is_tax_interface_profile */
	a.loc_src_physical_origin, /* --> tax_profile_hdr.loc_src_physical_origin */
	a.loc_src_admin_origin, /* --> tax_profile_hdr.loc_src_admin_origin */
	a.loc_src_dest_asset, /* --> tax_profile_hdr.loc_src_dest_asset */
	a.loc_src_dest, /* --> tax_profile_hdr.loc_src_dest */
	a.loc_src_admin_dest, /* --> loc_src_admin_dest */
	CASE WHEN ss.system_setting_value IS NULL THEN CAST(0 AS NUMBER) ELSE CAST(1 AS NUMBER) END is_default_tax_intf_profile,
	a.is_approval_pending, /* tax_type_hdr.is_approval_pending */
	a.stamp, /* --> tax_profile_hdr.stamp */
	nvl((SELECT MAX(effect_dt)	FROM tax_profile_det e
				WHERE e.tax_profile_hdr_id = a.tax_profile_hdr_id
				and e.effect_dt <= dt.server_dt), to_date('01-jan-1900','dd-mon-yyyy')) effect_dt,
	nvl((SELECT MAX(effect_dt)	FROM tax_profile_det f
				WHERE f.tax_profile_hdr_id = a.tax_profile_hdr_id), to_date('01-jan-1900','dd-mon-yyyy')) future_effect_dt,
	nvl((SELECT MIN(effect_dt)	FROM tax_profile_det g
				where g.tax_profile_hdr_id = a.tax_profile_hdr_id), to_date('01-jan-1900','dd-mon-yyyy')) past_effect_dt
FROM  axvw_get_datetime dt, tax_profile_hdr a
LEFT OUTER JOIN tax_default b
ON a.tax_profile_hdr_id = b.tax_profile_hdr_id
LEFT OUTER JOIN system_setting ss
ON a.tax_profile_hdr_id = CAST(to_number(trim(ss.system_setting_value)) AS int) AND ss.system_setting_type = 8403
WHERE a.tax_profile_hdr_id > 0
/
CREATE OR REPLACE FORCE VIEW  axvw_bankacc_view
AS
select a.bankacc_id, /* --> bankacc.bankacc_id */
a.account_owner_id, /* --> bankacc.account_owner_id */
a.bank_id, /* --> bankacc.bank_id */
a.branch_name, /* --> bankacc.branch_name */
a.branch_code, /* --> bankacc.branch_code */
a.code, /* --> bankacc.code */
a.name , /* --> bankacc.name */
a.ext_name, /* --> bankacc.ext_name */
a.currency_id, /* --> bankacc.currency_id */
c.code as currency, /* --> currency.code */
a.full_account_no, /* --> bankacc.full_account_no */
a.is_active, /* --> bankacc.is_active */
a.stamp /* --> bankacc.stamp */
from
bankacc a, currency c
where
c.currency_id = a.currency_id and
a.is_deleted = 0
/
CREATE OR REPLACE FORCE VIEW  axvw_bank_stmt_view_grouping
AS
SELECT     bs.bank_statement_det_id, SUM(DISTINCT m.bankflow_match_status + m.cashflow_match_status - 38103) flow_match_status
FROM         bank_statement_det bs LEFT OUTER JOIN
                      bank_rec_match m ON bs.bank_statement_det_id = m.bank_statement_det_id
GROUP BY bs.bank_statement_det_id
/
CREATE OR REPLACE FORCE VIEW  axvw_bank_stmt_view
AS
SELECT	bs.bank_statement_det_id,
			bsh.currency_id,
			ccy.code ccy,
			bs.transaction_dt actual_dt,
			bs.amount,
			CASE WHEN amount >= 0 THEN 5901 ELSE 5900 END flow_direction,
			bvg.flow_match_status,
			bs.contract_id,
			bs.cheque_no,
			bs.reference,
			p.name party_name,
			bs.party_account_id,
			pa.name party_account_name,
			bsh.bank_account_id,
			ba.ext_name business_unit_bank_account,
			bu.ext_name business_unit,
			(select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from note where bank_statement_det_id = bs.bank_statement_det_id and rownum <= 1) has_notes,
			(select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from bank_rec_match where bank_statement_det_id = bs.bank_statement_det_id and adjustment_contract_id != 0 and rownum <=1) has_adjustment,
			nvl((select adjustment_contract_id from bank_rec_match where bank_statement_det_id = bs.bank_statement_det_id and adjustment_contract_id != 0 and rownum <=1),0) adjustment_contract_id,
			bs.slot_1,
			bs.slot_2,
			bs.slot_3,
			bs.slot_4,
			bs.slot_5,
			bs.slot_6,
			bs.slot_7,
			bs.slot_8,
			bs.slot_9,
			bs.slot_10,
			bs.stamp
FROM		bank_statement_det bs INNER JOIN
			bank_statement_hdr bsh ON bs.bank_statement_hdr_id = bsh.bank_statement_hdr_id INNER JOIN
			currency ccy ON bsh.currency_id = ccy.currency_id INNER JOIN
			axvw_bank_stmt_view_grouping bvg ON bs.bank_statement_det_id = bvg.bank_statement_det_id INNER JOIN
			party_account pa ON bs.party_account_id = pa.party_account_id INNER JOIN
			party p ON bs.party_id = p.party_id INNER JOIN
			bankacc ba ON bsh.bank_account_id = ba.bankacc_id INNER JOIN
			party bu ON ba.account_owner_id = bu.party_id
/
CREATE OR REPLACE FORCE VIEW axvw_credit_line AS
SELECT  
 cl.party_id,
 cl.is_group,
 cl.owner_id,
 cl.name,
 cl.credit_line_id, 
 clm.name AS reservations,
 l.value As calc_type, 
 c.code ccy,
 cl.amt_limit,
 cl.amt_utilisation,
 cl.is_active,
 cl.stamp                       
FROM credit_line cl 
INNER JOIN  currency c ON cl.currency_id = c.currency_id 
LEFT OUTER JOIN  credit_line_match clm ON cl.credit_line_id = clm.credit_line_id and clm.is_active = 1 
INNER JOIN lookupset l ON cl.calc_type = l.lookupset_id 
Where cl.is_deleted = 0 
/
CREATE OR REPLACE FORCE VIEW  axvw_reconciled_bank_flows
AS
SELECT 	flow_id, /* --> bank_flow_match.flow_id */
	case when count(*)=sum(cast(is_reconciled as int)) then 1 else 0 end is_reconciled /* --> bank_flow_match.is_reconcsiled */
FROM 	bank_flow_match bfm
	inner join bank_flow bf on bf.bank_flow_id = bfm.bank_flow_id AND bf.is_shadow_copy = 0 AND bfm.is_deleted = 0
GROUP BY flow_id
/
CREATE OR REPLACE FORCE VIEW  axvw_reserve_view AS
select r.reserve_id, r.code, r.name, rt.value reserve_type_value, p.name owner_name,
ccy.code currency, r.balance, r.min_balance, r.next_review_dt, u.name manager, r.stamp
from reserve r, currency ccy, party p, lookupset rt, ax_user u
where rt.lookupset_id = r.reserve_type
and ccy.currency_id = r.currency_id
and p.party_id = r.business_unit_id
and u.ax_user_id = r.manager_id
/
CREATE OR REPLACE FORCE VIEW axvw_doc_view AS
SELECT     d.doc_id, d.doc_no, d.doc_gen_run_id, d.doc_rule_id, d.doc_output_method_id, dom.reference output_method_reference, p.ext_name party_name, p.party_id, 
                      p.party_no, d.contract_id, c.reference contract_ref, d.portfolio_id, po.name portfolio_name, d.facility_id, f.name facility_name, d.status, 
                      l1.value status_value, d.output_method, l2.value output_method_value, d.batch_no, i.invoice_no, s.statement_no, d.output_address, t.name template_fmt, t.is_encryption_enabled, 
                      d.effect_dt, d.created_dt, u1.name created_by_user, d.released_dt, u2.name released_by_user, d.output_dt, u3.name output_by_user, d.rejected_dt, 
                      u4.name rejected_by_user, d.file_name, d.file_name_copy, d.is_auto_batch, d.is_immediate_output, dr.name doc_rule_name, dr.is_print is_allow_print, 
                      dr.is_email is_allow_email, dr.is_fax is_allow_fax, dr.is_sms is_allow_sms, d.error_msg, d.context_branch_id, d.output_party_id, op.name output_party_name, d.output_party_role, 
                      l3.value output_party_role_value, d.is_copy, c.intercept_state, c.intercept_trigger_dt, d.bank_flow_batch_no, d.secondary_id, d.event_type, dp.path_value, 
                      dm.reference, dm.doc_metadata_id, d.stamp
FROM         doc_metadata dm INNER JOIN
                      doc_link dl ON dm.doc_metadata_id = dl.doc_metadata_id INNER JOIN
                      doc_provider dp ON dm.doc_provider_id = dp.doc_provider_id RIGHT OUTER JOIN
                      doc d INNER JOIN
                      party p ON d.party_id = p.party_id INNER JOIN
                      contract c ON d.contract_id = c.contract_id INNER JOIN
                      ax_user u1 ON d.created_by_user_id = u1.ax_user_id INNER JOIN
                      ax_user u2 ON d.released_by_user_id = u2.ax_user_id INNER JOIN
                      ax_user u3 ON d.output_by_user_id = u3.ax_user_id INNER JOIN
                      ax_user u4 ON d.rejected_by_user_id = u4.ax_user_id INNER JOIN
                      lookupset l1 ON d.status = l1.lookupset_id INNER JOIN
                      lookupset l2 ON d.output_method = l2.lookupset_id INNER JOIN
                      template_fmt t ON d.template_fmt_id = t.template_fmt_id INNER JOIN
                      invoice i ON d.invoice_id = i.invoice_id INNER JOIN
                      statement s ON d.statement_id = s.statement_id INNER JOIN
                      doc_rule dr ON d.doc_rule_id = dr.doc_rule_id INNER JOIN
                      portfolio po ON d.portfolio_id = po.portfolio_id INNER JOIN
                      facility f ON d.facility_id = f.facility_id INNER JOIN
                      doc_output_method dom ON d.doc_output_method_id = dom.doc_output_method_id INNER JOIN
                      party op ON d.output_party_id = op.party_id INNER JOIN
                      lookupset l3 ON d.output_party_role = l3.lookupset_id ON dl.doc_id = d.doc_id
  AND dm.doc_provider_id = dp.doc_provider_id
/
CREATE OR REPLACE FORCE VIEW axvw_doc_metadata_view AS
SELECT dm.doc_metadata_id, dm.name, dm.doc_category_id, dc.description doc_category_desc, dm.description, dm.file_suffix, dm.input_dt, 
  dm.input_user_id, u1.name input_user_name, 
  dm.doc_source, l3.value doc_source_value, 
  dm.is_deleted, dm.stamp, 
  d.status, d.output_method,
  l1.value status_value, l2.value output_method_value,
  dl.asset_hdr_id, dl.contract_id, dl.credit_line_id, dl.gl_journal_id, dl.opportunity_id, dl.party_id, dl.portfolio_id, dl.program_id, dl.reserve_id, dl.doc_id
FROM doc_metadata dm
  INNER JOIN doc_category dc ON dc.doc_category_id = dm.doc_category_id
  INNER JOIN ax_user u1 ON u1.ax_user_id = dm.input_user_id
  INNER JOIN lookupset l3 ON dm.doc_source = l3.lookupset_id 
  INNER JOIN doc_link dl ON dl.doc_metadata_id = dm.doc_metadata_id
  LEFT OUTER JOIN doc d ON d.doc_id = dl.doc_id
  INNER JOIN lookupset l1 ON d.status = l1.lookupset_id
  INNER JOIN lookupset l2 ON d.output_method = l2.lookupset_id
/
CREATE OR REPLACE FORCE VIEW  axvw_gl_entry
AS
SELECT 	gl_entry_id,contract_id,gl_journal_id,image_no,flow_id,
	tax_flow_id,input_dt,post_dt,ledger_id,business_unit_id,
	gl_account_id,currency_id,amount,base_currency_id,
	base_amount,fx_rate,fx_rate_dt,gl_interface_id,
	gl_tag_id,narration,reversal_status,flow_type,
	gl_entry_type,stamp,N'C' entry_type
FROM gl_entry
/
CREATE OR REPLACE FORCE VIEW axvw_party_account_flow
AS
SELECT  'C' flow_source, f.flow_id, f.contract_id,f.purchase_invoice_id,f.party_account_id,  f.currency_id, f.amt_gross,
	f.amount, f.amt_matched, f.statement_id, s.statement_no, i.invoice_id, i.invoice_no,
	f.actual_dt, f.expected_dt, f.input_dt, f.input_dt allocation_dt, f.flow_type, 0 bank_flow_type, f.installment_no, 0 batch_no, f.custom_flow_hdr_id,
	f.collection_state, f.payment_ref, f.release_dt, f.exclude_from_account_bal, f.exclude_from_late_fees, f.exclude_from_overdue_interest,
	f.penalty_grace_days + fm.collection_grace_period penalty_grace_days, f.in_recovery, 0 stamp, f.reversal_status, f.status, f.nett_no
   FROM    flow f, invoice i, statement s, flow_method fm
   WHERE   f.is_cash = 1
	and f.status not in (2099, 2104, 2105) --exlude projected and written off
    and f.is_shadow_copy = 0
	and f.statement_id = s.statement_id
	and f.invoice_id = i.invoice_id
	and f.flow_method_id = fm.flow_method_id
	and f.exclude_from_account_bal = 0 --exclude recovery flows
	and f.contract_id >= 0
UNION ALL
SELECT  'B' flow_source, f.bank_flow_id, f.contract_id,f.purchase_invoice_id, f.party_account_id,  f.currency_id, f.amount amt_gross, f.amount,
	(select nvl(sum(amt_matched), 0) from bank_flow_match where bank_flow_id = f.bank_flow_id and is_deleted = 0) amt_matched,
   f.statement_id, s.statement_no, 0 invoice_id, N'' invoice_no,
	f.actual_dt, f.actual_dt expected_dt, input_dt, allocation_dt, 0 flow_type, bank_flow_type, 0 installment_no, f.batch_no batch_no, 0 custom_flow_hdr_id,
	14800 collection_state, f.reference, f.actual_dt release_dt, 0 exclude_from_account_bal, 0 exclude_from_late_fees, 0 exclude_from_overdue_interest,
	0 penalty_grace_days, 0 in_recovery, 0 stamp, f.reversal_status, 0 status, 0 nett_no
   FROM bank_flow f, statement s
   where f.statement_id = s.statement_id
   AND f.is_shadow_copy = 0
   AND f.is_deleted = 0
/
CREATE OR REPLACE FORCE VIEW  axvw_party_rejected_stats
AS
SELECT contract_id, /* --> flow.contract_id */
       flow_id, /* --> flow.flow_id */
       input_dt, /* --> flow.input_dt */
       settlement_bank_info_id, /* --> flow.settlement_bank_info_id */
       rejected_reason /* --> flow.rejected_reason */
FROM flow
WHERE status = 2103 AND reversal_status in (4201, 4203)
AND is_shadow_copy = 0
AND contract_id >= 0
/
CREATE OR REPLACE FORCE VIEW  axvw_statement
AS
SELECT  f.flow_id,f.contract_id,f.actual_dt,f.currency_id,
        f.bank_account_id,f.party_account_id,f.payment_ref,
        f.installment_no,f.statement_id,f.amt_gross,'C' flow_type
FROM    flow f, statement s
WHERE   f.statement_id = s.statement_id and f.statement_id > 0 and f.is_cash = 1
		and is_shadow_copy = 0
		and f.contract_id >= 0
UNION ALL
SELECT  bank_flow_id,contract_id,actual_dt,currency_id,
        bank_account_id,party_account_id,reference,
        0,statement_id,-amount,'B' flow_type
FROM    bank_flow
WHERE statement_id > 0
AND is_shadow_copy = 0
/
CREATE OR REPLACE FORCE VIEW  axvw_asset
AS
select ah.name, /* --> asset_hdr.name */
	ah.asset_type_id, /* --> asset_hdr.asset_type_id */
	ah.current_contract_id, /* --> asset_hdr.current_contract_id */
	ah.serial_no, /* --> asset_hdr.serial_no */
	ah.manufacturer_id, /* --> asset_hdr.manufacturer_id */
	ah.stock_code, /* --> asset_hdr.stock_code */
	num_items, /* --> asset_hdr.num_items */
	a.*
from asset a, asset_hdr ah
where ah.asset_hdr_id = a.asset_hdr_id and a.contract_id > 0
/
CREATE OR REPLACE FORCE VIEW axvw_address
AS
	SELECT address_id,
			 ad.street street,
			 ad.suburb suburb,
			 ad_city.name city,
			 ad_county.name county,
			 ad_state.name state_province,
			 ad_country.name country_region,
			 axsp_get_concat_address(ad.address_id) full_address
	FROM address ad,
		  location ad_city,
		  location ad_county,
		  location ad_state,
		  location ad_country
	WHERE ad.city_id = ad_city.location_id
	  AND ad.county_id = ad_county.location_id
	  AND ad.state_province_id = ad_state.location_id
	  AND ad.country_region_id = ad_country.location_id	
/
CREATE OR REPLACE FORCE VIEW axvw_asset_hdr_view_base
AS
SELECT
	ah.asset_hdr_id,
	ah.name,
	ah.serial_no,
	ah.stock_code,
	ah.num_items,
	ah.asset_class_enum,
	ah.current_contract_id,
	ah.original_contract_id,
	ah.owner_party_id,
	ah.location_type,
	ah.maturity_dt,
	ah.manager,
	ah.reference,
	ah.ownership_type asset_ownership_type,
	ah.asset_status,
	ah.branch_id,
	ah_branch.name branch_name,
	ah.owner_id,
	ah.parent_owner_id,
	ah.hdr_type,
	ah.amt_selling_price,
	ah.amt_reserve_price,
	ah.year_of_manufacture,
	ah.sub_location,
	ah.original_purchase_price,
	ah.commitment_dt,
	ah.model asset_hdr_model,
	xt1.value as security_agency_pri_ref_val,
	xt2.value as security_agency_sec_ref_val,
	ah.security_registration_required,
	c.code currency_code,
	ah_asset_type.name asset_type,
	ah_owner_party.name party_owner,
	ah_ownership_type.value asset_ownership_type_value,
	ah_class.value asset_class_value,
	ah_status.value asset_status_value,
	ah_manufacturer.name manufacturer,
	NVL((SELECT SUM(ad.amt_accum_depreciation) FROM asset_depr ad
			WHERE ad.asset_hdr_id = ah.asset_hdr_id),0) amt_accum_depreciation,
	base_address.street asset_street,
	base_address.suburb asset_suburb,
	base_address.city asset_city,
	base_address.county asset_county,
	base_address.state_province asset_state_province,
	base_address.country_region asset_country_region,
	base_address.full_address full_address,
	current_address.street asset_current_street,
	current_address.suburb asset_current_suburb,
	current_address.city asset_current_city,
	current_address.county asset_current_county,
	current_address.state_province asset_current_state_province,
	current_address.country_region asset_current_country_region,
	current_address.full_address full_current_address,
	delivery_address.street asset_delivery_street,
	delivery_address.suburb asset_delivery_suburb,
	delivery_address.city asset_delivery_city,
	delivery_address.county asset_delivery_county,
	delivery_address.state_province asset_delivery_state_province,
	delivery_address.country_region asset_delivery_country_region,
	delivery_address.full_address full_delivery_address,
	u1.name input_user,
	u2.name last_saved_by,
	con.contract_id,
	to_char(con.reference) as contract_reference,
	ah.stamp
FROM
	asset_hdr ah
	LEFT JOIN axvw_address base_address ON base_address.address_id = axsp_get_asset_hdr_address_id(ah.asset_hdr_id, 45100, axsp_dateonly(axsp_get_datetime()))
	LEFT JOIN axvw_address current_address ON current_address.address_id = axsp_get_asset_hdr_address_id(ah.asset_hdr_id, 45101, axsp_dateonly(axsp_get_datetime()))
	LEFT JOIN axvw_address delivery_address ON delivery_address.address_id = axsp_get_asset_hdr_address_id(ah.asset_hdr_id, 45102, axsp_dateonly(axsp_get_datetime())),
	asset_type ah_asset_type,
	party ah_manufacturer,
	party ah_branch,
	lookupset ah_status,
	lookupset ah_class,
	party ah_owner_party,
	lookupset ah_ownership_type,
	ax_user u1,
	ax_user u2,
	currency c,
	contract con,
	xt_lookupset xt1,
	xt_lookupset xt2
WHERE
	ah.asset_type_id = ah_asset_type.asset_type_id
	AND ah_manufacturer.party_id = ah.manufacturer_id
	AND ah_branch.party_id = ah.branch_id
	AND ah.asset_class_enum = ah_class.lookupset_id
	AND ah.asset_status = ah_status.lookupset_id
	AND ah.owner_party_id = ah_owner_party.party_id
	AND ah.ownership_type = ah_ownership_type.lookupset_id
	AND u1.ax_user_id = ah.input_user_id
	AND u2.ax_user_id = ah.last_saved_by_id
	AND ah.currency_id = c.currency_id
	AND ah.asset_hdr_id > 0
	AND (CASE WHEN ah.current_contract_id > 0 THEN ah.current_contract_id ELSE ah.original_contract_id END) = con.contract_id
	AND xt1.xt_lookupset_id = ah.security_agency_primary_ref
	AND xt2.xt_lookupset_id = ah.security_agency_secondary_ref
/
CREATE OR REPLACE FORCE VIEW axvw_asset_core_view
AS
SELECT
	ahv.asset_hdr_id,
	ahv.name,
	ahv.serial_no,
	ahv.stock_code,
	ahv.num_items,
	ahv.asset_class_enum,
	ahv.current_contract_id,
	ahv.owner_party_id,
	ahv.location_type,
	ahv.maturity_dt,
	ahv.manager,
	ahv.reference,
	ahv.asset_ownership_type,
	ahv.asset_status,
	ahv.branch_id,
	ahv.branch_name,
	ahv.owner_id,
	ahv.hdr_type,
	ahv.amt_selling_price,
	ahv.amt_reserve_price,
	ahv.asset_type,
	ahv.party_owner,
	ahv.asset_ownership_type_value,
	ahv.asset_class_value,
	ahv.asset_status_value,
	ahv.manufacturer,
	ahv.amt_accum_depreciation,
	ahv.asset_street,
	ahv.asset_suburb,
	ahv.asset_city,
	ahv.asset_county,
	ahv.asset_state_province,
	ahv.asset_country_region,
	ahv.full_address,
	ahv.asset_current_street,
	ahv.asset_current_suburb,
	ahv.asset_current_city,
	ahv.asset_current_county,
	ahv.asset_current_state_province,
	ahv.asset_current_country_region,
	ahv.full_current_address,
	ahv.asset_delivery_street,
	ahv.asset_delivery_suburb,
	ahv.asset_delivery_city,
	ahv.asset_delivery_county,
	ahv.asset_delivery_state_province,
	ahv.asset_delivery_country_region,
	ahv.full_delivery_address,
	ahv.input_user,
	ahv.last_saved_by,
	ahv.year_of_manufacture,
	ahv.sub_location,
	ahv.original_purchase_price,
	ahv.commitment_dt,
	ahv.currency_code,
	ahv.asset_hdr_model,
	ahv.security_agency_pri_ref_val,
	ahv.security_agency_sec_ref_val,
	ahv.security_registration_required,
	a.asset_id,
	ins1.name insurance_company,
	dd1.name drawdown_party,
	pa1.account_no drawdown_party_account,
	a.contract_id,
	cp1.name end_customer,
	cp1.ext_name party_ext_name,
	cp1.first_names party_first_name,
	cp1.middle_name party_middle_name,
	cp2.ext_name contract_customer,
	p.name product_name,
	pstyle.value product_style,
	rvt.value residual_value_type,
	a.cost,
	a.residual_value,
	a.installments,
	a.start_dt,
	a.calc_dt,
	a.delivery_dt,
	a.mature_dt,
	a.purchase_dt,
	a.sequence_no,
	a.equipment_code,
	a.is_terminated,
	a.terminated_dt,
	a.amt_terminated,
	a.cost_inclusive,
	a.amt_financed,
	case a.is_tax_exempt
		when 17800 then cast(0 AS NUMBER)
		when 17801 then cast(0 AS NUMBER)
		else cast(1 AS NUMBER)
	end is_tax_exempt,
	a.tax_exempt_certificate,
	l.name contract_location,
	c.cparty_id cparty_id,
	c.dealer_id,
	c.contract_state,
	to_char(c.reference) as contract_reference,
	a.stamp
FROM
	axvw_asset_hdr_view_base ahv,
	location l,
	contract c,
	party cp1,
	party cp2,
	party ins1,
	party dd1,
	party_account pa1,
	product p,
	lookupset pstyle,
	lookupset rvt,
	asset a,
	(
		SELECT asset_hdr_id,
				contract_id,
				MAX(asset_id) AS asset_id
		FROM asset
		GROUP BY asset_hdr_id,
				  contract_id
	) maxa
WHERE
	(CASE WHEN ahv.current_contract_id > 0 THEN ahv.current_contract_id ELSE ahv.original_contract_id END) = c.contract_id
	-- This is to ensure we only match to one financial asset record. SAL sub-assets have their own financial asset.
	AND (a.asset_hdr_id = (CASE WHEN EXISTS (SELECT 1 FROM asset WHERE asset.asset_hdr_id = ahv.asset_hdr_id) THEN ahv.asset_hdr_id ELSE ahv.parent_owner_id END))
	AND (maxa.asset_hdr_id = ahv.asset_hdr_id or maxa.asset_hdr_id = ahv.parent_owner_id)
	AND c.contract_id = a.contract_id
	AND c.contract_id = maxa.contract_id
	AND a.asset_id = maxa.asset_id
	AND l.location_id = c.location_id
	AND cp1.party_id = a.customer_id
	AND ins1.party_id = a.insurance_company_id
	AND cp2.party_id = c.cparty_id
	AND p.product_id = c.product_id
	AND c.product_style = pstyle.lookupset_id
	AND c.residual_value_type = rvt.lookupset_id
	AND dd1.party_id = a.drawdown_party_id
	AND pa1.party_account_id = a.drawdown_party_account_id
	AND c.contract_id > 0
/
CREATE OR REPLACE FORCE VIEW  axvw_asset_modular_view
AS
SELECT
	ahv.asset_hdr_id,
	ahv.name,
	ahv.serial_no,
	ahv.stock_code,
	ahv.num_items,
	ahv.asset_class_enum,
	ahv.current_contract_id,
	ahv.owner_party_id,
	ahv.location_type,
	ahv.maturity_dt,
	ahv.manager,
	ahv.reference,
	ahv.asset_ownership_type,
	ahv.asset_status,
	ahv.branch_id,
	ahv.branch_name,
	ahv.owner_id,
	ahv.hdr_type,
	ahv.amt_selling_price,
	ahv.amt_reserve_price,
	ahv.asset_type,
	ahv.party_owner,
	ahv.asset_ownership_type_value,
	ahv.asset_class_value,
	ahv.asset_status_value,
	ahv.manufacturer,
	ahv.amt_accum_depreciation,
	ahv.asset_street,
	ahv.asset_suburb,
	ahv.asset_city,
	ahv.asset_county,
	ahv.asset_state_province,
	ahv.asset_country_region,
	ahv.full_address,
	ahv.asset_current_street,
	ahv.asset_current_suburb,
	ahv.asset_current_city,
	ahv.asset_current_county,
	ahv.asset_current_state_province,
	ahv.asset_current_country_region,
	ahv.full_current_address,
	ahv.asset_delivery_street,
	ahv.asset_delivery_suburb,
	ahv.asset_delivery_city,
	ahv.asset_delivery_county,
	ahv.asset_delivery_state_province,
	ahv.asset_delivery_country_region,
	ahv.full_delivery_address,
	ahv.input_user,
	ahv.last_saved_by,
	ahv.year_of_manufacture,
	ahv.original_purchase_price,
	ahv.commitment_dt,
	ahv.currency_code,
	ahv.contract_id,
	ahv.contract_reference,
	ahv.security_agency_pri_ref_val,
	ahv.security_agency_sec_ref_val,
	ahv.security_registration_required,
	acr.value as ac_rating,
	co.value as condition,
	acm.complex_identification,
	ewt.value as electrical_wiring_type,
	fl.value as floor_load,
	ft.value as frame_type,
	hv.value as hvac,
	mfg.value as hvac_mfg,
	acm.hvac_model_serial,
	no.value as no_of_offices,
	nt.value as no_of_toilets,
	acm.notes,
	acm.overall_area,
	acm.overall_length,
	acm.overall_width,
	rl.value as roof_load,
	ro.value as roof,
	st.value as sprinkler_type,
	sc.value as standard_custom,
	sl.value as supply_line_type,
	ty.value as tyres,
	acm.unit_height,
	acm.unit_length,
	acm.unit_width,
	um.value as unit_measure,
	wl.value as wind_load,
	axsp_get_modUnit_List(acm.asset_class_id, 41400) as building_codes,
	axsp_get_modUnit_List(acm.asset_class_id, 41401) as Occupancy_Codes,
	axsp_get_modUnit_List(acm.asset_class_id, 41402) as Usage,
	axsp_get_modUnit_List(acm.asset_class_id, 41403) as Heat,
	axsp_get_modUnit_List(acm.asset_class_id, 41404) as Exterior_Finish,
	axsp_get_modUnit_List(acm.asset_class_id, 41405) as Exterior_Colour,
	axsp_get_modUnit_List(acm.asset_class_id, 41406) as Interior_Finish,
	axsp_get_modUnit_List(acm.asset_class_id, 41407) as Interior_Colour,
	axsp_get_modUnit_List(acm.asset_class_id, 41408) as Floor_Covering,
	axsp_get_modUnit_List(acm.asset_class_id, 41409) as Exterior_Doors,
	axsp_get_modUnit_List(acm.asset_class_id, 41410) as Windows,
	axsp_get_modUnit_List(acm.asset_class_id, 41411) as Window_Frame,
	axsp_get_modUnit_List(acm.asset_class_id, 41412) as Window_Glazing,
	axsp_get_modUnit_List(acm.asset_class_id, 41413) as Ceiling,
	axsp_get_modUnit_List(acm.asset_class_id, 41414) as Lighting,
	axsp_get_modUnit_List(acm.asset_class_id, 41415) as misc_options,
	axsp_get_modUnit_List(acm.asset_class_id, 41416) as built_in_furniture,
	axsp_get_modUnit_List(acm.asset_class_id, 41417) as axles,
	axsp_get_modUnit_List(acm.asset_class_id, 41418) as electrical_panel,
	axsp_get_modUnit_List(acm.asset_class_id, 41419) as water_heater_type,
	axsp_get_modUnit_List(acm.asset_class_id, 41420) as hitch_type,
	axsp_get_modUnit_List(acm.asset_class_id, 41421) as plumbing_fixtures,
	ahv.stamp
FROM
	axvw_asset_hdr_view_base ahv
	INNER JOIN asset_class_modular acm ON acm.asset_hdr_id = ahv.asset_hdr_id
	LEFT JOIN xt_lookupset acr ON acr.xt_lookupset_id = acm.ac_rating
	LEFT JOIN xt_lookupset co ON co.xt_lookupset_id = acm.condition
	LEFT JOIN xt_lookupset ewt ON ewt.xt_lookupset_id = acm.electrical_wiring_type
	LEFT JOIN xt_lookupset fl ON fl.xt_lookupset_id = acm.floor_load
	LEFT JOIN xt_lookupset ft ON ft.xt_lookupset_id = acm.frame_type
	LEFT JOIN xt_lookupset hv ON hv.xt_lookupset_id = acm.hvac
	LEFT JOIN xt_lookupset mfg ON mfg.xt_lookupset_id = acm.hvac_mfg
	LEFT JOIN xt_lookupset no ON no.xt_lookupset_id = acm.no_of_offices
	LEFT JOIN xt_lookupset nt ON nt.xt_lookupset_id = acm.no_of_toilets
	LEFT JOIN xt_lookupset rl ON rl.xt_lookupset_id = acm.roof_load
	LEFT JOIN xt_lookupset ro ON ro.xt_lookupset_id = acm.roof
	LEFT JOIN xt_lookupset st ON st.xt_lookupset_id = acm.sprinkler_type
	LEFT JOIN xt_lookupset sc ON sc.xt_lookupset_id = acm.standard_custom
	LEFT JOIN xt_lookupset sl ON sl.xt_lookupset_id = acm.supply_line_type
	LEFT JOIN xt_lookupset ty ON ty.xt_lookupset_id = acm.tyres
	LEFT JOIN xt_lookupset um ON um.xt_lookupset_id = acm.unit_measure
	LEFT JOIN xt_lookupset wl ON wl.xt_lookupset_id = acm.wind_load
/
CREATE OR REPLACE FORCE VIEW  axvw_asset_vehicle_view
AS
SELECT
	ahv.asset_hdr_id,
	ahv.name,
	ahv.serial_no,
	ahv.stock_code,
	ahv.num_items,
	ahv.asset_class_enum,
	ahv.current_contract_id,
	ahv.owner_party_id,
	ahv.location_type,
	ahv.maturity_dt,
	ahv.manager,
	ahv.reference,
	ahv.asset_ownership_type,
	ahv.asset_status,
	ahv.branch_id,
	ahv.branch_name,
	ahv.owner_id,
	ahv.hdr_type,
	ahv.amt_selling_price,
	ahv.amt_reserve_price,
	ahv.asset_type,
	ahv.party_owner,
	ahv.asset_ownership_type_value,
	ahv.asset_class_value,
	ahv.asset_status_value,
	ahv.manufacturer,
	ahv.amt_accum_depreciation,
	ahv.asset_street,
	ahv.asset_suburb,
	ahv.asset_city,
	ahv.asset_county,
	ahv.asset_state_province,
	ahv.asset_country_region,
	ahv.full_address,
	ahv.asset_current_street,
	ahv.asset_current_suburb,
	ahv.asset_current_city,
	ahv.asset_current_county,
	ahv.asset_current_state_province,
	ahv.asset_current_country_region,
	ahv.full_current_address,
	ahv.asset_delivery_street,
	ahv.asset_delivery_suburb,
	ahv.asset_delivery_city,
	ahv.asset_delivery_county,
	ahv.asset_delivery_state_province,
	ahv.asset_delivery_country_region,
	ahv.full_delivery_address,
	ahv.input_user,
	ahv.last_saved_by,
	ahv.year_of_manufacture,
	ahv.original_purchase_price,
	ahv.commitment_dt,
	ahv.currency_code,
	ahv.contract_id,
	ahv.contract_reference,
	ahv.security_agency_pri_ref_val,
	ahv.security_agency_sec_ref_val,
	ahv.security_registration_required,
	acv.vin_no,
	acv.comm_no,
	acv.registration_no,
	acv.is_personalised_plates,
	acv.first_registration_dt,
	acv.chassis_no,
	acv.engine_no,
	acv.engine_name,
	acv.engine_capacity,
	acv.colour,
	bs.value as body_style,
	acv.make_code,
	acv.model,
	acv.sub_model,
	acv.is_modified,
	acv.is_turbo,
	acv.doors,
	tr.value as transmission,
	acv.speeds,
	dt.value as drive_train,
	ft.value as fuel_type,
	acv.emission_rating,
	acv.tax_list_price,
	acv.country_of_assembly,
	acv.odometer,
	ahv.stamp
FROM
	axvw_asset_hdr_view_base ahv
	INNER JOIN asset_class_vehicle acv ON acv.asset_hdr_id = ahv.asset_hdr_id
	LEFT JOIN xt_lookupset bs ON bs.xt_lookupset_id = acv.body_style_id
	LEFT JOIN xt_lookupset tr ON tr.xt_lookupset_id = acv.transmission_id
	LEFT JOIN xt_lookupset dt ON dt.xt_lookupset_id = acv.drive_train_id
	LEFT JOIN xt_lookupset ft ON ft.xt_lookupset_id = acv.fuel_type_id
/

CREATE OR REPLACE FORCE VIEW  axvw_asset_aircraft_view
AS
SELECT     
	axvw_asset_hdr_view_base.*, 
	asset_class_aircraft.aircraft_nationality, 
	asset_class_aircraft.registration_marks
FROM
	axvw_asset_hdr_view_base INNER JOIN asset_class_aircraft ON axvw_asset_hdr_view_base.asset_hdr_id = asset_class_aircraft.asset_hdr_id
/

CREATE OR REPLACE FORCE VIEW axvw_asset_financing_view
AS
SELECT
	nvl(ahvb.asset_hdr_id, 0) asset_hdr_id,
	nvl(ahvb.name, ' ') name,
	nvl(ahvb.serial_no, ' ') serial_no,
	nvl(ahvb.stock_code, ' ') stock_code,
	nvl(ahvb.num_items, 0) num_items,
	nvl(ahvb.asset_class_enum, 0) asset_class_enum,
	nvl(ahvb.current_contract_id, 0) current_contract_id,
	nvl(ahvb.original_contract_id, 0) original_contract_id,
	nvl(ahvb.owner_party_id, 0) owner_party_id,
	nvl(ahvb.location_type, 0) location_type,
	nvl(ahvb.maturity_dt, to_date('1900-01-01','yyyy-mm-dd')) maturity_dt,
	nvl(ahvb.manager, ' ') manager,
	nvl(ahvb.reference, ' ') reference,
	nvl(ahvb.asset_ownership_type, 0) asset_ownership_type,
	nvl(ahvb.asset_status, 0) asset_status,
	nvl(ahvb.branch_id, 0) branch_id,
	nvl(ahvb.branch_name, ' ') branch_name,
	nvl(ahvb.owner_id, 0) owner_id,
	nvl(ahvb.parent_owner_id, 0) parent_owner_id,
	nvl(ahvb.hdr_type, 0) hdr_type,
	nvl(ahvb.amt_selling_price, 0) amt_selling_price,
	nvl(ahvb.amt_reserve_price, 0) amt_reserve_price,
	nvl(ahvb.year_of_manufacture, 0) year_of_manufacture,
	nvl(ahvb.sub_location, ' ') sub_location,
	nvl(ahvb.original_purchase_price, 0) original_purchase_price,
	nvl(ahvb.commitment_dt, to_date('1900-01-01','yyyy-mm-dd')) commitment_dt,
	nvl(ahvb.asset_hdr_model, ' ') asset_hdr_model, 
	nvl(ahvb.currency_code, ' ') currency_code,
	nvl(ahvb.asset_type, ' ') asset_type,
	nvl(ahvb.party_owner, ' ') party_owner,
	nvl(ahvb.asset_ownership_type_value, ' ')asset_ownership_type_value,
	nvl(ahvb.asset_class_value, ' ') asset_class_value,
	nvl(ahvb.asset_status_value, ' ')asset_status_value,
	nvl(ahvb.manufacturer, ' ') manufacturer,
	nvl(ahvb.amt_accum_depreciation, 0) amt_accum_depreciation,
	nvl(ahvb.asset_street, ' ') asset_street,
	nvl(ahvb.asset_suburb, ' ') asset_suburb,
	nvl(ahvb.asset_city, ' ') asset_city,
	nvl(ahvb.asset_county, ' ') asset_county,
	nvl(ahvb.asset_state_province, ' ') asset_state_province,
	nvl(ahvb.asset_country_region, ' ') asset_country_region,
	nvl(ahvb.full_address, ' ') full_address,
	nvl(ahvb.asset_current_street, ' ') asset_current_street,
	nvl(ahvb.asset_current_suburb, ' ') asset_current_suburb,
	nvl(ahvb.asset_current_city, ' ') asset_current_city,
	nvl(ahvb.asset_current_county, ' ') asset_current_county,
	nvl(ahvb.asset_current_state_province, ' ') asset_current_state_province,
	nvl(ahvb.asset_current_country_region, ' ') asset_current_country_region,
	nvl(ahvb.full_address, ' ') full_current_address,
	nvl(ahvb.asset_delivery_street, ' ') asset_delivery_street,
	nvl(ahvb.asset_delivery_suburb, ' ') asset_delivery_suburb,
	nvl(ahvb.asset_delivery_city, ' ') asset_delivery_city,
	nvl(ahvb.asset_delivery_county, ' ') asset_delivery_county,
	nvl(ahvb.asset_delivery_state_province, ' ') asset_delivery_state_province,
	nvl(ahvb.asset_delivery_country_region, ' ') asset_delivery_country_region,
	nvl(ahvb.full_address, ' ') full_delivery_address,
	nvl(ahvb.input_user, ' ') input_user,
	nvl(ahvb.last_saved_by, ' ') last_saved_by,
	nvl(ahvb.reference, ' ') as contract_reference,
	nvl(ahvb.security_agency_pri_ref_val, ' ') security_agency_pri_ref_val,
	nvl(ahvb.security_agency_sec_ref_val, ' ') security_agency_sec_ref_val,
	nvl(ahvb.security_registration_required, 0) security_registration_required,
	nvl(ahvb.stamp, 0) stamp,
	
	nvl(fsa.aircraft_nationality,' ') AS aircraft_nationality,
	nvl(fsa.asset_description,' ') AS asset_description,
	nvl(fsa.chassis_no,' ') AS chassis_no,
	nvl(fsa.collateral_class_id,0) AS collateral_class_id,
	nvl(fsa.is_included,0) AS is_included,
	nvl(fsa.manufacturer_name,' ') AS manufacturer_name,
	nvl(fsa.model,' ') AS model,
	nvl(fsa.reference,' ') AS registered_reference,
	nvl(fsa.registered_asset_name,' ') AS registered_asset_name,
	nvl(fsa.registered_new_used,0) AS registered_new_used,
	nvl(fsa.registered_serial_vin_no,' ') AS registered_serial_vin_no,
	nvl(fsa.registered_year_of_manufacture,' ') AS registered_year_of_manufacture,
	nvl(fsa.registration_marks,' ') AS registration_marks,
	nvl(fsa.registration_no,' ') AS registration_no,
	nvl(fsa.secondary_filing_id, ' ') AS secondary_filing_id,
	nvl(fsa.security_agency_primary_ref,' ') AS security_agency_pri_ref_code,
	nvl(fsa.security_agency_secondary_ref,' ') AS security_agency_sec_ref_code,
	nvl(fsa.vin_no,' ') AS vin_no,
	nvl(fsa.filing_id,' ') AS collateral_id, 
	to_char(axsp_get_lookupset_value(nvl(fsa.filing_type_status,0))) AS filing_type_status,
	
	nvl(fs.financing_statement_id, 0) AS financing_statement_id,
	nvl(fs.acceptance_dt,to_date('1900-01-01','yyyy-mm-dd')) AS acceptance_dt, 
	to_char(axsp_get_lookupset_value(nvl(fs.amendment_action_csc,0))) AS amendment_action_csc, 
	to_char(axsp_get_lookupset_value(nvl(fs.amendment_type_csc,0))) AS amendment_type_csc,
	nvl(fs.authority_filing_dt,to_date('1900-01-01','yyyy-mm-dd')) AS authority_filing_dt,
	nvl(fs.billing_reference,' ') AS billing_reference,
	nvl(fs.contract_id,0) AS contract_id,
	nvl(fs.discharge_dt,to_date('1900-01-01','yyyy-mm-dd')) AS discharge_dt, 
	to_char(axsp_get_lookupset_value(nvl(fs.duration_rule,0))) AS duration_rule,
	nvl(fs.expiry_dt,to_date('1900-01-01','yyyy-mm-dd')) AS expiry_dt,
	nvl(fs.filing_dt,to_date('1900-01-01','yyyy-mm-dd')) AS filing_dt,
	nvl(fs.filing_location_id,0) AS filing_location_id,
	nvl(fs.filing_method_id,0) AS filing_method_id,
	nvl(fs.filing_number,' ') AS filing_number, 
	to_char(axsp_get_lookupset_value(nvl(fs.fin_statement_category,0))) AS fin_statement_category,
	nvl(fs.is_assets_inventory,0) AS is_assets_inventory,
	nvl(fs.is_assets_subject_to_control,0) AS is_assets_subject_to_control,
	nvl(fs.is_claim_proceeds,0) AS is_claim_proceeds,
	nvl(fs.is_excluded_auto_processing,0) AS is_excluded_auto_processing,
	nvl(fs.is_purchase_money_security_int,0) AS is_purchase_money_security_int,
	nvl(fs.is_subordinate,0) AS is_subordinate,
	nvl(fs.link_filing_number,' ') AS link_filing_number,
	nvl(fs.order_id,0) AS order_id,
	nvl(fs.order_status,' ') AS order_status,
	nvl(fs.primary_party_id,0) AS primary_party_id,
	nvl(fs.primary_secured_party_id,0) AS primary_secured_party_id,
	nvl(fs.registration_authority_id,0) AS registration_authority_id,
	nvl(fs.requested_expiry_dt,to_date('1900-01-01','yyyy-mm-dd')) AS requested_expiry_dt,
	nvl(fs.secondary_party_id,0) AS secondary_party_id,
	nvl(fs.secondary_secured_party_id,0) AS secondary_secured_party_id,
	nvl(fs.validity_period_mths,0) AS validity_period_mths,
	nvl(fs.version,0) AS version,	
	nvl(fs.reference,' ') AS authority_reference_no,
	nvl(fs.filing_id,' ') AS filing_id, 
	
	to_char(axsp_get_lookupset_value(nvl(fs.filing_type,0))) AS filing_type,
	to_char(axsp_get_lookupset_value(nvl(fs.processing_state,0))) AS processing_state,

	p.ext_name as registration_authority_name, 
	pdp.ext_name as primary_debtor_name, 
	psp.ext_name as primary_secured_name, 

	CASE WHEN (ffm.uses_workflow = 1)
		THEN (CASE WHEN((fs.filing_type = 16602 AND w.current_state_type != 18003) OR (fs.filing_type = 16614 AND w.current_state_type != 18004)) THEN 51402
				  WHEN ((fs.filing_type = 16606 AND w.current_state_type != 18003) OR (fs.filing_type = 16614 AND w.current_state_type = 18004)) THEN 51400
				  WHEN ((fs.filing_type = 16606 OR fs.filing_type = 16602) AND w.current_state_type = 18003) THEN 
					(
						CASE WHEN EXISTS (SELECT 1 FROM fin_statement_hdr where current_fin_statement_id = fs.financing_statement_id)
						THEN 51400
						ELSE 51401
						END
					)
				  ELSE 0 END)
		ELSE 0
	END AS financing_statement_status,
		CASE WHEN (ffm.uses_workflow = 1)
		THEN (CASE WHEN((fs.filing_type = 16602 AND w.current_state_type != 18003) OR (fs.filing_type = 16614 AND w.current_state_type != 18004)) THEN to_char(axsp_get_lookupset_value(51402))
				  WHEN ((fs.filing_type = 16606 AND w.current_state_type != 18003) OR (fs.filing_type = 16614 AND w.current_state_type = 18004)) THEN to_char(axsp_get_lookupset_value(51400))
				  WHEN ((fs.filing_type = 16606 OR fs.filing_type = 16602) AND w.current_state_type = 18003) THEN 
					(
						CASE WHEN EXISTS (SELECT 1 FROM fin_statement_hdr where current_fin_statement_id = fs.financing_statement_id)
						THEN to_char(axsp_get_lookupset_value(51400))
						ELSE to_char(axsp_get_lookupset_value(51401))
						END 
					)
				  ELSE '' END)
		ELSE ''
	END AS financing_statement_status_val,
	CASE WHEN ffm.uses_workflow = 0 THEN 0 ELSE w.current_state_type END as workflow_state_id,
	CASE WHEN ffm.uses_workflow = 0 THEN ' ' ELSE to_char(wst.name) END as workflow_state_name,
	ffm.filing_method financing_filing_method

	FROM financing_statement fs 
	LEFT OUTER JOIN financing_statement_asset fsa ON fs.financing_statement_id = fsa.financing_filing_id
	LEFT OUTER JOIN asset a ON fsa.asset_id = a.asset_id 
	LEFT OUTER JOIN axvw_asset_hdr_view_base ahvb on (CASE WHEN fsa.asset_hdr_id  > 0 THEN fsa.asset_hdr_id ELSE a.asset_hdr_id END) = ahvb.asset_hdr_id
	JOIN financing_filing_method ffm on fs.filing_method_id = ffm.financing_filing_method_id
	LEFT OUTER JOIN workflow w ON fs.financing_statement_id = w.financing_statement_id
	LEFT OUTER JOIN wf_state_type wst ON w.current_state_type = wst.wf_state_type_id
	LEFT OUTER JOIN wf_type wft ON wft.wf_type_id = wst.wf_type_id 
	
	JOIN party p ON p.party_id = (CASE fs.filing_method_id WHEN 0 THEN fs.registration_authority_id ELSE ffm.nom_reg_party_id END)
	JOIN party pdp ON pdp.party_id = fs.primary_party_id
	JOIN party psp ON psp.party_id = fs.primary_secured_party_id
	
	WHERE wft.is_42_workflow IS NULL OR (wft.is_42_workflow = 1 AND wft.wf_context_id = 35812)
/

CREATE OR REPLACE FORCE VIEW  axvw_asset_hdr_po_row_view
AS
select c.contract_id purchase_order_no,
       c.cparty_id supplier_id,
       c.input_dt,
       c.contract_type,
       c.input_user_id,
       ahr.asset_hdr_id,
	    ahr.owner_id,
	    ahr.parent_owner_id,
	    ahr.hdr_type,
       sum_p_r.amt_total,
       sum_p_r.amt_total_tax,
       sum_p_r.amt_total_gross,
       p_r.po_row_id,
       52400 po_type,
       p_r.stamp
from po_row p_r
inner join contract c on p_r.contract_id = c.contract_id
inner join (select p_r2.po_row_id,
                   p_r2.contract_id,
                   SUM(p_r2.amt_total) amt_total,
                   SUM(p_r2.amt_total_tax) amt_total_tax,
                   SUM(p_r2.amt_total_gross) amt_total_gross
              from po_row p_r2
            group by p_r2.po_row_id, p_r2.contract_id) sum_p_r on p_r.po_row_id != 0 and sum_p_r.po_row_id = p_r.po_row_id and sum_p_r.contract_id = p_r.contract_id
inner join  asset_hdr ahr on p_r.po_row_id = ahr.po_row_id
where p_r.num_assets_received > 0
union all
select c.contract_id purchase_order_no,
       c.cparty_id supplier_id,
       c.input_dt,
       c.contract_type,
       c.input_user_id,
       ahr.asset_hdr_id,
	    ahr.owner_id,
	    ahr.parent_owner_id,
	    ahr.hdr_type,
       sum_p_r.amt_total,
       sum_p_r.amt_total_tax,
       sum_p_r.amt_total_gross,
       p_r.po_row_cf_id,
       52401 po_type,
       p_r.stamp
from po_row_cf p_r
inner join contract c on p_r.contract_id = c.contract_id
inner join (SELECT po_row_cf_id, (column_value).getnumberval() AS asset_hdr_id 
					FROM po_row_cf, xmltable(destination_csv)
					INNER JOIN custom_flow_hdr cfh ON cfh.custom_flow_hdr_id = custom_flow_hdr_id AND cfh.purpose = 14209
					WHERE LENGTH(TRIM(destination_csv)) != 0) prt ON prt.po_row_cf_id = p_r.po_row_cf_id
inner join (select p_r2.po_row_cf_id,
                   p_r2.contract_id,
                   SUM(p_r2.amt_total) amt_total,
                   SUM(p_r2.amt_total_tax) amt_total_tax,
                   SUM(p_r2.amt_total_gross) amt_total_gross
              from po_row_cf p_r2
            group by p_r2.po_row_cf_id, p_r2.contract_id) sum_p_r on p_r.po_row_cf_id != 0 and sum_p_r.po_row_cf_id = p_r.po_row_cf_id and sum_p_r.contract_id = p_r.contract_id
inner join asset_hdr ahr ON ahr.asset_hdr_id = prt.asset_hdr_id
where p_r.num_assets_received > 0
/
 CREATE OR REPLACE FORCE VIEW axvw_bankflow_view_internal
as
select bf.bank_flow_id,
       bf.input_dt,
       bf.actual_dt,
       bf.allocation_dt,
       bf.amount,
       case WHEN bf.amount >=0 THEN 5901 ELSE 5900 END flow_direction,
       bf.party_account_id,
       pa.name party_account_name,
       bf.contract_id,
       bf.reference,
       bf.cheque_no,
       s.statement_no,
       ccy.code ccy,
       ba.bankacc_id bank_account_id,
       ba.code bank_account,
       p2.name business_unit,
       ba.ext_name AS business_unit_bank_account,
       p.name party_name,
       p.ext_name party_full_name,
       p.party_business_unit_id business_unit_id,
       nvl((select sum(bfm.amt_matched) from bank_flow_match bfm where bfm.bank_flow_id = bf.bank_flow_id and bfm.flow_id = bfm.recovery_flow_id and bfm.is_deleted = 0),0) amt_matched,
       bf.batch_no,
       bf.is_reconciled,
       bf.bank_flow_type,
       bf.reversal_status,
       bf.reversed_bank_flow_id,
       bf.save_type,
       bf.extra_info,
	    bf.rejected_reason,
	    bf.reconciled_dt,
       bf.is_manual_dd,
       bf.flow_method_id,
       bf.bank_interface_run_id,
       bf.rejected_dt,
       bf.input_user_id,
       iu.name input_user,
       bf.rejected_user_id,
       ru.name rejected_user,
       bf.source_flow_id,
       bf.bank_flow_item_type,
       bf.invoice_no,
       bf.purchase_invoice_id,
       bf.purchase_invoice_reference,
       bf.currency_id,
       bf.amt_teller_receipt,
       bf.amt_teller_change,
       bf.is_deleted,
       bf.party_bank_name,
       bf.party_bank_account_no,
       bf.cheque_dt,
       bf.cleared_funds_dt,
       bf.party_name drawer,
       bf.stamp
from   bank_flow bf,
       currency ccy,
       party_account pa,
       bankacc ba,
       party p,
       party p2,
       ax_user iu,
       ax_user ru,
       statement s
where  bf.party_account_id = pa.party_account_id and
       bf.currency_id = ccy.currency_id and
       bf.bank_account_id = ba.bankacc_id and
       bf.party_id = p.party_id and
       p.party_business_unit_id = p2.party_id and
       s.statement_id = bf.statement_id and
       iu.ax_user_id = bf.input_user_id and
       ru.ax_user_id = bf.rejected_user_id and
       bf.is_shadow_copy = 0
/
CREATE OR REPLACE FORCE VIEW axvw_bankflow_batch_view
as
select	bf.bank_flow_id,
	bf.receipt_package_id,
	bf.batch_no,	 
	bf.actual_dt, 
	bf.party_name drawer,
	bf.contract_id,
	bf.bank_account_id,
	ba.full_account_no bank_account_no,
	bf.bank_flow_type,
	l1.value bank_flow_type_str,
	bf.party_bank_name,
	bf.party_bank_account_no,
	bf.invoice_no,
	bf.amount,
	bf.currency_id,
	ccy.code currency_code,
	bf.party_id,
	bf.party_account_id,
	p.ext_name customer_name,
	pa.account_no customer_party_account_no,
	bf.is_deleted,
	bf.is_manual_dd,
	bf.bank_interface_run_id,
	bf.cleared_funds_dt,
	bf.is_approval_pending,
	bf.stamp
from  	bank_flow bf, 
	currency ccy, 
	party_account pa, 
	party p,
	bankacc ba, 
	lookupset l1
where	bf.party_account_id = pa.party_account_id and 
	bf.currency_id = ccy.currency_id and 
	bf.bank_account_id = ba.bankacc_id and 
	bf.party_id = p.party_id and
	l1.lookupset_id = bf.bank_flow_type
/
CREATE OR REPLACE FORCE VIEW axvw_bankflow_actg_view
AS
  SELECT bfm.bank_flow_match_id,
    bf.bank_flow_id,
    f.flow_id,
    c.contract_id,
    p.purchase_invoice_id,
    f.flow_type,
    f.currency_id,
    bf.bank_account_id,
    bfm.matched_dt,
    bf.actual_dt,
    bf.input_dt,
    bfm.amt_matched,
    bfm.amt_matched_principal,
    bfm.amt_matched_interest,
    bfm.amt_matched_tax,
    bfm.amt_matched -(bfm.amt_matched_principal + bfm.amt_matched_interest + bfm.amt_matched_tax ) amt_matched_other,
    bf.amount,
    bf.save_type,
    bf.save_status,
    bf.reversal_status,
	 p.save_status purchase_invoice_save_status,
	 c.save_status contract_save_status,
	 p.is_active purchase_invoice_is_active,
	 c.is_active contract_is_active,
    bf.stamp
  from bank_flow_match bfm
  -- ensure bfm.flow_id and bfm.recovery_flow_id are the same but done like this to ensure db doesn't do a self join which avoids a table scan on bfm
  inner join bank_flow bf on (bf.bank_flow_id = bfm.bank_flow_id and bfm.is_deleted = 0 and bfm.flow_id - bfm.recovery_flow_id = 0)
  inner join flow f on bfm.flow_id = f.flow_id
  inner join contract c on (f.contract_id = c.contract_id and f.contract_id+0 >= 0)
  inner join purchase_invoice p on (f.purchase_invoice_id = p.purchase_invoice_id and f.purchase_invoice_id+0 >= 0)
/
CREATE OR REPLACE FORCE VIEW axvw_bank_flow_match_view
AS
SELECT bfm.bank_flow_match_id, /* --> bank_flow_match.bank_flow_match_id */
	bfm.bank_flow_id, /* --> bank_flow_match.bank_flow_id */
	f.flow_id, /* --> flow.flow_id */
	f.contract_id, /* --> flow.contract_id */
	bfm.matched_dt, /* --> bank_flow_match.matched_dt */
	bfm.amt_matched, /* --> bank_flow_match.amt_matched */
	bfm.is_deleted, /* --> bank_flow_match.is_deleted */
	bfm.is_rejected, /* --> bank_flow_match.is_rejected */
	f.amt_gross flow_amt_gross, /* --> flow.amt_gross */
	f.purchase_invoice_id, /* --> flow.purchase_invoice_id */
	piv.reference purchase_invoice_reference, /* --> purchase_invoice.reference */
	f.custom_flow_link_no, /* --> flow.custom_flow_link_no */
	ccf.reference contract_custom_flow_reference, /* --> contract_custom_flow.reference */
	bfm.stamp /* --> bank_flow_match.stamp */
FROM bank_flow_match bfm
	INNER JOIN flow f on bfm.flow_id = f.flow_id
	INNER JOIN purchase_invoice piv ON f.purchase_invoice_id = piv.purchase_invoice_id
	LEFT OUTER JOIN contract_custom_flow ccf ON f.custom_flow_link_no = ccf.link_no AND
		f.custom_flow_hdr_id = ccf.custom_flow_hdr_id AND
		f.contract_id = ccf.contract_id
WHERE bfm.flow_id = bfm.recovery_flow_id
/
CREATE OR REPLACE FORCE VIEW axvw_coll_act_flow_internal
AS
SELECT  paf.flow_id bank_flow_id,
	paf.contract_id,
	paf.party_account_id,
	paf.currency_id,
	paf.amt_gross,
	paf.amount,
	nvl(bfm.amt_matched, 0) amt_matched,
	paf.statement_id,
	paf.statement_no,
	paf.invoice_id,
	paf.invoice_no,
	paf.input_dt,
	paf.actual_dt,
	paf.expected_dt,
	paf.allocation_dt,
	paf.bank_flow_type,
	paf.installment_no,
	paf.batch_no,
	paf.custom_flow_hdr_id,
	paf.collection_state,
	paf.payment_ref,
	paf.release_dt,
	case WHEN paf.exclude_from_late_fees = 0 and paf.exclude_from_overdue_interest = 0 THEN 19700
				WHEN paf.exclude_from_late_fees = 1 and paf.exclude_from_overdue_interest = 0 THEN 19701
				WHEN paf.exclude_from_late_fees = 0 and paf.exclude_from_overdue_interest = 1 THEN 19702
				ELSE 19703
	END penalty_exclusion_status,
	paf.penalty_grace_days,
	paf.in_recovery,
	paf.reversal_status
FROM	axvw_party_account_flow paf
LEFT OUTER JOIN bank_flow_match bfm ON bfm.bank_flow_id = paf.flow_id AND bfm.flow_id = bfm.recovery_flow_id AND bfm.is_deleted = 0
WHERE	paf.flow_source = 'B' and
paf.flow_id != 0
/
CREATE OR REPLACE FORCE VIEW axvw_coll_hist_sum
AS
SELECT 	f.party_account_id,
	f.currency_id,
	ch.coll_hist_sum_id,

	case when ch.coll_hist_sum_id = 1 then
		sum (case WHEN m.matched_dt <= f.expected_dt THEN 1 ELSE 0 END)
	else
		sum (case WHEN m.matched_dt <= f.expected_dt THEN f.amt_matched ELSE 0 END)
	end bucket_on_time,

	case when ch.coll_hist_sum_id = 1 then
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 1) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 1) THEN 1 ELSE 0 END)
	else
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 1) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 1) THEN f.amt_matched ELSE 0 END)
	end bucket_1,

	case when ch.coll_hist_sum_id = 1 then
  	sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 2) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 2) THEN 1 ELSE 0 END)
	else
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 2) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 2) THEN f.amt_matched ELSE 0 END)
	end bucket_2,

	case when ch.coll_hist_sum_id = 1 then
  	sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 3) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 3) THEN 1 ELSE 0 END)
	else
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 3) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 3) THEN f.amt_matched ELSE 0 END)
	end bucket_3,

	case when ch.coll_hist_sum_id = 1 then
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 4) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 4) THEN 1 ELSE 0 END)
	else
  	sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 4) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 4) THEN f.amt_matched ELSE 0 END)
	end bucket_4,

	case when ch.coll_hist_sum_id = 1 then
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 5) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 5) THEN 1 ELSE 0 END)
	else
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 5) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 5) THEN f.amt_matched ELSE 0 END)
	end bucket_5,

	case when ch.coll_hist_sum_id = 1 then
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 6) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 6) THEN 1 ELSE 0 END)
	else
    sum (case WHEN m.matched_dt - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 6) and m.matched_dt - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 6) THEN f.amt_matched ELSE 0 END)
	end bucket_6,

	0 stamp
FROM	flow f
INNER JOIN bank_flow_match m ON f.flow_id = m.flow_id AND m.flow_id = m.recovery_flow_id AND m.is_deleted = 0
INNER JOIN (SELECT 1 as coll_hist_sum_id FROM dual UNION ALL SELECT 2 FROM dual) ch ON (1=1)

WHERE 	collection_state = 14803 and amt_gross > 0
	AND m.matched_dt = (SELECT MAX(matched_dt)
				FROM bank_flow_match m2
				WHERE m2.flow_id = f.flow_id AND m2.flow_id = m2.recovery_flow_id AND m2.is_deleted = 0)
	AND f.is_cash = 1
	AND f.is_shadow_copy = 0
	AND reversal_status = 4200
GROUP BY ch.coll_hist_sum_id, f.party_account_id,
	f.currency_id
/
CREATE OR REPLACE FORCE VIEW  axvw_task_view
AS
SELECT  t.task_id, /* --> task.task_id */
        t.subject, /* --> task.subject */
        t.input_dt, /* --> task.input_dt */
        t.effect_dt, /* --> task.effect_dt */
        t.due_dt, /* --> task.due_dt */
        t.task_type, /* --> task.task_type */
        t.status,	 /* --> task.status */
        t.approval_status, /* --> task.approval_status */
        t.assigned_to_user_id, /* --> task.assigned_to_user_id */
        t.owner_user_id, /* --> task.owner_user_id */
        t.priority, /* --> task.priority */
        t.party_id, /* --> task.party_id */
        t.party_account_id, /* --> task.party_account_id */
        t.contract_id, /* --> task.contract_id */
        t.asset_hdr_id, /* --> task.asset_hdr_id */
        t.flow_id, /* --> task.flow_id */
        t.invoice_id, /* --> task.invoice_id */
        t.statement_id, /* --> task.statement_id */
        t.purchase_invoice_id, /* --> task.purchase_invoice_id */
        u1.name assigned_to_user, /* --> ax_user.name */
        u2.name previous_user, /* --> ax_user.name */
        u3.name initiated_by_user, /* --> ax_user.name */
        u4.name owner_user, /* --> ax_user.name */
        l1.value task_type_value, /* --> lookupset.value */
        l2.value status_value, /* --> lookupset.value */
        l3.value priority_value, /* --> lookupset.value */
        nvl(po.overdue_bal_ccy_id, 0) currency_id, /* --> party_overdue_stats.overdue_bal_ccy_id */
        nvl(po.overdue_bal,0) overdue_bal, /* --> party_overdue_stats.overdue_bal */
        nvl(po.overdue_days,0) overdue_days, /* --> party_overdue_stats.overdue_days */
        p.party_no, /* --> party.party_no */
        p.ext_name party_name, /* --> party.ext_name */
        pa.account_no party_account_no, /* --> party_account.account_no */
        pa.name party_account_name, /* --> party_account.name */
        l4.value process_result_value,
        l5.value approval_status_value,
        t.credit_line_id,
		cl.name,
		t.upload_package_id, /* --> Upload Party Id */
		t.upload_batch_id, /* --> Upload Batch Id */
		t.policy_surrender_value_id, /* --> task.policy_surrender_value_id */
        t.stamp, /* --> task.stamp */
        case when (t.due_dt < axsp_get_datetime() and (t.status = 6400 or t.status = 6401 or t.status = 6403)) then 1 else 0 end is_task_overdue, /* --> task is overdue */
      t.workflow_id, /* --> Workflow Id */
      t.is_workflow /* --> Is Workflow Task */
FROM    task t
        inner join ax_user u1 on t.assigned_to_user_id = u1.ax_user_id
        inner join ax_user u2 on t.previous_user_id = u2.ax_user_id
        inner join ax_user u3 on t.initiated_by_user_id = u3.ax_user_id
        inner join ax_user u4 on t.owner_user_id = u4.ax_user_id
        inner join lookupset l1 on t.task_type = l1.lookupset_id AND t.task_type != 6233  /* BugzId: 33017 */
        inner join lookupset l2 on t.status = l2.lookupset_id
        inner join lookupset l3 on t.priority = l3.lookupset_id
        inner join lookupset l4 on t.process_result = l4.lookupset_id
        inner join lookupset l5 on t.approval_status = l5.lookupset_id
        inner join party p on  t.party_id = p.party_id
        inner join party_account pa on t.party_account_id = pa.party_account_id
        inner join credit_line cl on t.credit_line_id = cl.credit_line_id
        left outer join party_overdue_stats po on t.party_id = po.party_id and t.task_type = po.task_type
			and t.task_type = po.task_type and po.task_type in (6205,6221)
/
CREATE OR REPLACE FORCE VIEW  axvw_bankflow_view_grouping
AS
SELECT     bf.bank_flow_id, SUM(DISTINCT m.bankflow_match_status) flow_match_status
FROM         bank_flow bf LEFT OUTER JOIN
                      bank_rec_match m ON bf.bank_flow_id = m.bank_flow_id
GROUP BY bf.bank_flow_id
/
CREATE OR REPLACE FORCE VIEW  axvw_bankflow_view
as
/* NB: Optimised for Oracle, is faster if it doesn't use axvw_bankflow_view_internal and links to amt_matched via a left join */
select bf.bank_flow_id,
	bf.input_dt,
	bf.actual_dt,
	axsp_dateonly(bf.allocation_dt) allocation_dt,
	bf.allocation_dt allocation_dt_time,
	bf.amount,
	case WHEN bf.amount >=0 THEN 5901 ELSE 5900 END flow_direction,
	l2.value as flow_direction_value,
	pa.name AS party_account_name,
	case when pi.purchase_invoice_id > 0 and pi.contract_id > 0 then pi.contract_id else bf.contract_id end contract_id,
	bf.reference,
	bf.cheque_no,
	s.statement_no,
	ccy.code ccy,
	bf.bank_account_id,
	ba.code bank_account,
	ba.ext_name AS business_unit_bank_account,
	p.name party_name,
	p.ext_name party_full_name,
	p.party_business_unit_id business_unit_id,
	p2.name business_unit,
	NVL(bfmq.amt_matched, 0) as amt_matched,
	case when NVL(bfmq.amt_matched, 0) = bf.amount then 15500 when NVL(bfmq.amt_matched, 0) != 0 then 15501 else 15502 end match_status,
	l1.value as match_status_value,
	bf.batch_no,
	bf.is_reconciled,
	bf.party_account_id,
	bf.bank_flow_type,
	l6.value as bank_flow_type_value,
	bf.reversal_status,
	l3.value as reversal_status_value,
	bf.reversed_bank_flow_id,
	bf.save_type,
	bf.extra_info,
	bf.rejected_reason,
	x4.value as rejected_reason_value,
	bf.reconciled_dt,
	bf.is_manual_dd,
	bf.flow_method_id,
	fm.ext_name as flow_method_value,
	bf.bank_interface_run_id,
	bf.rejected_dt,
	bf.input_user_id,
	iu.name input_user,
	bf.rejected_user_id,
	ru.name rejected_user,
	bf.source_flow_id,
	bf.bank_flow_item_type,
	l5.value as bank_flow_item_type_value,
	bf.invoice_no,
	bf.purchase_invoice_id,
	bf.purchase_invoice_reference,
	bf.currency_id,
	bf.amt_teller_receipt,
	bf.amt_teller_change,
	bf.is_deleted,
	bf.party_bank_name,
	bf.party_bank_account_no,
	bf.cheque_dt,
	bf.cleared_funds_dt,
	bf.party_name drawer,
	bf.stamp
from	bank_flow bf 
	left join (SELECT     bank_flow_id, SUM(amt_matched) AS amt_matched
		FROM         bank_flow_match bfm
		WHERE     flow_id = recovery_flow_id AND is_deleted = 0
		GROUP BY bank_flow_id) bfmq on bf.bank_flow_id = bfmq.bank_flow_id
	inner join currency ccy on bf.currency_id = ccy.currency_id
	inner join party_account pa on bf.party_account_id = pa.party_account_id
	inner join bankacc ba on bf.bank_account_id = ba.bankacc_id
	inner join party p on bf.party_id = p.party_id
	inner join party p2 on p.party_business_unit_id = p2.party_id
	inner join ax_user iu on iu.ax_user_id = bf.input_user_id
	inner join ax_user ru on ru.ax_user_id = bf.rejected_user_id
	inner join statement s on s.statement_id = bf.statement_id
	inner join lookupset l1 on l1.lookupset_id = CASE WHEN NVL(bfmq.amt_matched,0) = bf.amount THEN 15500 WHEN NVL(bfmq.amt_matched,0) != 0 THEN 15501 ELSE 15502 END
	inner join lookupset l2 on l2.lookupset_id = case WHEN bf.amount >=0 THEN 5901 ELSE 5900 END
	inner join lookupset l3 on l3.lookupset_id = bf.reversal_status
	inner join xt_lookupset x4 on x4.xt_lookupset_id = bf.rejected_reason
	inner join lookupset l5 on l5.lookupset_id = bf.bank_flow_item_type
	inner join lookupset l6 on l6.lookupset_id = bf.bank_flow_type
	inner join flow_method fm on bf.flow_method_id = fm.flow_method_id
	inner join purchase_invoice pi on bf.purchase_invoice_id = pi.purchase_invoice_id
where bf.is_shadow_copy = 0
/
CREATE OR REPLACE FORCE VIEW  axvw_brec_bankflow_view
AS
SELECT	bf.*,
		bfg.flow_match_status,
    (select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from note where bank_flow_id = bf.bank_flow_id and rownum <= 1) has_notes,
    (select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from bank_rec_match where bank_flow_id = bf.bank_flow_id and adjustment_contract_id != 0 and rownum <=1) has_adjustment,
    nvl((select adjustment_contract_id from bank_rec_match where bank_flow_id = bf.bank_flow_id and adjustment_contract_id != 0 and rownum <=1),0) adjustment_contract_id
from axvw_bankflow_view bf
inner join axvw_bankflow_view_grouping bfg
on bf.bank_flow_id = bfg.bank_flow_id
/
CREATE OR REPLACE FORCE VIEW axvw_coll_act_flow
AS
SELECT 	bank_flow_id,
	party_account_id,
	currency_id,
	batch_no,
	contract_id,
	expected_dt,
	allocation_dt,
	input_dt,
	payment_ref,
	bank_flow_type,
	amount,
	sum(amt_matched) AS amt_matched,
	amount - sum(amt_matched) AS amt_unmatched,
	release_dt,
	penalty_exclusion_status,
	penalty_grace_days,
	in_recovery,
	reversal_status,
	0 AS stamp
FROM 	axvw_coll_act_flow_internal
GROUP BY
	bank_flow_id,
	party_account_id,
	currency_id,
	batch_no,
	contract_id,
	expected_dt,
	allocation_dt,
	input_dt,
	payment_ref,
	bank_flow_type,
	release_dt,
	penalty_exclusion_status,
	penalty_grace_days,
	in_recovery,
   reversal_status,
	amount
/
CREATE OR REPLACE FORCE VIEW  axvw_party_account_view
AS
select pa.party_account_id, pa.account_no, pa.name,
    case (select count(*) from flow f1 where f1.party_account_id = pa.party_account_id
          and collection_state = 14802 -- overdue
          and reversal_status = 4200 --current
          and is_cash = 1
          and contract_id >= 0)
          when 0 then cast(0 AS NUMBER) else cast(1 AS NUMBER) end is_overdue,
    pa.party_id,
    p.ext_name party_name,
    p.party_business_unit_id business_unit_id,
    pa.branch_id,
    (select currency_id from currency where currency_id = nvl(cf.currency_id, 0)) currency_id,
    (select code from currency where currency_id = nvl(cf.currency_id, 0)) currency_code,
    nvl((select balance from statement_bal sb
         where sb.party_account_id = pa.party_account_id and sb.balance_dt = pa.last_statement_dt
         and sb.currency_id = cf.currency_id),0) last_statement_bal,
    nvl((select sum(f2.amt_gross-f2.amt_matched) from flow f2
         where f2.party_account_id = pa.party_account_id
         and f2.currency_id = cf.currency_id
         and f2.collection_state = 14802 -- overdue
         and f2.reversal_status = 4200 --current
         and f2.is_cash = 1
         and f2.contract_id >= 0),0) overdue_bal,
    nvl((select min(f3.expected_dt) from flow f3
         where f3.expected_dt >= trunc(dt.server_dt)
         and f3.party_account_id = pa.party_account_id
         and f3.flow_type = 1003 -- Installment
         and reversal_status = 4200 --current
         and f3.is_cash = 1
         and f3.status != 2099
         and f3.currency_id = cf.currency_id
         and f3.contract_id >= 0), axsp_datemin()) next_installment_dt,
    pa.last_invoice_dt, pa.last_statement_dt,
    nvl((select max(activity_dt) from note pa2
         where pa2.party_account_id = pa.party_account_id
         and pa2.activity_area = 8100 and activity_type = 8002), axsp_datemin()) last_collection_letter_dt,
    pa.input_dt,
    nvl((select max(contract_id) from contract
         where is_active = 1
         and save_status >= 2201
         and (currency_id = 0 or currency_id = cf.currency_id)
         and party_account_id = pa.party_account_id), 0) last_contract_id,
    nvl((select reference from contract
         where contract_id = (select max(contract_id) from contract
                              where is_active = 1
                              and save_status >= 2201
                              and (currency_id = 0 or currency_id = cf.currency_id)
                              and party_account_id = pa.party_account_id)), ' ') last_contract_ref,
    pa.class_id,
    pa.party_account_contact_id,
    pa.stamp
from axvw_get_datetime dt,
     party_account pa left outer join
     (select distinct f.party_account_id, f.currency_id from flow f) cf
     on pa.party_account_id = cf.party_account_id, party p
where pa.party_id = p.party_id
  and pa.party_account_id > 0 --exclude None
  and p.party_type > 1
/
CREATE OR REPLACE FORCE VIEW axvw_coll_curr_overdue
AS
SELECT 	v.party_account_id,
	v.currency_id,
	co.coll_curr_overdue_id,

	case co.coll_curr_overdue_id when 1 then count(*) else sum(overdue_amt0) end  bucket_total,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt1 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt1)
	end bucket_1,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt2 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt2)
	end bucket_2,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt3 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt3)
	end bucket_3,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt4 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt4)
	end bucket_4,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt5 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt5)
	end bucket_5,

	case co.coll_curr_overdue_id when 1 then
		sum(case overdue_amt6 WHEN 0 THEN 0 ELSE 1 END)
	else
		sum(overdue_amt6)
	end bucket_6,

	0 stamp
FROM	axvw_overdue_view v
INNER JOIN (
				SELECT 1 as coll_curr_overdue_id from dual
				UNION ALL
				SELECT 2 from dual
	 	   ) co on (1=1)
WHERE	v.collection_state = 14802 and v.overdue_amt0 > 0
GROUP BY co.coll_curr_overdue_id, v.party_account_id,
	v.currency_id

/
CREATE OR REPLACE FORCE VIEW axvw_party_address_curr_street as
select p.party_id,
       pa.party_address_id,
       pa.address_type,
       pa.street,
       pa.suburb,
       pa.city_id,
       pa.county_id,
       pa.state_province_id,
       pa.zip_code,
       pa.country_region_id,
       pa.is_current,
       l1.name city,
       l4.name county,
       l2.name state,
       l3.name country,
       pd.party_type_csv,
       pd.is_business_unit,
       pd.is_branch,
       pd.is_direct_customer,
       pd.is_vendor,
       pd.is_bank,
       pd.is_insurance,
       pd.is_manufacturer,
       pd.is_courier,
	pd.is_searches_agent,
       pd.is_tax_authority,
       pd.is_end_customer,
       pd.is_broker,
       pd.is_dealer,
       pd.is_registration_authority,
       pd.is_funder,
       pd.is_permit_holder,
		 pd.is_repossession_agency,
		 pd.is_life_company,
       pd.is_medical_assessor
from party p
    inner join party_det pd on p.party_id = pd.party_id
    inner join party_address pa on pd.curr_street_address_id = pa.party_address_id
    inner join location l1 on l1.location_id = pa.city_id
    inner join location l2 on l2.location_id = pa.state_province_id
    inner join location l3 on l3.location_id = pa.country_region_id
    inner join location l4 on l4.location_id = pa.county_id
/
CREATE OR REPLACE FORCE VIEW axvw_party_address_curr_mail as
select p.party_id,
       pa.party_address_id,
       pa.address_type,
       pa.street,
       pa.suburb,
       pa.city_id,
       pa.county_id,
       pa.state_province_id,
       pa.zip_code,
       pa.country_region_id,
       pa.is_current,
       l1.name city,
       l4.name county,
       l2.name state,
       l3.name country,
       pd.party_type_csv,
       pd.is_business_unit,
       pd.is_branch,
       pd.is_direct_customer,
       pd.is_vendor,
       pd.is_bank,
       pd.is_insurance,
       pd.is_manufacturer,
       pd.is_tax_authority,
       pd.is_end_customer,
       pd.is_broker,
       pd.is_dealer,
       pd.is_registration_authority,
       pd.is_funder,
       pd.is_permit_holder,
	pd.is_repossession_agency,
	pd.is_courier,
	pd.is_searches_agent,
	pd.is_life_company,
       pd.is_medical_assessor
from party p
    inner join party_det pd on p.party_id = pd.party_id
    inner join party_address pa on pd.curr_mail_address_id = pa.party_address_id
    inner join location l1 on l1.location_id = pa.city_id
    inner join location l2 on l2.location_id = pa.state_province_id
    inner join location l3 on l3.location_id = pa.country_region_id
    inner join location l4 on l4.location_id = pa.county_id
/

CREATE OR REPLACE FORCE VIEW axvw_party_contacts AS
SELECT	p.party_id,
			p.party_no,
			pc.party_contact_id,
			pc.contact_type,
			pc.relationship,
			pc.is_primary_contact,
			cp.party_id contact_party_id,
         cp.party_no contact_party_no,
         cp.name contact_party_name,
         cp.reference contact_party_reference,
         pc.stamp
FROM		party p
			INNER JOIN party_contact pc ON p.party_id = pc.party_id
			INNER JOIN party cp ON pc.contact_id = cp.party_id
/

CREATE OR REPLACE FORCE VIEW axvw_party_view_curr_mail_addr AS
SELECT
	p.party_id,
	p.party_no,
	p.name,
	p.first_names,
	p.is_active,
	p.party_type,
	'' party_type_name,
	p.phone_home,
	p.phone_business,
	p.phone_mobile,
	a.address_type,
	a.street,
	a.suburb,
	a.city,
	a.county,
	a.state,
	a.zip_code,
	a.country,
	p.stamp
from party p inner join axvw_party_address_curr_mail a on p.party_id = a.party_id
/


CREATE OR REPLACE FORCE VIEW axvw_party_view_int AS
SELECT
    a.party_id,
    a.party_no,
    a.name,
    CASE WHEN a.title not in (0,4499) THEN l.value ELSE N' ' END title,
    a.first_names,
    a.ext_name,
    a.owner_id,
    owner_party.ext_name owner_ext_name,
    a.party_business_unit_id business_unit_id,
    a.branch_id,
    a.is_active,
    a.business_individual,
    l1.value business_individual_value,
    a.party_type,
    b.party_type_csv,
    a.phone_home,
    a.phone_business,
    a.phone_mobile,
    b.address_type,
    b.street,
    b.suburb,
    b.city_id city_id,
    b.city,
    b.county_id county_id,
    b.county,
	b.state_province_id state_province_id,
    b.state,
    b.zip_code,
    b.country_region_id country_region_id,
    b.country,
    l2.value upload_status,
    cp.status upload_status_enum,
    xl.value party_status,
    a.input_dt,
    b.is_business_unit,
    b.is_branch,
    b.is_direct_customer,
    b.is_vendor,
    b.is_bank,
    b.is_insurance,
    b.is_manufacturer,
    b.is_tax_authority,
    b.is_end_customer,
    b.is_broker,
    b.is_dealer,
    b.is_registration_authority,
    b.is_funder,
    b.is_permit_holder,
    b.is_repossession_agency,
    b.is_courier,
    b.is_searches_agent,
    b.is_life_company,
    b.is_medical_assessor,
    u1.name input_user,
    u2.name last_saved_by,
    i.name industry_name,
    i.code industry_code,
    a.do_not_solicit,
    p_acc_mgr.ext_name account_mgr,
    l3.value primary_contact_type,
    pcp.invoice_name primary_contact_inv_name,
    pcp.ext_name primary_contact_ext_name,
    pcp.phone_business primary_contact_phone,
    a.is_on_stoplist,
    a.reference,
    a.upload_batch_id,
    a.upload_package_id,
    l5.value is_tax_exempt,
    a.tax_exempt_certificate,
    a.tax_exempt_certificate_eff_dt,
    l4.value entity_type,
    a.email_home,
    a.email_business,
	a.job_title,
	a.national_no,
	a.business_ref_no,
	a.business_ref_no2,
	a.business_ref_no3,
    coll_party.name coll_officer,
    coll_party.ext_name coll_officer_ext_name,
    coll_party.first_names coll_officer_first_name,
    coll_party.middle_name coll_officer_middle_name,
    coll_party.phone_business coll_officer_phone_business,
    l6.value coll_officer_title,
    a.trading_as,
    a.stamp
FROM
    party a inner JOIN axvw_party_address_curr_street b ON a.party_id = b.party_id,
    party owner_party,
    lookupset l,    
    lookupset l1,
    contract_package cp,
    lookupset l2,
    xt_lookupset xl,
    ax_user u1,
    ax_user u2,
    industry i,
    party p_acc_mgr,
    ax_user u_p_acc_mgr,
    party_det pd,
    party_contact pc,
    party pcp,
    lookupset l3,
    lookupset l4,
    lookupset l5,
    party coll_party,
    ax_user coll_user,
    lookupset l6
WHERE
    a.owner_id = owner_party.party_id AND
    a.title = l.lookupset_id AND
    a.business_individual = l1.lookupset_id AND
    a.is_portfolio = 0 AND
    a.contract_package_id = cp.contract_package_id AND
    cp.status = l2.lookupset_id AND
    a.party_status = xl.xt_lookupset_id AND
    a.is_deleted = 0 AND
    a.input_user_id = u1.ax_user_id AND
    a.last_saved_by_id = u2.ax_user_id AND
    a.industry_id = i.industry_id AND
    u_p_acc_mgr.ax_user_id = a.account_manager_id AND
    p_acc_mgr.party_id = u_p_acc_mgr.external_party_id AND
    pd.party_id = a.party_id AND
    pc.party_contact_id = pd.primary_party_contact_id AND
    pcp.party_id = pc.contact_id AND
    l3.lookupset_id = pc.contact_type AND
    l4.lookupset_id = a.entity_type AND
    l5.lookupset_id = a.is_tax_exempt AND
    coll_user.ax_user_id = a.collections_user_id AND
    coll_user.external_party_id = coll_party.party_id AND
    coll_party.title = l6.lookupset_id
/
CREATE OR REPLACE FORCE VIEW  axvw_party_view AS
SELECT distinct
    a.party_id,
    a.party_no,
    a.name,
    a.title,
    a.first_names,
    a.ext_name,
    a.owner_id,
    a.owner_ext_name,
    a.business_unit_id,
    a.branch_id,
    a.is_active,
    a.business_individual,
    a.business_individual_value,
    a.party_type,
    a.party_type_csv party_type_name,
    a.phone_home,
    a.phone_business,
    a.phone_mobile,
    a.address_type,
    a.street,
    a.suburb,
    a.city,
    a.county,
    a.state state_province,
    a.zip_code,
    a.country country_region,
    a.is_business_unit,
    a.is_branch,
    a.is_direct_customer,
    a.is_vendor,
    a.is_bank,
    a.is_insurance,
    a.is_manufacturer,
    a.is_tax_authority,
    a.is_end_customer,
    a.is_broker,
    a.is_dealer,
    a.is_registration_authority,
    a.is_funder,
    a.is_permit_holder,
    a.is_repossession_agency,
    a.is_courier,
    a.is_searches_agent,
    a.is_life_company,
    a.is_medical_assessor,
    a.upload_status,
    a.upload_status_enum,
    a.party_status,
    a.input_dt,
    a.input_user,
    a.last_saved_by,
    a.industry_name,
    a.industry_code,
    a.do_not_solicit,
    a.account_mgr,
    a.primary_contact_type,
    a.primary_contact_inv_name,
    a.primary_contact_ext_name,
    a.primary_contact_phone,
    a.is_on_stoplist,
    a.reference,
    a.upload_batch_id,
    a.upload_package_id,
    a.is_tax_exempt,
    a.tax_exempt_certificate,
    a.tax_exempt_certificate_eff_dt,
    a.entity_type,
    a.email_home,
    a.email_business,
	a.job_title,
	a.national_no,
	a.business_ref_no,
	a.business_ref_no2,
	a.business_ref_no3,
    a.coll_officer,
    a.coll_officer_ext_name,
    a.coll_officer_first_name,
    a.coll_officer_middle_name,
    a.coll_officer_phone_business,
    a.coll_officer_title,
    a.trading_as,
    a.stamp
FROM
    axvw_party_view_int a
/
CREATE OR REPLACE FORCE VIEW axvw_portfolio_trans_view as
select
    fc.facility_id, /* --> facility_contract.facility_id */
    c.contract_id, /* --> contract.contract_id */
    c.reference, /* --> contract.reference */
    c.business_unit_id, /* --> contract.business_unit_id */
    bu.name business_unit, /* --> business_unit.name */
    c.cparty_id, /* --> contract.cparty_id */
    case WHEN save_status = 2200 THEN cast(1 AS NUMBER) ELSE cast(0 AS NUMBER) END is_draft,
    fc.approval_status, /* --> facility_contract.approval_status */
    fc.originator_approval_status, /* --> facility_contract.originator_approval_status */
    CASE WHEN c.is_active = 1 THEN c.amt_financed ELSE 0 END amt_financed,
    c.input_dt, /* --> contract.input_dt */
    c.mature_dt1 maturity_dt, /* --> contract.mature_dt1 */
    c.interest_rate, /* --> contract.interest_rate */
    c.fixed_floating, /* --> contract.fixed_floating */
    c.term, /* --> contract.term */
    c.stamp,   /* --> contract.stamp */
    ccy.code ccy, /* --> currency.code */
    c.product_style, /* --> contract.product_style */
    c.product_id, /* --> contract.product_id */
    pr.name product, /* --> product.name */
    p.name customer_vendor, /* --> party.name */
    pa.account_no party_account_no, /* --> party_account.account_no */
    l1.value fixed_floating_value, /* --> lookupset.value */
    l4.value installment_freq_value, /* --> lookupset.value */
    l5.value contract_state_value,   /* --> lookupset.value */
    l6.value credit_state_value, /* --> lookupset.value */
    l7.value port_amt_financed_formula_val /* --> lookupset.value */
 from
    party p,
    party bu,
    party_account pa,
    product pr,
    lookupset l1,
    lookupset l4,
    lookupset l5,
    lookupset l6,
    lookupset l7,
    currency ccy,
    --left outer join between portfolioXcontract and facility_contract
    --to return all contracts and facility contracts from a portfolio
    ((portfolio pf inner join contract c on pf.business_unit_id = c.business_unit_id
      inner join contract_det cd on c.contract_id = cd.contract_id)
    left outer join facility_contract fc on fc.contract_id = c.contract_id)
where
    c.cparty_id = p.party_id AND
    c.business_unit_id = bu.party_id AND
    c.party_account_id = pa.party_account_id AND
    c.product_id = pr.product_id AND
    c.currency_id = ccy.currency_id AND
    c.fixed_floating = l1.lookupset_id AND
    c.installment_frequency = l4.lookupset_id AND
    c.contract_state = l5.lookupset_id AND
    c.credit_state = l6.lookupset_id AND
    cd.portfolio_amt_financed_formula = l7.lookupset_id AND
    c.contract_id > 0
/
CREATE OR REPLACE FORCE VIEW AXVW_NOTE_VIEW as
SELECT n.note_id,
       n.party_id,
       p.name party_name,
       p.ext_name party_ext_name,
       p.first_names party_first_name,
       p.middle_name party_middle_name,
       p.party_no,
       pa.account_no party_account,
       pa.party_account_id party_account_id,
       n.activity_type,
       lu.value activity_type_value,
       activity_area,
       lu2.value activity_area_value,
       activity_dt,
       n.input_dt,
       u.name user_name,
       n.subject,
       n.comments,
       n.contract_id,
       'VONote' external_object_type,
       n.note_id external_object_id,
       n.task_id,
       t.subject task_subject,
       n.flow_id,
       n.bank_flow_id,
       (SELECT count(*) FROM note_attachment WHERE note_id = n.note_id) is_attachment,
       n.asset_hdr_id,
       ah.name asset_hdr_name,
       n.doc_id,
       n.orig_asset_hdr_id,
       n.contract_restructure_id,
       0 invoice_id,
       n.hidden_links,
       n.is_manual,
       n.credit_line_id,
       cl.name credit_line_name,
       n.custom_action_id,
       ca.code custom_action_code,
       n.portfolio_id,
       n.bank_statement_det_id,
       n.bank_flow_batch_no,
       n.opportunity_id,
       r.value response_value,
       sr.value sub_response_value,
       n.stamp
FROM   note n, party p, party_account pa, lookupset lu, lookupset lu2, ax_user u, doc d, credit_line cl, custom_action ca, portfolio po,
       asset_hdr ah, xt_lookupset r, xt_lookupset sr, task t
WHERE  n.party_id = p.party_id
   AND n.party_account_id = pa.party_account_id
   AND n.activity_type = lu.lookupset_id
   AND n.activity_area = lu2.lookupset_id
   AND n.user_id = u.ax_user_id
   AND n.doc_id = d.doc_id
   AND d.invoice_id = 0
   AND n.credit_line_id = cl.credit_line_id
   AND n.custom_action_id = ca.custom_action_id
   AND n.portfolio_id = po.portfolio_id
   AND n.asset_hdr_id = ah.asset_hdr_id
   AND n.response_id = r.xt_lookupset_id 
   AND n.sub_response_id = sr.xt_lookupset_id
   AND n.task_id = t.task_id
UNION ALL
SELECT 0 note_id,
       c.cparty_id party_id,
       p.name party_name,
       p.ext_name party_ext_name,
       p.first_names party_first_name,
       p.middle_name party_middle_name,
       p.party_no,
       pa.account_no party_account,
       pa.party_account_id party_account_id,
       8006 activity_type,
       lu1.value activity_type_value,
       8104 activity_area,
       lu2.value activity_area_value,
       c.input_dt activity_dt,
       c.input_dt,
       u.name user_name,
       CASE
       WHEN c.save_status = 2200 THEN lu_new_draft_contract.value || TO_NCHAR(contract_id) || N' - ' || pr.name
       ELSE lu_new_contract.value || TO_NCHAR(contract_id) || N' - ' || pr.name
       END subject,
       TO_CLOB(N' ') comments,
       c.contract_id,
       'VOContract' external_object_type,
       c.contract_id external_object_id,
       0 task_id,
       N' ' task_subject,
       0 flow_id,
       0 bank_flow_id,
       0 is_attachment,
       0 asset_hdr_id,
       N' ' asset_hdr_name,
       0 doc_id,
       0 orig_asset_hdr_id,
       0 contract_restructure_id,
       0 invoice_id,
       N' ' hidden_links,
       0 is_manual,
       0 credit_line_id,
       lu_none.value credit_line_name,
       0 custom_action_id,
       lu_none.value custom_action_code,
       0 portfolio_id,
       0 bank_statement_det_id,
       0 bank_flow_batch_no,
       0 opportunity_id,
       N' ' response_value,
       N' ' sub_response_value,
       c.stamp
FROM   contract c, product pr, party p, party_account pa, ax_user u, lookupset lu1, lookupset lu2, lookupset lu_new_draft_contract, lookupset lu_new_contract, lookupset lu_none
WHERE  c.is_active = 1
   AND c.contract_id >= 0
   AND c.contract_type != 2300
   AND c.product_id = pr.product_id
   AND c.cparty_id = p.party_id
   AND c.party_account_id = pa.party_account_id
   AND c.input_user_id = u.ax_user_id
   AND lu1.lookupset_id = 8006
   AND lu2.lookupset_id = 8104
   AND lu_new_draft_contract.lookupset_id = 46600
   AND lu_new_contract.lookupset_id = 46601
   AND lu_none.lookupset_id = 0
UNION ALL
SELECT 0 note_id,
       qc.cparty_id party_id,
       p.name party_name,
       p.ext_name party_ext_name,
       p.first_names party_first_name,
       p.middle_name party_middle_name,
       p.party_no,
       pa.account_no party_account,
       pa.party_account_id party_account_id,
       8007 activity_type,
       lu1.value activity_type_value,
       8104 activity_area,
       lu2.value activity_area_value,
       qc.input_dt activity_dt,
       qc.input_dt,
       u.name user_name,
       lu_new_quote.value || TO_NCHAR(contract_id) || N' - ' || pr.name subject,
       TO_CLOB(N' ') comments,
       qc.contract_id,
       'VOContract' external_object_type,
       qc.contract_id external_object_id,
       0 task_id,
       N' ' task_subject,
       0 flow_id,
       0 bank_flow_id,
       0 is_attachment,
       0 asset_hdr_id,
       N' ' asset_hdr_name,
       0 doc_id,
       0 orig_asset_hdr_id,
       0 contract_restructure_id,
       0 invoice_id,
       N' ' hidden_links,
       0 is_manual,
       0 credit_line_id,
       lu_none.value credit_line_name,
       0 custom_action_id,
       lu_none.value custom_action_code,
       0 portfolio_id,
       0 bank_statement_det_id,
       0 bank_flow_batch_no,
       0 opportunity_id,
       N' ' response_value,
       N' ' sub_response_value,
       qc.stamp
FROM   contract qc, product pr, party p, party_account pa, ax_user u, lookupset lu1, lookupset lu2, lookupset lu_new_quote, lookupset lu_none
WHERE  qc.contract_id >= 0
   AND qc.contract_type = 2300
   AND qc.product_id = pr.product_id
   AND qc.cparty_id = p.party_id
   AND qc.party_account_id = pa.party_account_id
   AND qc.input_user_id = u.ax_user_id
   AND lu1.lookupset_id = 8007
   AND lu2.lookupset_id = 8104
   AND lu_new_quote.lookupset_id = 46602
   AND lu_none.lookupset_id = 0
UNION ALL
SELECT pn.note_id,
       pa.party_id party_id,
       p.name party_name,
       p.ext_name party_ext_name,
       p.first_names party_first_name,
       p.middle_name party_middle_name,
       p.party_no,
       pa.account_no party_account,
       pa.party_account_id party_account_id,
       8005 activity_type,
       lu1.value activity_type_value,
       8104 activity_area,
       lu2.value activity_area_value,
       pn.activity_dt,
       pn.input_dt,
       N'System' user_name,
       lu_invoice.value || i.invoice_no || N' output' subject,
       TO_CLOB(N' ') comments,
       pn.contract_id,
       'VOInvoice' external_object_type,
       i.invoice_id external_object_id,
       0 task_id,
       N' ' task_subject,
       0 flow_id,
       0 bank_flow_id,
       0 is_attachment,
       0 asset_hdr_id,
       N' ' asset_hdr_name,
       pn.doc_id,
       0 orig_asset_hdr_id,
       0 contract_restructure_id,
       i.invoice_id,
       pn.hidden_links,
       pn.is_manual,
       0 credit_line_id,
       lu_none.value credit_line_name,
       0 custom_action_id,
       lu_none.value custom_action_code,
       0 portfolio_id,
       0 bank_statement_det_id,
       0 bank_flow_batch_no,
       0 opportunity_id,
       r.value response_value,
       r.value sub_response_value,
       i.stamp
FROM   invoice i, doc d, note pn, party_account pa, party p, lookupset lu1, lookupset lu2, lookupset lu_invoice, lookupset lu_none,
       xt_lookupset r, xt_lookupset sr
WHERE  i.invoice_id > 0
   AND i.invoice_id = d.invoice_id
   AND d.doc_id = pn.doc_id
   AND pn.doc_id > 0
   AND i.party_account_id = pa.party_account_id
   AND pa.party_id = p.party_id
   AND lu1.lookupset_id = 8005
   AND lu2.lookupset_id = 8104
   AND lu_invoice.lookupset_id = 46603
   AND lu_none.lookupset_id = 0
   AND r.xt_lookupset_id = pn.response_id
   AND sr.xt_lookupset_id = pn.sub_response_id
UNION ALL
SELECT 0 note_id,
       pa.party_id party_id,
       p.name party_name,
       p.ext_name party_ext_name,
       p.first_names party_first_name,
       p.middle_name party_middle_name,
       p.party_no,
       pa.account_no party_account,
       pa.party_account_id party_account_id,
       8004 activity_type,
       lu1.value activity_type_value,
       8104 activity_area,
       lu2.value activity_area_value,
       s.statement_run_dt activity_dt,
       s.statement_run_dt input_dt,
       N'System' user_name,
       lu_new_statement.value || TO_NCHAR(s.statement_no) subject,
       TO_CLOB(N' ') comments,
       0 contract_id,
       'VOStatement' external_object_type,
       s.statement_id external_object_id,
       0 task_id,
       N' ' task_subject,
       0 flow_id,
       0 bank_flow_id,
       0 is_attachment,
       0 asset_hdr_id,
       N' ' asset_hdr_name,
       0 doc_id,
       0 orig_asset_hdr_id,
       0 contract_restructure_id,
       0 invoice_id,
       N' ' hidden_links,
       0 is_manual,
       0 credit_line_id,
       lu_none.value credit_line_name,
       0 custom_action_id,
       lu_none.value custom_action_code,
       0 portfolio_id,
       0 bank_statement_det_id,
       0 bank_flow_batch_no,
       0 opportunity_id,
       N' ' response_value,
       N' ' sub_response_value,
       s.stamp
FROM   statement s, party_account pa, party p, lookupset lu1, lookupset lu2, lookupset lu_new_statement, lookupset lu_none
WHERE  s.statement_id > 0
   AND s.party_account_id = pa.party_account_id
   AND pa.party_id = p.party_id
   AND lu1.lookupset_id = 8004
   AND lu2.lookupset_id = 8104
   AND lu_new_statement.lookupset_id = 46604
   AND lu_none.lookupset_id = 0
/
CREATE OR REPLACE FORCE VIEW axvw_coll_task
AS
SELECT 	v.party_id,
	v.party_account_id,
	v.currency_id,
	v.party_no,
	v.party_ext_name,
	v.party_account,
	v.party_account_name,
	v.ccy,
	t.task_id,
	t.subject,
	t.due_dt,
	t.reminder_dt,
	t.is_reminder_required,
	lu1.value priority_value,
	lu2.value status_value,
	u2.name owner_user,
	u1.name assigned_to_user,
	u1.ext_name assigned_to_user_ext_name,
	count(*) bucket_total_count,
	sum (case overdue_amt1 WHEN 0 THEN 0 ELSE 1 END) bucket_1_count,
	sum (case overdue_amt2 WHEN 0 THEN 0 ELSE 1 END) bucket_2_count,
	sum (case overdue_amt3 WHEN 0 THEN 0 ELSE 1 END) bucket_3_count,
	sum (case overdue_amt4 WHEN 0 THEN 0 ELSE 1 END) bucket_4_count,
	sum (case overdue_amt5 WHEN 0 THEN 0 ELSE 1 END) bucket_5_count,
	sum (case overdue_amt6 WHEN 0 THEN 0 ELSE 1 END) bucket_6_count,
	sum(overdue_amt0) bucket_total,
	sum(overdue_amt1) bucket_1,
	sum(overdue_amt2) bucket_2,
	sum(overdue_amt3) bucket_3,
	sum(overdue_amt4) bucket_4,
	sum(overdue_amt5) bucket_5,
	sum(overdue_amt6) bucket_6,
	0 stamp
FROM	axvw_overdue_view v
INNER 	JOIN task t ON t.party_id = v.party_id
INNER 	JOIN ax_user u1 ON t.assigned_to_user_id = u1.ax_user_id
INNER 	JOIN ax_user u2 ON t.owner_user_id = u2.ax_user_id,
	lookupset lu1, lookupset lu2
WHERE 	t.task_type = 6205 and
	--t.status != 6402 and -- exclude completed tasks
	lu1.lookupset_id = t.priority and
	lu2.lookupset_id = t.status and
	v.collection_state = 14802 and v.overdue_amt0 > 0
GROUP BY v.party_account_id,
	v.party_id,
	v.currency_id,
	v.party_no,
	v.party_ext_name,
	v.party_account,
	v.party_account_name,
	v.ccy,
	t.task_id,
	t.subject,
	t.due_dt,
	t.reminder_dt,
   t.is_reminder_required,
	lu1.value,
	lu2.value,
	u2.name,
	u1.name,
	u1.ext_name
/
CREATE OR REPLACE FORCE VIEW axvw_coll_exp_flow_internal
AS
SELECT  paf.flow_id,
	paf.contract_id,
	paf.purchase_invoice_id,
	paf.party_account_id,
	paf.currency_id,
	paf.amt_gross,
	paf.amount,
	paf.amt_matched,
	case WHEN paf.amt_matched = paf.amt_gross THEN 15500
	     WHEN paf.amt_matched != 0 THEN 15501 ELSE 15502 END match_status,
	paf.statement_id,
	paf.statement_no,
	paf.invoice_id,
	paf.invoice_no,
	paf.actual_dt,
	paf.expected_dt,
	paf.allocation_dt,
	paf.flow_type,
	paf.installment_no,
	paf.batch_no,
	paf.custom_flow_hdr_id,
	paf.collection_state,
	paf.payment_ref,
	nvl((SELECT MAX(bfm.matched_dt)
         FROM bank_flow_match bfm
        WHERE bfm.flow_id = paf.flow_id
          AND bfm.flow_id = bfm.recovery_flow_id
          AND bfm.is_deleted = 0), to_date('19000101', 'YYYYMMDD')) matched_dt,
	case WHEN paf.flow_type = 1003 THEN 1
	     WHEN f.contract_id IS NULL THEN 0 ELSE 1
	END is_installment,
	case WHEN c.installment_frequency = 2600 THEN 16
	     WHEN c.installment_frequency = 2601 THEN 32
	     WHEN c.installment_frequency = 2603 THEN 92
	     WHEN c.installment_frequency = 2604 THEN 182
	     WHEN c.installment_frequency = 2605 THEN 367
	     WHEN c.installment_frequency = 2606 THEN 0
	END days_in_period,
	paf.release_dt,
	case WHEN paf.exclude_from_late_fees = 0 and paf.exclude_from_overdue_interest = 0 THEN 19700
		  WHEN paf.exclude_from_late_fees = 1 and paf.exclude_from_overdue_interest = 0 THEN 19701
		  WHEN paf.exclude_from_late_fees = 0 and paf.exclude_from_overdue_interest = 1 THEN 19702
	ELSE 19703
	END penalty_exclusion_status,
	paf.penalty_grace_days,
	paf.in_recovery,
	paf.input_dt input_dt,
	cfh.timing timing,
	cfh.purpose purpose,
	res.name loss_reserve_name,
   paf.status,
	paf.reversal_status,
	paf.nett_no
FROM	axvw_party_account_flow paf
INNER JOIN custom_flow_hdr cfh ON cfh.custom_flow_hdr_id = paf.custom_flow_hdr_id
INNER JOIN contract c ON c.contract_id = paf.contract_id
INNER JOIN reserve res ON res.reserve_id = c.reserve_id
--if custom flow is on installment date and of installment timing then it will be groupped with installment
LEFT OUTER JOIN flow f
	ON paf.expected_dt = f.expected_dt
	AND paf.installment_no = f.installment_no
	AND paf.contract_id = f.contract_id
	AND paf.flow_type != f.flow_type
	AND paf.flow_type = 1010
	AND (cfh.timing = 14003 OR cfh.timing = 14011 OR (cfh.timing = 14004 AND cfh.purpose = 14210))
	AND f.flow_type = 1003
	AND f.is_cash = 1
	AND f.status not in (2099, 2104, 2105) --exlude projected and written off
	AND f.contract_id >= 0
WHERE	paf.flow_source = 'C'
UNION
SELECT  f.flow_id,
	f.contract_id,
	f.purchase_invoice_id,
	f.party_account_id,
	f.currency_id,
	f.amt_gross,
	f.amount,
	f.amt_matched,
	case WHEN f.amt_matched = f.amt_gross THEN 15500
	     WHEN f.amt_matched != 0 THEN 15501 ELSE 15502 END match_status,
	f.statement_id,
	s.statement_no,
	i.invoice_id,
	i.invoice_no,
	f.actual_dt,
	f.expected_dt,
	f.input_dt allocation_dt,
	f.flow_type,
	f.installment_no,
	0 batch_no,
	f.custom_flow_hdr_id,
	f.collection_state,
	f.payment_ref,
	nvl((SELECT MAX(bfm.matched_dt)
         FROM bank_flow_match bfm
        WHERE bfm.flow_id = f.flow_id
          AND bfm.flow_id = bfm.recovery_flow_id
          AND bfm.is_deleted = 0), to_date('19000101', 'YYYYMMDD')) matched_dt,
	case WHEN f.flow_type = 1003 THEN 1
	     WHEN f2.contract_id IS NULL THEN 0 ELSE 1
	END is_installment,
 	case WHEN c.installment_frequency = 2600 THEN 16
	     WHEN c.installment_frequency = 2601 THEN 32
	     WHEN c.installment_frequency = 2603 THEN 92
	     WHEN c.installment_frequency = 2604 THEN 182
	     WHEN c.installment_frequency = 2605 THEN 367
	     WHEN c.installment_frequency = 2606 THEN 0
	END days_in_period,
	f.release_dt,
 	case WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 0 THEN 19700
	     WHEN f.exclude_from_late_fees = 1 and f.exclude_from_overdue_interest = 0 THEN 19701
	     WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 1 THEN 19702
	ELSE 19703
	END penalty_exclusion_status,
	f.penalty_grace_days + fm.collection_grace_period penalty_grace_days,
	f.in_recovery,
	f.input_dt,
	cfh.timing timing,
	cfh.purpose purpose,
	res.name loss_reserve_name,
	f.status,
	f.reversal_status,
	f.nett_no
FROM 	flow f
INNER JOIN custom_flow_hdr cfh ON cfh.custom_flow_hdr_id = f.custom_flow_hdr_id
INNER JOIN contract c ON c.contract_id = f.contract_id
INNER JOIN reserve res ON res.reserve_id = c.reserve_id
INNER JOIN invoice i ON i.invoice_id = f.invoice_id
INNER JOIN statement s ON f.statement_id = s.statement_id
INNER JOIN flow_method fm ON f.flow_method_id = fm.flow_method_id
        AND f.is_shadow_copy = 0
	AND f.statement_id = s.statement_id
	AND f.invoice_id = i.invoice_id
	AND f.flow_method_id = fm.flow_method_id
	AND f.exclude_from_account_bal = 1 --recovery flows
	AND f.contract_id >= 0
LEFT OUTER JOIN flow f2
	ON  f.expected_dt = f2.expected_dt
	AND f.installment_no = f2.installment_no
	AND f.contract_id = f2.contract_id
	AND f.flow_type != f2.flow_type
	AND f.flow_type = 1010
	AND (cfh.timing = 14003 OR cfh.timing = 14011 OR (cfh.timing = 14004 AND cfh.purpose = 14210))
	AND f2.flow_type = 1003
	AND f2.is_cash = 1
	AND f2.status not in (2099, 2104, 2105) --exclude projected and written off
	AND f2.contract_id >= 0
/
CREATE OR REPLACE FORCE VIEW axvw_coll_exp_flow
AS
SELECT 	flow_id,
	party_account_id,
	currency_id,
	contract_id,
	purchase_invoice_id,
	expected_dt,
	installment_no,
	MAX(collection_state) collection_state,
	MAX(matched_dt) matched_dt,
	case WHEN MAX(collection_state) = 14800 THEN 0 ELSE 1 END is_due,
	sum(amt_gross) amt_total,

	case when is_installment=1 then
		sum(case when flow_type = 1003 then amount else 0 end)
	else
		sum(case when flow_type != 1023 then amount else 0 end)
	end amt_rent,	--Installment

	case when is_installment=1 then
		sum(case when flow_type = 1010 then amount else 0 end)
	else
		0
	end amt_other, --Custom Flow

	sum(case when flow_type = 1023 then amount else 0 end) amt_purchase_invoice,	--Purchase Invoice
	sum(amt_gross) - sum(amount) amt_tax,
	sum(amt_matched) amt_matched,
	case WHEN max(expected_dt) > axsp_dateonly(axsp_get_datetime()) THEN 0
		ELSE cast(axsp_dateonly(axsp_get_datetime()) - MAX(expected_dt) AS int) END days_over,

	case when is_installment=1 then
		MIN(flow_type)
	else
		case when is_installment=0 then flow_type else 1 end
	end flow_type,

	case when is_installment=1 then
		0
	else
		case when is_installment=0 then custom_flow_hdr_id else 1 end
	end custom_flow_hdr_id,

	MAX(days_in_period) days_in_period,
	release_dt,
	penalty_exclusion_status,
	penalty_grace_days,
	in_recovery,
	invoice_no,
	input_dt,
	timing,
   purpose,
   loss_reserve_name,
   match_status,
   status,
   reversal_status,
	nett_no,
   0 stamp
FROM 	axvw_coll_exp_flow_internal
GROUP BY
	is_installment,
	flow_id,
	party_account_id,
	currency_id,
	contract_id,
	purchase_invoice_id,
	expected_dt,
	installment_no,

	case when is_installment=0 then flow_type else 1 end,
	case when is_installment=0 then custom_flow_hdr_id else 1 end,

	release_dt,
	penalty_exclusion_status,
	penalty_grace_days,
	in_recovery,
	invoice_no,
   input_dt,
	timing,
   purpose,
   loss_reserve_name,
   match_status,
   status,
   reversal_status,
	nett_no
HAVING 	sum(amt_gross) != 0
/
CREATE OR REPLACE FORCE VIEW axvw_cash_mgmt_view_grp
AS
SELECT     f.flow_id, SUM(DISTINCT m.cashflow_match_status) flow_match_status
FROM         flow f LEFT OUTER JOIN
                      bank_rec_match m ON f.flow_id = m.flow_id
GROUP BY f.flow_id
/
CREATE OR REPLACE FORCE VIEW axvw_cash_mgmt_view_base
AS
SELECT	f.flow_id,
		f.contract_id,
		case when f.purchase_invoice_id>0 then pinv.business_unit_id else c.business_unit_id end business_unit_id,
		c.suspension_state,
		case when f.purchase_invoice_id>0 then pinv.stamp else c.stamp end contract_stamp,
		case when f.purchase_invoice_id>0 then pinv.supplier_id else c.cparty_id end cparty_id,
		pa.party_id fparty_id,
		case WHEN f.amt_matched != f.amt_gross THEN cast(0 as NUMBER) ELSE cast(1 as NUMBER) END is_matched,
		f.installment_no,
		f.calc_dt,
		f.actual_dt,
		f.expected_dt,
		f.currency_id,
		f.amt_gross amount,
		f.amount amt_nett,
		f.amt_interest + f.amt_contingent_rental amt_interest,
		f.amt_principal,
		f.status,
		f.bank_account_id,
		f.nett_no,
		fm.payment_method payment_method,
		f.flow_method_id,
		sbf.status party_bankacc_status,
		f.party_account_id,
		case WHEN f.nett_no = 0 THEN 0 ELSE f.amt_gross_netted END netted_amount_gross,
		f.reversal_status,
		f.is_set,
		case WHEN collection_state != 14802 THEN 0 ELSE (f.amt_gross - f.amt_matched) END overdue_amount,
		f.collection_state,
		f.amt_matched,
		f.release_dt,
		f.settled_dt,
		f.rejected_dt,
		f.amt_matched_principal,
		f.amt_matched_interest,
		f.amt_matched_tax,
		f.exclude_from_account_bal,
		f.settle_count,
		f.split_no,
		f.invoice_id,
		f.tax_point_dt,
		f.bank_interface_run_id,
		f.in_recovery,
		case WHEN cfh.custom_flow_hdr_id != 0 and cfh.purpose = 14210 THEN
			 cast(1 as NUMBER) ELSE cast(0 as NUMBER) END is_recovery,
		f.amt_gross - f.amt_matched amt_unallocated,
		f.settlement_bank_info_id,
		f.grp_link_id,
		f.is_shadow_copy,
		f.purchase_invoice_id,
		c.settle_upto_dt,
		c.save_status,
		f.can_process,
		f.leg_no,
		f.custom_flow_link_no,
		f.image_no,
		f.flow_type,
		f.is_cash,
		f.custom_flow_hdr_id,
		f.flow_link_id,
		f.payment_confirmation_status,
		f.payment_confirmation_dt,
		f.payment_confirmation_user_id,
		f.payment_credited_dt,
		f.pp_nett_no,
		c.intercept_state,
		f.is_first_settlement_bank_info,
		f.stamp
FROM	flow f
		inner join contract c on c.contract_id = f.contract_id
		inner join flow_method fm on fm.flow_method_id = f.flow_method_id 
		inner join settlement_bank_info sbf on sbf.settlement_bank_info_id = f.settlement_bank_info_id
		inner join party_account pa on f.party_account_id = pa.party_account_id
		inner join custom_flow_hdr cfh on f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
		inner join purchase_invoice pinv on f.purchase_invoice_id = pinv.purchase_invoice_id

WHERE	f.is_cash = 1 AND
		f.status not in (2104, 2105) AND
		f.is_set = 7801 AND
		f.reversal_status IN (4200, 4202) AND
		f.is_shadow_copy = 0 AND
		c.contract_id >= 0
/
CREATE OR REPLACE FORCE VIEW axvw_cash_mgmt_view_int
AS
SELECT  f.flow_id,
        c.contract_id,
        c.business_unit_id,
        c.suspension_state,
        l9.value suspension_state_value,
        c.stamp contract_stamp,
        c.cparty_id,
        pa.party_id fparty_id,
        p1.party_no party_no,
        p1.name cparty,
        p1.ext_name party_ext_name,
        p1.first_names party_first_name,
        p1.middle_name party_middle_name,
        p5.name coll_officer,
        p5.ext_name coll_officer_ext_name,
        p5.first_names coll_officer_first_name,
        p5.middle_name coll_officer_middle_name,
        p5.phone_business coll_officer_phone_business,
        l12.value coll_officer_title,
        DATEDIFF('day', f.expected_dt, axsp_get_datetime()) days_overdue,
        case WHEN f.amt_matched != f.amt_gross THEN cast(0 as NUMBER) ELSE cast(1 as NUMBER) END is_matched,
        l7.value flow_direction_value,
        f.installment_no,
        case WHEN f.flow_type = 1010 THEN cfh.name ELSE l1.value END flow_type_value,
        f.calc_dt,
        f.actual_dt,
        f.expected_dt,
        ccy.code ccy,
        ccy.currency_id,
        f.amt_gross amount,
        f.amount amt_nett,
        f.amt_interest + f.amt_contingent_rental amt_interest,
        f.amt_principal,
        f.status,
        l2.value status_value,
        f.bank_account_id,
        case WHEN ob.code = 'None' or ob.branch_code = ' ' THEN ob.code ELSE ob.branch_code || ' ' || ob.code END bank_account,
        p2.name bank,
        p3.name beneficiary,
        f.nett_no,
        fm.ext_name flow_method,
        fm.payment_method payment_method,
        f.flow_method_id,
        case WHEN pb.code = 'None' or pb.branch_code = ' ' THEN pb.code ELSE pb.branch_code || ' ' || pb.code END party_bankacc,
        sbf.status party_bankacc_status,
        l8.value party_bankacc_status_value,
        p4.name party_bank,
        pr.name product,
        f.party_account_id,
        pa.account_no party_account,
        case WHEN f.nett_no = 0 THEN 0 ELSE f.amt_gross_netted END netted_amount_gross,
        case WHEN f.nett_no = 0 THEN 5901
                        WHEN (f.amt_gross_netted - f.amt_matched_netted) >= 0 THEN 5901 ELSE 5900 END netted_flow_direction,
        case WHEN f.nett_no = 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 5901)
                        WHEN (f.amt_gross_netted - f.amt_matched_netted) >= 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 5901)
                                                                                        ELSE (SELECT value FROM lookupset WHERE lookupset_id = 5900) END netted_flow_direction_value,
        f.reversal_status,
        l3.value reversal_status_value,
        f.is_set,
        l4.value is_set_value,
        l5.value contract_state_value,
		case WHEN f.collection_state = 14802 THEN (SELECT DISTINCT tb.name FROM overdues_timeband tb WHERE
			trunc(dt.server_dt)-f.expected_dt >= tb.start_day AND
			trunc(dt.server_dt)-f.expected_dt <= tb.end_day) ELSE N' ' END overdue_status,
		case WHEN collection_state = 14802 THEN (f.amt_gross - f.amt_matched) ELSE 0 END overdue_amount,
        f.collection_state,
        l6.value collection_state_value,
        f.amt_matched,
        case WHEN f.amt_matched = f.amt_gross THEN 15500
                        WHEN f.amt_matched != 0 THEN 15501 ELSE 15502 END match_status,
        case WHEN f.amt_matched = f.amt_gross THEN (SELECT value FROM lookupset WHERE lookupset_id = 15500)
                        WHEN f.amt_matched != 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 15501)
                                                                                ELSE (SELECT value FROM lookupset WHERE lookupset_id = 15502) END match_status_value,
        f.release_dt,
        f.settled_dt,
        f.rejected_dt,
        f.amt_matched_principal,
        f.penalty_grace_days + fm.collection_grace_period penalty_grace_days,
        f.amt_matched_interest,
        f.amt_matched_tax,
        f.exclude_from_account_bal,
        f.settle_count,
        case WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 0 THEN 19700
                        WHEN f.exclude_from_late_fees = 1 and f.exclude_from_overdue_interest = 0 THEN 19701
                        WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 1 THEN 19702
                        ELSE 19703 END penalty_exclusion_status,
        f.split_no,
        f.invoice_id,
        f.tax_point_dt,
        f.bank_interface_run_id,
        f.in_recovery,
        case WHEN cfh.custom_flow_hdr_id != 0 and cfh.purpose = 14210 THEN
                 cast(1 as NUMBER) ELSE cast(0 as NUMBER) END is_recovery,
        i.invoice_no,
        f.amt_gross - f.amt_matched amt_unallocated,
        sbf.settlement_bank_info_id,
        f.grp_link_id,
        f.is_shadow_copy,
        f.purchase_invoice_id,
        pinv.reference purchase_invoice_ref,
		c.settle_upto_dt,
		c.save_status,
		f.can_process,
		f.leg_no,
		f.custom_flow_link_no,
		f.image_no,
		f.flow_type,
		f.is_cash,
		f.custom_flow_hdr_id,
		f.flow_link_id,
		f.payment_confirmation_status,
		l10.value paymnt_confirmation_status_val,
		f.payment_confirmation_dt,
		f.payment_confirmation_user_id,
		f.payment_credited_dt,
		c.intercept_state,
		l11.value intercept_state_value,
		f.pp_nett_no,
		f.is_first_settlement_bank_info,
		ccf.reference contract_custom_flow_reference,
		ccf.grp_no contract_custom_flow_grp_no,
		ccf.frequency contract_custom_flow_frequency,
		f.stamp
		
FROM	flow f
		inner join contract c on c.contract_id = f.contract_id
		left outer join (select * from contract_custom_flow where link_no != 0) ccf on (f.custom_flow_link_no = ccf.link_no AND f.contract_id = ccf.contract_id AND f.custom_flow_hdr_id = ccf.custom_flow_hdr_id)
		inner join currency ccy on f.currency_id = ccy.currency_id
		inner join bankacc ob on f.bank_account_id = ob.bankacc_id  												-- our bankacc
		inner join settlement_bank_info sbf on sbf.settlement_bank_info_id = f.settlement_bank_info_id	-- settlement bank of the flow
		inner join party_bankacc pb on pb.bankacc_id = sbf.party_bankacc_id       								-- party bankacc inflow,
		inner join flow_method fm on fm.flow_method_id = f.flow_method_id
		inner join party_account pa on f.party_account_id = pa.party_account_id
		inner join party p1 on pa.party_id = p1.party_id  										-- party account party
		inner join party p2 on ob.bank_id = p2.party_id  										-- our bank party
		inner join party p3 on f.beneficiary_id = p3.party_id  								-- beneficiary party
		inner join party p4 on pb.bank_id = p4.party_id 										-- their bank party
		inner join ax_user u on u.ax_user_id = p1.collections_user_id						-- collection officer user
		inner join party p5 on u.external_party_id = p5.party_id								-- collection officer party		
		inner join lookupset l1 on f.flow_type = l1.lookupset_id 							-- flow type
		inner join lookupset l2 on f.status = l2.lookupset_id 								-- flow status
		inner join lookupset l3 on f.reversal_status = l3.lookupset_id 					-- flow reversal status
		inner join lookupset l4 on f.is_set = l4.lookupset_id 								-- flow is set
		inner join lookupset l5 on c.contract_state = l5.lookupset_id 						-- contract state
		inner join lookupset l6 on f.collection_state = l6.lookupset_id					-- collection state
		inner join lookupset l7 on sbf.flow_direction = l7.lookupset_id 					-- flow_direction_value
		inner join lookupset l8 on sbf.status = l8.lookupset_id								-- party_bankacc_status_value
		inner join lookupset l9 on c.suspension_state = l9.lookupset_id					-- suspension state value
		inner join lookupset l10 on f.payment_confirmation_status = l10.lookupset_id	-- manual payment confirmation status value
		inner join lookupset l11 on c.intercept_state = l11.lookupset_id					-- intercept state value
		inner join lookupset l12 on p5.title = l12.lookupset_id								-- collection officer title
		inner join product pr on c.product_id = pr.product_id		
		inner join custom_flow_hdr cfh on f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
		inner join invoice i on f.invoice_id = i.invoice_id
		inner join purchase_invoice pinv on f.purchase_invoice_id = pinv.purchase_invoice_id,
		axvw_get_datetime dt

WHERE	 c.transfer_type != 9002 AND
		(f.is_cash = 1 OR f.pp_nett_no > 0) AND
		f.status not in (2104, 2105) AND
		c.contract_id >= 0 AND
		f.purchase_invoice_id = 0
UNION ALL
SELECT  f.flow_id,
        pinv.contract_id,
        pinv.business_unit_id,
        c.suspension_state,
        l9.value suspension_state_value,
        pinv.stamp contract_stamp,
        pinv.supplier_id cparty_id,
        pa.party_id fparty_id,
        p1.party_no party_no,
        p1.name cparty,
        p1.ext_name party_ext_name,
        p1.first_names party_first_name,
        p1.middle_name party_middle_name,
        p5.name coll_officer,
        p5.ext_name coll_officer_ext_name,
        p5.first_names coll_officer_first_name,
        p5.middle_name coll_officer_middle_name,
        p5.phone_business coll_officer_phone_business,
        l12.value coll_officer_title,
        DATEDIFF('day', f.expected_dt, axsp_get_datetime()) days_overdue,
        case WHEN f.amt_matched != f.amt_gross THEN cast(0 as NUMBER) ELSE cast(1 as NUMBER) END is_matched,
        l7.value flow_direction_value,
        f.installment_no,
        case WHEN f.flow_type = 1010 THEN cfh.name ELSE l1.value END flow_type_value,
        f.calc_dt,
        f.actual_dt,
        f.expected_dt,
        ccy.code ccy,
        ccy.currency_id,
        f.amt_gross amount,
        f.amount amt_nett,
        f.amt_interest + f.amt_contingent_rental amt_interest,
        f.amt_principal,
        f.status,
        l2.value status_value,
        f.bank_account_id,
        case WHEN ob.code = 'None' or ob.branch_code = ' ' THEN ob.code ELSE ob.branch_code || ' ' || ob.code END bank_account,
        p2.name bank,
        p3.name beneficiary,
        f.nett_no,
        fm.ext_name flow_method,
        fm.payment_method payment_method,
        f.flow_method_id,
        case WHEN pb.code = 'None' or pb.branch_code = ' ' THEN pb.code ELSE pb.branch_code || ' ' || pb.code END party_bankacc,
        sbf.status party_bankacc_status,
        l8.value party_bankacc_status_value,
        p4.name party_bank,
        pr.name product,
        f.party_account_id,
        pa.account_no party_account,
        case WHEN f.nett_no = 0 THEN 0 ELSE f.amt_gross_netted END netted_amount_gross,
        case WHEN f.nett_no = 0 THEN 5901
                        WHEN (f.amt_gross_netted - f.amt_matched_netted) >= 0 THEN 5901 ELSE 5900 END netted_flow_direction,
        case WHEN f.nett_no = 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 5901)
                        WHEN (f.amt_gross_netted - f.amt_matched_netted) >= 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 5901)
                                                                                        ELSE (SELECT value FROM lookupset WHERE lookupset_id = 5900) END netted_flow_direction_value,
        f.reversal_status,
        l3.value reversal_status_value,
        f.is_set,
        l4.value is_set_value,
        l5.value contract_state_value,
		case WHEN f.collection_state = 14802 THEN (SELECT DISTINCT tb.name FROM overdues_timeband tb WHERE
			trunc(dt.server_dt)-f.expected_dt >= tb.start_day AND
			trunc(dt.server_dt)-f.expected_dt <= tb.end_day) ELSE N' ' END overdue_status,
		case WHEN collection_state = 14802 THEN (f.amt_gross - f.amt_matched) ELSE 0 END overdue_amount,
        f.collection_state,
        l6.value collection_state_value,
        f.amt_matched,
        case WHEN f.amt_matched = f.amt_gross THEN 15500
                        WHEN f.amt_matched != 0 THEN 15501 ELSE 15502 END match_status,
        case WHEN f.amt_matched = f.amt_gross THEN (SELECT value FROM lookupset WHERE lookupset_id = 15500)
                        WHEN f.amt_matched != 0 THEN (SELECT value FROM lookupset WHERE lookupset_id = 15501)
                                                                                ELSE (SELECT value FROM lookupset WHERE lookupset_id = 15502) END match_status_value,
        f.release_dt,
        f.settled_dt,
        f.rejected_dt,
        f.amt_matched_principal,
        f.penalty_grace_days + fm.collection_grace_period penalty_grace_days,
        f.amt_matched_interest,
        f.amt_matched_tax,
        f.exclude_from_account_bal,
        f.settle_count,
        case WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 0 THEN 19700
                        WHEN f.exclude_from_late_fees = 1 and f.exclude_from_overdue_interest = 0 THEN 19701
                        WHEN f.exclude_from_late_fees = 0 and f.exclude_from_overdue_interest = 1 THEN 19702
                        ELSE 19703 END penalty_exclusion_status,
        f.split_no,
        f.invoice_id,
        f.tax_point_dt,
        f.bank_interface_run_id,
        f.in_recovery,
        case WHEN cfh.custom_flow_hdr_id != 0 and cfh.purpose = 14210 THEN
                 cast(1 as NUMBER) ELSE cast(0 as NUMBER) END is_recovery,
        i.invoice_no,
        f.amt_gross - f.amt_matched amt_unallocated,
        sbf.settlement_bank_info_id,
        f.grp_link_id,
        f.is_shadow_copy,
        f.purchase_invoice_id,
        pinv.reference purchase_invoice_ref,
		c.settle_upto_dt,
		c.save_status,
		f.can_process,
		f.leg_no,
		f.custom_flow_link_no,
		f.image_no,
		f.flow_type,
		f.is_cash,
		f.custom_flow_hdr_id,
		f.flow_link_id,
		f.payment_confirmation_status,
		l10.value paymnt_confirmation_status_val,
		f.payment_confirmation_dt,
		f.payment_confirmation_user_id,
		f.payment_credited_dt,
		c.intercept_state,
		l11.value intercept_state_value,
		f.pp_nett_no,
		f.is_first_settlement_bank_info,
		ccf.reference contract_custom_flow_reference,
		ccf.grp_no contract_custom_flow_grp_no,
		ccf.frequency contract_custom_flow_frequency,
		f.stamp
		
FROM	flow f
		inner join contract c on c.contract_id = f.contract_id
		left outer join (select * from contract_custom_flow where link_no != 0) ccf on (f.custom_flow_link_no = ccf.link_no AND f.contract_id = ccf.contract_id AND f.custom_flow_hdr_id = ccf.custom_flow_hdr_id)
		inner join currency ccy on f.currency_id = ccy.currency_id
		inner join bankacc ob on f.bank_account_id = ob.bankacc_id  												-- our bankacc
		inner join settlement_bank_info sbf on sbf.settlement_bank_info_id = f.settlement_bank_info_id	-- settlement bank of the flow
		inner join party_bankacc pb on pb.bankacc_id = sbf.party_bankacc_id       								-- party bankacc inflow,
		inner join flow_method fm on fm.flow_method_id = f.flow_method_id
		inner join party_account pa on f.party_account_id = pa.party_account_id
		inner join party p1 on pa.party_id = p1.party_id  										-- party account party
		inner join party p2 on ob.bank_id = p2.party_id  										-- our bank party
		inner join party p3 on f.beneficiary_id = p3.party_id  								-- beneficiary party
		inner join party p4 on pb.bank_id = p4.party_id 										-- their bank party
		inner join ax_user u on u.ax_user_id = p1.collections_user_id						-- collection officer user
		inner join party p5 on u.external_party_id = p5.party_id								-- collection officer party		
		inner join lookupset l1 on f.flow_type = l1.lookupset_id 							-- flow type
		inner join lookupset l2 on f.status = l2.lookupset_id 								-- flow status
		inner join lookupset l3 on f.reversal_status = l3.lookupset_id 					-- flow reversal status
		inner join lookupset l4 on f.is_set = l4.lookupset_id 								-- flow is set
		inner join lookupset l5 on c.contract_state = l5.lookupset_id 						-- contract state
		inner join lookupset l6 on f.collection_state = l6.lookupset_id					-- collection state
		inner join lookupset l7 on sbf.flow_direction = l7.lookupset_id 					-- flow_direction_value
		inner join lookupset l8 on sbf.status = l8.lookupset_id								-- party_bankacc_status_value
		inner join lookupset l9 on c.suspension_state = l9.lookupset_id					-- suspension state value
		inner join lookupset l10 on f.payment_confirmation_status = l10.lookupset_id	-- manual payment confirmation status value
		inner join lookupset l11 on c.intercept_state = l11.lookupset_id					-- intercept state value
		inner join lookupset l12 on p5.title = l12.lookupset_id								-- collection officer title
		inner join product pr on c.product_id = pr.product_id		
		inner join custom_flow_hdr cfh on f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
		inner join invoice i on f.invoice_id = i.invoice_id
		inner join purchase_invoice pinv on f.purchase_invoice_id = pinv.purchase_invoice_id,
		axvw_get_datetime dt

WHERE	 c.transfer_type != 9002 AND
		(f.is_cash = 1 OR f.pp_nett_no > 0) AND
		f.status not in (2104, 2105) AND
		c.contract_id >= 0 AND
		f.purchase_invoice_id > 0
/
CREATE OR REPLACE FORCE VIEW  axvw_cash_mgmt_view
AS
SELECT	* from axvw_cash_mgmt_view_int f
WHERE	f.is_set = 7801 AND f.reversal_status IN (4200, 4202)
		AND f.is_shadow_copy = 0
/
CREATE OR REPLACE FORCE VIEW  axvw_brec_cash_mgmt_view
AS
SELECT	f.*,
		cmg.flow_match_status,
    (select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from note where flow_id = f.flow_id and rownum <= 1) has_notes,
    (select case count(*) when 0 then cast(0 as NUMBER) else cast(1 as NUMBER) end from bank_rec_match where flow_id = f.flow_id and adjustment_contract_id != 0 and rownum <=1) has_adjustment,
    nvl((select adjustment_contract_id from bank_rec_match where flow_id = f.flow_id and adjustment_contract_id != 0 and rownum <=1),0) adjustment_contract_id
from axvw_cash_mgmt_view f
inner join axvw_cash_mgmt_view_grp cmg
on f.flow_id = cmg.flow_id
/
CREATE OR REPLACE FORCE VIEW  axvw_tmpl_cash_mgmt_view
AS
SELECT f.*,
	case WHEN f.custom_flow_link_no != 0 THEN	
		(select count(*) from contract_custom_flow ccf 
		where ccf.grp_no = f.contract_custom_flow_grp_no and 
		ccf.contract_id = f.contract_id) 
  ELSE
		0
  END num_ccf_installments
from axvw_cash_mgmt_view f
/
CREATE OR REPLACE FORCE VIEW AXVW_CORE_CONTRACT_VIEW AS
SELECT
    c.contract_id,
    c.product_style,
    case WHEN save_status = 2200 THEN cast(1 AS NUMBER) ELSE cast(0 AS NUMBER) END is_draft,
    c.input_dt,
    c.mature_dt1 maturity_dt,
    c.is_active,
    c.contract_state,
    c.save_status,
    c.approval_status2,
    c.stamp,
    c.business_unit_id,
    c.expiry_dt,
    c.amt_financed,
    cast(round(c.term/30.5,0) as NUMBER) term_months,
    bu.name business_unit,
    c.reference,
    cd.ext_reference reference2,
    ccy.code ccy,
    pr.name product,
    c.cparty_id,
    p.name customer_vendor,
    p.ext_name party_ext_name,
    p.first_names party_first_names,
    p.middle_name party_middle_name,
    pa.account_no party_account_no,
    l1.value approval_status_value,
    l2.value contract_state_value,
    l3.value credit_state_value,
    l4.value upload_status,
    cb.originator,
    cp.contract_batch_id,
    cp.contract_package_id,
    cb.input_dt batch_input_dt,
    cp.error_description,
    cp.status,
    c.provider_consumer,
    l5.value provider_consumer_value,
    l6.value product_style_value,
    c.is_variation_pending,
    u1.name input_user,
    u2.name last_saved_by,
    p.party_no cparty_party_no,
    location.name location_name,
    c.suspension_state,
    l7.value suspension_state_value,
    c.contract_type,
    l8.value contract_type_value,
    c.branch_id,
    p.reference party_reference,
    br.ext_name branch_name,
    c.opportunity_id,
    c.calc_dt,
	 cd.current_business_unit_id,
	 bu2.name current_business_unit,
	 c.last_updated_dt,
    NVL((SELECT 1 FROM DUAL WHERE EXISTS (SELECT 1 FROM task 
				where task_type in (6200, 6204, 6209, 6226) --Approve Contract; Approve Credit; Approve Restructure; Approve Generic Workflow 
				AND status NOT IN (6402, 6405) -- Completed; Not Required 
				AND approval_status != 6501 -- Approved 
				AND contract_id = c.contract_id 
		)),0) as workflow_approval_pending
FROM
    contract c,
    contract_det cd,
    party p,
    party bu,
    party br,
	 party bu2,
    party_account pa,
    product pr,
    lookupset l1,
    lookupset l2,
    lookupset l3,
    lookupset l4,
    lookupset l5,
    lookupset l6,
    lookupset l7,
    lookupset l8,
    currency ccy,
    ax_user u1,
    ax_user u2,
    contract_package cp,
    contract_batch cb,
    location location
WHERE
    c.contract_id > 0 AND
    c.cparty_id = p.party_id AND
    c.business_unit_id = bu.party_id AND
    c.party_account_id = pa.party_account_id AND
    c.product_id = pr.product_id AND
    c.currency_id = ccy.currency_id AND
    c.approval_status2 = l1.lookupset_id AND
    c.contract_state = l2.lookupset_id AND
    c.credit_state = l3.lookupset_id AND
    c.input_user_id = u1.ax_user_id AND
    c.last_saved_by_id = u2.ax_user_id AND
    c.contract_package_id = cp.contract_package_id AND
    cp.status = l4.lookupset_id AND
    cp.contract_batch_id = cb.contract_batch_id AND
    c.provider_consumer = l5.lookupset_id AND
    c.product_style = l6.lookupset_id AND
    c.location_id = location.location_id AND
    c.suspension_state = l7.lookupset_id AND
    c.contract_type = l8.lookupset_id AND
    br.party_id = c.branch_id AND
    cd.contract_id = c.contract_id AND
	 cd.current_business_unit_id = bu2.party_id
/
CREATE OR REPLACE FORCE VIEW AXVW_CONTRACT_VIEW_INTERNAL AS
SELECT
    c.contract_id,
    case WHEN f.installment_count IS NULL THEN 0 ELSE f.installment_count END installment_count,
    case WHEN f.first_installment_dt IS NULL THEN to_date('01-jan-1900','dd-mon-yyyy') ELSE f.first_installment_dt END first_installment_dt,
    CASE WHEN name1.name IS NULL THEN n' ' ELSE name1.name ||
    CASE WHEN name2.name IS NULL THEN n'' ELSE n',' || name2.name ||
    CASE WHEN name3.name IS NULL THEN n'' ELSE n',' || name3.name ||
    CASE WHEN name4.name IS NULL THEN n'' ELSE n',...' END END END END asset_names,
    c.product_style,
    cast(0 AS NUMBER) is_follow_up,
    case WHEN save_status = 2200 THEN cast(1 AS NUMBER) ELSE cast(0 AS NUMBER) END is_draft,
    cast(0 AS NUMBER) is_securitized,
    c.is_set,
    l10.value is_set_value,
    c.input_dt,
    c.calc_dt,
    c.mature_dt1 maturity_dt,
    c.interest_rate,
    c.fixed_floating,
    c.term,
    cast(round(c.term/30.5,0) as NUMBER) term_months,
    c.amt_financed,
    c.original_loan_amt,
    c.is_active,
    c.contract_state,
    c.save_status,
    c.approval_status2,
    c.stamp,
    c.business_unit_id,
    bu.name business_unit,
    c.reference,
    cd.ext_reference reference2,
    ccy.code ccy,
    pr.name product,
    c.cparty_id,
    p.name customer_vendor,
    p.ext_name party_ext_name,
    p.first_names party_first_names,
    p.middle_name party_middle_name,
    pa.account_no party_account_no,
    l1.value fixed_floating_value,
    l2.value rateset_freq_value,
    l3.value approval_status_value,
    l4.value installment_freq_value,
    l5.value contract_state_value,
    l6.value credit_state_value,
    u1.name approver1,
    u2.name approver2,
    c.is_customised,
    nvl((select gl.contract_id from contract_grp_link gl where gl.linked_contract_id = c.contract_id), 0) contract_group_id,
    l7.value upload_status,
    cp.is_incorrect_tax,
    cb.originator,
    cp.contract_batch_id,
    cp.contract_package_id,
    cb.input_dt batch_input_dt,
    cp.error_description,
    cp.status,
    c.is_funded,
    CASE WHEN c.is_funded = 1
			THEN axsp_funding_loan_csv(c.contract_id)
			ELSE NULL END funding_contract_id,
    c.rec_discount_rate,
    c.provider_consumer,
    l11.value provider_consumer_value,
    c.dealer_id,
    pd.ext_name dealer_ext_name,
    c.vendor_id,
    pv.ext_name vendor_ext_name,
    c.permit_holder_id,
    pp.ext_name permit_holder_ext_name,
    l12.value product_style_value,
    c.allow_funding,
    c.is_variation_pending,
    u3.name input_user,
    u4.name last_saved_by,
    c.last_updated_dt,
    p.party_no cparty_party_no,
    location.name location_name,
    p_acc_mgr.ext_name cparty_account_mgr,
    pd_acc_mgr.ext_name dealer_account_mgr,
    u6_coll_user.name cparty_coll_user,
    axsp_get_party_addr_state_name(c.cparty_id) cparty_state_province,
    c.suspension_state,
    cs.allow_suspension,
    l13.value suspension_state_value,
    c.interest_free_installments,
    c.deferred_installments,
    cd.inertia_ext_count,
    case when cd.inertia_ext_count > 0
		then 1 else 0
	 end is_extended,
    c.rate_level,
    c.contract_type,
    l14.value contract_type_value,
    c.expiry_dt,
    cd.contract_lost_reason,
    xt1.value contract_lost_reason_value,
    c.reserve_id loss_reserve_id,
    res.name loss_reserve_name,
    c.is_brokered,
    c.is_invoice_chain,
    c.program_id,
    prg.lu_name program_name,
    c.private_label_name,
    c.branch_id,
    br.ext_name branch_name,
    p.reference party_reference,
    c.credit_approval_expiry_dt,
    cd.orig_calc_dt,
    cd.orig_mature_dt,
    cd.primary_mature_dt,
    c.is_tax_exempt,
    c.tax_exempt_certificate,
    c.tax_exempt_certificate_eff_dt,
    c.intercept_state,
    l15.value intercept_state_value,
    cs.suspended_from_dt suspension_dt,
    cs.intercepted_from_dt intercept_dt,
    c.intercept_trigger_dt,
    c.opportunity_id,
    axsp_get_party_ext_name(sales_contact_id_1) sales_contact1_ext_name,
    axsp_get_party_ext_name(sales_contact_id_2) sales_contact2_ext_name,
    axsp_get_party_ext_name(sales_contact_id_3) sales_contact3_ext_name,
    axsp_get_party_ext_name(sales_contact_id_4) sales_contact4_ext_name,
    axsp_get_party_ext_name(sales_contact_id_5) sales_contact5_ext_name,
    c.appr_change_class,
    xt2.value appr_origin_class_value,
    c.appr_origin_class,
    xt3.value appr_change_class_value,
	 l16.value contract_grp_style,
	 cd.current_business_unit_id,
	 bu2.name current_business_unit,
	 a.residual_value,
    NVL((SELECT 1 FROM DUAL WHERE EXISTS (SELECT 1 FROM task 
				where task_type in (6200, 6204, 6209, 6226) --Approve Contract; Approve Credit; Approve Restructure; Approve Generic Workflow 
				AND status NOT IN (6402, 6405) -- Completed; Not Required 
				AND approval_status != 6501 -- Approved 
				AND contract_id = c.contract_id 
		)),0) as workflow_approval_pending
FROM
    contract c
	 left join (select sum(residual_value) as residual_value, contract_id
		   from asset
		   group by contract_id) a on a.contract_id = c.contract_id
    left join (select count(flow_id) as installment_count, MIN(expected_dt) as first_installment_dt, contract_id
			from  flow
			where flow_type = 1003
			group by contract_id) f on f.contract_id = c.contract_id
    LEFT JOIN
		(SELECT asset_hdr.name,
		  asset.contract_id,
		  ROW_NUMBER() OVER (PARTITION by asset.contract_id ORDER BY asset.asset_id) AS row_number
		FROM asset_hdr INNER JOIN asset ON asset_hdr.asset_hdr_id = asset.asset_hdr_id) name1
    ON name1.contract_id = c.contract_id and name1.row_number = 1
    LEFT JOIN
      (SELECT asset_hdr.name,
        asset.contract_id,
        ROW_NUMBER() OVER (PARTITION by asset.contract_id ORDER BY asset.asset_id) AS row_number
      FROM asset_hdr INNER JOIN asset ON asset_hdr.asset_hdr_id = asset.asset_hdr_id) name2
    ON name1.contract_id = name2.contract_id and name2.row_number = 2
    LEFT JOIN
      (SELECT asset_hdr.name,
        asset.contract_id,
        ROW_NUMBER() OVER (PARTITION by asset.contract_id ORDER BY asset.asset_id) AS row_number
      FROM asset_hdr INNER JOIN asset ON asset_hdr.asset_hdr_id = asset.asset_hdr_id) name3
    ON name1.contract_id = name3.contract_id and name2.row_number = 3
    LEFT JOIN
      (SELECT asset_hdr.name,
        asset.contract_id,
        ROW_NUMBER() OVER (PARTITION by asset.contract_id ORDER BY asset.asset_id) AS row_number
      FROM asset_hdr INNER JOIN asset ON asset_hdr.asset_hdr_id = asset.asset_hdr_id) name4
    ON name1.contract_id = name4.contract_id and name4.row_number = 4,
    party p,
    party bu,
    party pd,
    party pv,
    party pp,
    party br,
	 party bu2,
    party p_acc_mgr,
    party pd_acc_mgr,
    party_account pa,
    product pr,
    lookupset l1,
    lookupset l2,
    lookupset l3,
    lookupset l4,
    lookupset l5,
    lookupset l6,
    lookupset l7,
    lookupset l10,
    lookupset l11,
    lookupset l12,
    lookupset l13,
    lookupset l14,
    lookupset l15,
	 lookupset l16,
    xt_lookupset xt1,
    currency ccy,
    ax_user u1,
    ax_user u2,
    ax_user u3,
    ax_user u4,
    contract_package cp,
    contract_batch cb,
    location location,
    ax_user u5_p_acc_mgr,
    ax_user u6_coll_user,
    ax_user u7_pd_acc_mgr,
    contract_det cd,
    reserve res,
    program prg,
    contract_suspension cs,
	 xt_lookupset xt2,
    xt_lookupset xt3
WHERE
    c.contract_id > 0 AND
    c.cparty_id = p.party_id AND
    c.business_unit_id = bu.party_id AND
    c.dealer_id = pd.party_id AND
    c.vendor_id = pv.party_id AND
    c.permit_holder_id = pp.party_id AND
    c.party_account_id = pa.party_account_id AND
    c.product_id = pr.product_id AND
    c.currency_id = ccy.currency_id AND
    c.fixed_floating = l1.lookupset_id AND
    c.rateset_freq = l2.lookupset_id AND
    c.approval_status2 = l3.lookupset_id AND
    c.installment_frequency = l4.lookupset_id AND
    c.contract_state = l5.lookupset_id AND
    c.credit_state = l6.lookupset_id AND
    c.approver1_id = u1.ax_user_id AND
    c.approver2_id = u2.ax_user_id AND
    c.input_user_id = u3.ax_user_id AND
    c.last_saved_by_id = u4.ax_user_id AND
    c.contract_package_id = cp.contract_package_id AND
    cp.status = l7.lookupset_id AND
    cp.contract_batch_id = cb.contract_batch_id AND
    c.is_set = l10.lookupset_id AND
    c.provider_consumer = l11.lookupset_id AND
    c.product_style = l12.lookupset_id AND
    c.location_id = location.location_id AND
    p.account_manager_id = u5_p_acc_mgr.ax_user_id AND
    pd.account_manager_id = u7_pd_acc_mgr.ax_user_id AND
    u5_p_acc_mgr.external_party_id = p_acc_mgr.party_id AND
    u7_pd_acc_mgr.external_party_id = pd_acc_mgr.party_id AND
    p.collections_user_id = u6_coll_user.ax_user_id AND
    c.suspension_state = l13.lookupset_id AND
    c.product_style not in (2010, 2012, 2016) AND
    c.contract_grp_style != 18203 AND
    c.contract_type = l14.lookupset_id AND
    cd.contract_id = c.contract_id AND
	 cd.current_business_unit_id = bu2.party_id AND
    cd.contract_lost_reason = xt1.xt_lookupset_id AND
    res.reserve_id = c.reserve_id AND
    prg.program_id = c.program_id AND
    br.party_id = c.branch_id AND
    cd.contract_id = c.contract_id AND
    cs.contract_id = c.contract_id AND
    c.intercept_state = l15.lookupset_id AND
    c.appr_origin_class = xt2.xt_lookupset_id AND
    c.appr_change_class = xt3.xt_lookupset_id AND
	 c.contract_grp_style =	l16.lookupset_id
/
CREATE OR REPLACE FORCE VIEW AXVW_CONTRACT_VIEW AS
SELECT
    cvi.contract_id,
    cvi.product_style,
    cvi.is_follow_up,
    cvi.is_draft,
    cvi.is_securitized,
    cvi.is_set,
    cvi.is_set_value,
    cvi.input_dt,
    cvi.calc_dt,
    cvi.maturity_dt,
    cvi.interest_rate,
    cvi.fixed_floating,
    cvi.term,
    cvi.term_months,
    cvi.amt_financed,
    cvi.original_loan_amt,
    cvi.is_active,
    cvi.contract_state,
    cvi.save_status,
    cvi.approval_status2,
    cvi.stamp,
    cvi.business_unit_id,
    cvi.business_unit,
    cvi.reference,
    cvi.reference2,
    cvi.ccy,
    cvi.product,
    cvi.cparty_id,
    cvi.customer_vendor,
    cvi.party_ext_name,
    cvi.party_first_names,
    cvi.party_middle_name,
    cvi.party_account_no,
    cvi.fixed_floating_value,
    cvi.rateset_freq_value,
    cvi.approval_status_value,
    cvi.installment_freq_value,
    cvi.contract_state_value,
    cvi.credit_state_value,
    cvi.approver1,
    cvi.approver2,
    cvi.is_customised,
    cvi.contract_group_id,
    cvi.upload_status,
    cvi.is_incorrect_tax,
    cvi.originator,
    cvi.contract_batch_id,
    cvi.contract_package_id,
    cvi.input_dt batch_input_dt,
    cvi.error_description,
    cvi.status,
    cvi.is_funded,
	 cvi.funding_contract_id,
    cvi.rec_discount_rate,
    cvi.provider_consumer,
    cvi.provider_consumer_value,
    cvi.dealer_id,
    cvi.dealer_ext_name,
    cvi.vendor_id,
    cvi.vendor_ext_name,
    cvi.permit_holder_id,
    cvi.permit_holder_ext_name,
    cvi.product_style_value,
    cvi.allow_funding,
    cvi.is_variation_pending,
    cvi.input_user,
    cvi.last_saved_by,
    cvi.cparty_party_no,
    cvi.location_name,
    cvi.cparty_account_mgr,
    cvi.dealer_account_mgr,
    cvi.cparty_coll_user,
    cvi.cparty_state_province,
    cvi.suspension_state,
    cvi.allow_suspension,
    cvi.suspension_state_value,
    cvi.interest_free_installments,
    cvi.deferred_installments,
    cvi.inertia_ext_count,
    cvi.is_extended,
	 cvi.rate_level,
    cvi.contract_type,
    cvi.contract_type_value,
    cvi.expiry_dt,
    cvi.contract_lost_reason,
    cvi.contract_lost_reason_value,
    cvi.loss_reserve_id,
    cvi.loss_reserve_name,
    cvi.is_brokered,
    cvi.is_invoice_chain,
    cvi.program_id,
    cvi.program_name,
    cvi.private_label_name,
    cvi.branch_id,
    cvi.branch_name,
    cvi.party_reference,
    cvi.credit_approval_expiry_dt,
    cvi.orig_calc_dt,
    cvi.orig_mature_dt,
    cvi.primary_mature_dt,
    cvi.is_tax_exempt,
    cvi.tax_exempt_certificate,
    cvi.tax_exempt_certificate_eff_dt,
    cvi.intercept_state,
    cvi.intercept_state_value,
    cvi.suspension_dt,
    cvi.intercept_dt,
    cvi.intercept_trigger_dt,
    cvi.opportunity_id,
    cvi.sales_contact1_ext_name,
    cvi.sales_contact2_ext_name,
    cvi.sales_contact3_ext_name,
    cvi.sales_contact4_ext_name,
    cvi.sales_contact5_ext_name,
    cvi.appr_origin_class,
    cvi.appr_origin_class_value,
    cvi.appr_change_class,
    cvi.appr_change_class_value,
	 cvi.contract_grp_style,
	 cvi.current_business_unit_id,
	 cvi.current_business_unit,
	 cvi.residual_value,
	 cvi.last_updated_dt,
	 cvi.workflow_approval_pending,
	 cvi.installment_count,
	 cvi.first_installment_dt,
	 cvi.asset_names
FROM
    axvw_contract_view_internal cvi
WHERE
    cvi.contract_type = 2301
/
CREATE OR REPLACE FORCE VIEW AXVW_QUOTE_CONTRACT_VIEW
AS
SELECT
    cvi.contract_id,
    cvi.product_style,
    cvi.is_follow_up,
    cvi.is_draft,
    cvi.is_securitized,
    cvi.is_set,
    cvi.is_set_value,
    cvi.input_dt,
    cvi.calc_dt,
    cvi.maturity_dt,
    cvi.interest_rate,
    cvi.fixed_floating,
    cvi.term,
    cvi.term_months,
    cvi.amt_financed,
    cvi.original_loan_amt,
    cvi.is_active,
    cvi.contract_state,
    cvi.save_status,
    cvi.approval_status2,
    cvi.stamp,
    cvi.business_unit_id,
    cvi.business_unit,
    cvi.reference,
    cvi.reference2,
    cvi.ccy,
    cvi.product,
    cvi.cparty_id,
    cvi.customer_vendor,
    cvi.party_ext_name,
    cvi.party_first_names,
    cvi.party_middle_name,
    cvi.party_account_no,
    cvi.fixed_floating_value,
    cvi.rateset_freq_value,
    cvi.approval_status_value,
    cvi.installment_freq_value,
    cvi.contract_state_value,
    cvi.credit_state_value,
    cvi.approver1,
    cvi.approver2,
    cvi.is_customised,
    cvi.contract_group_id,
    cvi.upload_status,
    cvi.is_incorrect_tax,
    cvi.originator,
    cvi.contract_batch_id,
    cvi.contract_package_id,
    cvi.input_dt batch_input_dt,
    cvi.error_description,
    cvi.status,
    cvi.is_funded,
	 cvi.funding_contract_id,
    cvi.rec_discount_rate,
    cvi.provider_consumer,
    cvi.provider_consumer_value,
    cvi.dealer_id,
    cvi.dealer_ext_name,
    cvi.vendor_id,
    cvi.vendor_ext_name,
    cvi.permit_holder_id,
    cvi.permit_holder_ext_name,
    cvi.product_style_value,
    cvi.allow_funding,
    cvi.is_variation_pending,
    cvi.input_user,
    cvi.last_saved_by,
    cvi.cparty_party_no,
    cvi.location_name,
    cvi.cparty_account_mgr,
    cvi.dealer_account_mgr,
    cvi.cparty_coll_user,
    cvi.cparty_state_province,
    cvi.suspension_state,
    cvi.allow_suspension,
    cvi.suspension_state_value,
    cvi.interest_free_installments,
    cvi.deferred_installments,
    cvi.inertia_ext_count,
    cvi.is_extended,
    cvi.rate_level,
    cvi.contract_type,
    cvi.contract_type_value,
    cvi.expiry_dt,
    cvi.contract_lost_reason,
    cvi.contract_lost_reason_value,
    cvi.loss_reserve_id,
    cvi.loss_reserve_name,
    cvi.is_brokered,
    cvi.is_invoice_chain,
    cvi.program_id,
    cvi.program_name,
    cvi.private_label_name,
    cvi.branch_id,
    cvi.branch_name,
    cvi.party_reference,
    cvi.credit_approval_expiry_dt,
    cvi.orig_calc_dt,
    cvi.orig_mature_dt,
    cvi.primary_mature_dt,
    cvi.is_tax_exempt,
    cvi.tax_exempt_certificate,
    cvi.tax_exempt_certificate_eff_dt,
    cvi.intercept_state,
    cvi.intercept_state_value,
    cvi.suspension_dt,
    cvi.intercept_dt,
    cvi.intercept_trigger_dt,
    cvi.opportunity_id,
    cvi.sales_contact1_ext_name,
    cvi.sales_contact2_ext_name,
    cvi.sales_contact3_ext_name,
    cvi.sales_contact4_ext_name,
    cvi.sales_contact5_ext_name,
    cvi.appr_origin_class,
    cvi.appr_origin_class_value,
    cvi.appr_change_class,
    cvi.appr_change_class_value,
	 cvi.contract_grp_style,	 
	 cvi.current_business_unit_id,
	 cvi.current_business_unit,
	 cvi.residual_value,
	 cvi.last_updated_dt,
	 cvi.workflow_approval_pending,
	 cvi.installment_count,
	 cvi.first_installment_dt,
	 cvi.asset_names
FROM
    axvw_contract_view_internal cvi
WHERE
    cvi.contract_type = 2300
/
CREATE OR REPLACE FORCE VIEW axvw_policy_view AS
SELECT c.contract_id,
	 c.product_style,
	 l4.value product_style_value,
    case WHEN save_status = 2200 THEN cast(1 as number) ELSE cast(0 as number) END is_draft,
    c.input_dt,
    c.mature_dt1 maturity_dt,
    c.is_active,
    c.contract_state,
	 c.save_status,
    c.business_unit_id,
    bu.name business_unit,
    c.reference,
    cd.ext_reference reference2,
	 p.external_policy_no,
    ccy.code ccy,
    pr.name product,
    c.cparty_id,
	 lc.party_no cparty_party_no,
    lc.name customer_vendor,
    lc.ext_name party_ext_name,
    l1.value contract_state_value,
    l2.value upload_status,
    cb.originator,
    cp.contract_batch_id,
    cp.contract_package_id,
	 cb.input_dt batch_input_dt,
	 cp.error_description,
	 cp.status,
    c.is_variation_pending,
    u1.name input_user,
    u2.name last_saved_by,
    psel.party_no policy_seller_party_no,
	 psel.name policy_seller,
    psel.ext_name policy_seller_ext_name,
    psel.first_names policy_seller_first_names,
    psel.middle_name policy_seller_middle_name,
	 axsp_get_party_addr_state_name(p.initial_seller_id) policy_seller_state_province,
    loc.name location_name,
    c.branch_id,
    br.ext_name branch_name,
	 ps.name policy_series,
	 p.amt_sum_assured sum_assured,
	 p.amt_death_benefit death_benefit,
	 l3.value installment_freq_value,
	 c.calc_dt,
	 p.amt_purchase_price purchase_price,
	 p.is_documentation_lost,
	 p.has_endorsements,
	 p.has_variable_death_benefit,
	 p.is_loan_attached,
	 p.is_paid_up,
	 round(p.purchase_age,2) purchase_age,
	 p.maturity_pending_dt,
	 la1.ext_name life_assured_1,
	 la2.ext_name life_assured_2,
	 p.life_assured_id_1,
	 p.life_assured_id_2,
	 c.product_formula policy_type,
	 l5.value policy_type_value,
	 c.last_updated_dt,
	 c.stamp
FROM
    contract c,
    contract_det cd,
    party lc,
    party bu,
    party br,
	 party psel,
	 party la1,
	 party la2,
	 policy p,
	 policy_series ps,
    product pr,
    lookupset l1,
    lookupset l2,
	 lookupset l3,
	 lookupset l4,
	 lookupset l5,
    currency ccy,
    ax_user u1,
    ax_user u2,
    contract_package cp,
    contract_batch cb,
    location loc
WHERE
    c.contract_id > 0 AND
	 c.contract_id = p.contract_id AND
	 p.policy_series_id = ps.policy_series_id AND
    c.cparty_id = lc.party_id AND
	 p.initial_seller_id = psel.party_id AND
    c.business_unit_id = bu.party_id AND
    c.product_id = pr.product_id AND
    c.currency_id = ccy.currency_id AND
    c.contract_state = l1.lookupset_id AND
	 c.product_style = l4.lookupset_id AND
	 c.product_formula = l5.lookupset_id AND
    c.input_user_id = u1.ax_user_id AND
    c.last_saved_by_id = u2.ax_user_id AND
    c.contract_package_id = cp.contract_package_id AND
    cp.status = l2.lookupset_id AND
	 c.installment_frequency = l3.lookupset_id AND
    cp.contract_batch_id = cb.contract_batch_id AND
    c.location_id = loc.location_id AND
    br.party_id = c.branch_id AND
    p.life_assured_id_1 = la1.party_id AND
    p.life_assured_id_2 = la2.party_id AND
    cd.contract_id = c.contract_id
union
SELECT c.contract_id,
	 c.product_style,
	 l4.value product_style_value,
    case WHEN save_status = 2200 THEN cast(1 as number) ELSE cast(0 as number) END is_draft,
    c.input_dt,
    c.mature_dt1 maturity_dt,
    c.is_active,
    c.contract_state,
	 c.save_status,
    c.business_unit_id,
    bu.name business_unit,
    c.reference,
    cd.ext_reference reference2,
	 N' ' external_policy_no,
    ccy.code ccy,
    pr.name product,
    c.cparty_id,
	 bf.party_no cparty_party_no,
    bf.name customer_vendor,
    bf.ext_name party_ext_name,
    l1.value contract_state_value,
    l2.value upload_status,
    cb.originator,
    cp.contract_batch_id,
    cp.contract_package_id,
	 cb.input_dt batch_input_dt,
	 cp.error_description,
	 cp.status,
    c.is_variation_pending,
    u1.name input_user,
    u2.name last_saved_by,
    bu.party_no policy_seller_party_no,
	 bu.name policy_seller,
    bu.ext_name policy_seller_ext_name,
    N' ' policy_seller_first_names,
    N' ' policy_seller_middle_name,
	 N' ' policy_seller_state_province,
    loc.name location_name,
    c.branch_id,
    br.ext_name branch_name,
	 N' ' policy_series,
	 0 sum_assured,
	 0 death_benefit,
	 l3.value installment_freq_value,
	 c.calc_dt,
	 0 purchase_price,
	 0 is_documentation_lost,
	 0 has_endorsements,
	 0 has_variable_death_benefit,
	 0 is_loan_attached,
	 0 is_paid_up,
	 0 purchase_age,
	 to_date('01/01/1900','dd/mm/yyyy') maturity_pending_dt,
	 N' ' life_assured_1,
	 N' ' life_assured_2,
	 0 life_assured_id_1,
	 0 life_assured_id_2,
	 c.product_formula policy_type,
	 l5.value policy_type_value,
	 c.last_updated_dt,
	 c.stamp
FROM
    contract c,
    contract_det cd,
    party bf,
    party bu,
    party br,
    product pr,
    lookupset l1,
    lookupset l2,
	 lookupset l3,
	 lookupset l4,
	 lookupset l5,
    currency ccy,
    ax_user u1,
    ax_user u2,
    contract_package cp,
    contract_batch cb,
    location loc
WHERE
    c.contract_id > 0 AND
    c.cparty_id = bf.party_id AND
    c.business_unit_id = bu.party_id AND
    c.product_id = pr.product_id AND
    c.currency_id = ccy.currency_id AND
    c.contract_state = l1.lookupset_id AND
	 c.product_style = l4.lookupset_id AND
	 c.product_formula = l5.lookupset_id AND
    c.input_user_id = u1.ax_user_id AND
    c.last_saved_by_id = u2.ax_user_id AND
    c.contract_package_id = cp.contract_package_id AND
    cp.status = l2.lookupset_id AND
	 c.installment_frequency = l3.lookupset_id AND
    cp.contract_batch_id = cb.contract_batch_id AND
    c.location_id = loc.location_id AND
    br.party_id = c.branch_id AND
	 c.contract_grp_style = 18203 AND
    cd.contract_id = c.contract_id
/
CREATE OR REPLACE FORCE VIEW  axvw_lease_portfolio_report
as
select
	c.business_unit_id,
	bu.name business_unit_name,
	c.contract_id,
	c.reference,
	c.product_id,
	pr.name product_name,
	c.product_style,
	axsp_get_lookupset_value(c.product_style) product_style_name,
	c.cparty_id,
	cp.name cparty_name,
	axsp_get_lookupset_value(cp.entity_type) entity_type,
	cp.industry_id,
	i.name industry_name,
	nvl((select addr.city_id from party_address addr where addr.party_id = cp.party_id and addr.address_type = 5000 and addr.is_current = 1),0) city_id,
	nvl((select addr.state_province_id from party_address addr where addr.party_id = cp.party_id and addr.address_type = 5000 and addr.is_current = 1),0) state_province_id,
	a.sequence_no asset_no,
	ah.asset_type_id,
	at.name asset_type_name,
	ah.name asset_name,
	a.equipment_code,
	(select sum(amount) from flow f where f.contract_id = c.contract_id and f.flow_type = 1003 and reversal_status = 4200) original_bal,
	a.amt_financed original_cost,
	(select sum(amount) from flow f where f.contract_id = c.contract_id and f.flow_type = 1003 and reversal_status = 4200 and f.expected_dt >  axsp_dateonly(axsp_get_datetime())) current_bal,
	a.residual_value,
	c.amt_security_deposit,
	c.stream_rate,
	a.installments,
	(select count(*) from flow f where f.contract_id = c.contract_id and f.flow_type = 1003 and reversal_status = 4200 and f.collection_state = 14802) overdue_installments,
	(select count(*) from flow f where f.contract_id = c.contract_id and f.flow_type = 1003 and reversal_status = 4200 and f.actual_dt >  axsp_dateonly(axsp_get_datetime())) remaining_installments,
	 axsp_get_lookupset_value(c.installment_frequency) installment_frequency,

	fcl_temp.portfolio_id temp_portfolio_id, fcl_temp.funding_status temp_portfolio_status, axsp_get_lookupset_value(fcl_temp.funding_status) temp_portfolio_status_val,
	fcl_perm.portfolio_id perm_portfolio_id, fcl_perm.funding_status perm_portfolio_status, axsp_get_lookupset_value(fcl_perm.funding_status) perm_portfolio_status_val,
	nvl(nvl(fcl_perm.funding_contract_id, fcl_temp.funding_contract_id), 0) funding_contract_id,
	nvl(nvl(fcl_perm.facility_id, fcl_temp.facility_id), 0) funding_facility_id,

	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 0
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) current_amt,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 1
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_1,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 2
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_2,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 3
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_3,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 4
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_4,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 5
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_5,
	nvl((select sum(f.amt_gross-f.amt_matched) from flow f, overdues_timeband t where
		f.contract_id = c.contract_id and f.party_account_id = c.party_account_id
		and f.flow_type != 1001 --exclude drawdown flow
		and f.reversal_status = 4200 and f.is_cash = 1 and t.overdues_timeband_id = 6
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt >= t.start_day
	       and axsp_dateonly(axsp_get_datetime())-f.expected_dt <= t.end_day), 0) overdue_6
from
	contract c
	inner join asset a on c.contract_id = a.contract_id
	inner join asset_hdr ah on ah.asset_hdr_id = a.asset_hdr_id
	inner join party bu on bu.party_id = c.business_unit_id
	inner join product pr on pr.product_id = c.product_id
	inner join party cp on cp.party_id = c.cparty_id
	inner join asset_type at on at.asset_type_id = ah.asset_type_id
	inner join industry i on cp.industry_id = i.industry_id
	left join funding_contract_link fcl_temp on fcl_temp.contract_id = c.contract_id and fcl_temp.portfolio_type = 16001
	left join funding_contract_link fcl_perm on fcl_perm.contract_id = c.contract_id and fcl_perm.portfolio_type = 16002
where
	c.contract_id >= 0
	and a.sequence_no = (select min(sequence_no) from asset where contract_id = c.contract_id) -- limit to 1st asset in contracts
	and c.product_style = 2001
/
CREATE OR REPLACE FORCE VIEW axvw_overdue_view AS
SELECT  f.flow_id,
        c.contract_id,
        pinv.purchase_invoice_id,
        case when f.purchase_invoice_id>0 then pinv.business_unit_id else c.business_unit_id end business_unit_id,
        case when f.purchase_invoice_id>0 then pinv.stamp else c.stamp end contract_stamp,
        p.party_id party_id,
        p.party_no party_no,
        p.name party_name,
        p.ext_name party_ext_name,
        p.first_names party_first_name,
        p.middle_name party_middle_name,
        pa.party_account_id,
        pa.account_no party_account,
        pa.name party_account_name,
        f.installment_no,
        CASE WHEN f.flow_type = 1010 THEN cfh.name ELSE lu.value END flow_type_value,
        f.actual_dt,
        f.expected_dt,
        f.currency_id,
        ccy.code ccy,
        CASE WHEN (f.expected_dt < trunc(dt.server_dt)) THEN f.amt_gross - f.amt_matched ELSE 0 END overdue_amt0,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 1) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 1) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt1,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 2) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 2) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt2,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 3) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 3) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt3,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 4) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 4) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt4,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 5) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 5) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt5,
        CASE WHEN trunc(dt.server_dt) - f.expected_dt >= (SELECT start_day FROM overdues_timeband WHERE overdues_timeband_id = 6) AND trunc(dt.server_dt) - f.expected_dt <= (SELECT end_day FROM overdues_timeband WHERE overdues_timeband_id = 6) THEN f.amt_gross-f.amt_matched  ELSE 0 END overdue_amt6,
        NVL ((SELECT MAX(bf.actual_dt) FROM bank_flow bf WHERE bf.contract_id = f.contract_id and bf.is_deleted = 0 and bf.is_shadow_copy = 0), axsp_datemin()) last_payment_dt,
        NVL ((SELECT MAX(pa.activity_dt) FROM note pa WHERE pa.party_id = c.cparty_id AND pa.activity_area = 8100), axsp_datemin()) last_activity_dt,
        NVL ((SELECT MIN(f1.expected_dt) FROM flow f1 WHERE f1.contract_id = f.contract_id AND f1.expected_dt >= trunc(dt.server_dt)), axsp_datemax()) next_payment_dt,
        NVL ((SELECT MAX(activity_dt) FROM note pa2 WHERE pa2.party_account_id = f.party_account_id AND pa2.activity_area = 8100 AND activity_type = 8002), axsp_datemin()) last_collection_letter_dt,
        case when f.purchase_invoice_id>0 then pinv_lu.value else pr.name end product_name,
        f.collection_state,
        l2.value collection_state_value,
        trunc(dt.server_dt) - f.expected_dt overdue_days,
        CASE WHEN cfh.custom_flow_hdr_id != 0 AND cfh.purpose = 14210 THEN cast(1 AS INTEGER) ELSE cast(0 AS INTEGER) END is_recovery,
        res.name loss_reserve_name,
        CASE WHEN f.purchase_invoice_id > 0 AND f.contract_id = 0 THEN cast(1 AS INTEGER) ELSE cast(0 AS INTEGER) END is_purchase_invoice,
        f.stamp
FROM contract c, currency ccy, flow f , party p, party_account pa, axvw_get_datetime dt,
     product pr, lookupset lu, lookupset pinv_lu, custom_flow_hdr cfh, lookupset l2, purchase_invoice pinv, reserve res
WHERE c.contract_id = f.contract_id
  AND c.contract_id >= 0
  AND res.reserve_id = c.reserve_id
  AND f.currency_id = ccy.currency_id
  AND pa.party_id = p.party_id
  AND f.party_account_id = pa.party_account_id
  AND c.product_id = pr.product_id
  AND pinv.purchase_invoice_type = pinv_lu.lookupset_id
  AND lu.lookupset_id = f.flow_type
  AND l2.lookupset_id = f.collection_state
  AND pinv.purchase_invoice_id = f.purchase_invoice_id
  AND f.reversal_status = 4200
  AND f.is_cash = 1
  AND f.is_shadow_copy = 0
  AND (f.collection_state = 14801 OR f.collection_state = 14802)
  AND f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
  AND cfh.purpose != 14210
  AND f.status NOT IN (2099, 2104, 2105)
/
CREATE OR REPLACE FORCE VIEW AXVW_GL_JOURNAL_VIEW
  AS
 SELECT  a.gl_journal_id,
	a.post_dt,
	b.name user_name,
	(SELECT  count(*) FROM gl_journal_entry c WHERE c.gl_journal_id = a.gl_journal_id AND c.reversal_status = 4200  ) gl_entries_cnt,
	(SELECT  count(DISTINCT ledger_id) FROM gl_journal_entry d WHERE d.gl_journal_id = a.gl_journal_id AND d.reversal_status = 4200) ledgers_cnt,
	(SELECT  count(DISTINCT currency_id) FROM gl_journal_entry e WHERE e.gl_journal_id = a.gl_journal_id AND e.reversal_status = 4200) currencies_cnt,
	(SELECT * FROM
		(SELECT  g.code FROM gl_journal_entry f, currency g
       WHERE f.gl_journal_id = gl_journal_id AND f.reversal_status = 4200 AND f.currency_id = g.currency_id
       AND amount = (SELECT  MAX(amount) FROM gl_journal_entry h WHERE H.gl_journal_id = gl_journal_id AND H.reversal_status = 4200)
       ORDER BY 1)
    WHERE ROWNUM <= 1) primary_currency,
	(SELECT  MAX(f.amount) FROM gl_journal_entry f WHERE f.gl_journal_id = a.gl_journal_id AND f.reversal_status = 4200) amount,
   (SELECT count(DISTINCT cost_centre) FROM gl_journal_entry e WHERE e.gl_journal_id = a.gl_journal_id AND e.reversal_status = 4200 AND e.cost_centre != 0) cost_centres_cnt,
	(SELECT count(DISTINCT branch_id) FROM gl_journal_entry e WHERE e.gl_journal_id = a.gl_journal_id AND e.reversal_status = 4200 AND e.branch_id != 0) branches_cnt,
	a.upload_batch_id,
	a.upload_package_id,
	a.comments,
	(	SELECT max(current_state_type) FROM workflow w WHERE w.gl_journal_id = a.gl_journal_id) current_workflow_state_id,
	a.stamp
 FROM gl_journal a, ax_user b
 WHERE a.user_id = b.ax_user_id
/
CREATE OR REPLACE FORCE VIEW AXVW_STATEMENT_BALANCE
   (STATEMENT_ID,PARTY_ACCOUNT_ID,CURRENCY_ID,OPENING_BALANCE,CLOSING_BALANCE,CHARGES,RECEIPTS) AS
 SELECT  sb.statement_id,
         sb.party_account_id,
         sb.currency_id,
         axsp_statement_open_bal(sb.party_account_id,sb.currency_id,sb.balance_dt) opening_balance,
         sb.balance closing_balance,
         NVL((SELECT SUM(amt_gross)
              FROM axvw_statement sfc
              WHERE sfc.statement_id = sb.statement_id
                AND sfc.party_account_id = sb.party_account_id
                AND sfc.currency_id = sb.currency_id
                AND sfc.flow_type = 'C'  ), 0) charges,
         NVL((SELECT  NVL(SUM(amt_gross), 0)
              FROM axvw_statement sfb
              WHERE sfb.statement_id = sb.statement_id
              AND sfb.party_account_id = sb.party_account_id
              AND sfb.currency_id = sb.currency_id
              AND sfb.flow_type = 'B'  ), 0) receipts
  FROM statement_bal sb
/
CREATE OR REPLACE FORCE VIEW ORDER_OBJECT_BY_DEPENDENCY (DLEVEL, OBJECT_ID) AS
select max(level) dlevel, o.object_id
from user_dependencies d, user_objects o
where d.name = o.object_name
and d.type = o.object_type
connect by nocycle d.name = prior d.referenced_name
and d.type = prior d.referenced_type
group by o.object_id
/
CREATE OR REPLACE FORCE VIEW axvw_bank_interface
AS
SELECT	0 is_bank_flow,
	f.flow_id, /* --> flow.flow_id */
	f.contract_id,  /* --> flow.contract_id */
	c.reference contract_ref, /* --> contract.reference */
	c.product_style, /* --> contract.product_style */
	cd.ext_reference contract_ref2, /* --> contract_det.ext_reference */
	f.flow_type, /* --> flow.flow_type */
	f.actual_dt, /* --> flow.actual_dt */
	CASE WHEN f.release_dt > f.expected_dt THEN f.release_dt ELSE f.expected_dt END expected_dt,  /* --> flow.expected_dt */
	f.currency_id, /* --> flow.currency_id */
	f.amount, /* --> flow.amount */
	CASE WHEN fm.is_expected = 0 or (fm.is_expected = 1 AND (f.nett_no != 0 or (cfh.custom_flow_hdr_id != 0 and cfh.purpose = 14210))) THEN f.amt_gross - f.amt_matched ELSE f.amt_gross END amt_gross, /* --> flow.amt_gross - flow.amt_matched*/
	f.amt_gross_netted,
	f.bank_account_id, /* --> flow.bank_account_id */
	f.party_account_id, /* --> flow.party_account_id */
	f.nett_no, /* --> flow.nett_no */
	f.installment_no, /* --> flow.installment_no */
	f.payee_ref, /* --> flow.payee_ref */
	f.payment_ref, /* --> flow.payment_ref */
	b.account_owner_id business_unit_id, /* --> bankacc.account_owner_id */
	b.code bank_account_no, /* --> bankacc.code */
	b.full_account_no bank_account_no_full, /* --> bankacc.full_account_no */
	b.identification_no bankacc_identification_no, /* --> bankacc.identification_no used by BACS Interface */
	case WHEN NVL(LENGTH(TRIM(c.private_label_name)), 0) = 0 THEN p1.ext_name ELSE c.private_label_name END business_unit_name, /* --> contract.private_label_name */
	p1.reference business_unit_reference, /* --> Business units reference field */
	b.branch_code bank_account_branch_code,  /* --> bankacc.branch_code */
	b.name bank_account_name, /* --> bankacc.name */
	b.swift_bic bank_account_swift_bic, /* --> bankacc.swift_bic */
	b.iban bank_account_iban, /* --> bankacc.iban */
	-- Linked bank account for bi-directional netting
	b.auto_net_bankacc_id,  /* bankacc.auto_net_bankacc_id */
	p3.ext_name bank_name, /* --> bankacc.bank_name */
	pa.account_no party_account_no, /* --> party_account.party_account_no */
	pa.name party_account_name, /* --> party_account.name */
	pa.party_id, /* --> party_account.party_id */
	p.party_no, /* --> party.party_no */
	p.name party_name, /* --> party.name */
	p.reference party_reference, /* --> party.reference */
	p.business_individual party_bus_individual, /* --> party.business_individual */
	p.business_ref_no party_business_ref_no, /* --> party.business_ref_no */
	p.national_no party_national_no, /* --> party.national_no */
	p.identification_ref1 party_identification_ref1, /* party.Identification Ref 1 */
	p.identification_ref2 party_identification_ref2,  /* party.Identification Ref 2 */
	sbf.flow_direction, /* --> settlement_bank_info.flow_direction */
	f.flow_method_id, /* --> flow.flow_method_id */
	fm.bank_flag, /* --> flow_method.bank_flag */
	pb.code party_bankacc_no, /* --> party_bankacc.code */
	pb.name party_bankacc_name, /* --> party_bankacc.name */
	pb.swift_bic party_bankacc_swift_bic, /* --> party_bankacc.swift_bic */
	pb.iban party_bankacc_iban, /* --> party_bankacc.iban */
	pb.branch_code party_bankacc_branch_code, /* --> party_bankacc.branch_code */
	p2.ext_name party_bank_name, /* --> party_bankacc.bank_id */
	p2.business_ref_no2 party_bank_bus_ref_no2, /* --> party_bankacc.business_ref_no2 */
	f.settlement_bank_info_id, /* --> flow.settlement_bank_info_id */
	f.beneficiary_id, /* --> flow.beneficiary_id */
	f.status, /* --> flow.status */
	f.release_dt,  /* --> flow.release_dt */
	f.in_recovery, /* --> flow.in_recovery */
	f.bank_interface_run_id, /* --> flow.bank_interface_run_id */
	f.is_first_settlement_bank_info, /* --> flow.is_first_settlement_bank_info */
	xt.value party_bankacc_cc_association, /* --> xt_lookupset.value */
	pb.credit_card_expiry_dt party_bankacc_cc_expiry_dt, /* --> party_bankacc.credit_card_expiry_dt */
	pb.credit_card_holder_name party_bankacc_cc_holder_name, /* --> party_bankacc.credit_card_holder_name */
	pb.credit_card_number party_bankacc_cc_number, /* --> party_bankacc.credit_card_number */
	pb.credit_card_security party_bankacc_cc_security, /* --> party_bankacc.credit_card_security */
	pb.credit_card_version party_bankacc_cc_version, /* --> party_bankacc.credit_card_version */
	f.purchase_invoice_id, /* --> flow.purchase_invoice_id */
	piv.reference purchase_invoice_reference, /* --> purchase_invoice.reference */
	piv.purchase_invoice_type, /* --> purchase_invoice.purchase_invoice_type */
	f.custom_flow_hdr_id, /* --> flow.custom_flow_hdr_id */
	f.settle_count, /* --> flow.settle_count */
	sbf.signature_dt, /* --> settlement_bank_info.signature_dt */
	sbf.reference settlement_bank_info_reference, /* --> settlement_bank_info.reference */
	p1.business_ref_no business_unit_ref_no, /* --> Business unit reference number field */
	--Party Address and Party Account
	pb.branch_address party_bankacc_address, /* --> branch_address */
	padd.street party_street, /* --> street */
	padd.suburb party_suburb, /* --> suburb */
	padd.city_id party_city_id, /* --> city_id  */
	padd.state_province_id party_state_province_id, /* --> state_province_id */
	ploc.ext_name party_province_name, /* --> province/state name */
	padd.zip_code party_zip_code, /* --> zip_code */
	padd.country_region_id party_country_region_id, /* --> country_region_id */
	ploc2.ext_name party_country_region_name, /* --> country_region_name */
	--BU address and BU Account
	b.branch_address bunit_bankacc_address, /* --> party_bankacc.address */
	badd.street bunit_street, /* --> street */
	badd.suburb bunit_suburb, /* --> suburb */
	badd.city_id bunit_city_id, /* --> city_id */
	badd.state_province_id bunit_state_province, /* --> state_province_id */
	badd.zip_code bunit_zip_code, /* --> zip_code */
	badd.country_region_id bunit_country_region, /* --> country_region_id */
	ploc3.ext_name bunit_country_region_name, /* --> country_region_name */
	axsp_purchase_invoice_comments(f.purchase_invoice_id) purchase_invoice_comments, /* --> purchase_invoice_comments */
	f.amt_matched, /* --> flow.amt_matched */
	b.gl_account_id gl_account_id, /* --> bankacc.gl_account_id */
	c.is_apportion_remaining_amt, /* Should any remaining bankflow amount be allocate to the contract party, see appiontment rules. */
	f.stamp /* --> flow.stamp */
FROM
	contract c
	INNER JOIN contract_det cd ON c.contract_id = cd.contract_id
	INNER JOIN flow f ON c.contract_id = f.contract_id
	--Joins to Flow
	INNER JOIN party_account pa ON f.party_account_id = pa.party_account_id
	INNER JOIN bankacc b ON f.bank_account_id = b.bankacc_id
	INNER JOIN flow_method fm ON f.flow_method_id = fm.flow_method_id
	INNER JOIN purchase_invoice piv ON f.purchase_invoice_id = piv.purchase_invoice_id
	INNER JOIN settlement_bank_info sbf ON f.settlement_bank_info_id = sbf.settlement_bank_info_id
	INNER JOIN party p ON pa.party_id = p.party_id
	INNER JOIN party p1 ON b.account_owner_id = p1.party_id
	INNER JOIN party p3 ON b.bank_id = p3.party_id
	INNER JOIN party_bankacc pb ON sbf.party_bankacc_id = pb.bankacc_id
	INNER JOIN party p2 ON pb.bank_id = p2.party_id
	INNER JOIN xt_lookupset xt ON pb.credit_card_association_id = xt.xt_lookupset_id
	INNER JOIN custom_flow_hdr cfh ON f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
	LEFT JOIN party_address padd ON padd.party_id = p.party_id AND padd.address_type = 5001 AND padd.is_current = 1
	LEFT JOIN party_address badd ON badd.party_id = b.account_owner_id AND badd.address_type = 5001 AND badd.is_current = 1
	LEFT JOIN location ploc ON ploc.location_id = padd.state_province_id AND padd.state_province_id <> 0
	LEFT JOIN location ploc2 ON ploc2.location_id = padd.country_region_id AND padd.country_region_id <> 0
	LEFT JOIN location ploc3 ON ploc3.location_id = badd.country_region_id AND badd.country_region_id <> 0
WHERE
	c.contract_id >= 0 AND
	c.transfer_type != 9002 AND
	f.is_cash = 1 AND
	f.is_shadow_copy = 0 AND
	f.status not in (2104, 2105) AND
	f.reversal_status IN (4200, 4202)
/
-- NOTE: If you add a new column to this view, remember to add it to the 
--       axvw_bank_interface_bf_rej view below (SYN).
CREATE OR REPLACE FORCE VIEW axvw_bank_interface_bf
AS
SELECT	1 is_bank_flow,
	bf.bank_flow_id flow_id, /* --> bank_flow.flow_id */
	bf.contract_id,  /* --> bank_flow.contract_id */
	c.reference contract_ref, /* --> contract.reference */
	c.product_style, /* --> contract.product_style */
	cd.ext_reference contract_ref2, /* --> contract_det.ext_reference */
	1002 flow_type, /* --> Manual */
	bf.actual_dt, /* --> bank_flow.actual_dt */
	bf.actual_dt expected_dt, /* --> bank_flow.actual_dt */
	bf.currency_id, /* --> bank_flow.currency_id */
	bf.amount, /* --> bank_flow.amount */
	bf.amount amt_gross, /* --> bank_flow.amt_gross - flow.amt_matched*/
	0 amt_gross_netted,  /* --> amount gross netted is not applicable for a bank flow */
	bf.bank_account_id, /* --> bank_flow.bank_account_id */
	bf.party_account_id, /* --> bank_flow.party_account_id */
	0 nett_no, /* --> nett_no */
	bf.installment_no, /* --> bank_flow.installment_no */
	bf.extra_info, /* --> bank_flow.extra_info */
	p4.name branch_name, /* --> party.name */
	bf.cheque_no, /* --> bank_flow.cheque_no */
	bf.cheque_dt, /* --> bank_flow.cheque_dt */
	N' ' payee_ref, /* --> payee_ref */
	N' ' payment_ref, /* --> payment_ref */
	b.account_owner_id business_unit_id, /* --> bankacc.account_owner_id */
	b.code bank_account_no, /* --> bankacc.code */
	b.full_account_no bank_account_no_full, /* --> bankacc.full_account_no */
	b.identification_no bankacc_identification_no, /* --> bankacc.identification_no used by BACS Interface */
	case WHEN NVL(LENGTH(TRIM(c.private_label_name)), 0) = 0 THEN p1.ext_name ELSE c.private_label_name END business_unit_name, /* --> contract.private_label_name */
	p1.reference business_unit_reference, /* --> Business units reference field */
	b.branch_code bank_account_branch_code,  /* --> bankacc.branch_code */
	b.name bank_account_name, /* --> bankacc.name */
	b.swift_bic bank_account_swift_bic, /* --> bankacc.swift_bic */
	b.iban bank_account_iban, /* --> bankacc.iban */
	-- Linked bank account for bi-directional netting
	b.auto_net_bankacc_id,  /* bankacc.auto_net_bankacc_id */
	p3.ext_name bank_name, /* --> bankacc.bank_name */
	pa.account_no party_account_no, /* --> party_account.party_account_no */
	pa.name party_account_name, /* --> party_account.name */
	pa.party_id, /* --> party_account.party_id */
	p.party_no, /* --> party.party_no */
	p.name party_name, /* --> party.name */
	p.reference party_reference, /* --> party.reference */
	sbf.flow_direction, /* --> settlement_bank_info.flow_direction */
	sbf.reference settlement_bank_info_reference, /* --> settlement_bank_info.reference */
	bf.flow_method_id, /* --> bank_flow.flow_method_id */
	fm.bank_flag, /* --> flow_method.bank_flag */
	pb.code party_bankacc_no, /* --> party_bankacc.code */
	pb.name party_bankacc_name, /* --> party_bankacc.name */
	pb.swift_bic party_bankacc_swift_bic, /* --> party_bankacc.swift_bic */
	pb.iban party_bankacc_iban, /* --> party_bankacc.iban */
	pb.branch_code party_bankacc_branch_code, /* --> party_bankacc.branch_code */
	p2.ext_name party_bank_name, /* --> party_bankacc.bank_id */
	bf.settlement_bank_info_id, /* --> bank_flow.settlement_bank_info_id */
	0 beneficiary_id, /* --> bank_flow.beneficiary_id */
	xt.value party_bankacc_cc_association, /* --> xt_lookupset.value */
	pb.credit_card_expiry_dt party_bankacc_cc_expiry_dt, /* --> party_bankacc.credit_card_expiry_dt */
	pb.credit_card_holder_name party_bankacc_cc_holder_name, /* --> party_bankacc.credit_card_holder_name */
	pb.credit_card_number party_bankacc_cc_number, /* --> party_bankacc.credit_card_number */
	pb.credit_card_security party_bankacc_cc_security, /* --> party_bankacc.credit_card_security */
	pb.credit_card_version party_bankacc_cc_version, /* --> party_bankacc.credit_card_version */
	bf.purchase_invoice_id, /* --> bank_flow.purchase_invoice_id */
	piv.reference purchase_invoice_reference, /* --> purchase_invoice.reference */
	NVL(sq2.comments, ' ') purchase_invoice_comments, /* --> purchase_invoice_comments */
	NVL(xt2.value, ' ') bank_flow_remarks, /* xt_lookupset for bank_flow.remarks_code */
	bf.rejected_reason, /* --> bank_flow.rejected_reason */
	bf.stamp /* --> bank_flowstamp */
FROM	bank_flow bf
			left join (
					select
						sq1.pivid,
						case when sq1.pivid = 0 then TO_CLOB(' ')
						else note.comments
						end as comments
					from
						note
					join
						(	select
								note.purchase_invoice_id pivid,
								min(note.note_id) noteid
							from
								note
							group by
								note.purchase_invoice_id
						) sq1
					on sq1.noteid = note.note_id
				) sq2 on (sq2.pivid = bf.purchase_invoice_id),
	party_account pa,
	party p,
	party p1,
	party p2,
	party p3,
	party p4,
	bankacc b,
	settlement_bank_info sbf,
	party_bankacc pb,
	contract c,
	contract_det cd,
	flow_method fm,
	xt_lookupset xt,
	xt_lookupset xt2,
	purchase_invoice piv
WHERE	c.contract_id = bf.contract_id AND
	c.transfer_type != 9002 AND
	cd.contract_id = bf.contract_id AND
	pa.party_account_id = bf.party_account_id AND
	bf.is_shadow_copy = 0 AND
	bf.reversal_status IN (4200, 4202) AND
	/*
	Manual or Post-Dated Cheques
	The PDCs are processed separately in BRCashMgmtProcess.
	Possibly we need to extract this view to a base view and
	select from it separately based on is_manual_dd and flow_method_id?
	*/
	(bf.is_manual_dd = 1 OR fm.flow_method_id = 5137) AND
	bf.bank_interface_run_id = 0 AND
	pa.party_id = p.party_id AND
	b.account_owner_id = p1.party_id AND
	b.bankacc_id = bf.bank_account_id AND
	fm.flow_method_id = bf.flow_method_id AND
	sbf.settlement_bank_info_id = bf.settlement_bank_info_id AND
	pb.bankacc_id = sbf.party_bankacc_id AND
	pb.bank_id = p2.party_id AND
	pb.credit_card_association_id = xt.xt_lookupset_id AND
	c.contract_id >= 0 AND
	bf.purchase_invoice_id = piv.purchase_invoice_id AND
	b.bank_id = p3.party_id AND
	bf.branch_id = p4.party_id AND
	bf.remarks_code = xt2.xt_lookupset_id 
/
CREATE OR REPLACE FORCE VIEW axvw_bank_interface_bf_rej
AS
SELECT	1 is_bank_flow,
	bf.bank_flow_id flow_id, /* --> bank_flow.flow_id */
	bf.contract_id,  /* --> bank_flow.contract_id */
	c.reference contract_ref, /* --> contract.reference */
	c.product_style, /* --> contract.product_style */
	cd.ext_reference contract_ref2, /* --> contract_det.ext_reference */
	1002 flow_type, /* --> Manual */
	bf.actual_dt, /* --> bank_flow.actual_dt */
	bf.actual_dt expected_dt, /* --> bank_flow.actual_dt */
	bf.currency_id, /* --> bank_flow.currency_id */
	bf.amount, /* --> bank_flow.amount */
	bf.amount amt_gross, /* --> bank_flow.amt_gross - flow.amt_matched*/
	0 amt_gross_netted,  /* --> amount gross netted is not applicable for a bank flow */
	bf.bank_account_id, /* --> bank_flow.bank_account_id */
	bf.party_account_id, /* --> bank_flow.party_account_id */
	0 nett_no, /* --> nett_no */
	bf.installment_no, /* --> bank_flow.installment_no */
	bf.extra_info, /* --> bank_flow.extra_info */
	p4.name branch_name, /* --> party.name */
	bf.cheque_no, /* --> bank_flow.cheque_no */
	bf.cheque_dt, /* --> bank_flow.cheque_dt */
	N' ' payee_ref, /* --> payee_ref */
	N' ' payment_ref, /* --> payment_ref */
	b.account_owner_id business_unit_id, /* --> bankacc.account_owner_id */
	b.code bank_account_no, /* --> bankacc.code */
	b.full_account_no bank_account_no_full, /* --> bankacc.full_account_no */
	b.identification_no bankacc_identification_no, /* --> bankacc.identification_no used by BACS Interface */
	case WHEN NVL(LENGTH(TRIM(c.private_label_name)), 0) = 0 THEN p1.ext_name ELSE c.private_label_name END business_unit_name, /* --> contract.private_label_name */
	p1.reference business_unit_reference, /* --> Business units reference field */
	b.branch_code bank_account_branch_code,  /* --> bankacc.branch_code */
	b.name bank_account_name, /* --> bankacc.name */
	b.swift_bic bank_account_swift_bic, /* --> bankacc.swift_bic */
	b.iban bank_account_iban, /* --> bankacc.iban */
	-- Linked bank account for bi-directional netting
	b.auto_net_bankacc_id,  /* bankacc.auto_net_bankacc_id */
	p3.ext_name bank_name, /* --> bankacc.bank_name */
	pa.account_no party_account_no, /* --> party_account.party_account_no */
	pa.name party_account_name, /* --> party_account.name */
	pa.party_id, /* --> party_account.party_id */
	p.party_no, /* --> party.party_no */
	p.name party_name, /* --> party.name */
	p.reference party_reference, /* --> party.reference */
	sbf.flow_direction, /* --> settlement_bank_info.flow_direction */
	sbf.reference settlement_bank_info_reference, /* --> settlement_bank_info.reference */
	bf.flow_method_id, /* --> bank_flow.flow_method_id */
	fm.bank_flag, /* --> flow_method.bank_flag */
	pb.code party_bankacc_no, /* --> party_bankacc.code */
	pb.name party_bankacc_name, /* --> party_bankacc.name */
	pb.swift_bic party_bankacc_swift_bic, /* --> party_bankacc.swift_bic */
	pb.iban party_bankacc_iban, /* --> party_bankacc.iban */
	pb.branch_code party_bankacc_branch_code, /* --> party_bankacc.branch_code */
	p2.ext_name party_bank_name, /* --> party_bankacc.bank_id */
	bf.settlement_bank_info_id, /* --> bank_flow.settlement_bank_info_id */
	0 beneficiary_id, /* --> bank_flow.beneficiary_id */
	xt.value party_bankacc_cc_association, /* --> xt_lookupset.value */
	pb.credit_card_expiry_dt party_bankacc_cc_expiry_dt, /* --> party_bankacc.credit_card_expiry_dt */
	pb.credit_card_holder_name party_bankacc_cc_holder_name, /* --> party_bankacc.credit_card_holder_name */
	pb.credit_card_number party_bankacc_cc_number, /* --> party_bankacc.credit_card_number */
	pb.credit_card_security party_bankacc_cc_security, /* --> party_bankacc.credit_card_security */
	pb.credit_card_version party_bankacc_cc_version, /* --> party_bankacc.credit_card_version */
	bf.purchase_invoice_id, /* --> bank_flow.purchase_invoice_id */
	piv.reference purchase_invoice_reference, /* --> purchase_invoice.reference */
	N' ' purchase_invoice_comments, /* --> purchase_invoice_comments */
	bf.rejected_reason, /* --> bank_flow.rejected_reason*/
	NVL(xt2.value, ' ') bank_flow_remarks, /* xt_lookupset for bank_flow.remarks_code */
	bf.stamp /* --> bank_flowstamp */
FROM	bank_flow bf,
	party_account pa,
	party p,
	party p1,
	party p2,
	party p3,
	party p4,
	bankacc b,
	settlement_bank_info sbf,
	party_bankacc pb,
	contract c,
	contract_det cd,
	flow_method fm,
	xt_lookupset xt,
	xt_lookupset xt2,
	purchase_invoice piv
WHERE	c.contract_id = bf.contract_id AND
	c.transfer_type != 9002 AND
	cd.contract_id = bf.contract_id AND
	pa.party_account_id = bf.party_account_id AND
	bf.is_shadow_copy = 0 AND
	bf.reversal_status IN (4200, 4202) AND
	(	/* rejected bank flows that have not been interfaced */		
		bf.bank_interface_run_id > 0 AND 
		bf.deleted_bank_interface_run_id = 0 AND
		bf.rejected_reason > (SELECT xt_lookupset_id FROM xt_lookupset WHERE set_name = 'RejectedReason' AND value = 'None') AND
		bf.is_deleted = 1
	) AND
	pa.party_id = p.party_id AND
	b.account_owner_id = p1.party_id AND
	b.bankacc_id = bf.bank_account_id AND
	fm.flow_method_id = bf.flow_method_id AND
	sbf.settlement_bank_info_id = bf.settlement_bank_info_id AND
	pb.bankacc_id = sbf.party_bankacc_id AND
	pb.bank_id = p2.party_id AND
	pb.credit_card_association_id = xt.xt_lookupset_id AND
	c.contract_id >= 0 AND
	bf.purchase_invoice_id = piv.purchase_invoice_id AND
	b.bank_id = p3.party_id AND
	bf.branch_id = p4.party_id AND
	bf.remarks_code = xt2.xt_lookupset_id 
/
CREATE OR REPLACE FORCE VIEW axvw_programs_view AS
SELECT p.program_id,
       p.name,
       p.code,
       p.ext_name,
       p.effect_dt,
       p.expiry_dt,
       bm.business_model_id business_model_value,
       u1.name manager_name,
       case (select count(*) from program_list pl where pl.program_id = p.program_id and pl.list_type = 27500)
         when 0 then cast('User''s Business Unit' as NVARCHAR2(50))
         when 1 then
           (case (select pl.value_id from program_list pl where pl.program_id = p.program_id and pl.list_type = 27500)
             when 0 then cast('All' as NVARCHAR2(50))
             else (select ext_name from party where party_id = (select pl.value_id from program_list pl
                   where pl.program_id = p.program_id and pl.list_type = 27500)) END)
         else cast('Multiple' as NVARCHAR2(50)) end business_units,
       case (select count(*) from program_list pl where pl.program_id = p.program_id and pl.list_type = 27512)
         when 0 then cast('No Overrides' as NVARCHAR2(50))
         when 1 then
           (case (select pl.value_id from program_list pl where pl.program_id = p.program_id and pl.list_type = 27512)
             when 0 then cast('All' as NVARCHAR2(50))
             else (select ext_name from party where party_id = (select pl.value_id from program_list pl
                   where pl.program_id = p.program_id and pl.list_type = 27512)) end)
         else cast('Multiple' as NVARCHAR2(50)) end branches,
       case p.branch_id when 0 then cast('User''s Operating Unit' as NVARCHAR2(50))
         else pa.ext_name end branch_name,
       pp1.name product_name,
       lu2.value residual_value_type,
       lu3.value ins_timing_value,
       p.is_active,
       p.is_approval_pending,
       p.stamp
FROM  program p,
      business_model bm,
      lookupset lu2,
      lookupset lu3,
      ax_user u1,
      product pp1,
      party pa
WHERE bm.business_model_id = p.business_model_id
  and lu2.lookupset_id = p.residual_value_type
  and lu3.lookupset_id = p.ins_timing
  and u1.ax_user_id = p.manager_id
  and pp1.product_id = p.product_id
  and pa.party_id = p.branch_id
  and p.is_deleted = 0
  and p.credit_line_id = 0
/
CREATE OR REPLACE FORCE VIEW axvw_reserve_stmt_flows AS
--select cash flows and bank_flow_matches for this reserve and reserve contract
--select a row for each allocation (bfm) record
select f.flow_id, f.contract_id, f.party_account_id, f.reserve_id, f.flow_type,
bfm.bank_flow_match_id, f.amt_gross, bfm.amt_matched amt_matched, f.is_cash, f.calc_dt, f.expected_dt,
bfm.matched_dt matched_dt, bfm.reserve_statement_id, f.reversal_status, f.currency_id,
f.stamp, f.custom_flow_hdr_id, cf.name cf_name, cf.timing cf_timing, cf.purpose cf_purpose
from flow f, custom_flow_hdr cf, reserve r, bank_flow_match bfm
where r.reserve_id <> 0
and cf.custom_flow_hdr_id = f.custom_flow_hdr_id
and bfm.flow_id = f.flow_id
and bfm.is_deleted = 0
and f.is_cash = 1
and f.contract_id >= 0
and (f.contract_id = r.contract_id or
(f.reserve_id = r.reserve_id and f.is_shadow_copy = 0))
UNION ALL
--select non cash flows - assume matched
select f.flow_id, f.contract_id, f.party_account_id, f.reserve_id, f.flow_type,
0, f.amt_gross, f.amt_gross amt_matched, f.is_cash, f.calc_dt, f.expected_dt,
f.expected_dt matched_dt, f.reserve_statement_id, f.reversal_status, f.currency_id,
f.stamp, f.custom_flow_hdr_id, cf.name cf_name, cf.timing cf_timing, cf.purpose cf_purpose
from flow f, custom_flow_hdr cf, reserve r
where r.reserve_id <> 0
and cf.custom_flow_hdr_id = f.custom_flow_hdr_id
and f.is_cash = 0
and f.contract_id >= 0
and (f.contract_id = r.contract_id or
(f.reserve_id = r.reserve_id and f.is_shadow_copy = 0))
/
CREATE OR REPLACE FORCE VIEW axvw_reserve_unalloc_flows AS
--select unmatched cash flows and non cash drawdown timed reserve contributions
select f.flow_id, f.contract_id, f.party_account_id, f.reserve_id, f.flow_type,
f.amt_gross, f.amt_matched, f.is_cash, f.calc_dt, f.expected_dt,
f.reserve_statement_id, f.reversal_status, f.currency_id,
f.stamp, f.custom_flow_hdr_id, cf.name cf_name, cf.timing cf_timing, cf.purpose cf_purpose
from flow f, custom_flow_hdr cf, reserve r
where r.reserve_id <> 0
and f.contract_id >= 0
and cf.custom_flow_hdr_id = f.custom_flow_hdr_id
and ((f.amt_matched <> f.amt_gross and f.is_cash = 1) or
--include non cash drawdown timed reserve contributions
(f.is_cash = 0 and f.flow_type = 1010 and cf.purpose = 14211 and cf.timing = 14007))
and (f.contract_id = r.contract_id or
(f.reserve_id = r.reserve_id and f.is_shadow_copy = 0))
/
CREATE OR REPLACE FORCE VIEW axvw_rev_bonus_view
AS
select
	psbh.policy_series_bonus_hdr_id,
	psbh.policy_series_id,
	psbh.declaration_dt,
	psbh.effective_dt,
	psbh.input_user_id,
	psbh.input_dt,
	psbh.bonus_status,
	psbd.policy_series_bonus_det_id,
	psbd.sum_assured_pct,
	psbd.bonus_method,
	psbd.attaching_bonus_pct,
	psbh.stamp
from policy_series_bonus_hdr psbh, policy_series_bonus_det psbd
where psbh.policy_series_bonus_hdr_id = psbd.policy_series_bonus_hdr_id
and psbh.bonus_type = 34400
/
CREATE OR REPLACE FORCE VIEW axvw_upload_batch_view
AS
	SELECT
		bank_statement_batch_id upload_batch_id,
		batch_upload_type,
		batch_dt,
		reference,
		originator,
		file_path,
		status,
		input_dt,
		error_description,
		approval_status,
		stamp
	FROM
		bank_statement_batch
	UNION ALL
	SELECT
		contract_batch_id upload_batch_id,
		batch_upload_type,
		batch_dt,
		reference,
		originator,
		file_path,
		status,
		input_dt,
		error_description,
		approval_status,
		stamp
	FROM
		contract_batch
	UNION ALL
	SELECT
		receipt_batch_id upload_batch_id,
		batch_upload_type,
		batch_dt,
		reference,
		originator,
		file_path,
		status,
		input_dt,
		error_description,
		approval_status,
		stamp
	FROM
		receipt_batch
	UNION ALL
	SELECT
		upload_batch_id,
		batch_upload_type,
		batch_dt,
		reference,
		originator,
		file_path,
		status,
		input_dt,
		error_description,
		approval_status,
		stamp
	FROM
		upload_batch
/
CREATE OR REPLACE FORCE VIEW axvw_upload_package_view
AS
	SELECT
		p.bank_statement_package_id upload_package_id,
		p.bank_statement_batch_id upload_batch_id,
		p.batch_upload_type,
		b.originator,
		b.batch_dt,
		b.input_dt,
		p.file_path,
		p.status,
		p.error_description,
		p.stamp
	FROM
		bank_statement_package p
	INNER JOIN bank_statement_batch b ON b.bank_statement_batch_id = p.bank_statement_batch_id
	UNION ALL
	SELECT
		p.contract_package_id upload_package_id,
		p.contract_batch_id upload_batch_id,
		p.batch_upload_type,
		b.originator,
		b.batch_dt,
		b.input_dt,
		p.file_path,
		p.status,
		p.error_description,
		p.stamp
	FROM
		contract_package p
	INNER JOIN contract_batch b ON b.contract_batch_id = p.contract_batch_id
	UNION ALL
	SELECT
		p.receipt_package_id upload_package_id,
		p.receipt_batch_id upload_batch_id,
		p.batch_upload_type,
		b.originator,
		b.batch_dt,
		b.input_dt,
		p.file_path,
		p.status,
		p.error_description,
		p.stamp
	FROM
		receipt_package p
	INNER JOIN receipt_batch b ON b.receipt_batch_id = p.receipt_batch_id
	UNION ALL
	SELECT
		p.upload_package_id,
		p.upload_batch_id,
		p.batch_upload_type,
		b.originator,
		b.batch_dt,
		b.input_dt,
		p.file_path,
		p.status,
		p.error_description,
		p.stamp
	FROM
		upload_package p
	INNER JOIN upload_batch b ON b.upload_batch_id = p.upload_batch_id
/
CREATE OR REPLACE FORCE VIEW axvw_credit_line_takedown
AS
SELECT
clt.credit_line_takedown_id, clt.credit_line_id, cl.owner_id, clt.party_id, c.contract_id, c.reference,
c.contract_state, c.business_unit_id, clt.contract_party_role,
clt.currency_id, clt.amt_utilisation,  clt.amt_takedown, clt.takedown_dt, clt.takedown_user_id,
c.calc_dt, c.mature_dt1, c.product_id, c.program_id, cl.party_id as cl_party_id, 0 stamp
FROM credit_line_takedown clt, contract c, credit_line cl
WHERE cl.credit_line_id = clt.credit_line_id
and c.contract_id = clt.contract_id
/
CREATE OR REPLACE FORCE VIEW axvw_loan_account_flow
AS
	SELECT
	f.flow_id,
	f.contract_id,
	f.calc_dt,
	CASE WHEN f.flow_type = 1507 THEN f.calc_dt ELSE f.expected_dt END expected_dt,
	f.flow_type,
	f.custom_flow_hdr_id,
	f.installment_no,
	f.amount,
	f.amt_gross,
	f.amt_principal,
	f.amt_interest,
	f.amt_gross - f.amount amt_tax,
	f.rate,
	f.party_account_id,
	f.bank_account_id,
	f.flow_method_id,
	0 loan_account_flow_type,
	c.product_style,
	c.contract_grp_style,
	f.stamp
from flow f
INNER JOIN contract c ON c.contract_id = f.contract_id
INNER JOIN custom_flow_hdr cfh ON cfh.custom_flow_hdr_id = f.custom_flow_hdr_id
where (f.is_cash = 1 or f.flow_type in (1000,1003,1010,1503,1505,1507,1031,1032))
and f.amt_matched = 0
and f.contract_id > 0
and f.reversal_status = 4200
and (cfh.include_in_ext_flow = 1 or cfh.custom_flow_hdr_id = 0 or c.product_style = 2016)
UNION ALL
	SELECT
	f.flow_id,
	f.contract_id,
	f.calc_dt,
	bf.actual_dt expected_dt,
	f.flow_type,
	f.custom_flow_hdr_id,
	f.installment_no,
	bfm.amt_matched - bfm.amt_matched_tax amount,
	bfm.amt_matched amt_gross,
	bfm.amt_matched_principal amt_principal,
	bfm.amt_matched_interest amt_interest,
	bfm.amt_matched_tax amt_tax,
	f.rate,
	f.party_account_id,
	f.bank_account_id,
	f.flow_method_id,
	1 loan_account_flow_type,
	c.product_style,
	c.contract_grp_style,
	bf.stamp
from bank_flow_match bfm
INNER JOIN flow f ON f.flow_id = bfm.flow_id
INNER JOIN bank_flow bf ON  bf.bank_flow_id = bfm.bank_flow_id
INNER JOIN custom_flow_hdr cfh ON cfh.custom_flow_hdr_id = f.custom_flow_hdr_id
INNER JOIN contract c ON c.contract_id = f.contract_id
where bfm.is_deleted = 0
and bf.is_deleted = 0
and bf.is_shadow_copy = 0
and f.is_cash = 1
and f.contract_id > 0
and f.is_shadow_copy = 0
and f.reversal_status = 4200
and f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
and (cfh.include_in_ext_flow = 1 or cfh.custom_flow_hdr_id = 0 or c.product_style = 2016)
/
CREATE OR REPLACE FORCE VIEW axvw_report_view as
select r.report_id, /* --> report.report_id */
       r.report_fmt_id, /* --> report.report_fmt_id */
       rf.name report_fmt_name, /* --> report_fmt.name */
       r.file_name, /* --> report.file_name */
       r.file_name_copy, /* --> report.file_name_copy */
       r.output_path || N'\' || r.file_name file_path,
       r.output_path, /* --> report.output_path */
       r.created_dt, /* --> report.created_dt */
       r.status, /* --> report.status */
       l1.value status_value, /* Report status */
       r.is_deleted is_output_deleted, /* --> report.is_deleted */
       u.ext_name created_by_user, /* --> user.ext_name */
       u2.ext_name deleted_by_user, /* --> user.ext_name */
       r.deleted_dt, /* --> report.deleted_dt */
       d.device_category output_method, /* --> doc_output_method.device_category */
       l2.value output_method_value, /* Output method value */
       r.output_address, /* --> report.output_address */
       rtg.report_task_grp_id,
       rtg.name report_group_name, /* --> report_task_group.name */
       r.doc_output_method_id, /* --> report.doc_output_method_id */
       d.reference output_method_reference, /* --> doc_output_method.reference */
       r.is_active,
       r.stamp
from report r, report_fmt rf, report_task_grp rtg, lookupset l1, lookupset l2, ax_user u, ax_user u2, doc_output_method d
where r.report_fmt_id = rf.report_fmt_id
and   rf.report_task_grp_id = rtg.report_task_grp_id
and   l1.lookupset_id = r.status
and   l2.lookupset_id = d.device_category
and   u.ax_user_id = r.created_by_user_id
and   u2.ax_user_id = r.deleted_by_user_id
and   d.doc_output_method_id = r.doc_output_method_id
/
CREATE OR REPLACE FORCE VIEW axvw_fs_interface
AS
SELECT
fs.financing_statement_id, /* financing_statement.financing_statement_id */
fs.contract_id, /* financing_statement.contract_id */
fs.registration_authority_id, /* financing_statement.registration_authority_id */
p1.ext_name registration_authority_name, /* party.ext_name */
fs.reference, /* financing_statement.reference */
fs.filing_method_id, /* financing_statement.filing_method_id */
ffm.name financing_filing_method_name, /* financing_filing_method.name */
ffm.reg_secured_party_grp_id, /* financing_filing_method.reg_secured_party_grp_id */
fs.filing_type, /* financing_statement.filing_type */
l1.value filing_type_value, /* lookupset.value (FinancingFilingType) */
fs.filing_dt, /* financing_statement.filing_dt */
fs.validity_period_mths, /* financing_statement.validity_period_mths */
fs.expiry_dt, /* financing_statement.expiry_dt */
fs.primary_party_id, /* financing_statement.primary_party_id */
p2.ext_name primary_party_name, /* party.ext_name */
fs.secondary_party_id, /* financing_statement.secondary_party_id */
p3.ext_name secondary_party_name, /* party.ext_name */
fs.comments, /* financing_statement.comments */
fs.acceptance_dt, /* financing_statement.acceptance_dt */
fs.discharge_dt, /* financing_statement.discharge_dt */
fs.secured_party_pin, /* financing_statement.secured_party_pin - HPI Customer Code */
fs.debtor_party_pin, /* financing_statement.debtor_party_pin */
fs.billing_reference, /* financing_statement.billing_reference - HPI Agreement Type */
fs.primary_debtor_ident, /* financing_statement.primary_debtor_ident */
fs.secondary_debtor_ident, /* financing_statement.secondary_debtor_ident */
fs.processing_state, /* financing_statement.processing_state */
l2.value processing_state_value, /* lookupset.value (FinancingFilingState) */
fs.version, /* financing_statement.version */
fs.stamp    /* financing_statement.stamp */
FROM financing_statement fs
INNER JOIN financing_filing_method ffm ON fs.filing_method_id = ffm.financing_filing_method_id
INNER JOIN lookupset l1 ON fs.filing_type = l1.lookupset_id
INNER JOIN lookupset l2 ON fs.processing_state = l2.lookupset_id
INNER JOIN party p1 ON fs.registration_authority_id = p1.party_id
INNER JOIN party p2 ON fs.primary_party_id = p2.party_id
INNER JOIN party p3 ON fs.secondary_party_id = p3.party_id
/

CREATE OR REPLACE FORCE VIEW axvw_financing_statmnt_view AS
	SELECT DISTINCT fs.filing_dt, 
		fs.expiry_dt, 
		fs.filing_type,
		(CASE WHEN ffm.uses_workflow = 0 THEN '' ELSE (SELECT to_char(value) FROM xt_lookupset WHERE set_name = 'SecurityAgencyPrimaryRef' and code = fs.security_agency_primary_ref)END) AS security_agency_primary_ref,
		fsa.asset_hdr_id,
		(CASE WHEN ffm.uses_workflow = 0 THEN 0 ELSE w.workflow_id END) AS workflow_id,
		(CASE WHEN ffm.uses_workflow = 0 THEN 0 ELSE (SELECT wf_state_type_id FROM wf_state_type WHERE wf_state_type_id = w.current_state_type)END) AS workflow_state_id,
		(CASE WHEN ffm.uses_workflow = 0 THEN '' ELSE (SELECT to_char(wf_state_type.name) FROM wf_state_type WHERE wf_state_type_id = w.current_state_type) END) AS workflow_state,
		(CASE WHEN fsh.request_fin_statement_id = 0 THEN 0 ELSE (SELECT filing_type FROM financing_statement WHERE financing_statement_id = fsh.request_fin_statement_id) END) AS pending_request_filing_type,
		(CASE WHEN fsh.request_fin_statement_id = 0 THEN '' ELSE (SELECT to_char(value) FROM financing_statement join lookupset on financing_statement.filing_type = lookupset.lookupset_id WHERE financing_statement_id = fsh.request_fin_statement_id) END) AS pending_request,
		(CASE WHEN fsh.request_fin_statement_id = 0 THEN 0 ELSE (SELECT wf_state_type.wf_state_type_id FROM workflow JOIN wf_state_type ON current_state_type = wf_state_type_id JOIN wf_type ON wf_state_type.wf_type_id = wf_type.wf_type_id AND wf_type.is_42_workflow = 1 WHERE workflow.financing_statement_id = fsh.request_fin_statement_id) END) AS request_state_id,
		(CASE WHEN fsh.request_fin_statement_id = 0 THEN '' ELSE (SELECT to_char(wf_state_type.name) FROM workflow JOIN wf_state_type ON current_state_type = wf_state_type_id JOIN wf_type ON wf_state_type.wf_type_id = wf_type.wf_type_id AND wf_type.is_42_workflow = 1 WHERE workflow.financing_statement_id = fsh.request_fin_statement_id) END) AS request_state,
		fs.filing_number, 
		fs.reference,
		fs.processing_state,
		fs.order_id, 
		fsh.orig_fin_statement_id, 
		fsh.current_fin_statement_id, 
		fsh.request_fin_statement_id, 
		fsh.contract_id, 
		fs.registration_authority_id, 
		fs.filing_method_id,
		fs.primary_secured_party_id, 
		fsh.fin_statement_hdr_id,
		fsa.asset_id,
		ffm.uses_workflow,
		ffm.asset_linking_rule,
		fsa.is_included,
		fs.is_excluded_auto_processing,
		fsh.stamp
	FROM 
		fin_statement_hdr fsh JOIN financing_statement fs ON fsh.current_fin_statement_id = fs.financing_statement_id
		LEFT OUTER JOIN financing_statement_asset fsa ON fs.financing_statement_id = fsa.financing_filing_id
		JOIN financing_filing_method ffm ON fs.filing_method_id = ffm.financing_filing_method_id
		LEFT OUTER JOIN workflow w ON fsh.current_fin_statement_id = w.financing_statement_id
		LEFT OUTER JOIN wf_state_type wst ON w.current_state_type = wst.wf_state_type_id 
		LEFT OUTER JOIN wf_type wt ON wst.wf_type_id = wt.wf_type_id WHERE (w.workflow_id is null OR wt.is_42_workflow = 1)
/

CREATE OR REPLACE FORCE VIEW axvw_funding_contract AS
	SELECT
		fcl.funding_contract_link_id,
		fcl.contract_id,
		fcl.funding_status,
		c.input_dt,
		c.maturity_dt,
		c.interest_rate,
		fcl.amt_financed,
		c.term_months,
		c.ccy,
		c.product,
		c.customer_vendor,
		c.business_unit,
		c.party_account_no,
		c.fixed_floating_value,
		c.installment_freq_value,
		c.contract_state_value,
		c.credit_state_value,
		c.reference,
		fcl.funding_contract_id,
		fcl.facility_id,
		c.Status,
		CASE WHEN fcl.contract_restructure_id = 0 THEN 0 ELSE 1 END as is_restructure,
		fcl.portfolio_id,
		fcl.stamp
	FROM
		funding_contract_link fcl
			left join axvw_contract_view c on fcl.contract_id = c.contract_id
		where c.contract_id > 0
/

CREATE OR REPLACE FORCE VIEW axvw_asset_hdr_xml_view AS
	SELECT
		a.asset_hdr_id,
		a.name,
		a.model,
		a.serial_no,
		a.reference
	FROM ASSET_HDR a
/

CREATE OR REPLACE FORCE VIEW axvw_cpu_meter_xml_view AS
	SELECT
		m.cpu_meter_id,
		m.code cpu_meter_code,
		m.reference cpu_meter_reference,
		bgn.name billing_group_name,
		ah.asset_hdr_id,
		ah.name asset_hdr_name,
		ah.model asset_hdr_model,
		ah.serial_no asset_hdr_serial_no,
		ah.reference asset_hdr_reference
	FROM cpu_meter m
	INNER JOIN asset_hdr ah ON m.asset_hdr_id = ah.asset_hdr_id
	LEFT JOIN cpu_meter_cbg_name mn ON m.cpu_meter_id = mn.cpu_meter_id
	LEFT JOIN cpu_cbg_name bgn ON mn.cpu_cbg_name_id = bgn.cpu_cbg_name_id
/

CREATE OR REPLACE FORCE VIEW axvw_flow_auto_net_view AS
Select *
from (
	SELECT f.expected_dt,
		f.flow_id,
		f.contract_id,
		f.settlement_bank_info_id,
		sbi.auto_net_sbi_id,
		f.bank_account_id,
		ba.auto_net_bankacc_id,
		f.flow_method_id,
		f.amt_gross,
		f.amt_matched,
    CASE WHEN (fm.inflow_auto_net_contract = 1 AND f.amt_gross > 0)
        OR (fm.outflow_auto_net_contract = 1 AND f.amt_gross <= 0)
      THEN 1 ELSE 0 END is_net_within_contract,
    CASE WHEN ( c.suspension_state != 22501 AND c.intercept_state != 40801 AND c.contract_id >= 0) Then 1 else 0 end Include,
     f.stamp
  FROM flow f
    INNER JOIN contract c ON f.contract_id = c.contract_id
    INNER JOIN flow_method fm ON f.flow_method_id = fm.flow_method_id
    AND ((f.amt_gross > 0 AND fm.inflow_auto_net = 1)
      OR (f.amt_gross <= 0 AND fm.outflow_auto_net = 1))
    INNER JOIN settlement_bank_info sbi ON f.settlement_bank_info_id = sbi.settlement_bank_info_id
    INNER JOIN bankacc ba ON f.bank_account_id = ba.bankacc_id
  WHERE f.can_process = 1
    AND f.is_shadow_copy = 0
    AND f.status = 2102 -- Only released flows can be netted
    AND f.flow_method_id != 0
    AND f.settlement_bank_info_id != 0
    AND f.bank_account_id != 0
    AND f.nett_no = 0
    AND (f.amt_gross - f.amt_matched) != 0
) a
where Include = 1
/

CREATE OR REPLACE FORCE VIEW axvw_flow_adj_expected_dt AS
	SELECT f.expected_dt,
		f.release_dt,
		f.flow_id,
		f.contract_id,
		f.bank_account_id,
		f.flow_method_id,
		f.rejected_dt,
		fm.business_days_location_id,
		fm.currency_id,
		fm.processing_day,
		f.stamp
	FROM flow f,
		flow_method fm,
		settlement_bank_info sbi,
		contract c,
		party_bankacc pb
	WHERE f.can_process = 1
		AND f.is_shadow_copy = 0
		AND f.flow_method_id = fm.flow_method_id
		AND f.settlement_bank_info_id = sbi.settlement_bank_info_id
		AND f.status = 2100 --Pending
		AND f.reversal_status = 4200
		AND f.is_set = 7801
		AND fm.flow_method_id != 0
		AND sbi.status != 6701 --Required
		AND sbi.approval_status in (6501, 6503) -- status = Approved or None
		AND f.contract_id = c.contract_id
		AND c.suspension_state != 22501
		AND c.intercept_state != 40801
		AND c.contract_id >= 0
		AND pb.bankacc_id = sbi.party_bankacc_id
/

CREATE OR REPLACE FORCE VIEW axvw_interest_rates_view_int
AS
-- Contract level internal rates for leases with no rates schedule
SELECT c.contract_id, a.asset_id, a.asset_hdr_id, c.cof_rate, c.margin_rate, c.interest_rate, c.calc_dt
FROM contract c INNER JOIN contract_det cd ON c.contract_id = cd.contract_id
INNER JOIN asset a ON a.contract_id = c.contract_id
WHERE cd.tbl_rate_level = 25400 --contract lvl rates
AND c.rates_price_schedule_id = 0
AND c.product_style = 2001 --lease
UNION ALL
-- Contract level internal rates for leases with rates schedule
SELECT c.contract_id, a.asset_id, a.asset_hdr_id, p.cof_rate, p.margin_rate, p.interest_rate, p.calc_dt
FROM contract c INNER JOIN contract_det cd ON c.contract_id = cd.contract_id
INNER JOIN price_segment_templ p ON p.price_schedule_id = c.rates_price_schedule_id
INNER JOIN asset a ON a.contract_id = c.contract_id
WHERE cd.tbl_rate_level = 25400 --contract lvl rates
AND c.product_style = 2001 --lease
AND c.rates_price_schedule_id != 0
AND p.segment_no = (SELECT MAX(segment_no) FROM price_segment_templ p2 WHERE p2.price_schedule_id = p.price_schedule_id)
UNION ALL
-- Asset level internal rates for leases with no rates schedule - no rows!
SELECT a.contract_id, a.asset_id, a.asset_hdr_id, a.cof_rate, a.margin_rate, a.interest_rate, c.calc_dt
FROM contract c INNER JOIN contract_det cd ON c.contract_id = cd.contract_id
INNER JOIN asset a ON a.contract_id = c.contract_id
WHERE cd.tbl_rate_level = 25401 --asset lvl rates
AND c.product_style = 2001 --lease
AND a.rates_price_schedule_id = 0
UNION ALL
-- Asset level internal rates for leases with rates schedule
SELECT a.contract_id, a.asset_id, a.asset_hdr_id, a.cof_rate, a.margin_rate, a.interest_rate, p.calc_dt
FROM contract c INNER JOIN contract_det cd ON c.contract_id = cd.contract_id
INNER JOIN price_segment_templ p ON p.price_schedule_id = c.rates_price_schedule_id
INNER JOIN asset a ON a.contract_id = c.contract_id
WHERE cd.tbl_rate_level = 25401 --asset lvl rates
AND c.product_style = 2001 --lease
AND a.rates_price_schedule_id != 0
AND p.segment_no = (SELECT MAX(segment_no) FROM price_segment_templ p2 WHERE p2.price_schedule_id = p.price_schedule_id)
/

CREATE OR REPLACE FORCE VIEW axvw_interest_rates_view_ext
AS
-- Loan accounts with no rates schedule
SELECT c.contract_id, a.asset_id, a.asset_hdr_id, c.cof_rate, c.margin_rate, c.interest_rate, c.calc_dt
FROM contract c
INNER JOIN asset a ON a.contract_id = c.contract_id
WHERE c.rates_price_schedule_ext_id = 0
AND c.product_style = 2011 --loan accounts
UNION ALL
-- Loan accounts with rates schedule
SELECT c.contract_id, a.asset_id, a.asset_hdr_id, p.cof_rate, p.margin_rate, p.interest_rate, p.calc_dt
FROM contract c INNER JOIN asset a ON a.contract_id = c.contract_id
INNER JOIN price_segment_templ p ON p.price_schedule_id = c.rates_price_schedule_ext_id
WHERE
c.rates_price_schedule_ext_id != 0
AND c.product_style = 2011 --loan accounts
AND p.segment_no = (SELECT MAX(segment_no) FROM price_segment_templ p2 WHERE p2.price_schedule_id = p.price_schedule_id)
/

CREATE OR REPLACE FORCE VIEW axvw_interest_rates_view
AS
SELECT c.contract_id, c.reference, a.name, i.asset_id, a.asset_hdr_id,
0 margin_pattern_basis,
i.calc_dt,
0 eff_bus_pct,
ROUND(i.cof_rate, 4) cof_rate,
ROUND(i.margin_rate, 4) margin_rate,
ROUND(i.interest_rate, 4) interest_rate,
op.code op_unit,
bu.name bus_unit,
p.name product_code,
p.product_id
FROM axvw_interest_rates_view_int i
INNER JOIN asset_hdr a ON a.asset_hdr_id = i.asset_hdr_id
INNER JOIN contract c ON i.contract_id = c.contract_id
INNER JOIN product p ON p.product_id = c.product_id
INNER JOIN party op ON op.party_id = c.branch_id
INNER JOIN party bu ON bu.party_id = c.business_unit_id
WHERE i.contract_id+0 > 0 AND c.product_style IN (2001, 2011)
UNION ALL
SELECT c.contract_id, c.reference, a.name, i.asset_id, a.asset_hdr_id,
0 margin_pattern_basis,
i.calc_dt,
0 eff_bus_pct,
ROUND(i.cof_rate, 4) tax_eff_int_margin_pre_tax, -- pre tax effective internal cof (annual)
ROUND(i.margin_rate, 4) tax_eff_int_cof_pre_tax, -- pre tax effective internal margin (annual)
ROUND(i.interest_rate, 4) tax_eff_interest_rate_pre_tax, -- pre tax effective internal interest (annual)
op.code op_unit,
bu.name bus_unit,
p.name product_code,
p.product_id
FROM axvw_interest_rates_view_ext i
INNER JOIN asset_hdr a ON a.asset_hdr_id = i.asset_hdr_id
INNER JOIN contract c ON i.contract_id = c.contract_id
INNER JOIN product p ON p.product_id = c.product_id
INNER JOIN party op ON op.party_id = c.branch_id
INNER JOIN party bu ON bu.party_id = c.business_unit_id
WHERE i.contract_id+0 > 0
/

CREATE OR REPLACE FORCE VIEW axvw_opportunity_view
AS
SELECT
o.opportunity_id,
o.name,
xt.value source,
o.party_id,
p.ext_name party_ext_name,
pc.ext_name primary_contact,
o.description,
c.code ccy,
o.amt_revenue_potential,
o.bid_dt,
o.close_dt,
ps.ext_name sales_lead,
ps.party_id sales_lead_id,
o.rating,
o.business_unit_id,
pb.ext_name business_unit,
o.product_id,
prod.name product,
o.asset_type_id,
at.name asset_type,
o.reference,
o.num_assets,
o.is_active,
o.branch_id,
br.ext_name branch_name,
o.program_id,
prg.lu_name program_name,
x.amt_financed,
x.contract_state_value,
NVL(x.last_updated_dt, o.last_updated_dt) AS last_updated_dt,
o.stamp
  FROM opportunity o
  INNER JOIN party p ON o.party_id = p.party_id
  INNER JOIN party pc ON o.primary_contact_id = pc.party_id
  INNER JOIN party ps ON o.sales_lead_id = ps.party_id
  INNER JOIN party pb ON o.business_unit_id = pb.party_id
  INNER JOIN party br ON o.branch_id = br.party_id
  INNER JOIN currency c on o.currency_id = c.currency_id
  INNER JOIN product prod on o.product_id = prod.product_id
  INNER JOIN asset_type at on o.asset_type_id = at.asset_type_id
  INNER JOIN xt_lookupset xt ON o.source = xt.xt_lookupset_id
  INNER JOIN program prg ON o.program_id = prg.program_id
  LEFT JOIN (select c.opportunity_id, c.last_updated_dt, c.contract_state,c.amt_financed,l.value as contract_state_value,
	     	    RANK() OVER(PARTITION BY c.opportunity_id ORDER BY c.last_updated_dt DESC, contract_id desc) my_order
	          FROM contract c
	          INNER JOIN lookupset l on l.lookupset_id = c.contract_state
) x on x.opportunity_id=o.opportunity_id
where ISNULL(x.my_order,1)=1
/

CREATE OR REPLACE FORCE VIEW axvw_party_dup_match_view AS
select
	pdm.party_dup_match_id,
	pdm.party1_id,
	pdm.party2_id,
	pdm.similarity_score,
	p1.name party1_name,
	p2.name party2_name,
	pdm.kept_party_seq_no,
	pdm.stamp
from party_dup_match pdm, party p1, party p2
where pdm.party1_id = p1.party_id
and pdm.party2_id = p2.party_id
/

CREATE OR REPLACE FORCE VIEW axvw_pdc_view
AS
SELECT pdc.pdc_id,pdc.party_id,pdc.party_account_id,
p.name party_name,
p.party_no,
pa.name party_account_name,
pa.account_no party_account,
pdc.contract_id,
pdc.pdc_status as pdc_status_value, l1.value as pdc_status,
pdc.pdc_type as pdc_type_value, l2.value as pdc_type,
pdc.input_dt,pdc.cheque_dt,pdc.bank_in_dt,pdc.currency_id,pdc.amount,pdc.bank_account_id,pdc.cheque_no,
p_bank.ext_name as bank_name,
pdc.branch_name,pdc.drawer_name,
xtl_loc.value as custody_location,
pdc.remarks,pdc.bank_flow_id,
contract.suspension_state as suspension_state_value, l3.value as suspension_state,
contract.intercept_state as intercept_state_value, l4.value as intercept_state,
pdc.stamp
FROM pdc
INNER JOIN contract ON pdc.contract_id = contract.contract_id
INNER JOIN party p ON pdc.party_id = p.party_id
INNER JOIN party_account pa ON pdc.party_account_id = pa.party_account_id
INNER JOIN party p_bank ON pdc.bank_id = p_bank.party_id
INNER JOIN lookupset l1 ON pdc.pdc_status = l1.lookupset_id
INNER JOIN lookupset l2 ON pdc.pdc_type = l2.lookupset_id
INNER JOIN lookupset l3 ON contract.suspension_state = l3.lookupset_id
INNER JOIN lookupset l4 ON contract.intercept_state = l4.lookupset_id
INNER JOIN xt_lookupset xtl_loc ON pdc.custody_location_id = xtl_loc.xt_lookupset_id
WHERE pdc_id <> 0
/

CREATE OR REPLACE FORCE VIEW AXVW_ASSET_ADDRESS
AS
  SELECT ah.vo_type,
    ah.address_id,
    ah.contract_id,
    ah.asset_id,
    ah.asset_hdr_id,
    ah.original_purchase_dt,
    ah.name,
    ah.full_address,
    ah.sub_location,
    ah.asset_description,
    asset_type.name asset_type_name,
    manufacturer.name manufacturer,
    ah.model,
    ah.reference,
    ah.serial_no,
    ah.stock_code,
    ah.asset_type_id,
    ah.asset_status,
    asset_class_vehicle.registration_no,
    address.linked_party_address_id,
    ah.asset_hdr_address_type,
    ah.stamp
  FROM
    (SELECT asset.*,
      axsp_get_concat_address(asset.address_id) AS full_address
    FROM
      (SELECT 'VOAsset' vo_type,
        axsp_get_asset_address_id(asset.asset_id, axsp_dateonly(axsp_get_datetime())) AS address_id,
        asset.contract_id,
        asset.asset_id,
        asset.asset_hdr_id,
        asset_hdr.original_purchase_dt,
        asset_hdr.name,
        asset_hdr.sub_location,
        asset_hdr.asset_description,
        asset_hdr.model,
        asset_hdr.reference,
        asset_hdr.manufacturer_id,
        asset_hdr.serial_no,
        asset_hdr.stock_code,
        asset_hdr.asset_type_id,
        asset_hdr.asset_status,
        0 AS asset_hdr_address_type,
        asset.stamp
      FROM asset
      INNER JOIN asset_hdr
      ON asset_hdr.asset_hdr_id = asset.asset_hdr_id
      WHERE asset.asset_id+0 > 0
      ) asset
    UNION ALL
    SELECT asset_hdr.*,
      axsp_get_concat_address(asset_hdr.address_id) AS full_address
    FROM
      (SELECT 'VOAssetHdr' vo_type,
        ca.address_id,
        CASE
          WHEN asset_hdr.current_contract_id > 0
          THEN asset_hdr.current_contract_id
          ELSE asset_hdr.original_contract_id
        END contract_id,
        0 asset_id,
        asset_hdr.asset_hdr_id,
        asset_hdr.original_purchase_dt,
        asset_hdr.name,
        asset_hdr.sub_location,
        asset_hdr.asset_description,
        asset_hdr.model,
        asset_hdr.reference,
        asset_hdr.manufacturer_id,
        asset_hdr.serial_no,
        asset_hdr.stock_code,
        asset_hdr.asset_type_id,
        asset_hdr.asset_status,
        ca.asset_hdr_address_type,
        asset_hdr.stamp
      FROM asset_hdr
      cross join table(AXSP_GET_ASSET_HDR_ADDRESS_TBL(asset_hdr.asset_hdr_id, axsp_dateonly(axsp_get_datetime()))) ca
      WHERE asset_hdr.asset_hdr_id+0 > 0
      ) asset_hdr
    ) ah
  INNER JOIN asset_type
  ON ah.asset_type_id = asset_type.asset_type_id
  INNER JOIN party manufacturer
  ON ah.manufacturer_id = manufacturer.party_id
  INNER JOIN address
  ON ah.address_id = address.address_id
  LEFT OUTER JOIN asset_class_vehicle
  ON ah.asset_hdr_id = asset_class_vehicle.asset_hdr_id
/

--View needs to be before the function axsp_get_indr_exp_for_party
CREATE OR REPLACE FORCE VIEW axvw_party_indirect_exp
AS
	SELECT contract_id,product_id,business_unit_id,currency_id,party_id,amount_direct_exposure,amount_memorandum,party_role,calculation_type,contract_state,calc_dt
	FROM (
		SELECT
			pep.party_id,
			de.contract_id,
			c.product_id,
			c.business_unit_id,
			de.currency_id,
			de.amount_direct_exposure,
			de.amount_memorandum,
			pep.party_relationship party_role,
			de.calculation_type,
			ide.calc_dt,
			c.contract_state, 
			Rank() OVER (PARTITION BY de.contract_id, de.currency_id ORDER BY de.amount_direct_exposure DESC, de.party_id ASC) MyRank
		FROM direct_exposure de
		inner join (SELECT DISTINCT party_id,related_party_id,party_relationship FROM party_exposure_party) pep ON de.party_id = pep.related_party_id
		inner join contract c ON de.contract_id = c.contract_id
		inner join indirect_exposure ide on ide.party_id = pep.party_id
		WHERE
		--do not count indirect exposure for contracts that the party already has direct exposure for.
		not exists (SELECT 1 FROM direct_exposure de_check WHERE de_check.party_id = pep.party_id and de_check.contract_id = de.contract_id)
	) de_ranked
	WHERE de_ranked.MyRank = 1
/

--REM ====== Functions ======
 
create or replace
FUNCTION axsp_get_credit_utilisation
(p_party_id NUMBER, p_credit_Line_Id NUMBER, p_contract_Id NUMBER, p_credit_Line_Takedown_Role_Id NUMBER, p_credit_line_calc_type NUMBER)
return NUMBER IS

 -- Credit Line Calc Type
 lv_reducingPrincipalOnly INTEGER := 28000;
 lv_revolvingPrincipalOnly INTEGER := 28001;
 lv_reducing INTEGER := 28002;
 lv_revolvingDirectExposure INTEGER := 28003;
 lv_reducingInstallmentOnly INTEGER := 28005;
 lv_revolvingRental INTEGER := 28006;
 lv_revolvingInstallmentOnly INTEGER := 28007;

 --Calc type we are calling with
 lv_calcType INTEGER := p_credit_line_calc_type;

 -- Role Types
 lv_customerRole INTEGER := 36801;
 lv_guarantorRole INTEGER := 36810;
 lv_dealerRole INTEGER := 36802;
 lv_vendorRole INTEGER := 36803;
 lv_drawdownRecipientRole INTEGER := 36811;

 -- Title Receive WF State
 lv_titleReceivedState INTEGER := 3;

 -- Asset Ownership Type
 lv_assetOwnerTypeOwnedFixed INTEGER := 18702;
 lv_slot_type INTEGER :=0;

 -- Flow Types
 lv_flowTypeInstallment INTEGER := 1003;
 lv_flowTypeResidualValue INTEGER := 1007;
 lv_flowTypeInterestOnlyInstal INTEGER := 1043;

 -- Reversal Status
 lv_reversalStatusCurrent INTEGER := 4200;
 lv_isIncludeFlowsForAllParty INTEGER := 0;

 -- Contract States
 lv_cont_CompleteActivated INTEGER := 11130;
 lv_cont_PartiallyTerminated INTEGER := 11140;
 lv_cont_TerminatedPendingSale INTEGER := 11145;
 lv_cont_MaturityPending INTEGER := 11175;

 -- Termination Type
 lv_terminationType_Full INTEGER := 14902;

 -- Termination Status
 lv_terminationStatus_Accepted INTEGER := 14501;

 lv_credit_utilisation NUMBER;
 StoO_error      INTEGER;
 StoO_errmsg     VARCHAR2(255);
BEGIN

   SELECT s.slot_type
   INTO lv_slot_type
	FROM 	system_defs_party_role s
	WHERE	system_defs_party_role_id = p_credit_Line_Takedown_Role_Id;

	BEGIN
		StoO_error := 0;
		SELECT bmp.is_include_flows_for_all_party
		INTO lv_isIncludeFlowsForAllParty
		FROM business_model_party bmp
		WHERE bmp.business_model_id = (select business_model_id from contract where contract_id = p_contract_Id)
		AND bmp.system_defs_party_role_id = p_credit_Line_Takedown_Role_Id;
	EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
	END;

	IF (lv_calcType = 0) THEN
  	  SELECT cr.calc_type
	  INTO lv_calcType
	  FROM credit_line cr
	  WHERE cr.credit_line_id = p_credit_Line_Id;
	END IF;

	SELECT
	CASE WHEN lv_calcType = lv_reducingPrincipalOnly then
			CASE WHEN lv_slot_type = lv_customerRole or lv_slot_type = lv_guarantorRole or
					lv_slot_type = lv_dealerRole or lv_slot_type = lv_vendorRole THEN
				(select SUM(case when f.amount=0 and f.amt_gross>0 and c.product_style=2016 then f.amt_gross else f.amt_principal end)
				from flow f
					inner join party_account pa on f.party_account_id = pa.party_account_id
					inner join contract c on f.contract_id = c.contract_id
				where
				-- need to always include the RV flow, whether its cash or non-cash, a cash custom flow provided ins an "in flow"
				-- bugzId 38731: need to include non-cash installment on a crb loan account
				((f.is_cash = 1 and (f.flow_type != 1010 OR f.amount >= 0)) OR ((f.flow_type = 1007 or f.flow_type = 1003) and f.is_cash = 0)) and
				f.reversal_status = lv_reversalStatusCurrent and
				f.contract_id = p_contract_Id and
				(pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
            WHEN lv_slot_type = lv_drawdownRecipientRole THEN
				(select SUM(cost)
				from asset a
                inner join contract c on c.contract_id = a.contract_id
				where
				a.contract_id = p_contract_Id and
				(a.drawdown_party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1) and
				c.is_active = 1)
			ELSE
				0
			END
		WHEN  lv_calcType = lv_revolvingPrincipalOnly then
			CASE WHEN lv_slot_type = lv_customerRole or lv_slot_type = lv_guarantorRole or
					lv_slot_type = lv_dealerRole or lv_slot_type = lv_vendorRole THEN
				(select SUM(amt_principal-amt_matched_principal)
				from flow f
					inner join party_account pa on f.party_account_id = pa.party_account_id
				where
				-- need to always include the RV flow, whether its cash or non-cash
				-- bugzId: 38731 need to include non-cash installment on a crb loan account
				(f.is_cash = 1 OR ((f.flow_type = 1007 or f.flow_type = 1003) and f.is_cash = 0)) and
				f.reversal_status = lv_reversalStatusCurrent and
				f.contract_id = p_contract_Id and
				(pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
            WHEN lv_slot_type = lv_drawdownRecipientRole THEN
				(select SUM(cost)
				from asset a
                inner join contract c on c.contract_id = a.contract_id
                inner join asset_hdr ah on ah.asset_hdr_id = a.asset_hdr_id and ah.ownership_type = lv_assetOwnerTypeOwnedFixed
                left join workflow w on w.asset_hdr_id = a.asset_hdr_id AND w.current_state_type = lv_titleReceivedState
				where
				a.contract_id = p_contract_Id and
				(a.drawdown_party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1) and
				c.is_active = 1 and
                w.workflow_id is null)
			ELSE
				0
			END
		WHEN lv_calcType = lv_reducing then
			CASE WHEN lv_slot_type = lv_customerRole or lv_slot_type = lv_guarantorRole or
						lv_slot_type = lv_dealerRole or lv_slot_type = lv_vendorRole THEN
				(select SUM(amount)
				from flow f
					inner join party_account pa on f.party_account_id = pa.party_account_id
				where
				-- need to always include the RV flow, whether its cash or non-cash
				-- bugzId: 38731 need to include non-cash installment on a crb loan account
				(f.is_cash = 1 OR ((f.flow_type = 1007 or f.flow_type = 1003) and f.is_cash = 0)) and
				f.reversal_status = lv_reversalStatusCurrent and
				f.contract_id = p_contract_Id and
				(pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
            WHEN lv_slot_type = lv_drawdownRecipientRole THEN
				(select SUM(cost)
				from asset a
                inner join contract c on c.contract_id = a.contract_id
				where
				a.contract_id = p_contract_Id and
				(a.drawdown_party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1) and
				c.is_active = 1)
			ELSE
				0
			END
		WHEN lv_calcType = lv_revolvingDirectExposure then
			CASE WHEN lv_slot_type = lv_customerRole or lv_slot_type = lv_guarantorRole or lv_slot_type = lv_dealerRole THEN
				(select SUM(amount_direct_exposure)
				from direct_exposure
				where
				party_id = p_party_id and
				contract_id = p_contract_Id and
				party_role = CASE WHEN lv_slot_type = lv_customerRole THEN 36801 WHEN lv_slot_type = lv_dealerRole THEN 36802 ELSE 36810 END)
			WHEN lv_slot_type = lv_vendorRole THEN
				(select SUM(amount-amt_matched)
				from flow f
					inner join party_account pa on f.party_account_id = pa.party_account_id
				where
				-- need to always include the RV flow, whether its cash or non-cash
				-- bugzId: 38731 need to include non-cash installment on a crb loan account
				(f.is_cash = 1 OR ((f.flow_type = 1007 or f.flow_type = 1003) and f.is_cash = 0)) and
				f.reversal_status = lv_reversalStatusCurrent and
				f.contract_id = p_contract_Id and
				(pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
            WHEN lv_slot_type = lv_drawdownRecipientRole THEN
				(select SUM(cost)
				from asset a
                inner join contract c on c.contract_id = a.contract_id
                inner join asset_hdr ah on ah.asset_hdr_id = a.asset_hdr_id and ah.ownership_type = lv_assetOwnerTypeOwnedFixed
                left join workflow w on w.asset_hdr_id = a.asset_hdr_id AND w.current_state_type = lv_titleReceivedState
				where
				a.contract_id = p_contract_Id and
				(a.drawdown_party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1) and
				c.is_active = 1 and
                w.workflow_id is null)
			ELSE
				0
			END
		WHEN lv_calcType = lv_reducingInstallmentOnly then
			(SELECT SUM(amount)
			FROM flow f
				inner join party_account pa on f.party_account_id = pa.party_account_id
			WHERE f.contract_id = p_contract_Id
			-- need to always include the RV flow if its cash
			-- bugzId: 38731 need to include non-cash installment on a crb loan account
			AND (f.is_cash = 1 or (f.flow_type = 1003 and f.is_cash = 0))
			AND f.amount > 0
			AND f.reversal_status = lv_reversalStatusCurrent
			AND f.flow_type IN (lv_flowTypeInstallment, lv_flowTypeResidualValue, lv_flowTypeInterestOnlyInstal)
			AND (pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
		WHEN lv_calcType = lv_revolvingRental THEN
		 (SELECT NVL(installments,0) +
		 	(SELECT SUM(residual_value) -- We only want the residuals from assets that have not matured or been terminated
		    FROM asset a
		    INNER JOIN contract c ON c.contract_id = a.contract_id
		    LEFT JOIN termination_quote tq ON c.contract_id = tq.contract_id AND tq.termination_type = lv_terminationType_Full AND tq.status = lv_terminationStatus_Accepted
		    LEFT JOIN termination_asset ta ON ta.termination_quote_id = tq.termination_quote_id AND ta.asset_id = a.asset_id
		    WHERE a.contract_id = p_contract_Id
		    AND ((((contract_state <= lv_cont_CompleteActivated OR contract_state = lv_cont_PartiallyTerminated) AND a.is_terminated = 0)) OR
		          (contract_state in (lv_cont_MaturityPending, lv_cont_TerminatedPendingSale) AND a.is_terminated=1 AND NVL(tq.termination_quote_id, 0) > 0))
		    )
 		  FROM
 			(SELECT SUM(amount-amt_matched) as installments
			 FROM flow
			 WHERE contract_id = p_contract_Id
			  -- need to always include the RV flow, whether it's cash or non-cash
			 -- bugzId: 38731 need to include non-cash installment on a crb loan account
			 AND (is_cash = 1 OR ((flow_type = 1007 or flow_type = 1003) and is_cash = 0))
			 AND amount > 0
			 AND reversal_status = lv_reversalStatusCurrent
			 AND flow_type in (lv_flowTypeInstallment, lv_flowTypeResidualValue, lv_flowTypeInterestOnlyInstal)))
		WHEN lv_calcType = lv_revolvingInstallmentOnly then
			CASE WHEN  lv_slot_type = lv_guarantorRole THEN
				(SELECT amount FROM guarantee
				WHERE contract_id = lv_guarantorRole and guarantor_party_id = p_party_id)
			ELSE
				(SELECT SUM(amount - (amt_matched_interest + amt_matched_principal))
				FROM flow f
					inner join party_account pa on f.party_account_id = pa.party_account_id
				WHERE f.contract_id = p_contract_Id
				-- need to always include the RV flow if it's cash
				-- bugzId: 38731 need to include non-cash installment on a crb loan account
				AND (f.is_cash = 1 or (f.flow_type = 1003 and f.is_cash = 0))
				AND f.amount > 0
				AND f.reversal_status = lv_reversalStatusCurrent
				AND f.flow_type IN (lv_flowTypeInstallment, lv_flowTypeResidualValue, lv_flowTypeInterestOnlyInstal)
				AND (pa.party_id = p_party_id or lv_isIncludeFlowsForAllParty = 1))
			END
		ELSE
		0
		END as utilisation INTO lv_credit_utilisation
		FROM
		credit_line cr
		WHERE
		cr.credit_line_id = p_credit_Line_Id;

	return lv_credit_utilisation;

END;
/


CREATE OR REPLACE FUNCTION DateDiff(interval VARCHAR2, date1 DATE, date2 DATE) RETURN number as
  result number;
BEGIN
  /*days*/
  If (UPPER(interval) = 'D') OR (UPPER(interval) = 'Y') OR (UPPER(interval) ='W') OR
    (UPPER(interval) = 'DD') OR (UPPER(interval) = 'DDD') OR (UPPER(interval) = 'DAY') THEN
  result := round(date2-date1);
  ELSIF /*weeks*/ (UPPER(interval) = 'WW') OR (UPPER(interval) = 'DW') OR (UPPER(interval) = 'WEEK')  OR (UPPER(interval) = 'IW') THEN
  result := round((date2-date1)/7);
  ELSIF /*years*/ (UPPER(interval) = 'YYYY') OR (UPPER(interval) = 'YY') OR (UPPER(interval) = 'YEAR') THEN
  result := round(months_between(date2,date1) / 12.0);
  ELSIF /*quarters*/ (UPPER(interval) = 'Q') OR (UPPER(interval) = 'QQ') OR (UPPER(interval) = 'QUARTER') THEN
  result := round(months_between(date2,date1) / 3.0);
  ELSIF /*months*/ (UPPER(interval) = 'M') OR (UPPER(interval) = 'MM') OR (UPPER(interval) = 'MONTH') THEN
  result := round(months_between(date2,date1)) ;
  ELSIF /*hours*/ (UPPER(interval) = 'H') OR (UPPER(interval) = 'HH') OR (UPPER(interval) = 'HOUR') THEN
  result := round((date2-date1) * 24.0);
  ELSIF /*minutes*/ (UPPER(interval) = 'N') OR (UPPER(interval) = 'MI') OR (UPPER(interval) = 'MINUTE') THEN
  result := round((date2-date1) * 24.0 * 60.0);
  ELSIF /*seconds*/ (UPPER(interval) = 'S') OR (UPPER(interval) = 'SS') OR (UPPER(interval) = 'SECOND') THEN
  result := round((date2-date1) * 24.0 *60.0 * 60.0);
  END IF;
  RETURN result;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end; --DateDiff
/
CREATE OR REPLACE FUNCTION Alter_SPL_Regexp(INSTR VARCHAR2,ESCAPE CHAR)
RETURN VARCHAR2
AS
CURPOS INTEGER;
OUTSTRING VARCHAR2(4000);
SIZEINT INTEGER;
NEXTCHAR CHAR(1);
TOTALSIZE INTEGER;
INSQUAREBRACKET BOOLEAN:=false;
BEGIN
 CURPOS:=0;
 TOTALSIZE:=LENGTH(INSTR);
 OUTSTRING:='^';
 while (CURPOS<TOTALSIZE) LOOP
  CURPOS:=CURPOS+1;
  NEXTCHAR:=SUBSTR(INSTR,CURPOS,1);
  IF (NEXTCHAR = ESCAPE) then
   BEGIN
    OUTSTRING:=OUTSTRING||'\';
    CURPOS:=CURPOS+1;
    IF (CURPOS<TOTALSIZE+1) then
     NEXTCHAR:=SUBSTR(INSTR,CURPOS,1);
     OUTSTRING:=OUTSTRING||NEXTCHAR;
    END IF;
   END;
  ELSIF (NEXTCHAR='[') then
   BEGIN
    OUTSTRING:=OUTSTRING||'[';
    INSQUAREBRACKET:=TRUE;
   END;
  ELSIF (NEXTCHAR=']') then
   BEGIN
    OUTSTRING:=OUTSTRING||']';
    INSQUAREBRACKET:=FALSE;
   END;
  ELSIF (NEXTCHAR='*') then
   BEGIN
    IF (INSQUAREBRACKET = TRUE) then
     OUTSTRING:=OUTSTRING||'*';
    ELSE
     OUTSTRING:=OUTSTRING||'.*';
    END IF;
   END;
  ELSIF (NEXTCHAR='?') then
   BEGIN
    IF (INSQUAREBRACKET = TRUE) then
     OUTSTRING:=OUTSTRING||'?';
    ELSE
     OUTSTRING:=OUTSTRING||'.?';
    END IF;
   END;
  ELSIF ((INSQUAREBRACKET = FALSE)AND((NEXTCHAR='^')OR(NEXTCHAR='$')OR(NEXTCHAR='*')
   OR(NEXTCHAR='+')OR(NEXTCHAR='(')OR(NEXTCHAR=')')
   OR(NEXTCHAR='{')OR(NEXTCHAR='}')OR(NEXTCHAR='\'))) then
   OUTSTRING:=OUTSTRING||'\'||NEXTCHAR;
  ELSE
   OUTSTRING:=OUTSTRING||NEXTCHAR;
  END IF;
 END LOOP;
OUTSTRING:=OUTSTRING||'$';
RETURN OUTSTRING;
END Alter_SPL_Regexp;
/
CREATE OR REPLACE FUNCTION Alter_TSQL_Regexp(INSTR VARCHAR2,ESCAPE CHAR)
RETURN VARCHAR2
AS
CURPOS INTEGER;
OUTSTRING VARCHAR2(4000);
SIZEINT INTEGER;
NEXTCHAR CHAR(1);
TOTALSIZE INTEGER;
INSQUAREBRACKET BOOLEAN:=false;
BEGIN
 CURPOS:=0;
 TOTALSIZE:=LENGTH(INSTR);
 IF (SUBSTR(INSTR,1,1)!='^') THEN
  OUTSTRING:='^'; END IF;
 while (CURPOS<TOTALSIZE) LOOP
  CURPOS:=CURPOS+1;
  NEXTCHAR:=SUBSTR(INSTR,CURPOS,1);
  IF (NEXTCHAR = ESCAPE) then
   BEGIN
    OUTSTRING:=OUTSTRING||'\';
    CURPOS:=CURPOS+1;
    IF (CURPOS<TOTALSIZE+1) then
     NEXTCHAR:=SUBSTR(INSTR,CURPOS,1);
     OUTSTRING:=OUTSTRING||NEXTCHAR;
    END IF;
   END;
  ELSIF (NEXTCHAR='[') then
   BEGIN
    OUTSTRING:=OUTSTRING||'[';
    INSQUAREBRACKET:=TRUE;
   END;
  ELSIF (NEXTCHAR=']') then
   BEGIN
    OUTSTRING:=OUTSTRING||']';
    INSQUAREBRACKET:=FALSE;
   END;
  ELSIF (NEXTCHAR='%') then
   BEGIN
    IF (INSQUAREBRACKET = TRUE) then
     OUTSTRING:=OUTSTRING||'%';
    ELSE
     OUTSTRING:=OUTSTRING||'.*';
    END IF;
   END;
  ELSIF (NEXTCHAR='_') then
   BEGIN
    IF (INSQUAREBRACKET = TRUE) then
     OUTSTRING:=OUTSTRING||'_';
    ELSE
     OUTSTRING:=OUTSTRING||'.';
    END IF;
   END;
  ELSIF ((INSQUAREBRACKET = FALSE)AND((NEXTCHAR='^')OR(NEXTCHAR='$')OR(NEXTCHAR='*')
   OR(NEXTCHAR='?')OR(NEXTCHAR='+')OR(NEXTCHAR='(')OR(NEXTCHAR=')')
   OR(NEXTCHAR='{')OR(NEXTCHAR='}')OR(NEXTCHAR='\'))) then
   OUTSTRING:=OUTSTRING||'\'||NEXTCHAR;
  ELSE
   OUTSTRING:=OUTSTRING||NEXTCHAR;
  END IF;
 END LOOP;
OUTSTRING:=OUTSTRING||'$';
RETURN OUTSTRING;
END Alter_TSQL_Regexp;
/
CREATE OR REPLACE FUNCTION Atn(vExpression number) return number as
BEGIN
  return atan(vExpression);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION CDate(vChar varchar2) RETURN date as
BEGIN
  RETURN to_date(vChar);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION CDbl(vValue varchar2) return number as
BEGIN
  return to_number(vValue);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Ceiling(vValue number) RETURN number as
BEGIN
  RETURN ceil(vValue);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION Char_Length(vString varchar2) RETURN number as
BEGIN
  return length(vString);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION CStr(vNumber number) RETURN VARCHAR2 as
BEGIN
  RETURN to_char(vNumber);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION DateAdd(interval VARCHAR2, adding NUMBER, entry_date DATE)  RETURN DATE as
  result DATE;
BEGIN
  /*days*/
  If (UPPER(interval) = 'D') OR (UPPER(interval) = 'Y')
    OR (UPPER(interval) ='W') OR (UPPER(interval) = 'DD') OR
    (UPPER(interval) = 'DDD') OR (UPPER(interval) = 'DAY')  THEN
  result := entry_date + adding;
  ELSIF /*weeks*/ (UPPER(interval) = 'WW') OR
    (UPPER(interval) = 'IW')  OR (UPPER(interval) = 'WEEK') THEN
  result := entry_date + (adding * 7);
  ELSIF /*years*/ (UPPER(interval) = 'YYYY')
     OR (UPPER(interval) = 'YEAR') THEN
  result := add_months(entry_date,adding * 12);
  ELSIF /*quarters*/ (UPPER(interval) = 'Q')
     OR (UPPER(interval) = 'QUARTER') THEN
  result := add_months(entry_date,adding * 3);
  ELSIF /*months*/ (UPPER(interval) = 'M') OR
    (UPPER(interval) = 'MM') OR (UPPER(interval) = 'MONTH') THEN
  result := add_months(entry_date,adding);
  ELSIF /*hours*/ (UPPER(interval) = 'H') OR
    (UPPER(interval) = 'HH') OR (UPPER(interval) = 'HOUR') THEN
  result := entry_date+ (adding /24);
  ELSIF /*minutes*/ (UPPER(interval) = 'N') OR
    (UPPER(interval) = 'MI') OR (UPPER(interval) = 'MINUTE') THEN
  result := entry_date+ (adding /24/60);
  ELSIF /*seconds*/ (UPPER(interval) = 'S') OR
    (UPPER(interval) = 'SS') OR (UPPER(interval) = 'SECOND') THEN
  result := entry_date+ (adding /24/60/60);
  END IF;
  RETURN result;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION DateSerial(vYear number, vMonth number, vDay number) RETURN date as
BEGIN
  RETURN to_date(to_char(vDay)||'.'||to_char(vMonth)||'.'||to_char(vYear));
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION DateToStr(vDate date) RETURN varchar2 as
BEGIN
  return to_char(vDate);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Date_ RETURN date as
BEGIN
  RETURN sysdate;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION Day_(entry DATE) RETURN NUMBER as
BEGIN
 return TO_NUMBER(TO_CHAR( entry,'DD'));
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION dbname RETURN varchar2 as
  vDBName varchar2(9);
BEGIN
  return sys_context('USERENV','DB_NAME');
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Fix(vNumber number) return number as
BEGIN
    return trunc(vNumber);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION getDate RETURN DATE as
BEGIN
 return sysdate;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION guid_mover(guid CHAR) return RAW as
result RAW(16);
BEGIN
 result := HEXTORAW( SUBSTR(guid, 7,2)||SUBSTR(guid, 5,2)||SUBSTR(guid, 3,2)||
                     SUBSTR(guid, 1,2)||SUBSTR(guid,12,2)||SUBSTR(guid,10,2)||
                     SUBSTR(guid,17,2)||SUBSTR(guid,15,2)||SUBSTR(guid,20,4)||
                     SUBSTR(guid,25,12) );
 return result;
END;
/
CREATE OR REPLACE FUNCTION InStr_(vStart number,vString1 varchar2,vString2 varchar2,vCompare number default 0) return number as
BEGIN
  if vCompare = 0 then
    return InStr(vString1,vString2,vStart);
  else
    return InStr(upper(vString1),upper(vString2),vStart);
  end if;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Int_(vNumber number) return number as
BEGIN
  if vNumber>=0 then
    return trunc(vNumber);
  else
    return trunc(vNumber)-1;
  end if;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION IsNull(vString varchar2,vIfNullValue varchar2 default null) return varchar2 as
BEGIN
  if nvl(vIfNullValue,'-1')='-1' then
    if nvl(vString,'-1')='-1' then
      return 'TRUE';
    else
      return 'FALSE';
    end if;
  else
    return nvl(vString,vIfNullValue);
  end if;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION LCase(vString varchar2) return varchar2 as
BEGIN
  return lower(vString);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Left(vString varchar2, vLength number) return varchar2 as
BEGIN
  return substr(vString,1,vLength);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Len(vString varchar2) return number as
BEGIN
  return length(vString);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Mid(vString varchar2,vStart varchar2,vLength number default 1000) return varchar2 as
BEGIN
  return SubStr(vString,vStart,vLength);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Month_(entry DATE) RETURN NUMBER as
BEGIN
 return TO_NUMBER(TO_CHAR( entry,'MM'));
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Now RETURN DATE as
BEGIN
 return sysdate;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION object_name(vID number) RETURN varchar2 as
  vReturn varchar2(30);
BEGIN
  select object_name into vReturn from user_objects where object_id = vID;
  return vReturn;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Replicate (vString varchar2, vNumber number) return varchar2 as
  iCounter number;
  vReturn varchar2(4000);
BEGIN
  vReturn:='';
  for iCounter in 1..vNumber loop
    vReturn:=vReturn || vString;
  end loop;
  return vReturn;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Reverse_(vString varchar2) RETURN varchar2 as
  vLength number(4);
  vReturn varchar2(4000);
BEGIN
  vLength:=length(vString);
  vReturn:='';
  For i in reverse 1..vLength loop
    vReturn:=vReturn || substr(vString,i,1);
  end loop;
  return vReturn;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION Right(vString varchar2, vLength number) return varchar2 as
BEGIN
  if vLength<=length(vString) then
    return substr(vString,-vLength);
  else
    return vString;
  end if;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Sgn(vExpression number) return number as
BEGIN
  return sign(vExpression);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Space_(vNumber number) return varchar2 as
iCounter number;
vReturn varchar2(500);
BEGIN
  vReturn:='';
  for iCounter in 1..vNumber loop
    vReturn:=vReturn || ' ';
  end loop;
  return vReturn;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Sqr(vExpression number) return number as
BEGIN
  return sqrt(vExpression);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Str(vNumber NUMBER, vLength NUMBER default 10, vDecimal NUMBER default 0) RETURN VARCHAR2 as
  vConverted varchar2(4000);
  vReturn varchar2(4000);
BEGIN
  if (vLength<0 or vDecimal<0) then
    raise_application_error('-20000','Indexes must be positive');
  end if;
  vConverted:=to_char(vNumber);
  if length(vConverted)>vLength then
    vConverted:=to_char(round(vNumber,length(vConverted)-vLength));
  end if;

  vReturn:=to_char(round(to_number(vConverted),vDecimal));
  return vReturn;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION String(vNumber NUMBER) return VARCHAR2 as
BEGIN
  RETURN to_char(vNumber);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Substring(character_string varchar2, starting__position number,length number) RETURN varchar2 as
BEGIN
  RETURN Substr(character_string, starting__position,length);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
end;
/
CREATE OR REPLACE FUNCTION Time_ RETURN DATE as
BEGIN
 return sysdate;
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION UCase(vString varchar2) return varchar2 as
BEGIN
  return upper(vString);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Val(vString varchar2) return number as
vTempString varchar2(200);
iCounter number;
BEGIN
  iCounter:=1;
  while (iCounter<=length(vString)) and (ascii(substr(vString,iCounter,1))<=59) loop
    iCounter:=iCounter+1;
  end loop;
  if ascii(substr(vString,iCounter,1))>59 then
    vTempString:=substr(vString,1,iCounter-1);
  else
    vTempString:=vString;
  end if;
  vTempString:=replace(vTempString,' ','');
  return to_number(vTempString);
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION Year_(entry DATE) RETURN NUMBER as
BEGIN
 return TO_NUMBER(TO_CHAR( entry,'YYYY'));
exception
  when others then
    raise_application_error('-20000',sqlerrm);
END;
/
CREATE OR REPLACE FUNCTION ISNUMERIC(
in_string varchar2)
RETURN NUMBER IS
  testnumber float;
BEGIN
  testnumber:=TO_NUMBER(RTRIM(LTRIM(in_string,'$+-')));
  return 1;
EXCEPTION
  when others then return 0;
END ISNUMERIC;
/
CREATE OR REPLACE FUNCTION AXSP_DATEMAX
RETURN DATE AS
BEGIN
  RETURN (to_date('99981231', 'YYYYMMDD'));
END AXSP_DATEMAX;
/
CREATE OR REPLACE FUNCTION AXSP_DATEMIN
RETURN DATE AS
BEGIN
  RETURN (to_date('19000101', 'YYYYMMDD'));
END AXSP_DATEMIN;
/
CREATE OR REPLACE FUNCTION AXSP_DATEONLY(
thedate   IN DATE  DEFAULT NULL)
RETURN DATE AS
BEGIN
  RETURN trunc(thedate);
END AXSP_DATEONLY;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_HDR_OWNER_LIST
(p_id IN NUMBER DEFAULT NULL)
RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR children_cur(c_id number) IS
    SELECT rid FROM (select owner_id, asset_hdr_id as rid from asset_hdr where asset_hdr_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
      END IF;

  for i in children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.rid);
    else
      child_csv := child_csv || ',' || to_char(i.rid);
    end if;
    END LOOP;
  RETURN child_csv;
END AXSP_GET_ASSET_HDR_OWNER_LIST;
/

create or replace
FUNCTION AXSP_GET_ASSET_HDR_CHILD_LIST
(p_id IN NUMBER DEFAULT NULL)
RETURN VARCHAR2 AS
--
  v_child_id   NUMBER;
  v_child_csv  VARCHAR2(2000);

  CURSOR asset_hdr_children_cur(c_id number) IS
    SELECT * from TABLE(axsp_get_asset_hdr_child_tbl(p_id));

BEGIN
  v_child_id := p_id;
  v_child_csv := to_char(p_id);

  FOR rec IN asset_hdr_children_cur(p_id) LOOP
    v_child_csv := v_child_csv || ',' || to_char(rec.asset_hdr_id);
  END LOOP;

  RETURN v_child_csv;
END AXSP_GET_ASSET_HDR_CHILD_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_TYPE_OWNER_LIST
(p_id IN NUMBER DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR children_cur(c_id number) IS
    SELECT rid FROM (select owner_id, asset_type_id as rid from asset_type where asset_type_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
      END IF;

  for i in children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.rid);
    else
      child_csv := child_csv || ',' || to_char(i.rid);
    end if;
    END LOOP;
  RETURN child_csv;

END AXSP_GET_ASSET_TYPE_OWNER_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_TYPE_OWNER_TBL
(p_id IN NUMBER DEFAULT NULL) RETURN ASSET_TYPE_OWNER_TBLTAB PIPELINED IS
--
 ASSET_TYPE_OWNER_TBL ASSET_TYPE_OWNER_TBLOBJ := ASSET_TYPE_OWNER_TBLOBJ (null);
 CURSOR cur(c_id number) IS
	  SELECT rid FROM (select owner_id, asset_type_id as rid from asset_type where asset_type_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
BEGIN
	FOR rec in cur(p_id) LOOP
	  ASSET_TYPE_OWNER_TBL.asset_type_id := rec.rid;
	  PIPE ROW(ASSET_TYPE_OWNER_TBL);
	END LOOP;
	RETURN;
END AXSP_GET_ASSET_TYPE_OWNER_TBL;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_ADDRESS_ID
  (p_asset_id IN NUMBER, p_effective_from IN DATE)
  RETURN NUMBER
AS
	v_ret_val NUMBER;
	cursor c1 is
		select effective_dt, address_id, asset_address_id
		from asset_address a1 
		where a1.asset_id = p_asset_id
		and a1.effective_dt <= p_effective_from
		order by effective_dt desc, asset_address_id desc;
BEGIN
	v_ret_val := 0;
	-- Grab the first record from the cursor.  This is the one we want
	for i in c1 loop
		v_ret_val := i.address_id;
		exit;
	end loop;
	--
	RETURN v_ret_val;
END AXSP_GET_ASSET_ADDRESS_ID;
/

CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_HDR_ADDRESS_ID
	(p_asset_hdr_id IN NUMBER, 
	 p_asset_hdr_address_type IN NUMBER,
	 p_effective_from IN DATE)
	RETURN NUMBER
AS
	v_ret_val NUMBER;
	cursor c1 is
		select effective_dt, address_id, asset_hdr_address_id from asset_hdr_address
		where asset_hdr_id = p_asset_hdr_id
		and asset_hdr_address_type = p_asset_hdr_address_type
		and effective_dt <= p_effective_from
		order by effective_dt desc, asset_hdr_address_id desc;
BEGIN
	v_ret_val := 0;
	-- Grab the first record from the cursor.  This is the one we want
	for i in c1 loop
		v_ret_val := i.address_id;
		exit;
	end loop;
	--
	RETURN v_ret_val;
END AXSP_GET_ASSET_HDR_ADDRESS_ID;
/

create or replace FUNCTION AXSP_GET_ASSET_HDR_ADDRESS_TBL
	(p_asset_hdr_id IN NUMBER,
	 p_effective_from IN DATE)
	RETURN ASSET_HDR_ADDRESS_TBLTAB PIPELINED 
IS
	v_ret_val NUMBER;
	cursor c1(c_asset_hdr_address_type number) is
		select effective_dt, address_id, asset_hdr_address_id from asset_hdr_address
		where asset_hdr_id = p_asset_hdr_id
		and asset_hdr_address_type = c_asset_hdr_address_type
		and effective_dt <= p_effective_from
		order by effective_dt desc, asset_hdr_address_id desc;
--
	ASSET_HDR_ADDRESS_TBL ASSET_HDR_ADDRESS_TBLOBJ := ASSET_HDR_ADDRESS_TBLOBJ (null, null);
BEGIN
	v_ret_val := 0;
	for i in 45100..45102 loop
		for j in c1(i) loop
			ASSET_HDR_ADDRESS_TBL.asset_hdr_address_type := i;
			ASSET_HDR_ADDRESS_TBL.address_id := j.address_id;
			PIPE ROW(ASSET_HDR_ADDRESS_TBL);
			exit; -- return first row of cursor only
		end loop;
	end loop;
	RETURN ;
END AXSP_GET_ASSET_HDR_ADDRESS_TBL;
/

create or replace
FUNCTION axsp_get_cont_asset_hdr_ch_tbl
(p_contract_id IN NUMBER DEFAULT NULL)
RETURN ASSET_HDR_CHILD_TBLTAB PIPELINED IS
  ASSET_HDR_CHILD_TBL ASSET_HDR_CHILD_TBLOBJ := ASSET_HDR_CHILD_TBLOBJ (null);

  CURSOR asset_hdr_children_cur(c_id number) IS
	  SELECT rid FROM (select owner_id, asset_hdr_id as rid from asset_hdr) start
	  WITH rid in (SELECT asset_hdr_id FROM asset a where contract_id=c_id)
	  CONNECT BY PRIOR rid = owner_id;

BEGIN
	FOR rec_hdr in asset_hdr_children_cur(p_contract_id) LOOP
	  ASSET_HDR_CHILD_TBL.asset_hdr_id := rec_hdr.rid;
      PIPE ROW(ASSET_HDR_CHILD_TBL);
  END LOOP;
END axsp_get_cont_asset_hdr_ch_tbl;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from asset_cfd where asset_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_ASSET_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from asset_cfv where asset_id = p_id and asset_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from asset_cfd where asset_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from asset_cfd where asset_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_ASSET_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_HDR_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from asset_hdr_cfd where asset_hdr_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_ASSET_HDR_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_HDR_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from asset_hdr_cfv where asset_hdr_id = p_id and asset_hdr_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from asset_hdr_cfd where asset_hdr_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from asset_hdr_cfd where asset_hdr_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_ASSET_HDR_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CONTRACT_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from contract_cfd where contract_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_CONTRACT_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CONTRACT_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from contract_cfv where contract_id = p_id and contract_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from contract_cfd where contract_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from contract_cfd where contract_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_CONTRACT_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CREDIT_INFO_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from credit_info_cfd where credit_info_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_CREDIT_INFO_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CREDIT_INFO_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from credit_info_cfv where credit_info_id = p_id and credit_info_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from credit_info_cfd where credit_info_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from credit_info_cfd where credit_info_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_CREDIT_INFO_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CREDIT_LINE_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from credit_line_cfd where credit_line_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_CREDIT_LINE_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CREDIT_LINE_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from credit_line_cfv where credit_line_id = p_id and credit_line_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from credit_line_cfd where credit_line_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from credit_line_cfd where credit_line_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_CREDIT_LINE_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from party_cfd where party_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_PARTY_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from party_cfv where party_id = p_id and party_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from party_cfd where party_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from party_cfd where party_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_PARTY_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PRODUCT_CFD
    (
      p_cfd_id      IN NUMBER )
    RETURN NUMBER
  AS
    v_ret_val NUMBER;

BEGIN
    select DATA_TYPE into v_ret_val from product_cfd where product_cfd_id = p_cfd_id;
    RETURN v_ret_val;
END AXSP_GET_PRODUCT_CFD;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PRODUCT_CFV
 (
   p_id IN NUMBER,
   p_cfd_id IN NUMBER,
   p_seq_no IN NUMBER
 )
 RETURN NVARCHAR2
AS
 v_ret_val NVARCHAR2(2000);
 v_data_type NUMBER;
 v_db_lu_table_name NVARCHAR2(45);
 v_db_lu_display_fld NVARCHAR2(45);
 v_db_lu_value_fld NVARCHAR2(45);
 v_db_lu_sql_filter NVARCHAR2(1000);
 v_sql VARCHAR2(2000);

BEGIN
  BEGIN
    select field_value into v_ret_val from product_cfv where product_id = p_id and product_cfd_id = p_cfd_id and seq_no = p_seq_no;
  EXCEPTION
    WHEN OTHERS THEN
      null;
  END;

  if v_ret_val is not null and v_ret_val != ' ' then
    select data_type into v_data_type from product_cfd where product_cfd_id = p_cfd_id;

    if v_data_type = 5507 then

      select db_lu_table_name, db_lu_display_fld, db_lu_value_fld, db_lu_sql_filter into v_db_lu_table_name, v_db_lu_display_fld, v_db_lu_value_fld, v_db_lu_sql_filter
        from product_cfd where product_cfd_id = p_cfd_id;

      v_sql := 'select ' || v_db_lu_display_fld || ' from ' || v_db_lu_table_name || ' where ' || v_db_lu_value_fld || ' = ' || v_ret_val;
      if v_db_lu_sql_filter is not null and v_db_lu_sql_filter != ' ' then
        v_sql := v_sql || ' and ' || v_db_lu_sql_filter;
      end if;

      execute immediate v_sql into v_ret_val;
    end if;
  end if;
  RETURN v_ret_val;
END AXSP_GET_PRODUCT_CFV;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_ADDR_STATE_NAME
  ( p_id      IN NUMBER )
  RETURN NVARCHAR2
AS
  v_ret_val NVARCHAR2(100);
BEGIN
  select l.name into v_ret_val from location l inner join party_address p_addr on l.location_id = p_addr.state_province_id and p_addr.party_id = p_id AND p_addr.address_type = 5000 AND p_addr.is_current = 1;
  RETURN v_ret_val;
END AXSP_GET_PARTY_ADDR_STATE_NAME;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CONCAT_ADDRESS(
 address_id   IN NUMBER  DEFAULT NULL) RETURN NVARCHAR2 AS
--
 address_id_  NUMBER := address_id;
 csv          NVARCHAR2(1000);
 street       NVARCHAR2(255); /* User Defined Type : axdt_short_comment */
 suburb       NVARCHAR2(255); /* User Defined Type : axdt_address */
 city         NVARCHAR2(100); /* User Defined Type : axdt_name */
 county       NVARCHAR2(100); /* User Defined Type : axdt_name */
 state        NVARCHAR2(100); /* User Defined Type : axdt_name */
 zip          NVARCHAR2(100); /* User Defined Type : axdt_code */
 country      NVARCHAR2(100); /* User Defined Type : axdt_name */
BEGIN
  --Show nothing if we're asking for the None record.
  IF (address_id = 0) THEN
    RETURN ' ';
  END IF;
  FOR rec IN ( SELECT  ad.street, ad.suburb, adcity.name city, adcounty.name county, adstate.name state,
               ad.zip_code,  adcountry.name country
        FROM address ad, location adcity, location adcounty, location adstate, location adcountry
       WHERE ad.address_id = address_id_
       and ad.city_id = adcity.location_id
       and ad.county_id = adcounty.location_id
       and ad.state_province_id = adstate.location_id
       and ad.country_region_id = adcountry.location_id) LOOP
     street := rec.street;
     suburb := rec.suburb;
     city := rec.city;
     county := rec.county;
     state := rec.state;
     zip := rec.zip_code;
     country := rec.country;
  END LOOP;
  IF  (LTRIM(RTRIM(street)) IS NOT NULL) THEN
    csv := street;
  END IF;
  IF  (LTRIM(RTRIM(suburb)) IS NOT NULL) THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || suburb;
    ELSE
      csv := suburb;
    END IF;
  END IF;
  IF  ( city <> 'None') THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || city;
    ELSE
      csv := city;
    END IF;
  END IF;
  IF  ( county <> 'None') THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || county;
    ELSE
      csv := county;
    END IF;
  END IF;
  IF  ( state <> 'None') THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || state;
    ELSE
      csv := state;
    END IF;
  END IF;
  IF  (RTRIM(LTRIM(zip)) IS NOT NULL) THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || zip;
    ELSE
      csv := zip;
    END IF;
  END IF;
  IF  ( country <> 'None') THEN
    IF  ( csv IS NOT NULL AND csv <> ' ') THEN
      csv := csv || ',' || country;
    ELSE
      csv := country;
    END IF;
  END IF;
  RETURN NVL(csv, ' ');
 END AXSP_GET_CONCAT_ADDRESS;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CONTACT_LIST(
p_party_id     IN NUMBER  DEFAULT NULL,
p_contact_type IN NUMBER  DEFAULT NULL)
RETURN VARCHAR2 AS
--
  csv           VARCHAR2(2000);
BEGIN
  FOR rec IN (SELECT DECODE(NULL,csv || ',',' ',csv || ',') || p.name tmpAlias1
              FROM party_contact pc, party p
              WHERE pc.party_id = p_party_id
              and p.party_id = pc.contact_id
              and pc.contact_type = p_contact_type) LOOP
    csv := rec.tmpAlias1 ;
  END LOOP;
  RETURN csv;
END AXSP_GET_CONTACT_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_CROSS_RATES
(p_rate_basis_id  IN NUMBER  DEFAULT NULL,
 thedate          IN DATE) RETURN CROSS_RATES_TBLTAB PIPELINED
IS
--
  CURSOR cur(c_rate_basis_id number, c_effect_dt date) IS
	select
		c1.code as code1,
		c1.currency_id as ccy1,
		c2.code as code2,
		c2.currency_id as ccy2,

		-- cross_rate
		case when c1.currency_id = c2.currency_id then
			1.0
		else
			case when isnull(detccy1.rate,0) != 0 and isnull(detccy2.rate,0) != 0 then
				round(detccy2.rate/detccy1.rate,6)
			else
				0.0
      end
		end as cross_rate

	from
		currency c1
		left join rate_basis_det detccy1 on (detccy1.rate_basis_id = c_rate_basis_id
					and detccy1.currency_id = c1.currency_id and detccy1.effect_dt = c_effect_dt),

		currency c2
		left join rate_basis_det detccy2 on (detccy2.rate_basis_id = c_rate_basis_id
					and detccy2.currency_id = c2.currency_id and detccy2.effect_dt = c_effect_dt)
	where
		c1.is_enabled = 1 and c1.currency_id != 0
		and c2.is_enabled = 1 and c2.currency_id != 0;

  v_effect_dt DATE;
BEGIN
  select max(effect_dt) into v_effect_dt
   from rate_basis_det where rate_basis_id = p_rate_basis_id  and effect_dt <= thedate;

   FOR i IN cur(p_rate_basis_id, v_effect_dt) loop
    PIPE ROW (CROSS_RATES_TBLOBJ(i.code1, i.ccy1, i.code2, i.ccy2, i.cross_rate));
      END LOOP;

--return result
  RETURN;
END AXSP_GET_CROSS_RATES;
/
CREATE OR REPLACE FUNCTION AXSP_GET_FX_CROSS_RATE(
p_rate_basis_id   IN NUMBER  DEFAULT NULL,
thedate         IN DATE,
ccy1   IN NUMBER  DEFAULT NULL,
ccy2   IN NUMBER  DEFAULT NULL) RETURN NUMBER
IS
--
--
  rate1      NUMBER; /* User Defined Type : axdt_rate */
  rate2      NUMBER; /* User Defined Type : axdt_rate */
  cross_rate NUMBER(18,6) :=0; /* User Defined Type : axdt_rate */
  v_effect_dt DATE;

BEGIN

    --
    cross_rate := 0;
    -- If we don't have a rate basis curve then we simply return 1
    IF ccy1 = ccy2 OR p_rate_basis_id <= 1 THEN
      cross_rate := 1;
    ELSE

      select max(effect_dt) into v_effect_dt
      from rate_basis_det
      where rate_basis_id = p_rate_basis_id
      and effect_dt <= thedate;

      FOR rec IN ( SELECT rate
                   FROM rate_basis_det
                   WHERE rate_basis_id = p_rate_basis_id
                   and currency_id = ccy1
                   and effect_dt = v_effect_dt) LOOP
        rate1 := rec.rate;
      END LOOP;
      FOR rec IN ( SELECT rate
                   FROM rate_basis_det
                   WHERE rate_basis_id = p_rate_basis_id
                   and currency_id = ccy2
                   and effect_dt = v_effect_dt) LOOP
        rate2 := rec.rate;
      END LOOP;
      IF rate1 != 0 and rate2 != 0 THEN
        cross_rate := round(rate2 / rate1, 6);
      END IF;
    END IF;
  RETURN  cross_rate;
END AXSP_GET_FX_CROSS_RATE;
/
CREATE OR REPLACE FUNCTION AXSP_GET_FLOW_METHOD_NAME(
 p_id   IN NUMBER  DEFAULT NULL) RETURN NVARCHAR2 AS
--
 v_name NVARCHAR2(100);
BEGIN
  SELECT ext_name into v_name
  FROM flow_method
  WHERE flow_method_id = p_id;
  RETURN (v_name);
END AXSP_GET_FLOW_METHOD_NAME;
/
CREATE OR REPLACE FUNCTION AXSP_GET_LOCATION_OWNER_LIST
(p_id IN NUMBER  DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR children_cur(c_id number) IS
    SELECT rid FROM (select owner_id, location_id as rid from location where location_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
      END IF;

  for i in children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.rid);
    else
      child_csv := child_csv || ',' || to_char(i.rid);
    end if;
    END LOOP;
  RETURN child_csv;

END AXSP_GET_LOCATION_OWNER_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_LOCATION_CHILD_LIST
(p_id IN NUMBER  DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR location_children_cur(c_id number) IS
    SELECT  location_id
    FROM location
    START WITH owner_id = c_id
    CONNECT BY PRIOR location_id = owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
  END IF;
  for i in location_children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.location_id);
    else
      child_csv := child_csv || ',' || to_char(i.location_id);
    end if;
  END LOOP;
  RETURN child_csv;
END AXSP_GET_LOCATION_CHILD_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_LOOKUPSET_VALUE(
  p_id   IN NUMBER  DEFAULT NULL) RETURN NVARCHAR2 AS
--
  v_value NVARCHAR2(100);
BEGIN
  SELECT value into v_value
  FROM lookupset
  WHERE lookupset_id = p_id;
  RETURN (v_value);
END AXSP_GET_LOOKUPSET_VALUE;
/
create or replace FUNCTION AXSP_GET_NEXT_NO_INTERNAL(
p_table_name   IN NVARCHAR2  DEFAULT NULL,
p_increment    IN NUMBER  DEFAULT 1,
p_log_id       IN NUMBER  DEFAULT 0) RETURN INTEGER AS
--
lv_next_no       NUMBER := -1;
--
BEGIN
  BEGIN
   UPDATE next_no
   SET next_no = nvl(next_no,1) + p_increment
   WHERE table_name = p_table_name
   RETURNING next_no INTO lv_next_no;
 --
   lv_next_no := lv_next_no - p_increment;

  EXCEPTION
    WHEN NO_DATA_FOUND THEN
     null;
  END;
--
  RETURN lv_next_no;
END AXSP_GET_NEXT_NO_INTERNAL;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_CHILD_LIST
(p_id IN NUMBER  DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR party_children_cur(c_id number) IS
    SELECT  party_id
    FROM party
    START WITH owner_id = c_id
    CONNECT BY PRIOR party_id = owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
  END IF;
  for i in party_children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.party_id);
    else
      child_csv := child_csv || ',' || to_char(i.party_id);
    end if;
  END LOOP;
  RETURN child_csv;
END AXSP_GET_PARTY_CHILD_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_NAME(p_id IN NUMBER DEFAULT NULL)
 RETURN NVARCHAR2 AS
--
  v_name  NVARCHAR2(100);
BEGIN
 SELECT name into v_name
 FROM party
 WHERE party_id = p_id;
 RETURN (v_name);
END AXSP_GET_PARTY_NAME;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_EXT_NAME(p_id IN NUMBER DEFAULT NULL)
 RETURN NVARCHAR2 AS
--
  v_name  NVARCHAR2(100);
BEGIN
 IF p_id = 0 THEN
    RETURN '';
 END IF;
 SELECT ext_name into v_name
 FROM party
 WHERE party_id = p_id;
 RETURN (v_name);
END AXSP_GET_PARTY_EXT_NAME;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_OWNER_LIST
(p_id IN NUMBER  DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR children_cur(c_id number) IS
    SELECT rid FROM (select owner_id, party_id as rid from party where party_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
      END IF;

  for i in children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.rid);
    else
      child_csv := child_csv || ',' || to_char(i.rid);
    end if;
    END LOOP;
  RETURN child_csv;

END AXSP_GET_PARTY_OWNER_LIST;
/

CREATE OR REPLACE FUNCTION AXSP_GET_PARTY_OWNER_LIST_TBL(
 p_id   IN NUMBER  DEFAULT NULL)
 RETURN PARTY_OWNER_TBLTAB PIPELINED IS
--
 PARTY_OWNER_TBL PARTY_OWNER_TBLOBJ := PARTY_OWNER_TBLOBJ (null);

  CURSOR cur(c_id number) IS
	  SELECT rid FROM (select owner_id, party_id as rid from party where party_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;

BEGIN
	FOR rec in cur(p_id) LOOP
	  PARTY_OWNER_TBL.party_id := rec.rid;
	  PIPE ROW(PARTY_OWNER_TBL);
   END LOOP;
   RETURN;
END AXSP_GET_PARTY_OWNER_LIST_TBL;
/
CREATE OR REPLACE FUNCTION AXSP_GET_LOGO_SELECT_ID
(p_id IN NUMBER DEFAULT NULL)
RETURN NUMBER AS
--
  v_owner_id   NUMBER;
  v_logo_id    NUMBER;
BEGIN
    v_owner_id := p_id;
    v_logo_id := 0;
     FOR rec IN ( select party_id  from party where party_id = v_owner_id and is_logo_select = 1) LOOP
         v_logo_id := rec.party_id ;
     END LOOP;
    WHILE v_owner_id != 0  LOOP
       FOR rec IN ( SELECT   owner_id
                   FROM party
                   WHERE party_id = v_owner_id)
        LOOP
         v_owner_id := rec.owner_id ;
      END LOOP;
      IF v_owner_id != 0  and v_logo_id = 0 THEN
         BEGIN
             FOR rec IN ( select party_id  from party where party_id = v_owner_id and is_logo_select = 1) LOOP
               v_logo_id := rec.party_id ;
             END LOOP;
        END;
      END IF;
    END LOOP;
    RETURN  v_logo_id;
END AXSP_GET_LOGO_SELECT_ID;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PRODUCT_OWNER_TBL
(p_id   IN NUMBER  DEFAULT NULL) RETURN PRODUCT_OWNER_TBLTAB PIPELINED IS
--
 PRODUCT_OWNER_TBL PRODUCT_OWNER_TBLOBJ := PRODUCT_OWNER_TBLOBJ (null);
 CURSOR cur(c_id number) IS
	  SELECT rid FROM (select owner_id, product_id as rid from product where product_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;

BEGIN
	FOR rec in cur(p_id) LOOP
	  PRODUCT_OWNER_TBL.product_id := rec.rid;
	  PIPE ROW(PRODUCT_OWNER_TBL);
  END LOOP;
  RETURN;

END AXSP_GET_PRODUCT_OWNER_TBL;
/
CREATE OR REPLACE FUNCTION AXSP_GET_PRODUCT_OWNER_LIST
(p_id IN NUMBER DEFAULT NULL) RETURN VARCHAR2 AS
--
  child_csv  VARCHAR2(2000) := '';
--
  CURSOR children_cur(c_id number) IS
	SELECT rid FROM (select owner_id, product_id as rid from product where product_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
--
BEGIN
  IF p_id = 0 THEN
    RETURN '';
      END IF;

  for i in children_cur(p_id) loop
    if child_csv is null then
      child_csv := to_char(i.rid);
    else
      child_csv := child_csv || ',' || to_char(i.rid);
    end if;
  END LOOP;
  RETURN child_csv;

END AXSP_GET_PRODUCT_OWNER_LIST;
/
CREATE OR REPLACE FUNCTION AXSP_PARTY_SEARCH_DUP_RANKING(
p_business_ref_no1       IN NVARCHAR2 DEFAULT NULL,
p_tax_no1                IN NVARCHAR2 DEFAULT NULL,
p_name1                  IN NVARCHAR2 DEFAULT NULL,
p_first_names1           IN NVARCHAR2 DEFAULT NULL,
p_middle_name1           IN NVARCHAR2 DEFAULT NULL,
p_phone_home_search1     IN NVARCHAR2 DEFAULT NULL,
p_phone_business_search1 IN NVARCHAR2 DEFAULT NULL,
p_date_of_birth1         IN DATE      DEFAULT NULL,
p_gender1                IN NUMBER    DEFAULT NULL,
p_street1                IN NVARCHAR2 DEFAULT NULL,
p_city_id1               IN NUMBER    DEFAULT NULL,
p_state_province_id1     IN NUMBER    DEFAULT NULL,
p_zip_code1              IN NVARCHAR2 DEFAULT NULL,
p_country_region_id1     IN NUMBER    DEFAULT NULL,
p_sdx_name1              IN VARCHAR2  DEFAULT NULL,
p_sdx_first_names1       IN VARCHAR2  DEFAULT NULL,
p_sdx_middle_name1       IN VARCHAR2  DEFAULT NULL,
p_sdx_street1            IN VARCHAR2  DEFAULT NULL,
p_business_ref_no2       IN NVARCHAR2 DEFAULT NULL,
p_tax_no2                IN NVARCHAR2 DEFAULT NULL,
p_name2                  IN NVARCHAR2 DEFAULT NULL,
p_first_names2           IN NVARCHAR2 DEFAULT NULL,
p_middle_name2           IN NVARCHAR2 DEFAULT NULL,
p_phone_home_search2     IN NVARCHAR2 DEFAULT NULL,
p_phone_business_search2 IN NVARCHAR2 DEFAULT NULL,
p_date_of_birth2         IN DATE      DEFAULT NULL,
p_gender2                IN NUMBER    DEFAULT NULL,
p_street2                IN NVARCHAR2 DEFAULT NULL,
p_city_id2               IN NUMBER    DEFAULT NULL,
p_state_province_id2     IN NUMBER    DEFAULT NULL,
p_zip_code2              IN NVARCHAR2 DEFAULT NULL,
p_country_region_id2     IN NUMBER    DEFAULT NULL,
p_sdx_name2              IN VARCHAR2  DEFAULT NULL,
p_sdx_first_names2       IN VARCHAR2  DEFAULT NULL,
p_sdx_middle_name2       IN VARCHAR2  DEFAULT NULL,
p_sdx_street2            IN VARCHAR2  DEFAULT NULL
)
RETURN NUMBER AS
--
  ranking                  NUMBER;
--business ref
BEGIN
  ranking := 0;
  IF (p_business_ref_no2 IS NULL OR p_business_ref_no2 = ' ') THEN
    ranking := ranking + 20;
  ELSIF (p_business_ref_no1 = p_business_ref_no2) THEN
    ranking := ranking + 100;
  END IF;
--tax no
  IF (p_tax_no2 IS NULL OR p_tax_no2 = ' ') THEN
    ranking := ranking + 20;
  ELSIF (p_tax_no1 = p_tax_no2) THEN
    ranking := ranking + 100;
  END IF;
--address
  IF ((p_street2 IS NULL OR p_street2 = ' ') and p_city_id2 = 0 and p_state_province_id2 = 0 and
      (p_zip_code2 IS NULL OR p_zip_code2 = ' ') and p_country_region_id2 = 0) THEN
    ranking := ranking + 0;
  ELSIF (p_street1 = p_street2 and p_city_id1 = p_city_id2 and
         p_state_province_id1 = p_state_province_id2 and p_zip_code1 = p_zip_code2
         and p_country_region_id1 = p_country_region_id2) THEN
    ranking := ranking + 100;
  ELSIF (p_sdx_street1 = p_sdx_street2 and p_city_id1 = p_city_id2 and
         p_state_province_id1 = p_state_province_id2 and
         p_zip_code1 = p_zip_code2) THEN
    ranking := ranking + 80;
  ELSIF (p_state_province_id1 = p_state_province_id2 and
         p_zip_code1 = p_zip_code2) THEN
    ranking := ranking + 60;
  ELSIF (p_country_region_id1 <> p_country_region_id2) THEN
    ranking := ranking - 20;
  END IF;
--name
  IF (p_name1 = p_name2 and p_first_names1 = p_first_names2 and
      p_middle_name1 = p_middle_name2) THEN
    ranking := ranking + 100;
-- Last name matched, middle name matched or not specified, first name has equivalent
  ELSIF (p_name1 = p_name2 and
         (p_middle_name1 = p_middle_name2 or p_middle_name1 IS NULL or
          p_middle_name2 IS NULL or p_middle_name1 = ' ' or p_middle_name2 = ' ') and
         (axsp_party_search_name_equiv(p_first_names1, p_first_names2, p_gender1, 1) = 1)) THEN
    ranking := ranking + 80;
-- Last name soundex matched, middle name soundex matched or not specified, first name has equivalent
  ELSIF (p_sdx_name1 = p_sdx_name2
	 and (p_sdx_middle_name1 = p_sdx_middle_name2 or
              p_middle_name1 = ' ' or p_middle_name1 is null or
              p_middle_name2 = ' ' or p_middle_name2 is null)
	 and (axsp_party_search_name_equiv(p_first_names1, p_first_names2, p_gender1, 1) = 1)) THEN
    ranking := ranking + 70;
-- Last name soundex matched, middle name soundex matched or not specified, first name soundex matched
  ELSIF (p_sdx_name1 = p_sdx_name2 and p_sdx_first_names1 = p_sdx_first_names2
	 and (p_sdx_middle_name1 = p_sdx_middle_name2 or
              p_middle_name1 = ' ' or p_middle_name1 is null or
              p_middle_name2 = ' ' or p_middle_name2 is null)) THEN
    ranking := ranking + 60;
  ELSE
    ranking := ranking - 100;
  END IF;
--home phone
  IF  ( p_phone_home_search2 IS NULL OR p_phone_home_search2 = ' ') THEN
    ranking := ranking + 20;
  ELSIF ( p_phone_home_search1 = p_phone_home_search2) THEN
    ranking := ranking + 80;
  END IF;
--business phone
  IF  ( p_phone_business_search2 IS NULL OR p_phone_business_search2 = ' ') THEN
    ranking := ranking + 20;
  ELSIF ( p_phone_business_search1 = p_phone_business_search2) THEN
    ranking := ranking + 60;
  END IF;
--date of birth
  IF ( p_date_of_birth2 = axsp_datemin()) THEN
    ranking := ranking + 0;
  ELSIF (p_date_of_birth1 = p_date_of_birth2) THEN
    ranking := ranking + 80;
  ELSIF ((TO_NUMBER(TO_CHAR( p_date_of_birth1, 'DD')) -
          TO_NUMBER(TO_CHAR( p_date_of_birth2, 'DD')) = 1) or
            (
              TO_NUMBER(TO_CHAR( p_date_of_birth1, 'DD')) -
              TO_NUMBER(TO_CHAR( p_date_of_birth2, 'DD')) = -1
            )
        ) THEN
    ranking := ranking + 30;
  ELSIF ((TO_NUMBER(TO_CHAR( p_date_of_birth1, 'MM')) -
          TO_NUMBER(TO_CHAR( p_date_of_birth2, 'MM')) = 1) or
            (
              TO_NUMBER(TO_CHAR( p_date_of_birth1, 'MM')) -
              TO_NUMBER(TO_CHAR( p_date_of_birth2, 'MM')) = -1
            )
        ) THEN
    ranking := ranking + 30;
  ELSIF ((TO_NUMBER(TO_CHAR( p_date_of_birth1, 'YYYY')) -
          TO_NUMBER(TO_CHAR( p_date_of_birth2, 'YYYY')) = 1) or
            (
              TO_NUMBER(TO_CHAR( p_date_of_birth1, 'YYYY')) -
              TO_NUMBER(TO_CHAR( p_date_of_birth2, 'YYYY')) = -1
            )
        ) THEN
    ranking := ranking + 30;
  END IF;
--gender
  IF  ( p_gender1 <> p_gender2) THEN
    ranking := ranking - 100;
  END IF;
  RETURN ranking;
END AXSP_PARTY_SEARCH_DUP_RANKING;
/
CREATE OR REPLACE FUNCTION AXSP_PARTY_SEARCH_NAME_EQUIV(
p_name1     IN NVARCHAR2  DEFAULT NULL,
p_name2     IN NVARCHAR2  DEFAULT NULL,
p_gender    IN NUMBER  DEFAULT NULL,
p_name_type IN NUMBER  DEFAULT NULL) RETURN NUMBER AS
--
  equivalent   NUMBER;
--name type: 1 - first name, 2 - last name
--
BEGIN
  equivalent := 0;
  IF ( p_name_type = 1) THEN
    IF (p_name1 = p_name2 or p_name1 = SUBSTRING(p_name2, 1, 1) or
        p_name2 = SUBSTRING(p_name1, 1, 1) ) THEN
      equivalent := 1;
    ELSE
      for rec in (SELECT name
                  FROM name_equivalent
                  WHERE name = p_name2 and name_group IN (
                    SELECT  name_group
                    FROM name_equivalent
                    WHERE name = p_name1
                      and gender = p_gender
                      and is_first_name = 1)
                 ) loop
        equivalent := 1;
      end loop;
    END IF;
  ELSIF ( p_name_type = 2) THEN
    IF  ( p_name1 = p_name2 or p_name1 = SUBSTRING(p_name2, 1, 1) or
          p_name2 = SUBSTRING(p_name1, 1, 1) ) THEN
       equivalent := 1;
    ELSE
      for rec in (SELECT name
                  FROM name_equivalent
                  WHERE name = p_name2
                    and name_group  IN (
                      SELECT name_group
                      FROM name_equivalent
                      WHERE name = p_name1
                        and gender = p_gender
                        and is_last_name = 1)
                 ) LOOP
          equivalent := 1;
       END LOOP;
    END IF;
  END IF;
  RETURN (equivalent);
END AXSP_PARTY_SEARCH_NAME_EQUIV;
/
CREATE OR REPLACE FUNCTION AXSP_PARTY_TYPE_CSV(
p_party_type   IN NUMBER  DEFAULT NULL) RETURN NVARCHAR2 AS
--
  csv   VARCHAR2(1000);
BEGIN
  csv := '';
  IF p_party_type = 0 THEN
     SELECT name INTO csv
     FROM party_type
     WHERE party_type = p_party_type;
  ELSE
    FOR rec IN ( SELECT name
                 FROM party_type
                 WHERE axsp_isbitand(p_party_type,party_type) = 1 AND party_type != 0
                 ORDER BY name ) LOOP
       csv := csv||rec.name || ',';
    END LOOP;
    IF (csv IS NOT NULL) THEN
      csv := SUBSTRING(csv, 1, length(csv) - 1);
    END IF;
  END IF;
  RETURN csv;
END AXSP_PARTY_TYPE_CSV;
/
CREATE OR REPLACE FUNCTION AXSP_STRIP_NUMERIC(
string   IN NVARCHAR2  DEFAULT NULL) RETURN NVARCHAR2 AS
--
  string_   NVARCHAR2(4000) := string;
  stringLen NUMBER;
  curChar   CHAR(1);
  curPos    NUMBER;
  newString NVARCHAR2(4000);
BEGIN
  IF  string_ IS NULL OR string_ = ' ' THEN
    RETURN string_;
  END IF;
  curPos := 1;
  stringLen := length(string_);
  newString := '';
  WHILE  curPos < stringLen + 1 LOOP
    curChar := SUBSTRING(string_, curPos, 1);
    /*[In converting line (24)]: New function ISNUMERIC() introduced to mimic T/SQL function*/
    IF NOT ISNUMERIC(curChar) = 1 THEN
      newString := newString || curChar;
    END IF;
    curPos := curPos + 1;
  END LOOP;
  newString := LTRIM(newString);
  newString := RTRIM(newString);
  RETURN newString;
END AXSP_STRIP_NUMERIC;
/
create or replace function axsp_statement_open_bal
(p_party_account_id IN NUMBER,
 p_currency_id IN NUMBER,
 p_balance_dt IN date) return number is
--
  v_retval number := 0;
begin
  for i in (SELECT balance
            FROM statement_bal
            WHERE party_account_id = p_party_account_id
            AND currency_id = p_currency_id
            AND balance_dt < p_balance_dt
            ORDER BY balance_dt desc) loop
    v_retval := i.balance;
    exit; -- exit immediately because we want the top 1 record
  end loop;
  return v_retval;
end axsp_statement_open_bal;
/
CREATE OR REPLACE FUNCTION axsp_bitand
(value1 IN NUMBER,
 value2 IN NUMBER) RETURN NUMBER IS
--
BEGIN
  return bitand(value1,value2);
END axsp_bitand;
/
CREATE OR REPLACE FUNCTION axsp_coll_party_address
(p_party_id NUMBER, p_address_type NUMBER)
RETURN party_details_tbltab PIPELINED IS
--
  v_street nvarchar2(255);
  v_suburb nvarchar2(255);
  v_city nvarchar2(30);
  v_state_province nvarchar2(30);
  v_zip_code nvarchar2(10);
  v_country_region nvarchar2(30);
  v_mailing_prefix nvarchar2(10);
  v_type_string NUMBER := 0;
  party_details_tbl party_details_tblobj := party_details_tblobj(null, null, null, null, null);
BEGIN
	SELECT 	pa.street,
	   pa.suburb,
		l1.name,
		l2.name,
		pa.zip_code,
		l3.name
        INTO v_street, v_suburb, v_city, v_state_province, v_zip_code, v_country_region
	FROM 	party_address pa, location l1, location l2, location l3
	WHERE   pa.party_id = p_party_id
	AND 	pa.city_id = l1.location_id
	AND	pa.state_province_id = l2.location_id
	AND	pa.country_region_id = l3.location_id
	AND	pa.address_type = p_address_type
	AND  	pa.is_current = 1;
	v_mailing_prefix := ' ';
	IF (p_address_type = 5001) THEN
	  v_mailing_prefix := 'mailing_';
        END IF;
	IF (v_street IS NOT NULL AND v_street <> ' ') THEN
	  party_details_tbl := party_details_tblobj (1, v_mailing_prefix || 'street', ' ', nvl(v_street, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
   IF (v_suburb IS NOT NULL AND v_suburb <> ' ') THEN
	  party_details_tbl := party_details_tblobj (1, v_mailing_prefix || 'suburb', ' ', nvl(v_suburb, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
	IF (v_city IS NOT NULL AND v_city <> ' ') THEN
	  party_details_tbl := party_details_tblobj (2, v_mailing_prefix || 'city_name', ' ', nvl(v_city, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
	IF (v_state_province IS NOT NULL and v_state_province <> ' ') THEN
	  party_details_tbl := party_details_tblobj (3, v_mailing_prefix || 'state_province_name', ' ', nvl(v_state_province, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
	IF (v_zip_code IS NOT NULL AND v_zip_code <> ' ') THEN
	  party_details_tbl := party_details_tblobj (4, v_mailing_prefix || 'zip_code', ' ', nvl(v_zip_code, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
	IF (v_country_region IS NOT NULL AND v_country_region <> ' ') THEN
	  party_details_tbl := party_details_tblobj (5, v_mailing_prefix || 'country_region_name', ' ', nvl(v_country_region, ' '), v_type_string) ;
          PIPE ROW (party_details_tbl);
        END IF;
RETURN;
END axsp_coll_party_address;
/
CREATE OR REPLACE FUNCTION axsp_coll_party_contact
(p_party_id NUMBER)
RETURN party_details_tbltab PIPELINED IS
--
  v_name nvarchar2(255);
  v_phone_business nvarchar2(255);
  v_phone_home nvarchar2(255);
  v_phone_mobile nvarchar2(255);
  v_email_business nvarchar2(255);
  v_invoice_name nvarchar2(255);
  v_title nvarchar2(255);
  v_email_home nvarchar2(255);
  v_business_individual NUMBER;
  v_type_string NUMBER := 0;
  v_type_not_show NUMBER := -1;
  party_details_tbl party_details_tblobj;
BEGIN
	SELECT 	p.business_individual
        INTO    v_business_individual
	FROM 	party p
	WHERE	party_id = p_party_id;
--
	IF (v_business_individual = 5201) THEN
		--for individual get contact details from main party
		SELECT 	p.ext_name,
			p.phone_business,
			p.phone_home,
			p.phone_mobile,
			p.email_business,
			p.invoice_name,
			lu.value,
			p.email_home
                INTO    v_name,
                        v_phone_business,
                        v_phone_home,
                        v_phone_mobile,
                        v_email_business,
                        v_invoice_name,
                        v_title,
                        v_email_home
		FROM 	party p, lookupset lu
		WHERE   p.party_id = p_party_id
		AND	lu.lookupset_id = p.title;
	ELSE
		--for business or partnership get contact details from primary contact
		SELECT 	p.ext_name,
			p.phone_business,
			p.phone_home,
			p.phone_mobile,
			p.email_business,
			p.invoice_name,
			lu.value,
			p.email_home
                INTO    v_name,
                        v_phone_business,
                        v_phone_home,
                        v_phone_mobile,
                        v_email_business,
                        v_invoice_name,
                        v_title,
                        v_email_home
		FROM 	party p, party_contact pc, lookupset lu
		WHERE   p.party_id = pc.contact_id
		AND 	pc.is_primary_contact = 1
		AND	pc.party_id = p_party_id
		AND	lu.lookupset_id = p.title;
	END IF;
	IF (v_name IS NOT NULL AND v_name <> ' ') THEN
          party_details_tbl := party_details_tblobj(1, 'contact_ext_name', ' ', nvl(v_name, ' '), v_type_string);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(2, 'contact_phone_business', ' ', nvl(v_phone_business, ' '), v_type_string);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(3, 'contact_phone_home', ' ', nvl(v_phone_home, ' '), v_type_string);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(4, 'contact_phone_mobile', ' ', nvl(v_phone_mobile, ' '), v_type_string);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(5, 'contact_email_business', ' ', nvl(v_email_business, ' '), v_type_string);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(6, 'contact_invoice_name', ' ', nvl(v_invoice_name, ' '), v_type_not_show);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(7, 'contact_title', ' ', nvl(v_title, ' '), v_type_not_show);
          PIPE ROW (party_details_tbl);
--
          party_details_tbl := party_details_tblobj(8, 'contact_email_home', ' ', nvl(v_email_home, ' '), v_type_not_show);
          PIPE ROW (party_details_tbl);
	END IF;
	RETURN;
END axsp_coll_party_contact;
/
CREATE OR REPLACE FUNCTION axsp_isbitand
(larger_val NUMBER, smaller_val NUMBER) RETURN NUMBER IS
--
BEGIN
  return CASE WHEN bitand(larger_val, smaller_val) = smaller_val THEN 1 ELSE 0 END;
END axsp_isbitand;
/
CREATE OR REPLACE FUNCTION axsp_coll_party_credit
(p_party_id int)
RETURN party_details_tbltab PIPELINED AS
--
  v_occupation nvarchar2(30);
  v_credit_rating nvarchar2(30);
  v_amount nvarchar2(30);
  v_approved_dt nvarchar2(30);
  v_credit_check_dt nvarchar2(30);
  v_review_dt nvarchar2(30);
  v_type_string NUMBER := 0;
  v_type_int NUMBER := 1;
  v_type_decimal NUMBER := 2;
  v_type_datetime NUMBER := 3;
  v_min_dt date := to_date('19000101','YYYYMMDD');
  party_details_tbl party_details_tblobj;
BEGIN
  BEGIN
  SELECT o.name,
	cr.rating,
	CAST(c.amount AS nvarchar2(30)),
	CAST(c.approved_dt AS nvarchar2(30)) ,
	CAST(c.credit_check_dt AS nvarchar2(30)),
	CAST(c.review_dt AS nvarchar2(30))
  INTO  v_occupation,
        v_credit_rating,
        v_amount,
        v_approved_dt,
        v_credit_check_dt,
        v_review_dt
  FROM 	credit_info c
	LEFT OUTER JOIN employment_info e ON e.party_id = c.party_id AND e.is_current = 1
	LEFT OUTER JOIN occupation_type o ON o.occupation_type_id = e.occupation_type_id
	LEFT OUTER JOIN credit_rating cr ON cr.credit_rating_id = c.rating_id
	WHERE   c.party_id = p_party_id
	AND  	c.is_current = 1;
	EXCEPTION
	  WHEN NO_DATA_FOUND THEN
	    v_occupation := null;
      v_credit_rating := null;
      v_amount := null;
      v_approved_dt := null;
      v_credit_check_dt := null;
      v_review_dt := null;
	END;
--
  party_details_tbl := party_details_tblobj(1, 'occupation_type_name', ' ', nvl(v_occupation, ' '), v_type_string);
  pipe row (party_details_tbl);
  party_details_tbl := party_details_tblobj(2, 'rating_name', ' ', nvl(v_credit_rating, ' '), v_type_string);
  pipe row (party_details_tbl);
  party_details_tbl := party_details_tblobj(3, 'amount', ' ', nvl(v_amount, 0), v_type_decimal);
  pipe row (party_details_tbl);
  party_details_tbl := party_details_tblobj(4, 'approved_dt', ' ', nvl(v_approved_dt, v_min_dt), v_type_datetime);
  pipe row (party_details_tbl);
  party_details_tbl := party_details_tblobj(5, 'credit_check_dt', ' ', nvl(v_credit_check_dt, v_min_dt), v_type_datetime);
  pipe row (party_details_tbl);
  party_details_tbl := party_details_tblobj(6, 'review_dt', ' ', nvl(v_review_dt, v_min_dt), v_type_datetime);
  pipe row (party_details_tbl);
  RETURN;
END axsp_coll_party_credit;
/
CREATE OR REPLACE FUNCTION axsp_coll_account_details
(p_party_id         NUMBER,
 p_party_account_id NUMBER,
 p_currency_id      NUMBER,
 p_today_dt         DATE) RETURN account_details_tbltab PIPELINED IS
--
  type_string NUMBER := 0;
  type_int NUMBER := 1;
  type_decimal NUMBER := 2;
  type_datetime NUMBER := 3;
  type_bool NUMBER := 4;
  party_account_cnt NUMBER;
  list varchar2(1000);
  min_dt DATE := to_date('19000101','YYYYMMDD');
  cnt NUMBER;
  contract_loop_cnt NUMBER := 0;
  account_details_tbl account_details_tblobj := account_details_tblobj(null,null,null,null,null);
BEGIN
-- If requesting all accounts of the selected party and
-- there is only one account then return no data
  IF (p_party_account_id = 0) THEN
    SELECT count(*) into party_account_cnt
    FROM party_account
    WHERE party_id = p_party_id;
    IF (party_account_cnt <= 1) THEN
      RETURN;
    END IF;
  END IF;
--
--First payment default
--
  SELECT COUNT(*)
  INTO cnt
  FROM axvw_overdue_view
  WHERE ((party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (party_account_id > p_party_account_id AND p_party_account_id = 0)
        )
  AND party_id = p_party_id
  AND currency_id = p_currency_id;
--
  IF (cnt = 1) THEN
    SELECT 1,
           'is_first_payment_def',
           ' ',
           to_char(COUNT(*)),
           type_bool
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
    FROM axvw_overdue_view
    WHERE ((party_account_id = p_party_account_id AND p_party_account_id > 0)
            OR (party_account_id > p_party_account_id AND p_party_account_id = 0))
    AND party_id = p_party_id
    AND currency_id = p_currency_id;
    PIPE ROW (account_details_tbl);
  ELSE
    account_details_tbl := account_details_tblobj(1,'is_first_payment_def',' ', '0', type_bool);
    PIPE ROW (account_details_tbl);
  END IF;
--
--Days past due (oldest item)
--
  SELECT 2,
         'days_past_due',
         ' ',
         to_char(p_today_dt - nvl(MIN(actual_dt), p_today_dt)),
         type_int
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_overdue_view
  WHERE ((party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND party_id = p_party_id
  AND currency_id = p_currency_id;
  PIPE ROW (account_details_tbl);
--
--Last payment received date
--
  SELECT 3,
         'last_payment_dt',
         ' ',
         to_char(nvl(MAX(f.actual_dt), min_dt),'DD-MON-YYYY'),
         type_datetime
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.party_account_id = pa.party_account_id
  AND f.currency_id = p_currency_id
  AND f.flow_source = 'B';
  PIPE ROW (account_details_tbl);
--
--Last payment received amount
--
  SELECT 4,
         'amt_last_payment',
         ' ',
         to_char(nvl(SUM(f.amount), 0)),
         type_decimal
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.party_account_id = pa.party_account_id
  AND currency_id = p_currency_id
  AND f.flow_source = 'B'
  AND f.actual_dt in
    (SELECT MAX(f1.actual_dt)
     FROM axvw_party_account_flow f1, party_account pa1
     WHERE ((f1.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f1.party_account_id > p_party_account_id AND p_party_account_id = 0)
    )
  AND pa1.party_id = p_party_id
  AND f1.party_account_id = pa1.party_account_id
  AND f1.currency_id = p_currency_id
  AND f1.flow_source = 'B');
  PIPE ROW (account_details_tbl);
--
--Paid to date
--
  SELECT 5,
         'paid_to_date_dt',
         ' ',
         to_char(nvl(MAX(f.actual_dt), min_dt),'DD-MON-YYYY'),
         type_datetime
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.party_account_id = pa.party_account_id
  AND f.currency_id = p_currency_id
  AND f.flow_type = 1003
  AND f.amt_gross = f.amt_matched
  AND f.flow_source = 'C';
  PIPE ROW (account_details_tbl);
--
--Last collection activity
--
  SELECT 6,
         'last_coll_activity_dt',
         ' ',
         to_char(nvl(MAX(activity_dt), min_dt),'DD-MON-YYYY'),
         type_datetime
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM note
  WHERE task_id in
    (SELECT task_id FROM axvw_coll_task
     WHERE ((party_account_id = p_party_account_id AND p_party_account_id > 0)
             OR (party_account_id > p_party_account_id AND p_party_account_id = 0))
     AND party_id = p_party_id
     AND currency_id = p_currency_id);
  PIPE ROW (account_details_tbl);
--
--Contract numbers
--
  list := '';
  for i in (SELECT ',' || cast(MAX(f.contract_id) AS varchar2(20)) list
              FROM flow f, party_account pa
              WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
              OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
              AND pa.party_id = p_party_id
              AND f.party_account_id = pa.party_account_id
              AND f.currency_id = p_currency_id
              AND f.contract_id > 0
			  AND f.is_cash = 1 and status != 2099
              GROUP BY f.contract_id
              ORDER BY f.contract_id desc) loop
	contract_loop_cnt := contract_loop_cnt + 1;
    list := list || i.list;
	exit when contract_loop_cnt = 10;
  end loop;
  IF (list IS NOT NULL) THEN
    list := substr(list, 2, length(list)); -- substr removes the leading ","
    account_details_tbl := account_details_tblobj(7,'contract_ids',' ', list, type_string);
    PIPE ROW (account_details_tbl);
  END IF;
--
--Exposure
--
  SELECT 8,
         'amt_exposure',
         ' ',
         to_char(nvl(SUM(f.amt_gross - f.amt_matched), 0)),
         type_decimal
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.party_account_id = pa.party_account_id
  AND f.currency_id = p_currency_id
  AND f.flow_type <> 1007
  AND f.flow_source = 'C';
  PIPE ROW (account_details_tbl);
--
--Account opened
--
  SELECT 9,
         'account_open_dt',
         ' ',
         to_char(nvl(MAX(input_dt), min_dt),'DD-MON-YYYY'),
         type_datetime
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM party_account
  WHERE ((party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND party_id = p_party_id;
  PIPE ROW (account_details_tbl);
--Last scheduled transaction
  SELECT 10,
         'last_transaction_dt',
         ' ',
         to_char(nvl(MAX(f.actual_dt), min_dt),'DD-MON-YYYY'),
         type_datetime
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.party_account_id = pa.party_account_id
  AND f.currency_id = p_currency_id
  AND f.flow_source = 'C';
  PIPE ROW (account_details_tbl);
--
--Scheduled term (month)
--
  SELECT 11,
         'scheduled_term',
         ' ',
         to_char(DATEDIFF('mm', nvl(MAX(pa.input_dt), p_today_dt), nvl(MAX(f.actual_dt), p_today_dt))),
         type_int
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND f.currency_id = p_currency_id
  AND f.flow_source = 'C'
  AND pa.party_account_id = f.party_account_id;
  PIPE ROW (account_details_tbl);
--
--Residual value
--
  SELECT 12,
         'amt_residual_value',
         ' ',
         to_char(nvl(SUM(f.amt_gross), 0)),
         type_decimal
    INTO   account_details_tbl.id,
           account_details_tbl.fld_name,
           account_details_tbl.caption,
           account_details_tbl.value_string,
           account_details_tbl.type
  FROM axvw_party_account_flow f, party_account pa
  WHERE ((f.party_account_id = p_party_account_id AND p_party_account_id > 0)
          OR (f.party_account_id > p_party_account_id AND p_party_account_id = 0))
  AND pa.party_id = p_party_id
  AND pa.party_account_id = f.party_account_id
  AND f.currency_id = p_currency_id
  AND f.flow_type = 1007
  AND f.flow_source = 'C';
  PIPE ROW (account_details_tbl);
  RETURN;
END axsp_coll_account_details;
/
CREATE OR REPLACE FUNCTION axsp_contract_susp_get_def(
p_party_id           IN NUMBER  DEFAULT NULL,
p_business_unit_id   IN NUMBER  DEFAULT NULL,
p_program_id         IN NUMBER  DEFAULT NULL,
p_product_id         IN NUMBER  DEFAULT NULL,
p_installment_freq   IN NUMBER  DEFAULT NULL
) RETURN NUMBER AS
--
--
  v_contract_suspension_def_id     NUMBER(10,0);
BEGIN
  v_contract_suspension_def_id := 0;
  FOR rec IN (SELECT contract_suspension_def_id
	            FROM contract_suspension_def
	            WHERE
		          is_active = 1
              and (party_id = 0 or party_id  IN
                    (
                      select party_id
                      from party
                      start with party_id = p_party_id
                      connect by NOCYCLE party_id = prior owner_id
                    )
                  )
              and (business_unit_id = 0 or business_unit_id  IN
                    (
                      select party_id
                      from party
                      start with party_id = p_business_unit_id
                      connect by NOCYCLE party_id = prior owner_id
                    )
                  )
              and (program_id = 0 or program_id = p_program_id)
              and (product_id = 0 or product_id  IN
                    (
                      select product_id
                      from product
                      start with product_id = p_product_id
                      connect by NOCYCLE  product_id = prior owner_id
                    )
                  )
              and (installment_frequency = 2605 or installment_frequency = p_installment_freq)

              ORDER BY priority DESC) LOOP
    v_contract_suspension_def_id := rec.contract_suspension_def_id;
    exit;
  END LOOP;
  RETURN v_contract_suspension_def_id;
END axsp_contract_susp_get_def;
/
CREATE OR REPLACE FUNCTION axsp_get_weighting
(
  p_cap_ad_prm_det_basel_std_id number,
  p_exposure_type number,
  p_external_rating number,
  p_acceptance_dt date,
  p_pricing_start_dt date,
  p_mature_dt1 date
)
RETURN number AS
--
  v_result number;
--
BEGIN
  select (case when p_exposure_type = 13700 /* Sovereign */ then
    (case when p_external_rating = 0 /* Unrated */ then sovereign_unrated
    when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then sovereign_aaa
    when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then sovereign_a
    when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 11 /* BB+ */ then sovereign_bbb
    when p_external_rating >= 11 /* BB+ */ and p_external_rating < 17 /* CCC+ */ then sovereign_bb
    else sovereign_b end)
  when p_exposure_type = 13701 /* PublicSectorEntities */ or p_exposure_type = 13702 /* MultilateralDevBanks */ or p_exposure_type = 13703 /* Banks */ then
    (case (select bank_option from cap_adeq_prm_det_basel_std where cap_adeq_prm_det_basel_std_id = p_cap_ad_prm_det_basel_std_id)
    when 0 /* Option 1 */ then
      (case when p_external_rating = 0 /* Unrated */ then bank_opt1_unrated
      when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then bank_opt1_aaa
      when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then bank_opt1_a
      when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 11 /* BB+ */ then bank_opt1_bbb
      when p_external_rating >= 11 /* BB+ */ and p_external_rating < 17 /* CCC+ */ then bank_opt1_bb
      else bank_opt1_b end)
    else /* Option 2 */
      (case when datediff('day', case when p_acceptance_dt < p_pricing_start_dt then p_acceptance_dt else p_pricing_start_dt end, p_mature_dt1) > 90 then
        (case when p_external_rating = 0 /* Unrated */ then bank_opt2_long_unrated
        when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then bank_opt2_long_aaa
        when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then bank_opt2_long_a
        when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 11 /* BB+ */ then bank_opt2_long_bbb
        when p_external_rating >= 11 /* BB+ */ and p_external_rating < 17 /* CCC+ */ then bank_opt2_long_bb
        else bank_opt2_long_b end)
      else
        (case when p_external_rating = 0 /* Unrated */ then bank_opt2_short_unrated
        when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then bank_opt2_short_aaa
        when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then bank_opt2_short_a
        when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 11 /* BB+ */ then bank_opt2_short_bbb
        when p_external_rating >= 11 /* BB+ */ and p_external_rating < 17 /* CCC+ */ then bank_opt2_short_bb
        else bank_opt2_short_b end)
      end)
    end)
  when p_exposure_type = 13704 /* Corporate */ or p_exposure_type = 13705 /* CorporateSME */ or p_exposure_type = 13706 /* CorparateRetail */ then
    (case when p_external_rating = 0 /* Unrated */ then corporate_unrated
    when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then corporate_aaa
    when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then corporate_a
    when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 14 /* B+ */ then corporate_bbb
    else corporate_bb end)
  else /* Retail */
    (case when p_external_rating = 0 /* Unrated */ then retail_unrated
    when p_external_rating >= 1 /* AAA */ and p_external_rating < 5 /* A+ */ then retail_aaa
    when p_external_rating >= 5 /* A+ */ and p_external_rating < 8 /* BBB+ */ then retail_a
    when p_external_rating >= 8 /* BBB+ */ and p_external_rating < 14 /* B+ */ then retail_bbb
    else retail_bb end)
  end)
  into v_result from cap_adeq_prm_det_basel_std
  where cap_adeq_prm_det_basel_std_id = p_cap_ad_prm_det_basel_std_id;
  return v_result;
END axsp_get_weighting;
/
CREATE OR REPLACE FUNCTION AXSP_SPLIT(p_string nvarchar2, p_delim varchar2)
RETURN TEMP_SPLIT_TBL IS
  v_pos           integer := 1;
  v_pos_current   integer;
  v_delim_pos     integer := 1;
  v_delim         varchar2(1);
  v_str           nvarchar2(255);
  v_remaining_str nvarchar2(255);
  v_loop_count    integer := 1;
  v_string_tbl TEMP_SPLIT_TBL := TEMP_SPLIT_TBL();
  type t_delim_array is table of varchar2(1);
  v_delim_array t_delim_array := t_delim_array();
begin
--
-- Build a temp table containing the delimiter characters
--
  while ( v_delim_pos <= length(p_delim)) loop
    v_delim := substr(p_delim,v_delim_pos,1);
    v_delim_array.extend;
    v_delim_array(v_delim_array.last) := v_delim;
    v_delim_pos := v_delim_pos + 1;
  end loop;
--
-- Remove leading and trailing delimiters
--
  v_str := p_string;
  while v_pos > 0 and
        (substr(v_str,1,1) member of v_delim_array OR
         substr(v_str,-1,1) member of v_delim_array) loop
    if LENGTH(v_str) > 1 then
      if substr(v_str,1,1) member of v_delim_array then
        v_str := substr(v_str,2); -- "LTRIM" one char
      else
        v_str := substr(v_str,1,length(v_str)-1); -- "RTRIM" one char
      end if;
    else
      v_str := '';
      v_pos := 0;
    end if;
  end loop;
--
  v_remaining_str := v_str;
  if v_str is not null then
  loop
    v_pos := 0;
-- Loop through all the delimiters passed in and find the minimum position
-- of any of those delimiters
    for i in v_delim_array.first..v_delim_array.last loop
      v_pos_current := instr(v_remaining_str, v_delim_array(i));
      if v_pos_current > 0 and (v_pos_current < v_pos OR v_pos = 0) then
        v_pos := v_pos_current;
      end if;
    end loop;
--
-- Now grab the string we have found and prepare remaining string to continue
--
    if v_pos > 0 then
      v_str := SUBSTR(v_remaining_str, 1, v_pos - 1);
      v_remaining_str := SUBSTR(v_remaining_str, v_pos+1);
    else
      v_str := v_remaining_str;
    end if;
--
-- Remove any delimiter characters from the start of the isolated string
--
    while (length(v_str) > 0 and substr(v_str,1,1) member of v_delim_array) loop
      v_str := substr(v_str,2); -- "LTRIM" one char
    end loop;
--
-- Put the isolated string into our table to return it later
--
    if length(v_str) > 0 then
      v_string_tbl.extend;
      v_string_tbl(v_string_tbl.last) := v_str;
    end if;
    v_loop_count := v_loop_count + 1;
    exit when v_pos = 0 OR v_loop_count > 1000;
  end loop;
  end if;
--
-- Bail out if we find ourselves in an endless loop
--
  if v_loop_count >= 1000 then
    raise_application_error(-20000, 'axsp_split: Endless loop when splitting "'||v_remaining_str||'"');
  end if;
  return v_string_tbl;
END AXSP_SPLIT;
/
CREATE OR REPLACE FUNCTION AXSP_GET_DATETIME
RETURN DATE AS
BEGIN
  RETURN SYSDATE;
END AXSP_GET_DATETIME;
/
CREATE OR REPLACE FUNCTION axsp_get_asset_hdr_child_tbl
(p_id IN NUMBER  DEFAULT NULL)
RETURN ASSET_HDR_CHILD_TBLTAB PIPELINED IS
--
 ASSET_HDR_CHILD_TBL ASSET_HDR_CHILD_TBLOBJ := ASSET_HDR_CHILD_TBLOBJ (null);
--
  CURSOR asset_hdr_children_cur(c_id number) IS
    SELECT  asset_hdr_id
    FROM asset_hdr
    START WITH owner_id = c_id
    CONNECT BY PRIOR asset_hdr_id = owner_id;
--
BEGIN
  FOR i IN asset_hdr_children_cur(p_id) LOOP
    ASSET_HDR_CHILD_TBL.asset_hdr_id := i.asset_hdr_id;
    PIPE ROW(ASSET_HDR_CHILD_TBL);
  END LOOP;
  RETURN;
END axsp_get_asset_hdr_child_tbl;
/
CREATE OR REPLACE FUNCTION axsp_get_asset_type_child_tbl
(p_id IN NUMBER  DEFAULT NULL)
RETURN ASSET_TYPE_CHILD_TBLTAB PIPELINED IS
--
 ASSET_TYPE_CHILD_TBL ASSET_TYPE_CHILD_TBLOBJ := ASSET_TYPE_CHILD_TBLOBJ (null);
--
  CURSOR asset_type_children_cur(c_id number) IS
    SELECT  asset_type_id
    FROM asset_type
    START WITH owner_id = c_id
    CONNECT BY PRIOR asset_type_id = owner_id;
--
BEGIN
  FOR i IN asset_type_children_cur(p_id) LOOP
    ASSET_TYPE_CHILD_TBL.asset_type_id := i.asset_type_id;
    PIPE ROW(ASSET_TYPE_CHILD_TBL);
  END LOOP;
  RETURN;
END axsp_get_asset_type_child_tbl;
/
CREATE OR REPLACE FUNCTION axsp_get_party_child_tbl
(p_id IN NUMBER  DEFAULT NULL)
RETURN PARTY_CHILD_TBLTAB PIPELINED IS
--
 PARTY_CHILD_TBL PARTY_CHILD_TBLOBJ := PARTY_CHILD_TBLOBJ (null);
--
  CURSOR party_children_cur(c_id number) IS
    SELECT  party_id
    FROM party
    START WITH owner_id = c_id
    CONNECT BY PRIOR party_id = owner_id;
--
BEGIN
  FOR i IN party_children_cur(p_id) LOOP
    PARTY_CHILD_TBL.party_id := i.party_id;
    PIPE ROW(PARTY_CHILD_TBL);
  END LOOP;
  RETURN;
END AXSP_GET_PARTY_CHILD_TBL;
/
CREATE OR REPLACE FUNCTION axsp_get_all_related_party_ids
(p_id IN NUMBER  DEFAULT NULL)
RETURN PARTY_CHILD_TBLTAB PIPELINED IS
--
 PARTY_CHILD_TBL PARTY_CHILD_TBLOBJ := PARTY_CHILD_TBLOBJ (null);
--
  CURSOR cur(c_id number) IS
    SELECT rid FROM (select owner_id, party_id as rid from party where party_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR owner_id;
  CURSOR cur2(c_id number) IS
    SELECT party_id FROM table(axsp_get_party_child_tbl(c_id)) union all select c_id from dual;
  v_top_level_party_id number;
--
BEGIN
  FOR i IN cur(p_id) LOOP
    v_top_level_party_id := i.rid;
  END LOOP;
  FOR i IN cur2(v_top_level_party_id) LOOP
    PARTY_CHILD_TBL.party_id := i.party_id;
    PIPE ROW(PARTY_CHILD_TBL);
  END LOOP;
  RETURN;
END axsp_get_all_related_party_ids;
/
CREATE OR REPLACE FUNCTION axsp_get_product_child_tbl
(p_id IN NUMBER  DEFAULT NULL)
RETURN PRODUCT_CHILD_TBLTAB PIPELINED IS
--
 PRODUCT_CHILD_TBL PRODUCT_CHILD_TBLOBJ := PRODUCT_CHILD_TBLOBJ (null);
--
  CURSOR product_children_cur(c_id number) IS
    SELECT  product_id
    FROM product
    START WITH owner_id = c_id
    CONNECT BY PRIOR product_id = owner_id;
--
BEGIN
  FOR i IN product_children_cur(p_id) LOOP
    PRODUCT_CHILD_TBL.product_id := i.product_id;
    PIPE ROW(PRODUCT_CHILD_TBL);
  END LOOP;
  RETURN;
END AXSP_GET_PRODUCT_CHILD_TBL;
/
CREATE OR REPLACE FUNCTION axsp_get_location_child_tbl
(l_id IN NUMBER  DEFAULT NULL)
RETURN LOCATION_CHILD_TBLTAB PIPELINED IS
--
 LOCATION_CHILD_TBL LOCATION_CHILD_TBLOBJ := LOCATION_CHILD_TBLOBJ (null);
--
  CURSOR loc_children_cur(c_id number) IS
    SELECT  location_id
    FROM location
    START WITH owner_id = c_id
    CONNECT BY PRIOR location_id = owner_id;
--
BEGIN
  FOR i IN loc_children_cur(l_id) LOOP
    LOCATION_CHILD_TBL.location_id := i.location_id;
    PIPE ROW(LOCATION_CHILD_TBL);
  END LOOP;
  RETURN;
END AXSP_GET_LOCATION_CHILD_TBL;
/

CREATE OR REPLACE FUNCTION axsp_funding_loan_csv (
p_contract_id   IN NUMBER  DEFAULT NULL) RETURN NVARCHAR2 AS
--
  csv   VARCHAR2(1000);
BEGIN
  csv := '';
  FOR rec IN ( SELECT funding_contract_id
               FROM funding_contract_link
               WHERE contract_id = p_contract_id and funding_contract_id <> 0
               ORDER BY funding_contract_id ) LOOP
     csv := csv||CAST(rec.funding_contract_id as VARCHAR2) || ',';
  END LOOP;
  IF (csv IS NOT NULL) THEN
    csv := SUBSTRING(csv, 1, length(csv) - 1);
  END IF;
  RETURN csv;
END axsp_funding_loan_csv;
/

--REM ====== Procedures ======

CREATE OR REPLACE PROCEDURE axsp_tax_address
(
  p_contract_id		IN INTEGER,
  p_first_dt		IN DATE,
  p_last_dt		IN DATE,
  p_invalid_only	IN NUMBER,
  RC1			IN OUT globalPkg.RCT1
)
AS
BEGIN
  BEGIN
	OPEN RC1 FOR

	--Contract Operating Unit
	SELECT	41701 as location_source, pad.party_address_id, pad.is_current, pad.address_type, pad.effect_dt_from, pad.tax_area, pad.city_id, pad.county_id, pad.state_province_id, pad.zip_code, pad.country_region_id, pad.suburb, 0 as asset_id, 0 as asset_hdr_id, pad.stamp
	FROM		party_address pad
	WHERE		pad.party_id = (SELECT branch_id FROM contract WHERE contract_id = p_contract_id) AND
				pad.effect_dt_from <= p_last_dt AND
				pad.effect_dt_to >= p_last_dt AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(pad.tax_area)), 0) = 0) AND
				pad.party_address_id > 0

	--Contract Business Unit
	UNION ALL
	SELECT	41702 as location_source, pad.party_address_id, pad.is_current, pad.address_type, pad.effect_dt_from, pad.tax_area, pad.city_id, pad.county_id, pad.state_province_id, pad.zip_code, pad.country_region_id, pad.suburb, 0 as asset_id, 0 as asset_hdr_id, pad.stamp
	FROM		party_address pad
	WHERE		pad.party_id = (SELECT business_unit_id FROM contract WHERE contract_id = p_contract_id) AND
				pad.effect_dt_from <= p_last_dt AND
				pad.effect_dt_to >= p_last_dt AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(pad.tax_area)), 0) = 0) AND
				pad.party_address_id > 0

	--Financial Asset. TODO complete effective date selection when ready.
	UNION ALL
	SELECT	41703 as location_source, ad.address_id, 1, 5000, TO_DATE('01-jan-1900','dd-mon-yyyy'), ad.tax_area , ad.city_id, ad.county_id, ad.state_province_id, ad.zip_code, ad.country_region_id, ad.suburb, asset.asset_id as asset_id, asset.asset_hdr_id as asset_hdr_id, ad.stamp
	FROM		address ad
	INNER JOIN  asset_address ON asset_address.address_id = ad.address_id
	INNER JOIN  asset ON asset_address.asset_id = asset.asset_id
	WHERE asset.contract_id = p_contract_id AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(ad.tax_area)), 0) = 0) AND
				ad.address_id > 0 AND
				asset_address.effective_dt <= p_last_dt

	--Customer
	UNION ALL
	SELECT	41704 as location_source, pad.party_address_id, pad.is_current, pad.address_type, pad.effect_dt_from, pad.tax_area, pad.city_id, pad.county_id, pad.state_province_id, pad.zip_code, pad.country_region_id, pad.suburb, 0 as asset_id, 0 as asset_hdr_id, pad.stamp
	FROM		party_address pad
	WHERE		pad.party_id = (SELECT cparty_id FROM contract WHERE contract_id = p_contract_id) AND
				pad.effect_dt_from <= p_last_dt AND
				pad.effect_dt_to >= p_last_dt AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(pad.tax_area)), 0) = 0) AND
				pad.party_address_id > 0

	--AssetHdr. TODO complete effective date selection when ready.
	UNION ALL
	SELECT	41705 as location_source, ad.address_id, 1, 5000, TO_DATE('01-jan-1900','dd-mon-yyyy'), ad.tax_area , ad.city_id, ad.county_id, ad.state_province_id, ad.zip_code, ad.country_region_id, ad.suburb, 0 as asset_id, asset_hdr.asset_hdr_id as asset_hdr_id, ad.stamp
	FROM		address ad
	INNER JOIN  asset_hdr_address ON asset_hdr_address.address_id = ad.address_id
	INNER JOIN  asset_hdr ON asset_hdr_address.asset_hdr_id = asset_hdr.asset_hdr_id
	WHERE (CASE WHEN asset_hdr.current_contract_id > 0 THEN asset_hdr.current_contract_id ELSE asset_hdr.original_contract_id END) = p_contract_id AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(ad.tax_area)), 0) = 0) AND
				ad.address_id > 0 AND
				asset_hdr_address.effective_dt <= p_last_dt

	--Contract Default Address
	UNION ALL
	SELECT	41706 as location_source, ad.address_id, 1, 5000, TO_DATE('01-jan-1900','dd-mon-yyyy'), ad.tax_area , ad.city_id, ad.county_id, ad.state_province_id, ad.zip_code, ad.country_region_id, ad.suburb, 0 as asset_id, 0 as asset_hdr_id, ad.stamp
	FROM		address ad, contract_det cd
	WHERE		cd.contract_id =  p_contract_id AND
				cd.default_asset_address_id = ad.address_id AND
				(p_invalid_only = 0 OR NVL(LENGTH(TRIM(ad.tax_area)), 0) = 0) AND
				ad.address_id > 0;
	END;
END axsp_tax_address;
/
CREATE OR REPLACE PROCEDURE axsp_get_next_installment (
p_contract_id IN number,
p_date IN date,
RC1 IN OUT globalPkg.RCT1) AS
--
BEGIN
OPEN RC1 FOR SELECT flow_id, flow_type, amount, expected_dt FROM flow f
				 WHERE f.contract_id = p_contract_id
				 AND is_funding_transfer = 0
				 AND f.expected_dt = (SELECT min(f.expected_dt) FROM flow f
											 WHERE f.contract_id = p_contract_id
											 AND is_funding_transfer = 0
											 AND f.expected_dt >= p_date)
				 AND f.expected_dt >= p_date;
END AXSP_GET_NEXT_INSTALLMENT;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_PAYMENT_HISTORY(
	isReceipt   IN INTEGER,
	contractId  IN INTEGER,
	RC1			IN OUT globalPkg.RCT1) AS
--
	reversalStatus	INTEGER;
--
BEGIN
	reversalStatus := 4202;

  BEGIN
	OPEN RC1 FOR
	SELECT * FROM (
	SELECT 'C' flow_source,
		f.flow_id,
		f.status,
		f.contract_id,
		f.party_account_id,
		f.expected_dt,
		ccy.code ccy,
		f.amount,
		f.amt_gross,
		f.amt_invoice,
		f.amt_matched,
		f.flow_method_id,
		fm.ext_name flow_method_name,
		fm.payment_method,
		l1.value payment_method_value,
		f.installment_no,
		f.flow_type,
		f.custom_flow_hdr_id,
		f.reversal_status,
		f.collection_state,
		l2.value collection_state_value,
		i.invoice_no,
		f.reversal_status flow_reversal_status,
		(case when f.reversal_status = 4201 then 0 else
		(case when f.reversal_status = 4203 then 0 else
		(case when f.reversal_status = 4202 then 1 else 2 end) end) end) sort_no,
		N' ' other_info,
		f.is_shadow_copy,
		0 upload_batch_id,
		0 upload_package_id,
		to_date('01/01/1900','dd/mm/yyyy') cleared_funds_dt,
		0 is_deleted,
		0 stamp
	FROM  flow f,
		flow_method fm,
		currency ccy,
		invoice i,
		lookupset l1,
		lookupset l2,
		(SELECT contract_id FROM bail_asset WHERE bail_acc_id = contractId union select contractId from dual) c
	WHERE f.contract_id = c.contract_id
		AND f.is_cash = 1
		AND (f.status not in (2099,2104,2105) OR (f.status = 2104 AND f.amt_gross != f.amt_matched AND f.amt_matched != 0 )) --exlude projected and written off
		AND f.flow_method_id = fm.flow_method_id
		AND f.exclude_from_account_bal = 0 -- Exclude recovery flows
		AND f.currency_id = ccy.currency_id
		AND l1.lookupset_id = fm.payment_method
		AND l2.lookupset_id = f.collection_state
		AND f.invoice_id = i.invoice_id
		--Display reciept flows and relevant opposite small balance write off flows for that reciept
	   --Or display payment flows and relevant opposite small balance write off flows for that payment
	   AND ((isReceipt = 1 and ((f.amt_invoice > 0 and f.reversal_status != reversalStatus and f.flow_type <> 1034)
					or (f.amt_invoice < 0 and f.reversal_status = reversalStatus and f.flow_type <> 1034)
					or (f.amount < 0 and f.reversal_status != reversalStatus and f.flow_type = 1034)
					or (f.amount > 0 and f.reversal_status = reversalStatus and f.flow_type = 1034))) OR
	        (isReceipt = 0 and ((f.amt_invoice < 0 and f.reversal_status != reversalStatus and f.flow_type <> 1034)
					or (f.amt_invoice > 0 and f.reversal_status = reversalStatus and f.flow_type <> 1034)
					or (f.amount > 0 and f.reversal_status != reversalStatus and f.flow_type = 1034)
					or (f.amount < 0 and f.reversal_status = reversalStatus  and f.flow_type = 1034))))

	UNION ALL

	SELECT 'B' flow_source,
		f.bank_flow_id flow_id,
		0 status,
		f.contract_id,
		f.party_account_id,
		f.actual_dt expected_dt,
		ccy.code ccy,
		f.amount,
		f.amount amt_gross,
		f.amount amt_invoice,
		(select sum(bfm.amt_matched) from bank_flow_match bfm, flow
								 where bfm.bank_flow_id = f.bank_flow_id
								 and bfm.is_deleted = 0
								 and bfm.flow_id = flow.flow_id
								 and flow.exclude_from_account_bal = 0) amt_matched,
		0 flow_method_id,
		N' ' flow_method_name,
		f.bank_flow_type payment_method,
		l1.value as payment_method_value,
		0 installment_no,
		0 flow_type,
		0 custom_flow_hdr_id,
		f.reversal_status,
		14800 collection_state,
		l2.value as collection_state_value,
		f.invoice_no,
		(NVL( (select max(reversal_status) from flow f2, bank_flow_match bfm where f2.flow_id = bfm.flow_id and bfm.bank_flow_id = f.bank_flow_id having count(*) = 1), 4200)) flow_reversal_status,
		(case when f.reversal_status = 4201 then 0 else
			(case when f.reversal_status = 4202 then 1 else 2 end) end) sort_no,
		N' ' other_info,
		f.is_shadow_copy,
		f.batch_no upload_batch_id,
		0 upload_package_id,
		f.cleared_funds_dt,
		f.is_deleted,
		0 stamp
	FROM  bank_flow f,
		currency ccy,
		lookupset l1,
		lookupset l2,
		(SELECT contract_id FROM bail_asset WHERE bail_acc_id = contractId union select contractId from dual) c
	WHERE f.contract_id = c.contract_id
		AND f.currency_id = ccy.currency_id
		AND l1.lookupset_id = f.bank_flow_type
		AND l2.lookupset_id = 14800
		AND ((isReceipt = 1 and ((f.amount > 0 and f.reversal_status != reversalStatus) or (f.amount < 0 and f.reversal_status = reversalStatus))) OR
		   (isReceipt = 0 and ((f.amount < 0 and f.reversal_status != reversalStatus) or (f.amount > 0 and f.reversal_status = reversalStatus))))

	UNION ALL

	-- Get bankflows with contract Id = 0 that is allocated to the contract
	SELECT DISTINCT 'B' flow_source,
		f.bank_flow_id flow_id,
		0 status,
		f.contract_id,
		f.party_account_id,
		f.actual_dt expected_dt,
		ccy.code ccy,
		f.amount,
		f.amount amt_gross,
		f.amount amt_invoice,
		(select sum(bfm.amt_matched) from bank_flow_match bfm inner join flow f2 on f2.flow_id = bfm.flow_id and f2.contract_id = contractId
		 where bfm.bank_flow_id = f.bank_flow_id and f.contract_id = 0 and bfm.is_deleted = 0) amt_matched,
		0 flow_method_id,
		N' ' flow_method_name,
		f.bank_flow_type payment_method,
		l1.value as payment_method_value,
		0 installment_no,
		0 flow_type,
		0 custom_flow_hdr_id,
		f.reversal_status,
		14800 collection_state,
		l2.value as collection_state_value,
		f.invoice_no,
		f2.reversal_status flow_reversal_status,
		(case when f.reversal_status = 4201 then 0 else
		(case when f.reversal_status = 4202 then 1 else 2 end) end) sort_no,
		N' ' other_info,
		f.is_shadow_copy,
		f.batch_no upload_batch_id,
		0 upload_package_id,
		f.cleared_funds_dt,
		f.is_deleted,
		0 stamp
	FROM  bank_flow f inner join bank_flow_match bfm on (f.bank_flow_id = bfm.bank_flow_id and f.contract_id = 0),
		flow f2,
		currency ccy,
		lookupset l1,
		lookupset l2,
		(SELECT contract_id FROM bail_asset WHERE bail_acc_id = contractId union select contractId from dual) c
	WHERE f.contract_id = 0 and f.currency_id = ccy.currency_id
		AND bfm.flow_id = f2.flow_id
		AND f2.contract_id = c.contract_id
		AND bfm.is_deleted = 0
		AND l1.lookupset_id = f.bank_flow_type
		AND l2.lookupset_id = 14800
		AND ((isReceipt = 1 and ((f.amount > 0 and f.reversal_status != reversalStatus) or (f.amount < 0 and f.reversal_status = reversalStatus))) OR
		   (isReceipt = 0 and ((f.amount < 0 and f.reversal_status != reversalStatus) or (f.amount > 0 and f.reversal_status = reversalStatus))))
	UNION ALL

	-- Get vendor receipts that are allocated to the contract
	SELECT 'V' flow_source,
		vd.vendor_receipt_det_id flow_id,
		0 status,
		vd.linked_contract_id,
		0 party_account_id,
		vh.input_dt expected_dt,
		ccy.code ccy,
		vd.amt_gross amount,
		vd.amt_gross amt_gross,
		vd.amt_gross amt_invoice,
		vd.amt_allocated amt_matched,
		0 flow_method_id,
		N' ' flow_method_name,
		5103 payment_method,
		l1.value as payment_method_value,
		0 installment_no,
		0 flow_type,
		0 custom_flow_hdr_id,
		4200 reversal_status,
		14800 collection_state,
		l2.value as collection_state_value,
		N'0' invoice_no,
		4200 flow_reversal_status,
		2 sort_no,
		vd.other_info,
		0 is_shadow_copy,
		vh.upload_batch_id,
		vh.upload_package_id,
		to_date('01/01/1900','dd/mm/yyyy') cleared_funds_dt,
		0 is_deleted,
		0 stamp
FROM   vendor_receipt_det vd,
		vendor_receipt_hdr vh,
		contract c,
		currency ccy,
		lookupset l1,
		lookupset l2
WHERE  vd.linked_contract_id = contractId
		AND vd.vendor_receipt_hdr_id = vh.vendor_receipt_hdr_id
		AND c.contract_id = vd.linked_contract_id
		AND c.currency_id = ccy.currency_id
		AND l1.lookupset_id = 5103
		AND l2.lookupset_id = 14800
		AND isReceipt = 1
	) p
	ORDER BY expected_dt, flow_source DESC, sort_no, flow_id;
	END;
END AXSP_CONTRACT_PAYMENT_HISTORY;
/
CREATE OR REPLACE PROCEDURE DDL_Manager(ddl_statement VARCHAR)
AUTHID CURRENT_USER IS
BEGIN
  EXECUTE IMMEDIATE ddl_statement;
EXCEPTION
  WHEN OTHERS THEN
    RAISE;
END DDL_Manager;
/

CREATE OR REPLACE PROCEDURE axsp_asset_search(
    TextFind                     IN NVARCHAR2 DEFAULT NULL,
    IdFind                       IN NUMBER DEFAULT NULL,
    TextFindNameFields           IN NUMBER DEFAULT NULL,
    TextFindDescription          IN NUMBER DEFAULT NULL,
    TextFindReference            IN NUMBER DEFAULT NULL,
    TextFindStockCode            IN NUMBER DEFAULT NULL,
    TextFindSerialNumbers        IN NUMBER DEFAULT NULL,
    TextFindCustomFields         IN NUMBER DEFAULT NULL,
    TextFindLocation             IN NUMBER DEFAULT NULL,
    TextFindLocationContract     IN NUMBER DEFAULT NULL,
    IdFindParty                  IN NUMBER DEFAULT NULL,
    IdFindOriginalContract       IN NUMBER DEFAULT NULL,
    IdFindCurrentContract        IN NUMBER DEFAULT NULL,
    IncludeAssetsWithoutInvoices IN NUMBER DEFAULT NULL,
    IncludeOwnedAssetsOnly       IN NUMBER DEFAULT NULL,
    IncludeAssetsWithoutContract IN NUMBER DEFAULT NULL,
    ExcludeAssetGroups				IN NUMBER DEFAULT NULL,
	 ExcludeAssetWithoutSubAssets IN NUMBER DEFAULT NULL,
	 ExcludeAssetWithSubAssets		IN NUMBER DEFAULT NULL,
	 ExcludeSubAssets					IN NUMBER DEFAULT NULL,
    ProductStyle                 IN NUMBER DEFAULT NULL,
	 AssetType							IN NUMBER DEFAULT NULL,
	 DisposalType						IN NUMBER DEFAULT NULL,
	 ContainFlag				 		IN NUMBER DEFAULT NULL,
	 ContractId					 		IN NUMBER DEFAULT NULL,
	 ContractIdAssetsOnly	 		IN NUMBER DEFAULT NULL,
    TextFindAssetType				IN NVARCHAR2 DEFAULT NULL,
	 TextFindYear						IN NUMBER DEFAULT NULL,
    RC1                          IN OUT globalPkg.RCT1)
AS
  TextFind_								NVARCHAR2(1024) := TextFind;
  IdFind_								NVARCHAR2(1024) := CAST(IdFind AS NVARCHAR2);
  IdFindTextContractIdAssetOnly_		NVARCHAR2(1024) := CAST(ContractIdAssetsOnly AS NVARCHAR2);
  ProductStyle_						NVARCHAR2(4)    := CAST(ProductStyle AS NVARCHAR2);
  StoO_error							INTEGER;
  StoO_errmsg							VARCHAR2(255);
  SqlQuery								VARCHAR2(4000);
  IncludeFilterFlag					NUMBER(1,0);
  ExcludeFilterFlag					NUMBER(1,0);
  TextFindFlag							NUMBER(1,0);
  IdFindFlag							NUMBER(1,0);
  AssetType_							NVARCHAR2(255) := CAST(AssetType AS NVARCHAR2);
  DisposalType_						NVARCHAR2(255) := CAST(DisposalType AS NVARCHAR2);
  ContainFlag_							NUMBER(1,0) := ContainFlag;
  row_limit								CONSTANT INTEGER := 500;
  IndividualSearchText_				NVARCHAR2(255);
  IndividualTextSearchSql_	      NVARCHAR2(3000);
  v_string_tbl						   TEMP_SPLIT_TBL := TEMP_SPLIT_TBL();
  ContainWildcard						NVARCHAR2(10);
  ContractId_							NVARCHAR2(255) := CAST(ContractId AS NVARCHAR2);
  TextFindAssetType_					NVARCHAR2(1024) := TextFindAssetType;
  TextFindYear_						NVARCHAR2(1024) := CAST(TextFindYear AS NVARCHAR2);
BEGIN
  SqlQuery := 'select * from (select distinct asset_hdr.asset_hdr_id, asset_hdr.original_purchase_dt, asset_hdr.name,
axsp_get_concat_address(axsp_get_asset_hdr_address_id(asset_hdr.asset_hdr_id, 45100, axsp_dateonly(axsp_get_datetime()))) as full_address,
axsp_get_concat_address(axsp_get_asset_address_id(fin_asset.asset_id, axsp_dateonly(axsp_get_datetime()))) as contract_address,
asset_hdr.sub_location, asset_hdr.asset_description, asset_hdr.year_of_manufacture, 
asset_type.name asset_type_name, 
manufacturer.name manufacturer, asset_hdr.model, asset_hdr.reference, 
asset_hdr.serial_no,asset_hdr.stock_code, asset_hdr.asset_type_id, 
asset_hdr.asset_status, asset_hdr.hdr_type, asset_hdr.parent_owner_id, 
(select value from lookupset where lookupset.lookupset_id = asset_hdr.asset_status) as asset_status_value,
contract.contract_id, contract.reference as contract_reference, product.name product_name,  '; 

  --
  IF(TextFindSerialNumbers = 1 OR TextFindDescription = 1) THEN
    SqlQuery := SqlQuery || 'asset_class_vehicle.registration_no, asset_class_vehicle.vin_no';
  ELSE
    SqlQuery := SqlQuery || 'NULL as registration_no, NULL as vin_no';
  END IF;
  --
  SqlQuery := SqlQuery || ', asset_hdr.stamp from asset_hdr inner join asset_type on asset_hdr.asset_type_id = asset_type.asset_type_id';

  IF(AssetType > 0) THEN
     SELECT name INTO TextFindAssetType_ FROM asset_type WHERE asset_type_id = AssetType_;
  END IF;

  IF (len(ltrim(rtrim(TextFindAssetType_))) > 0) THEN
   TextFindAssetType_ := UPPER(TextFindAssetType_);
   SqlQuery := SqlQuery || ' and INSTR(UPPER(axsp_get_asset_type_owner_tree(asset_type.asset_type_id) || asset_type.name), ''' || TextFindAssetType_ || ''' ) > 0 ' || ' '; 
  END IF;

  SqlQuery := SqlQuery || '  inner join party manufacturer on asset_hdr.manufacturer_id = manufacturer.party_id
left outer join asset fin_asset on asset_hdr.asset_hdr_id = fin_asset.asset_hdr_id
and fin_asset.contract_id = (CASE WHEN asset_hdr.current_contract_id <> 0 THEN asset_hdr.current_contract_id ELSE asset_hdr.original_contract_id END)
inner join contract on contract.contract_id = (CASE WHEN asset_hdr.current_contract_id <> 0 THEN asset_hdr.current_contract_id ELSE asset_hdr.original_contract_id END)
inner join product on contract.product_id = product.product_id';
  --
  IF(TextFindLocation  = 1 OR TextFindLocationContract = 1) THEN
    SqlQuery := SqlQuery || ' inner join address on ';
    IF(TextFindLocation = 1) THEN
      SqlQuery := SqlQuery || 'asset_hdr.address_id = address.address_id';
    END IF;
    IF(TextFindLocation = 1 AND TextFindLocationContract = 1) THEN
      SqlQuery := SqlQuery || ' or ';
    END IF;
    IF(TextFindLocationContract = 1) THEN
      SqlQuery := SqlQuery || 'fin_asset.address_id = address.address_id';
    END IF;
    --
    SqlQuery := SqlQuery || ' inner join location city_location on address.city_id = city_location.location_id
inner join location county_location on address.county_id = county_location.location_Id
inner join location state_province_location on address.state_province_id = state_province_location.location_id
inner join location country_region_location on address.country_region_id = country_region_location.location_id';
    --
  END IF;
  --
  IF(TextFindCustomFields = 1) THEN
    SqlQuery := SqlQuery || ' left outer join asset_hdr_cfv on asset_hdr.asset_hdr_id = asset_hdr_cfv.asset_hdr_id left outer join asset_hdr_cfd on asset_hdr_cfv.asset_hdr_cfd_id = asset_hdr_cfd.asset_hdr_cfd_id';
  END IF;
  --
  IF(TextFindSerialNumbers = 1 OR TextFindDescription = 1) THEN
    SqlQuery := SqlQuery || ' left outer join asset_secondary_ident on asset_hdr.asset_hdr_id = asset_secondary_ident.asset_hdr_id ';
    SqlQuery := SqlQuery || ' left outer join asset_class_vehicle on asset_hdr.asset_hdr_id = asset_class_vehicle.asset_hdr_id ';
    SqlQuery := SqlQuery || ' left outer join asset_hdr_registration on asset_hdr.asset_hdr_id = asset_hdr_registration.asset_hdr_id ';
  END IF;
  --
  SqlQuery := SqlQuery || ' where asset_hdr.asset_hdr_id > 0';
  IndividualSearchText_ := TextFind_;
  TextFind_ := AXSP_NORMALIZE_SEARCH_TERM(TextFind_);
  --
  
	IF (ISNULL(ContractId, 0) > 0) THEN
		SqlQuery := SqlQuery || ' and asset_hdr.asset_hdr_id NOT IN(
SELECT contr_asset_hdr.asset_hdr_id
FROM asset contr_asset INNER JOIN asset_hdr contr_asset_hdr ON
 contr_asset.asset_hdr_id = contr_asset_hdr.asset_hdr_id
 OR contr_asset.asset_hdr_id = contr_asset_hdr.owner_id
 OR contr_asset.asset_hdr_id = contr_asset_hdr.parent_owner_id
WHERE contract_id = '||ContractId_||')';
	END IF;

  --

  IF (ProductStyle != 0) THEN
    SqlQuery := SqlQuery || ' and contract.product_style = '||ProductStyle_;
  END IF;

  IF (DisposalType != 0) THEN
    SqlQuery := SqlQuery || ' and asset_hdr.asset_status = ' || DisposalType_;
  END IF;

  If(TextFindYear > 0) Then
    SqlQuery := SqlQuery || ' and year_of_manufacture = ' || TextFindYear_;
  End If;
  --
  IncludeFilterFlag := 0;
  IF(IncludeAssetsWithoutContract = 1) THEN
    IF(IncludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IncludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.current_contract_id = 0)';
  END IF;
  --
  IF(IncludeAssetsWithoutInvoices = 1) THEN
    IF(IncludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IncludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' ((select count(*) from purchase_invoice_row where purchase_invoice_row.asset_hdr_id = asset_hdr.asset_hdr_id) = 0 or asset_hdr.require_purch_invoice = 0)';
  END IF;
  --
  IF(IncludeOwnedAssetsOnly = 1) THEN
    IF(IncludeFilterFlag   = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IncludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.ownership_type = 18702 or asset_hdr.ownership_type = 18704)';
  END IF;
  --
  IF(IncludeFilterFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
  END IF;
  -- Excludes
  ExcludeFilterFlag := 0;
  IF(ExcludeAssetGroups = 1) THEN
    IF(ExcludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      ExcludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.hdr_type != 33402)';
  END IF;
  --
  IF(ExcludeSubAssets = 1) THEN
    IF(ExcludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      ExcludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.hdr_type != 33401)';
  END IF;
  --
  IF(ExcludeAssetWithSubAssets = 1) THEN
    IF(ExcludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      ExcludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.asset_hdr_id != 33400 AND ';
	 SqlQuery := SqlQuery || ' exists(select 1 ';
	 SqlQuery := SqlQuery || ' from asset_hdr h1 ';
	 SqlQuery := SqlQuery || ' where h1.hdr_type = 33401 and h1.owner_id = asset_hdr.asset_hdr_id ';
	 SqlQuery := SqlQuery || ' )) ';

  END IF;
    --
  IF(ExcludeAssetWithoutSubAssets = 1) THEN
    IF(ExcludeFilterFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      ExcludeFilterFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' and';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.asset_hdr_id != 33400 AND ';
	 SqlQuery := SqlQuery || ' not exists(select 1 ';
	 SqlQuery := SqlQuery || ' from asset_hdr h1 ';
	 SqlQuery := SqlQuery || ' where h1.hdr_type = 33401 and h1.owner_id = asset_hdr.asset_hdr_id ';
	 SqlQuery := SqlQuery || ' )) ';

  END IF;

  --
  IF(ExcludeFilterFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
  END IF;
  --
  ContainWildCard := '';
  IF (ContainFlag_ = 1) THEN
    ContainWildCard := '%';
  END IF;
  --
  TextFindFlag := 0;
  IF(TextFindNameFields = 1) THEN
    IF(TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr.name) like '''|| ContainWildcard  || TextFind_ || '%''';
	IF(ContainFlag_ = 1) THEN
	  v_string_tbl := axsp_split(IndividualSearchText_,' -');
	  IF v_string_tbl.COUNT > 0 then
	    FOR j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
		  IndividualTextSearchSql_ := IndividualTextSearchSql_ || ' or lower(asset_hdr.name) like ''%' || v_string_tbl(j) || '%''';
		END LOOP;
		v_string_tbl.delete;
	  END IF;
	  SqlQuery := SqlQuery || ' ' || IndividualTextSearchSql_ ;
	END IF;
	SqlQuery := SqlQuery || ') ';
  END IF;
  --
  IF(TextFindDescription = 1) THEN
    IF(TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr.asset_description) like '''|| ContainWildcard  || TextFind_ || '%'' or lower(asset_secondary_ident.name) like '''|| ContainWildcard  || TextFind_ || '%'')';
  END IF;
  --
  IF(TextFindReference = 1) THEN
    IF(TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr.reference) like '''|| ContainWildcard  || TextFind_ || '%'')';
  END IF;
  --
  IF(TextFindStockCode = 1) THEN
    IF(TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr.stock_code) like '''|| ContainWildcard  || TextFind_ || '%'')';
  END IF;
  --
  IF(TextFindSerialNumbers = 1) THEN
    IF(TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr.serial_no) like '''|| ContainWildcard  || TextFind_ || '%''  or lower(asset_secondary_ident.serial_no) like '''|| ContainWildcard  || TextFind_ || '%'')';
    SqlQuery := SqlQuery || ' or (lower(asset_class_vehicle.registration_no) like '''|| ContainWildcard  || TextFind_ || '%'') ';
    SqlQuery := SqlQuery || ' or (lower(asset_class_vehicle.vin_no) like '''|| ContainWildcard  || TextFind_ || '%'') ';
    SqlQuery := SqlQuery || ' or (lower(asset_hdr_registration.reg_number) like '''|| ContainWildcard  || TextFind_ || '%'') ';
  END IF;
  IF  (TextFindCustomFields = 1) THEN
    IF  (TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(asset_hdr_cfv.field_value) like '''|| ContainWildcard  || TextFind_ || '%'')';
  END IF;
  IF  (TextFindLocation = 1 OR TextFindLocationContract = 1) THEN
    IF  (TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(address.zip_code) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(address.street) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(address.suburb) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(city_location.ext_name) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(county_location.ext_name) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(state_province_location.ext_name) like '''|| ContainWildcard  || TextFind_
				|| '%'' or lower(country_region_location.ext_name) like '''|| ContainWildcard  || TextFind_ || '%'')';
  END IF;
  IF (TextFindFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
  END IF;
-- End Text Find Code

-- Begin Id Find Code
  IdFindFlag := 0;
  IF  (IdFindParty = 1) THEN
    IF  (IdFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IdFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.manufacturer_id = ' || IdFind_ || ' or asset_hdr.owner_party_id = ' || IdFind_ || ' or (asset_hdr.original_contract_id in (select asset.contract_id from asset where asset.contract_id > 0 and asset.drawdown_party_id = ' || IdFind_ || ')))';
  END IF;
  IF  (IdFindOriginalContract = 1) THEN
    IF  (IdFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IdFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.original_contract_id = ' || IdFind_ || ')';
  END IF;
  IF  (IdFindCurrentContract = 1) THEN
    IF  (IdFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      IdFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (asset_hdr.current_contract_id = ' || IdFind_ || ')';
  END IF;
  IF (IdFindFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
  END IF;
-- End Id Find Code

  IF  (ISNULL(ContractIdAssetsOnly, 0) > 0) THEN
    SqlQuery := SqlQuery || ' and ( ' || IdFindTextContractIdAssetOnly_ || ' = (CASE WHEN asset_hdr.current_contract_id <> 0 THEN asset_hdr.current_contract_id ELSE asset_hdr.original_contract_id END))';
  END IF;

  SqlQuery := SqlQuery || ') where rownum <= '||row_limit;

  OPEN RC1 for SqlQuery;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
END AXSP_ASSET_SEARCH;
/
CREATE OR REPLACE PROCEDURE AXSP_AUDIT_FLD_INS(
audit_hdr_id_1   IN NUMBER  DEFAULT NULL,
old_fld_value_2  IN NVARCHAR2  DEFAULT NULL,
new_fld_value_3  IN NVARCHAR2  DEFAULT NULL,
fld_name_4       IN VARCHAR2  DEFAULT NULL,
stamp_5          IN NUMBER  DEFAULT NULL)
AS
--
BEGIN
  INSERT INTO audit_fld (
    audit_fld_id,
    audit_hdr_id,
    old_fld_value,
    new_fld_value,
    fld_name,
    stamp)
  VALUES (
    s_audit_fld.nextval,
    audit_hdr_id_1,
    old_fld_value_2,
    new_fld_value_3,
    fld_name_4,
    stamp_5);
END AXSP_AUDIT_FLD_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_AUDIT_HDR_INS(
tbl_name_1    IN VARCHAR2  DEFAULT NULL,
user_id_2     IN NUMBER  DEFAULT NULL,
audit_dt_3    IN DATE  DEFAULT NULL,
change_type_4 IN NUMBER  DEFAULT NULL,
stamp_5       IN NUMBER  DEFAULT NULL,
trans_id_6    IN NUMBER  DEFAULT NULL,
tbl_id_7    IN NUMBER  DEFAULT NULL,
RC1           IN OUT globalPkg.RCT1) AS
--
BEGIN
  INSERT INTO audit_hdr (
    audit_hdr_id,
    tbl_name,
    user_id,
    audit_dt,
    change_type,
    stamp,
    trans_id,
    tbl_id)
  VALUES (
    s_audit_hdr.nextval,
    tbl_name_1,
    user_id_2,
    audit_dt_3,
    change_type_4,
    stamp_5,
    trans_id_6,
    tbl_id_7);
  OPEN RC1 FOR SELECT s_audit_hdr.currval FROM DUAL;
END AXSP_AUDIT_HDR_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_AUDIT_KEY_INS(
audit_hdr_id_1  IN NUMBER  DEFAULT NULL,
fld_name_2      IN VARCHAR2  DEFAULT NULL,
old_fld_value_3 IN NVARCHAR2  DEFAULT NULL,
new_fld_value_4 IN NVARCHAR2  DEFAULT NULL,
stamp_5         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO audit_key (
    audit_key_id,
    audit_hdr_id,
    fld_name,
    old_fld_value,
    new_fld_value,
    stamp)
  VALUES (
    s_audit_key.nextval,
    audit_hdr_id_1,
    fld_name_2,
    old_fld_value_3,
    new_fld_value_4,
    stamp_5);
END AXSP_AUDIT_KEY_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_AUTO_RELEASE(
  p_from_dt         IN DATE  DEFAULT NULL,
  p_run_dt         IN DATE  DEFAULT NULL,
  p_user_id        IN NUMBER DEFAULT NULL,
  p_bankacc_id     IN NUMBER DEFAULT NULL,
  p_flow_method_id IN NUMBER DEFAULT NULL) AS
--
  StoO_error      INTEGER;
  StoO_errmsg     VARCHAR2(255);
  v_amt_unmatched NUMBER(18,4);
--
  CURSOR UF_cursor1 IS
    SELECT /*+ leading(f) */ f.flow_id
    from custom_flow_hdr cfh, settlement_bank_info sbi, flow_method fm, party_bankacc pb, contract c, flow f
    WHERE f.is_cash = 1
    AND f.is_shadow_copy = 0
    AND f.expected_dt >= p_from_dt
    AND f.expected_dt <= p_run_dt
    AND f.release_dt <= p_run_dt
    AND f.status = 2100 --Pending
    AND f.reversal_status = 4200
    AND f.is_set = 7801
    AND c.contract_id = f.contract_id
    AND c.suspension_state != 22501
    AND c.intercept_state != 40801
    AND c.contract_id >= 0
    AND f.flow_method_id = fm.flow_method_id
    AND fm.flow_method_id != 0
    AND (((f.amt_gross_netted - f.amt_matched_netted) >= 0 AND fm.inflow_flag = 1 AND fm.inflow_auto_release = 1) -- inflow
      OR
      ((f.amt_gross_netted - f.amt_matched_netted) < 0 AND fm.outflow_flag = 1 AND fm.outflow_auto_release = 1) -- outflow
      OR
      ((f.amt_gross_netted - f.amt_matched_netted) < 0 AND fm.inflow_flag = 1 AND fm.inflow_auto_release = 1 AND cfh.purpose = 14220) -- credit note
      OR
      ((f.amt_gross_netted - f.amt_matched_netted) >= 0 AND fm.outflow_flag = 1 AND fm.outflow_auto_release = 1 AND cfh.purpose = 14219) -- debit note
      )
    AND f.settlement_bank_info_id = sbi.settlement_bank_info_id
    AND sbi.status != 6701 --Required
    AND sbi.approval_status IN (6501, 6503) -- status = Approved or None
    AND f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
    AND (f.leg_no = 0 OR cfh.settlement_method = 29700)
    AND f.bank_account_id = (case p_bankacc_id when -1 then f.bank_account_id else p_bankacc_id END)
    AND f.flow_method_id = (case p_flow_method_id when -1 then f.flow_method_id else p_flow_method_id END)
    AND pb.bankacc_id = sbi.party_bankacc_id
    AND f.expected_dt <= (case when (fm.payment_method in (5111, 5112) and fm.bank_flag = 1) then pb.credit_card_expiry_dt
                          else f.expected_dt END)
    AND pb.credit_card_association_id in
      (select distinct credit_card_association_id from flow_method_association fma
       where fma.flow_method_id = fm.flow_method_id union all
       select 0 from dual)
	--loan accounts flows can only be auto-released when invoiced.
	AND (c.product_style != 2011 OR f.invoice_id != 0 OR f.flow_type = 1010)
	--exclude flows of contracts that have outstanding manual notice
			AND not exists (select * from task where task.contract_id = f.contract_id AND task.party_id = sbi.party_id
								 AND task.task_type = 6222 AND task.status not in (6402,6405));

  --
  CURSOR UF_cursor2 IS
    SELECT /*+ leading(f) */ f.flow_id, f.contract_id, f.custom_flow_link_no, f.leg_no, f.calc_dt, ch.timing,
		f.party_account_id, f.custom_flow_hdr_id
    FROM flow f,
         flow_method fm,
         settlement_bank_info sbi,
         contract c,
         party_bankacc pb,
         custom_flow_hdr ch
    WHERE f.can_process = 1
    AND f.is_shadow_copy = 0
    AND f.expected_dt >= p_from_dt
    AND f.expected_dt <= p_run_dt
    AND f.release_dt <= p_run_dt
    AND f.flow_method_id = fm.flow_method_id
    AND f.settlement_bank_info_id = sbi.settlement_bank_info_id
    AND f.status = 2100 --Pending
    AND f.reversal_status = 4200
    AND f.is_set = 7801
    AND f.leg_no = 1
    AND ch.custom_flow_hdr_id = f.custom_flow_hdr_id
    AND ch.settlement_method = 29701
    AND fm.flow_method_id != 0
    AND sbi.status != 6701 --Required
    AND sbi.approval_status in (6501, 6503) -- status = Approved or None
    AND (((f.amt_gross_netted - f.amt_matched_netted) >= 0 AND fm.inflow_flag = 1 AND fm.inflow_auto_release = 1) --inflow
         OR
         ((f.amt_gross_netted - f.amt_matched_netted) < 0 AND fm.outflow_flag = 1 AND fm.outflow_auto_release = 1) --outflow
         OR
         ((f.amt_gross_netted - f.amt_matched_netted) < 0 AND fm.inflow_flag = 1 AND fm.inflow_auto_release = 1 AND ch.purpose = 14220) -- credit note
         OR
         ((f.amt_gross_netted - f.amt_matched_netted) >= 0 AND fm.outflow_flag = 1 AND fm.outflow_auto_release = 1 AND ch.purpose = 14219) -- debit note
         )
    AND f.contract_id = c.contract_id
    AND c.suspension_state != 22501
    AND c.intercept_state != 40801
    AND c.contract_id >= 0
    AND f.bank_account_id = (case p_bankacc_id when -1 then f.bank_account_id else p_bankacc_id END)
    AND f.flow_method_id = (case p_flow_method_id when -1 then f.flow_method_id else p_flow_method_id END)
    AND pb.bankacc_id = sbi.party_bankacc_id
    AND f.expected_dt <= (case when (fm.payment_method in (5111, 5112) and fm.bank_flag = 1) then pb.credit_card_expiry_dt
                          else f.expected_dt END)
    AND pb.credit_card_association_id in
        (select distinct credit_card_association_id from flow_method_association fma
         where fma.flow_method_id = fm.flow_method_id union all
         select 0 from dual)
	--loan accounts flows can only be auto-released when invoiced.
	AND (c.product_style != 2011 OR f.invoice_id != 0 OR f.flow_type = 1010)
	--exclude flows of contracts that have outstanding manual notice task
	AND not exists (select * from task where task.contract_id = f.contract_id AND task.party_id = sbi.party_id
						 AND task.task_type = 6222 AND task.status not in (6402,6405));

BEGIN
  --Auto Release flows with default settlement details
  --Auto-release non-custom flows, primary leg custom flows, custom flows with perfect pay method
  --Exclude flows that have an SBI that uses a credit card method where the attached
  --credit card has expired
  --Exclude flows of contracts that have outstanding manual notice task
  BEGIN
    StoO_error   := 0;
    FOR rec in UF_cursor1 LOOP
      UPDATE flow
      SET flow.status = 2102, --Released
          flow.last_user_id = p_user_id
      where flow_id = rec.flow_id;
    END LOOP;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM, true);
  END;
  --
  --Auto-release pay through custom flows with pay as paid method
  --Exclude flows that have an SBI that uses a credit card method where the attached
  --credit card has expired
  --Exclude flows of contracts that have outstanding manual notice task
	BEGIN
		StoO_error   := 0;
		FOR rec in UF_cursor2 LOOP
			--Check that primary leg is fully allocated
			SELECT sum(amt_gross - amt_matched) INTO v_amt_unmatched
			FROM flow WHERE contract_id = rec.contract_id AND leg_no = 0 AND custom_flow_link_no = rec.custom_flow_link_no
			AND reversal_status = 4200 AND (rec.calc_dt = calc_dt or rec.timing not in (14003,14005,14011,14012));
		      IF (v_amt_unmatched = 0) THEN
					--Third party custom flow cannot be released when there are reversal flows exist that have:
					--Custom Flow Hdr = same as on selected flow
					--Leg = third party leg
					--Allocation Status = Partially OR Not Allocated
					SELECT Nvl(sum(amt_gross - amt_matched),0) INTO v_amt_unmatched
					FROM flow f
					WHERE f.contract_id = rec.contract_id AND f.leg_no = 1 AND f.reversal_status = 4202
						AND (rec.calc_dt = f.calc_dt or rec.timing not in (14003,14005,14011,14012))
						AND f.custom_flow_hdr_id = rec.custom_flow_hdr_id;

					IF (v_amt_unmatched = 0) THEN
						UPDATE flow SET flow.status = 2102, flow.last_user_id = p_user_id WHERE flow_id = rec.flow_id;
		         END IF;
		      END IF;
		END LOOP;
	EXCEPTION
		WHEN NO_DATA_FOUND THEN
			NULL;
		WHEN OTHERS THEN
			StoO_error := SQLCODE;
			StoO_errmsg := SQLERRM;
			raise_application_error(SQLCODE, SQLERRM, true);
	END;
  --
  --Flag first released/settled flow for a settlement bank info that is not locked
  axsp_flow_set_first_sbi(p_from_dt, p_run_dt, -1);
  --
  BEGIN
    --Set locked state to settlement instructions that are loaded and linked to released flows
    --No need to exclude for expired credit cards because the flows should never
    --have reached this state
    StoO_error   := 0;
    UPDATE settlement_bank_info
    SET status = 6703 --Locked
    WHERE status in (6700, 6702) --NotRequired/Loaded
    AND approval_status in (6501, 6503) -- status = Approved or None
    AND settlement_bank_info_id != 0
    AND exists
    (
      SELECT *
      FROM flow
      WHERE flow.settlement_bank_info_id = settlement_bank_info.settlement_bank_info_id
      AND flow.expected_dt >= p_from_dt
      AND flow.expected_dt <= p_run_dt
      AND flow.reversal_status IN (4200, 4202)
      AND flow.status in (2101,2102)
      AND flow.is_shadow_copy = 0
      AND flow.contract_id >= 0
      AND flow.bank_account_id = (case p_bankacc_id when -1 then flow.bank_account_id else p_bankacc_id END)
      AND flow.flow_method_id = (case p_flow_method_id when -1 then flow.flow_method_id else p_flow_method_id END)
    );
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM, true);
  END;
END AXSP_FLOW_AUTO_RELEASE;
/
CREATE OR REPLACE PROCEDURE AXSP_BANK_FLOW_PARTY_ACC_ALLOC(
p_bank_flow_id IN NUMBER  DEFAULT NULL,
p_batch_no     IN NUMBER  DEFAULT NULL) AS
--
  StoO_error                   INTEGER;
  StoO_errmsg                  VARCHAR2(255);
  party_account_id_ranking     NUMBER;
  invoice_no_ranking           NUMBER;
  contract_id_ranking          NUMBER;
  party_name_ranking           NUMBER;
  party_name_soundext_ranking  NUMBER;
  contract_reference_ranking   NUMBER;
  flow_date_exact_ranking      NUMBER;
  flow_date_range_ranking      NUMBER;
BEGIN
  BEGIN
    party_account_id_ranking := 500;
    invoice_no_ranking := 150;
    contract_id_ranking := 140;
    party_name_ranking := 120;
    party_name_soundext_ranking := 80;
    contract_reference_ranking := 80;
    flow_date_exact_ranking := 40;
    flow_date_range_ranking := 30;
    DELETE FROM TP_TEMP_PARTY_ACCOUNT_ALLOCATE;
    DELETE FROM TP_TEMP_BANK_FLOW_PARTY_ACCOUN;
    DELETE FROM TP_TEMP_FLOW_INVOICE;
--Party account
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, pa.party_account_id,
             party_account_id_ranking
      FROM bank_flow bf, party_account pa
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = pa.party_account_id
      AND bf.party_account_id <> 0 AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Invoice Amount, Ccy, Date
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_FLOW_INVOICE
      SELECT  f.invoice_id, f.currency_id, i.invoice_start_dt, i.invoice_end_dt,
              i.party_account_id, SUM(f.amt_gross)
      FROM flow f, invoice i
      WHERE f.invoice_id > 0
      AND f.invoice_id = i.invoice_id
      GROUP BY f.invoice_id, f.currency_id, i.invoice_start_dt,
               i.invoice_end_dt, i.party_account_id;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, fi.party_account_id, invoice_no_ranking
      FROM bank_flow bf, TP_TEMP_FLOW_INVOICE fi
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.amount = fi.amount
      AND bf.currency_id = fi.currency_id
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0
      AND bf.actual_dt >= fi.invoice_start_dt
      AND bf.actual_dt <= fi.invoice_end_dt;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Contract Id
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, f.party_account_id, contract_id_ranking
      FROM bank_flow bf, contract c, flow f
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.contract_id = c.contract_id
      AND bf.contract_id > 0
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0
      AND f.contract_id = c.contract_id;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Party Name
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, pa.party_account_id, party_name_ranking
      FROM bank_flow bf, party p, party_account pa
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.party_name = p.ext_name
      AND (bf.party_name IS NOT NULL AND bf.party_name <> ' ')
      AND pa.party_id = p.party_id
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Party Name Soundex
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT bf.bank_flow_id, pa.party_account_id,
             party_name_soundext_ranking
      FROM bank_flow bf, party p, party_account pa
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND SOUNDEX(bf.party_name) = SOUNDEX(p.ext_name)
      AND (bf.party_name IS NOT NULL AND bf.party_name <> ' ')
      AND pa.party_id = p.party_id
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Contract reference
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, f.party_account_id,
             contract_reference_ranking
      FROM bank_flow bf, contract c, flow f
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.contract_reference = c.reference
      AND bf.party_name IS NOT NULL AND bf.party_name <> ' '
      AND f.contract_id = c.contract_id
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0
      AND c.contract_id >= 0;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Flow Amount, Ccy, Date exact
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, f.party_account_id,
             flow_date_exact_ranking
      FROM bank_flow bf, flow f
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.amount = f.amt_gross
      AND bf.currency_id = f.currency_id
      AND bf.actual_dt = f.expected_dt
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
--Flow Amount, Ccy, Date +- 7 days
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_BANK_FLOW_PARTY_ACCOUN
      SELECT DISTINCT  bf.bank_flow_id, f.party_account_id,
             flow_date_range_ranking
      FROM bank_flow bf, flow f
      WHERE bf.bank_flow_id=
      (
        CASE
        WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
        ELSE bf.bank_flow_id
        END
      )
      AND bf.batch_no=
      (
        CASE
        WHEN p_bank_flow_id = 0 THEN p_batch_no
        ELSE bf.batch_no
        END
      )
      AND bf.party_account_id = 0
      AND bf.amount = f.amt_gross
      AND bf.currency_id = f.currency_id
      AND bf.is_deleted = 0
      AND bf.is_shadow_copy = 0
      AND f.expected_dt >= DATEADD('DD', -7, bf.actual_dt)
      AND f.expected_dt <= DATEADD('DD', 7, bf.actual_dt);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
    BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TEMP_PARTY_ACCOUNT_ALLOCATE
      SELECT DISTINCT  tpap.bank_flow_id, tpap.party_account_id,
      SUM(tpap.ranking)
      FROM TP_TEMP_BANK_FLOW_PARTY_ACCOUN tpap
      GROUP BY tpap.bank_flow_id, tpap.party_account_id;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
    BEGIN
    StoO_error   := 0;
    UPDATE bank_flow
    SET party_account_ranking = NVL((
      SELECT * FROM (
                      SELECT  ranking
                      FROM TP_TEMP_BANK_FLOW_PARTY_ACCOUN tpap
                      WHERE tpap.bank_flow_id = bank_flow_id
                    )
      WHERE ROWNUM <= 1 ), 0)
    WHERE bank_flow_id=
    (
      CASE
      WHEN p_bank_flow_id <> 0 THEN p_bank_flow_id
      ELSE bank_flow_id
      END
    )
    AND batch_no=
    (
      CASE
      WHEN p_bank_flow_id = 0 THEN p_batch_no
      ELSE batch_no
      END
    );
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        NULL;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
    END;
  END;
END AXSP_BANK_FLOW_PARTY_ACC_ALLOC;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTGI_INS(
  p_image_key                    IN NUMBER  DEFAULT NULL,
  p_contract_actg_id             IN NUMBER  DEFAULT NULL,
  p_contract_id                  IN NUMBER  DEFAULT NULL,
  p_image_no                     IN NUMBER  DEFAULT NULL,
  p_seq_no                       IN NUMBER  DEFAULT NULL,
  p_is_secondary                 IN NUMBER  DEFAULT NULL,
  p_ledger_id                    IN NUMBER  DEFAULT NULL,
  p_accrual_actg_flag            IN NUMBER  DEFAULT NULL,
  p_last_accrual_dt              IN DATE    DEFAULT NULL,
  p_balance_sheet_model          IN NUMBER  DEFAULT NULL,
  p_contingent_rentals           IN NUMBER  DEFAULT NULL,
  p_balance_sheet_style          IN NUMBER  DEFAULT NULL,
  p_early_end_dt                 IN DATE    DEFAULT NULL,
  p_depreciation_basis           IN NUMBER  DEFAULT NULL,
  p_earnings_run_out             IN NUMBER  DEFAULT NULL,
  p_is_adjust_negative_depr      IN NUMBER  DEFAULT NULL,
  p_is_defer_accrual_on_term     IN NUMBER  DEFAULT NULL,
  p_asset_sale_treatment         IN NUMBER  DEFAULT NULL,
  p_fmv_adjustment					IN NUMBER  DEFAULT NULL,
  p_is_post_rv_separately        IN NUMBER  DEFAULT NULL,
  p_revaluation_recognitn_style IN NUMBER  DEFAULT NULL,
  p_inertia_rv_paydown_pct       IN NUMBER  DEFAULT NULL,
  p_version_no                   IN NUMBER  DEFAULT NULL,
  p_asset_fx_override				IN NUMBER DEFAULT NULL,
  p_is_post_at_asset_level			IN NUMBER DEFAULT NULL,
  p_is_include_tax_in_receivable	IN NUMBER DEFAULT NULL,
  p_accrual_tax_1                IN NUMBER  DEFAULT NULL,
  p_accrual_tax_2                IN NUMBER  DEFAULT NULL,
  p_accrual_tax_3                IN NUMBER  DEFAULT NULL,
  p_stamp                        IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO contract_actgi
    (image_key,
     contract_actg_id,
     contract_id,
     image_no,
     seq_no,
     is_secondary,
     ledger_id,
     accrual_actg_flag,
     last_accrual_dt,
     balance_sheet_model,
     contingent_rentals,
     balance_sheet_style,
     early_end_dt,
     depreciation_basis,
     earnings_run_out,
     is_adjust_negative_depr,
     is_defer_accrual_on_term,
     asset_sale_treatment,
     fmv_adjustment,
     is_post_rv_separately,
     revaluation_recognition_style,
     inertia_rv_paydown_pct,
	  version_no,
	  asset_fx_override,
	  is_post_at_asset_level,
	  is_include_tax_in_receivable,
	  accrual_tax_1,
	  accrual_tax_2,
	  accrual_tax_3,
     stamp)
   VALUES (
     p_image_key,
     p_contract_actg_id,
     p_contract_id,
     p_image_no,
     p_seq_no,
     p_is_secondary,
     p_ledger_id,
     p_accrual_actg_flag,
     p_last_accrual_dt,
     p_balance_sheet_model,
     p_contingent_rentals,
     p_balance_sheet_style,
     p_early_end_dt,
     p_depreciation_basis,
     p_earnings_run_out,
     p_is_adjust_negative_depr,
     p_is_defer_accrual_on_term,
     p_asset_sale_treatment,
     p_fmv_adjustment,
     p_is_post_rv_separately,
     p_revaluation_recognitn_style,
     p_inertia_rv_paydown_pct,
	  p_version_no,
	  p_asset_fx_override,
	  p_is_post_at_asset_level,
	  p_is_include_tax_in_receivable,
	  p_accrual_tax_1,
	  p_accrual_tax_2,
	  p_accrual_tax_3,
     p_stamp);
END AXSP_CONTRACT_ACTGI_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_INS(
p_contract_id                  IN NUMBER  DEFAULT NULL,
p_image_no                     IN NUMBER  DEFAULT NULL,
p_seq_no                       IN NUMBER  DEFAULT NULL,
p_is_secondary                 IN NUMBER  DEFAULT NULL,
p_ledger_id                    IN NUMBER  DEFAULT NULL,
p_accrual_actg_flag            IN NUMBER  DEFAULT NULL,
p_last_accrual_dt              IN DATE    DEFAULT NULL,
p_balance_sheet_model          IN NUMBER  DEFAULT NULL,
p_contingent_rentals           IN NUMBER  DEFAULT NULL,
p_balance_sheet_style          IN NUMBER  DEFAULT NULL,
p_early_end_dt                 IN DATE    DEFAULT NULL,
p_depreciation_basis           IN NUMBER  DEFAULT NULL,
p_earnings_run_out             IN NUMBER  DEFAULT NULL,
p_is_adjust_negative_depr      IN NUMBER  DEFAULT NULL,
p_is_defer_accrual_on_term     IN NUMBER  DEFAULT NULL,
p_asset_sale_treatment         IN NUMBER  DEFAULT NULL,
p_fmv_adjustment               IN NUMBER  DEFAULT NULL,
p_is_post_rv_separately        IN NUMBER  DEFAULT NULL,
p_revaluation_recognitn_style IN NUMBER  DEFAULT NULL,
p_inertia_rv_paydown_pct       IN NUMBER  DEFAULT NULL,
p_version_no                   IN NUMBER  DEFAULT NULL,
p_asset_fx_override				 IN NUMBER DEFAULT NULL,
p_is_post_at_asset_level		 IN NUMBER DEFAULT NULL,
p_is_include_tax_in_receivable	IN NUMBER DEFAULT NULL,
p_accrual_tax_1                IN NUMBER  DEFAULT NULL,
p_accrual_tax_2                IN NUMBER  DEFAULT NULL,
p_accrual_tax_3                IN NUMBER  DEFAULT NULL,
p_stamp                        IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO contract_actg
    (contract_id,
     image_no,
     seq_no,
	  is_secondary,
     ledger_id,
     accrual_actg_flag,
     last_accrual_dt,
     balance_sheet_model,
     contingent_rentals,
     balance_sheet_style,
     early_end_dt,
     depreciation_basis,
     earnings_run_out,
     is_adjust_negative_depr,
     is_defer_accrual_on_term,
     asset_sale_treatment,
     fmv_adjustment,
     is_post_rv_separately,
     revaluation_recognition_style,
     inertia_rv_paydown_pct,
	  version_no,
	  asset_fx_override,
	  is_post_at_asset_level,
	  is_include_tax_in_receivable,
	  accrual_tax_1,
	  accrual_tax_2,
	  accrual_tax_3,
     stamp)
  VALUES (
     p_contract_id,
     p_image_no,
     p_seq_no,
	  p_is_secondary,
     p_ledger_id,
     p_accrual_actg_flag,
     p_last_accrual_dt,
     p_balance_sheet_model,
     p_contingent_rentals,
     p_balance_sheet_style,
     p_early_end_dt,
     p_depreciation_basis,
     p_earnings_run_out,
     p_is_adjust_negative_depr,
     p_is_defer_accrual_on_term,
     p_asset_sale_treatment,
     p_fmv_adjustment,
     p_is_post_rv_separately,
     p_revaluation_recognitn_style,
     p_inertia_rv_paydown_pct,
	  p_version_no,
	  p_asset_fx_override,
	  p_is_post_at_asset_level,
	  p_is_include_tax_in_receivable,
	  p_accrual_tax_1,
	  p_accrual_tax_2,
	  p_accrual_tax_3,
     p_stamp);
END AXSP_CONTRACT_ACTG_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_UPD(
p_contract_actg_id   IN NUMBER  DEFAULT NULL,
p_contract_id   IN NUMBER  DEFAULT NULL,
p_image_no   IN NUMBER  DEFAULT NULL,
p_seq_no   IN NUMBER  DEFAULT NULL,
p_is_secondary   IN NUMBER  DEFAULT NULL,
p_ledger_id   IN NUMBER  DEFAULT NULL,
p_accrual_actg_flag   IN NUMBER  DEFAULT NULL,
p_last_accrual_dt   IN DATE  DEFAULT NULL,
p_balance_sheet_model   IN NUMBER  DEFAULT NULL,
p_contingent_rentals   IN NUMBER  DEFAULT NULL,
p_balance_sheet_style   IN NUMBER  DEFAULT NULL,
p_early_end_dt   IN DATE  DEFAULT NULL,
p_depreciation_basis   IN NUMBER  DEFAULT NULL,
p_earnings_run_out   IN NUMBER  DEFAULT NULL,
p_is_adjust_negative_depr   IN NUMBER  DEFAULT NULL,
p_is_defer_accrual_on_term  IN NUMBER  DEFAULT NULL,
p_asset_sale_treatment IN NUMBER DEFAULT NULL,
p_fmv_adjustment IN NUMBER DEFAULT NULL,
p_is_post_rv_separately        IN NUMBER  DEFAULT NULL,
p_revaluation_recognitn_style IN NUMBER  DEFAULT NULL,
p_inertia_rv_paydown_pct IN NUMBER DEFAULT NULL,
p_version_no                   IN NUMBER  DEFAULT NULL,
p_asset_fx_override				 IN NUMBER DEFAULT NULL,
p_is_post_at_asset_level		 IN NUMBER DEFAULT NULL,
p_is_include_tax_in_receivable	IN NUMBER DEFAULT NULL,
p_accrual_tax_1                IN NUMBER  DEFAULT NULL,
p_accrual_tax_2                IN NUMBER  DEFAULT NULL,
p_accrual_tax_3                IN NUMBER  DEFAULT NULL,
p_stamp   IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  UPDATE contract_actg
  SET
    contract_id = p_contract_id,
    image_no = p_image_no,
    seq_no = p_seq_no,
	 is_secondary = p_is_secondary,
    ledger_id = p_ledger_id,
    accrual_actg_flag = p_accrual_actg_flag,
    last_accrual_dt = p_last_accrual_dt,
    balance_sheet_model = p_balance_sheet_model,
    contingent_rentals = p_contingent_rentals,
    balance_sheet_style = p_balance_sheet_style,
    early_end_dt = p_early_end_dt,
    depreciation_basis = p_depreciation_basis,
    earnings_run_out = p_earnings_run_out,
    is_adjust_negative_depr = p_is_adjust_negative_depr,
    is_defer_accrual_on_term = p_is_defer_accrual_on_term,
    asset_sale_treatment = p_asset_sale_treatment,
    fmv_adjustment = p_fmv_adjustment,
    is_post_rv_separately = p_is_post_rv_separately,
    revaluation_recognition_style = p_revaluation_recognitn_style,
    inertia_rv_paydown_pct = p_inertia_rv_paydown_pct,
	 version_no = p_version_no,
	 asset_fx_override = p_asset_fx_override,
	 is_post_at_asset_level = p_is_post_at_asset_level,
	 is_include_tax_in_receivable = p_is_include_tax_in_receivable,
	 accrual_tax_1 = p_accrual_tax_1,
	 accrual_tax_2 = p_accrual_tax_2,
	 accrual_tax_3 = p_accrual_tax_3,
    stamp = p_stamp
    WHERE contract_actg_id = p_contract_actg_id;
END AXSP_CONTRACT_ACTG_UPD;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_CF_INS(
  p_contract_actg_cf_id        IN NUMBER  DEFAULT NULL,
  p_seq_no                     IN NUMBER  DEFAULT NULL,
  p_is_secondary                 IN NUMBER  DEFAULT NULL,
  p_ledger_id                     IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_flow_id                       IN NUMBER  DEFAULT NULL,
  p_image_no                      IN NUMBER  DEFAULT NULL,
  p_accrual_start_dt              IN DATE    DEFAULT NULL,
  p_accrual_end_dt                IN DATE    DEFAULT NULL,
  p_last_accrual_dt               IN DATE    DEFAULT NULL,
  p_last_accrual_amt              IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt2             IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np           IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np2          IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue                 IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue2                IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue_rental          IN NUMBER  DEFAULT NULL,
  p_amt_blended_income            IN NUMBER  DEFAULT NULL,
  p_is_financed                   IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id            IN NUMBER  DEFAULT NULL,
  p_account_class                 IN NUMBER  DEFAULT NULL,
  p_amortising_period             IN NUMBER  DEFAULT NULL,
  p_tax_accrual                   IN NUMBER  DEFAULT NULL,
  p_custom_flow_tax_mapping       IN NUMBER  DEFAULT NULL,
  p_balance_sheet_model           IN NUMBER  DEFAULT NULL,
  p_flow_type                     IN NUMBER  DEFAULT NULL,
  p_stamp                         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO contract_actg_cf (
contract_actg_cf_id,
seq_no,
is_secondary,
ledger_id,
contract_id,
flow_id,
image_no,
accrual_start_dt,
accrual_end_dt,
last_accrual_dt,
last_accrual_amt,
last_accrual_amt2,
last_accrual_amt_np,
last_accrual_amt_np2,
amt_to_accrue,
amt_to_accrue2,
amt_to_accrue_rental,
amt_blended_income,
is_financed,
custom_flow_hdr_id,
account_class,
amortising_period,
tax_accrual,
custom_flow_tax_mapping,
balance_sheet_model,
flow_type,
stamp)
  VALUES (
  p_contract_actg_cf_id,
  p_seq_no,
  p_is_secondary,
  p_ledger_id,
  p_contract_id,
  p_flow_id,
  p_image_no,
  p_accrual_start_dt,
  p_accrual_end_dt,
  p_last_accrual_dt,
  p_last_accrual_amt,
  p_last_accrual_amt2,
  p_last_accrual_amt_np,
  p_last_accrual_amt_np2,
  p_amt_to_accrue,
  p_amt_to_accrue2,
  p_amt_to_accrue_rental,
  p_amt_blended_income,
  p_is_financed,
  p_custom_flow_hdr_id,
  p_account_class,
  p_amortising_period,
  p_tax_accrual,
  p_custom_flow_tax_mapping,
  p_balance_sheet_model,
  p_flow_type,
     p_stamp);
END AXSP_CONTRACT_ACTG_CF_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_CF_UPD(
  p_contract_actg_cf_id           IN NUMBER  DEFAULT NULL,
  p_seq_no                        IN NUMBER  DEFAULT NULL,
  p_is_secondary                  IN NUMBER  DEFAULT NULL,
  p_ledger_id                     IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_flow_id                       IN NUMBER  DEFAULT NULL,
  p_image_no                      IN NUMBER  DEFAULT NULL,
  p_accrual_start_dt              IN DATE    DEFAULT NULL,
  p_accrual_end_dt                IN DATE    DEFAULT NULL,
  p_last_accrual_dt               IN DATE    DEFAULT NULL,
  p_last_accrual_amt              IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt2             IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np           IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np2          IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue                 IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue2                IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue_rental          IN NUMBER  DEFAULT NULL,
  p_amt_blended_income            IN NUMBER  DEFAULT NULL,
  p_is_financed                   IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id            IN NUMBER  DEFAULT NULL,
  p_account_class                 IN NUMBER  DEFAULT NULL,
  p_amortising_period             IN NUMBER  DEFAULT NULL,
  p_tax_accrual                   IN NUMBER  DEFAULT NULL,
  p_custom_flow_tax_mapping       IN NUMBER  DEFAULT NULL,
  p_balance_sheet_model           IN NUMBER  DEFAULT NULL,
  p_flow_type                     IN NUMBER  DEFAULT NULL,
  p_stamp                         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  UPDATE contract_actg_cf
  SET seq_no = p_seq_no,
   is_secondary = p_is_secondary,
    ledger_id = p_ledger_id,
    contract_id = p_contract_id,
    flow_id = p_flow_id,
    image_no = p_image_no,
    accrual_start_dt = p_accrual_start_dt,
    accrual_end_dt = p_accrual_end_dt,
    last_accrual_dt = p_last_accrual_dt,
    last_accrual_amt = p_last_accrual_amt,
    last_accrual_amt2 = p_last_accrual_amt2,
    last_accrual_amt_np = p_last_accrual_amt_np,
	 last_accrual_amt_np2 = p_last_accrual_amt_np2,
    amt_to_accrue = p_amt_to_accrue,
    amt_to_accrue2 = p_amt_to_accrue2,
    amt_to_accrue_rental = p_amt_to_accrue_rental,
    amt_blended_income = p_amt_blended_income,
    is_financed = p_is_financed,
    custom_flow_hdr_id = p_custom_flow_hdr_id,
    account_class = p_account_class,
    amortising_period = p_amortising_period,
    tax_accrual = p_tax_accrual,
    custom_flow_tax_mapping = p_custom_flow_tax_mapping,
    balance_sheet_model = p_balance_sheet_model,
    flow_type = p_flow_type,
    stamp = p_stamp
    WHERE contract_actg_cf_id = p_contract_actg_cf_id;
END AXSP_CONTRACT_ACTG_CF_UPD;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_CFI_INS(
  p_image_key					IN NUMBER  DEFAULT NULL,
  p_contract_actg_cf_id         IN NUMBER  DEFAULT NULL,
  p_seq_no                      IN NUMBER  DEFAULT NULL,
  p_is_secondary                IN NUMBER  DEFAULT NULL,
  p_ledger_id                   IN NUMBER  DEFAULT NULL,
  p_contract_id                 IN NUMBER  DEFAULT NULL,
  p_flow_id                     IN NUMBER  DEFAULT NULL,
  p_image_no                    IN NUMBER  DEFAULT NULL,
  p_accrual_start_dt            IN DATE    DEFAULT NULL,
  p_accrual_end_dt              IN DATE    DEFAULT NULL,
  p_last_accrual_dt             IN DATE    DEFAULT NULL,
  p_last_accrual_amt            IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt2           IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np         IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np2        IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue               IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue2              IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue_rental        IN NUMBER  DEFAULT NULL,
  p_amt_blended_income          IN NUMBER  DEFAULT NULL,
  p_is_financed                 IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id          IN NUMBER  DEFAULT NULL,
  p_account_class               IN NUMBER  DEFAULT NULL,
  p_amortising_period           IN NUMBER  DEFAULT NULL,
  p_tax_accrual                 IN NUMBER  DEFAULT NULL,
  p_custom_flow_tax_mapping     IN NUMBER  DEFAULT NULL,
  p_balance_sheet_model         IN NUMBER  DEFAULT NULL,
  p_flow_type                   IN NUMBER  DEFAULT NULL,
  p_stamp                       IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO contract_actg_cfi (
contract_actg_cfi_id,
image_key,
contract_actg_cf_id,
seq_no,
is_secondary,
ledger_id,
contract_id,
flow_id,
image_no,
accrual_start_dt,
accrual_end_dt,
last_accrual_dt,
last_accrual_amt,
last_accrual_amt2,
last_accrual_amt_np,
last_accrual_amt_np2,
amt_to_accrue,
amt_to_accrue2,
amt_to_accrue_rental,
amt_blended_income,
is_financed,
custom_flow_hdr_id,
account_class,
amortising_period,
tax_accrual,
custom_flow_tax_mapping,
balance_sheet_model,
flow_type,
stamp)
  VALUES (
  s_contract_actg_cfi.nextval,
  p_image_key,
  p_contract_actg_cf_id,
  p_seq_no,
  p_is_secondary,
  p_ledger_id,
  p_contract_id,
  p_flow_id,
  p_image_no,
  p_accrual_start_dt,
  p_accrual_end_dt,
  p_last_accrual_dt,
  p_last_accrual_amt,
  p_last_accrual_amt2,
  p_last_accrual_amt_np,
  p_last_accrual_amt_np2,
  p_amt_to_accrue,
  p_amt_to_accrue2,
  p_amt_to_accrue_rental,
  p_amt_blended_income,
  p_is_financed,
  p_custom_flow_hdr_id,
  p_account_class,
  p_amortising_period,
  p_tax_accrual,
  p_custom_flow_tax_mapping,
  p_balance_sheet_model,
  p_flow_type,
  p_stamp);
END AXSP_CONTRACT_ACTG_CFI_INS;
/

CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_CF_TAX_INS(
  p_contract_actg_cf_id       IN NUMBER  DEFAULT NULL,
  p_contract_id               IN NUMBER  DEFAULT NULL,
  p_flow_id                   IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt          IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np       IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue             IN NUMBER  DEFAULT NULL,
  p_tax_type_hdr_id           IN NUMBER  DEFAULT NULL,
  p_stamp                     IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO contract_actg_cf_tax (
contract_actg_cf_tax_id,
contract_actg_cf_id,
contract_id,
flow_id,
last_accrual_amt,
last_accrual_amt_np,
amt_to_accrue,
tax_type_hdr_id,
stamp)
  VALUES (
  s_contract_actg_cf_tax.nextval,
  p_contract_actg_cf_id,
  p_contract_id,
  p_flow_id,
  p_last_accrual_amt,
  p_last_accrual_amt_np,
  p_amt_to_accrue,
  p_tax_type_hdr_id,
     p_stamp);
END AXSP_CONTRACT_ACTG_CF_TAX_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_ACTG_CF_TAX_UPD(
  p_contract_actg_cf_tax_id       IN NUMBER  DEFAULT NULL,
  p_contract_actg_cf_id           IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_flow_id			  IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt              IN NUMBER  DEFAULT NULL,
  p_last_accrual_amt_np           IN NUMBER  DEFAULT NULL,
  p_amt_to_accrue                 IN NUMBER  DEFAULT NULL,
  p_tax_type_hdr_id               IN NUMBER  DEFAULT NULL,
  p_stamp                         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  UPDATE contract_actg_cf_tax
  SET contract_actg_cf_id = p_contract_actg_cf_id,
    contract_id = p_contract_id,
    flow_id = p_flow_id,
    last_accrual_amt = p_last_accrual_amt,
    last_accrual_amt_np = p_last_accrual_amt_np,
    amt_to_accrue = p_amt_to_accrue,
    tax_type_hdr_id = p_tax_type_hdr_id,
    stamp = p_stamp
    WHERE contract_actg_cf_tax_id = p_contract_actg_cf_tax_id;
END AXSP_CONTRACT_ACTG_CF_TAX_UPD;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_ALL_PORTFOLIO_FAC(
RC1   IN OUT globalPkg.RCT1,
RC2   IN OUT globalPkg.RCT1)
AS
BEGIN
      OPEN RC1 FOR
      SELECT p.portfolio_id
      ,  p.code portfolio_code
      ,  l1.value portfolio_type
      ,  f.code facility_code
      ,  f.facility_id
      ,  c.code ccy
      ,  f.limit
      ,  NVL((
                  SELECT  SUM(original_loan_amt)
                   FROM contract
            WHERE contract_id  IN (
                  SELECT  contract_id
                   FROM facility_contract
            WHERE facility_id = f.facility_id  )  ), 0) amt_drawn
       FROM portfolio p, facility f, party b, lookupset l1, currency c
            WHERE p.business_unit_id = b.party_id
             and f.business_unit_id = b.party_id
             and l1.lookupset_id = p.portfolio_type
             and f.currency_id = c.currency_id
             and p.portfolio_id > 0;
--
      OPEN RC2 FOR
        SELECT facility_id, name funder
        FROM facility_funder, party
        WHERE funder_id = party_id
        and facility_funder_id > 0;
END AXSP_DSKTP_ALL_PORTFOLIO_FAC;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_ASSET_BY_NAME(
p_yr   IN NUMBER  DEFAULT NULL,
RC1    IN OUT globalPkg.RCT1) AS
--
  StoO_error   INTEGER;
  StoO_errmsg  VARCHAR2(255);
BEGIN
  DELETE FROM TP_ASSET_TABLE;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE
    SELECT  asset_type.name, contract.currency_id, SUM(asset.cost)
     FROM asset, asset_hdr, contract, asset_type
        WHERE contract.product_style  IN (2000, 2001, 2003)
         and asset.contract_id = contract.contract_id
         and asset_hdr.asset_hdr_id = asset.asset_hdr_id
         and asset_hdr.asset_type_id = asset_type.asset_type_id
         and asset.contract_id = contract.contract_id
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
         and contract.is_active = 1
         and contract.save_status != 2200
         and contract.contract_id > 0
        GROUP BY asset_type.name, contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE
    SELECT  'No Asset', contract.currency_id, SUM(contract.original_loan_amt)
     FROM contract
        WHERE contract.product_style  IN (2002, 2005)
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
         and contract.is_active = 1
         and contract.save_status != 2200
         and contract.contract_id > 0
        GROUP BY contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  OPEN RC1 FOR
  SELECT a.name, a.currency_id, a.cost, axsp_get_datetime()
  FROM TP_ASSET_TABLE a;
END AXSP_DSKTP_ASSET_BY_NAME;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_ASSET_COST(
p_yr       IN NUMBER  DEFAULT NULL,
p_month    IN NUMBER  DEFAULT NULL,
RC1        IN OUT globalPkg.RCT1) AS
--
  StoO_error   INTEGER;
  StoO_errmsg  VARCHAR2(255);
BEGIN
  DELETE FROM TP_ASSET_TABLE_1;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE_1
    SELECT  contract.currency_id, SUM(asset.cost)
     FROM asset, contract
        WHERE contract.product_style  IN (2000, 2001, 2003)
         and asset.contract_id = contract.contract_id
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'MM')) = p_month
         and contract.is_active = 1
         and contract.save_status != 2200
         and contract.contract_id > 0
        GROUP BY contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE_1
    SELECT  contract.currency_id, SUM(contract.original_loan_amt)
     FROM contract
        WHERE contract.product_style  IN (2002, 2005)
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
         and TO_NUMBER(TO_CHAR( contract.input_dt, 'MM')) = p_month
         and contract.is_active = 1
         and contract.save_status != 2200
         and contract.contract_id > 0
        GROUP BY contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  OPEN RC1 FOR
    SELECT 'asset', a.currency_id, SUM(a.cost), axsp_get_datetime()
    FROM TP_ASSET_TABLE_1 a
    GROUP BY a.currency_id;
END AXSP_DSKTP_ASSET_COST;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_ASSET_COST_MTH_AVG(
p_yr   IN NUMBER  DEFAULT NULL,
RC1    IN OUT globalPkg.RCT1) AS
--
  StoO_error   INTEGER;
  StoO_errmsg  VARCHAR2(255);
BEGIN
  DELETE FROM TP_ASSET_TABLE_2;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE_2
    SELECT TO_NUMBER(TO_CHAR( contract.input_dt, 'MM')), contract.currency_id,
           SUM(asset.cost)
    FROM asset, contract
    WHERE contract.product_style  IN (2000, 2001, 2003)
    and asset.contract_id = contract.contract_id
    and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
    and contract.is_active = 1
    and contract.save_status != 2200
    and contract.contract_id > 0
    GROUP BY TO_NUMBER(TO_CHAR( contract.input_dt, 'MM')),
             contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_ASSET_TABLE_2
    SELECT TO_NUMBER(TO_CHAR( contract.input_dt, 'MM')), contract.currency_id,
           SUM(contract.original_loan_amt)
    FROM contract
    WHERE contract.product_style  IN (2002, 2005)
    and TO_NUMBER(TO_CHAR( contract.input_dt, 'YYYY')) = p_yr
    and contract.is_active = 1
    and contract.save_status != 2200
    and contract.contract_id > 0
    GROUP BY TO_NUMBER(TO_CHAR(contract.input_dt, 'MM')), contract.currency_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  OPEN RC1 FOR
    SELECT CAST(a.month_ AS NVARCHAR2(30)), a.currency_id, SUM(a.cost), axsp_get_datetime()
    FROM TP_ASSET_TABLE_2 a
    GROUP BY a.month_, a.currency_id;
END AXSP_DSKTP_ASSET_COST_MTH_AVG;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_OVERDUES_SUM
  (ccyId IN NUMBER  DEFAULT NULL,
   RC1   IN OUT globalPkg.RCT1,
   RC2   IN OUT globalPkg.RCT1) AS
--
  ccyId_ NUMBER(10,0) := ccyId;
  dt     DATE := axsp_dateonly(axsp_get_datetime());
  FxRate NUMBER := 0;
--
BEGIN
  FOR rec in (select system_setting_value
  from system_setting where system_setting_type = 8402
  and rownum = 1) LOOP
  FxRate := to_number(rec.system_setting_value);
  END LOOP;
--
  if ccyId_ = 0 then
    FOR rec in (select currency_id
    from currency where is_base = 1 and rownum = 1) LOOP
    ccyId_ := rec.currency_id;
    END LOOP;
  end if;
--
  OPEN RC1 FOR
    select nvl(sum(overdue_amt1*r.rate),0), nvl(sum(overdue_amt2*r.rate),0),
	   nvl(sum(overdue_amt3*r.rate),0), nvl(sum(overdue_amt4*r.rate),0),
     nvl(sum(overdue_amt5*r.rate),0), nvl(sum(overdue_amt6*r.rate),0)
    from axvw_overdue_view a, table(axsp_get_cross_rates(FXRate, dt)) r
    where overdue_amt0 > 0 and
          a.currency_id = r.currency_id1 and r.currency_id2 = ccyId_;
  OPEN RC2 FOR
    select distinct r.code1 from table(axsp_get_cross_rates(FXRate, dt)) r
    where rate = 0 and r.currency_id1 in
    (select distinct currency_id from axvw_overdue_view where overdue_amt0 > 0)
    and r.currency_id2 = ccyId_;
END AXSP_DSKTP_OVERDUES_SUM;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_OVERDUES_SUM_BIZ
(ccyId            IN NUMBER  DEFAULT NULL,
 bizUnitId IN NUMBER  DEFAULT NULL,
 RC1        IN OUT globalPkg.RCT1,
 RC2        IN OUT globalPkg.RCT1) AS

ccyId_         NUMBER(10,0) := ccyId;
dt             DATE := axsp_dateonly(axsp_get_datetime());
FXRate         NUMBER := 0;
--
BEGIN
  FOR rec in (select system_setting_value
  from system_setting where system_setting_type = 8402
  and rownum = 1) LOOP
  FxRate := to_number(rec.system_setting_value);
  END LOOP;
--
   if ccyId_ = 0 then
    FOR rec in (select currency_id
    from currency where is_base = 1 and rownum = 1) LOOP
    ccyId_ := rec.currency_id;
    END LOOP;
  end if;
--
  OPEN RC1 for
    select nvl(sum(overdue_amt1*r.rate),0), nvl(sum(overdue_amt2*r.rate),0),
           nvl(sum(overdue_amt3*r.rate),0), nvl(sum(overdue_amt4*r.rate),0),
           nvl(sum(overdue_amt5*r.rate),0), nvl(sum(overdue_amt6*r.rate),0)
    from axvw_overdue_view a, table(axsp_get_cross_rates(FXRate, dt)) r
    where business_unit_id = bizUnitId and overdue_amt0 > 0 and
          a.currency_id = r.currency_id1 and r.currency_id2 = ccyId_;
--
  OPEN RC2 FOR
    select distinct r.code1 from table(axsp_get_cross_rates(FXRate, dt)) r
    where rate = 0 and r.currency_id1 in
    (select distinct currency_id from axvw_overdue_view where overdue_amt0 > 0)
    and r.currency_id2 = ccyId_;
END AXSP_DSKTP_OVERDUES_SUM_BIZ;
/
CREATE OR REPLACE PROCEDURE AXSP_DSKTP_SYSTEM_PROCESSES
(	systemUserId IN NUMBER,
	intitiatedByUserId IN NUMBER,
	noHours IN NUMBER,
	RC1 IN OUT globalPkg.RCT1) AS
BEGIN
	OPEN RC1 FOR
  SELECT l.value subject,
  CASE WHEN t.process_result = 25600 THEN l2.value ELSE l1.value END status,
  t.input_dt FROM task t
		INNER JOIN lookupset l ON l.lookupset_id = t.process_category
		INNER JOIN lookupset l1 ON l1.lookupset_id = t.process_result
		INNER JOIN lookupset l2 ON l2.lookupset_id = t.status
	WHERE owner_user_id = systemUserId
	AND initiated_by_user_id = intitiatedByUserId
	AND datediff('hour', input_dt, axsp_get_datetime()) <= noHours
	ORDER BY input_dt DESC;
END AXSP_DSKTP_SYSTEM_PROCESSES;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOWI_INS(
  p_image_key                     IN NUMBER  DEFAULT NULL,
  p_flow_id                       IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_image_no                      IN NUMBER  DEFAULT NULL,
  p_input_dt                      IN DATE  DEFAULT NULL,
  p_calc_dt                       IN DATE  DEFAULT NULL,
  p_actual_dt                     IN DATE  DEFAULT NULL,
  p_expected_dt                   IN DATE  DEFAULT NULL,
  p_currency_id                   IN NUMBER  DEFAULT NULL,
  p_amount                        IN NUMBER  DEFAULT NULL,
  p_flow_type                     IN NUMBER  DEFAULT NULL,
  p_is_cash                       IN NUMBER  DEFAULT NULL,
  p_status                        IN NUMBER  DEFAULT NULL,
  p_owner_id                      IN NUMBER  DEFAULT NULL,
  p_beneficiary_id                IN NUMBER  DEFAULT NULL,
  p_nett_no                       IN NUMBER  DEFAULT NULL,
  p_bank_account_id               IN NUMBER  DEFAULT NULL,
  p_is_set                        IN NUMBER  DEFAULT NULL,
  p_amt_principal                 IN NUMBER  DEFAULT NULL,
  p_amt_interest                  IN NUMBER  DEFAULT NULL,
  p_installment_no                IN NUMBER  DEFAULT NULL,
  p_flow_method_id                IN NUMBER  DEFAULT NULL,
  p_payee_ref                     IN NVARCHAR2  DEFAULT NULL,
  p_payment_ref                   IN NVARCHAR2  DEFAULT NULL,
  p_party_account_id              IN NUMBER  DEFAULT NULL,
  p_amt_gross                     IN NUMBER  DEFAULT NULL,
  p_reversal_status               IN NUMBER  DEFAULT NULL,
  p_flow_link_id                  IN NUMBER  DEFAULT NULL,
  p_amt_matched                   IN NUMBER  DEFAULT NULL,
  p_rate                          IN NUMBER  DEFAULT NULL,
  p_invoice_id                    IN NUMBER  DEFAULT NULL,
  p_amt_contingent_rental         IN NUMBER  DEFAULT NULL,
  p_bank_interface_run_id         IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id            IN NUMBER  DEFAULT NULL,
  p_rejected_reason               IN NUMBER  DEFAULT NULL,
  p_settlement_bank_info_id       IN NUMBER  DEFAULT NULL,
  p_amt_gross_netted              IN NUMBER  DEFAULT NULL,
  p_collection_state              IN NUMBER  DEFAULT NULL,
  p_late_fee_upto                 IN NUMBER  DEFAULT NULL,
  p_is_overdue_interest_pending   IN NUMBER  DEFAULT NULL,
  p_last_overdue_interest_dt      IN DATE    DEFAULT NULL,
  p_statement_id                  IN NUMBER  DEFAULT NULL,
  p_amt_rental                    IN NUMBER  DEFAULT NULL,
  p_release_dt                    IN DATE    DEFAULT NULL,
  p_settled_dt                    IN DATE    DEFAULT NULL,
  p_rejected_dt                   IN DATE    DEFAULT NULL,
  p_amt_matched_principal         IN NUMBER  DEFAULT NULL,
  p_penalty_grace_days            IN NUMBER  DEFAULT NULL,
  p_amt_matched_interest          IN NUMBER  DEFAULT NULL,
  p_amt_matched_tax               IN NUMBER  DEFAULT NULL,
  p_exclude_from_account_bal      IN NUMBER  DEFAULT NULL,
  p_settle_count                  IN NUMBER  DEFAULT NULL,
  p_exclude_from_late_fees        IN NUMBER  DEFAULT NULL,
  p_exclude_from_overdue_int      IN NUMBER  DEFAULT NULL,
  p_split_no                      IN NUMBER  DEFAULT NULL,
  p_last_user_id                  IN NUMBER  DEFAULT NULL,
  p_in_recovery                   IN NUMBER  DEFAULT NULL,
  p_split_flow_variation_id       IN NUMBER  DEFAULT NULL,
  p_gross_rec_amt                 IN NUMBER  DEFAULT NULL,
  p_is_shadow_copy                IN NUMBER  DEFAULT NULL,
  p_grp_link_id                   IN NUMBER  DEFAULT NULL,
  p_reserve_id                    IN NUMBER  DEFAULT NULL,
  p_reserve_statement_id          IN NUMBER  DEFAULT NULL,
  p_gl_account_id                 IN NUMBER  DEFAULT NULL,
  p_purchase_invoice_id           IN NUMBER  DEFAULT NULL,
  p_amt_invoice                   IN NUMBER  DEFAULT NULL,
  p_can_process                   IN NUMBER  DEFAULT NULL,
  p_custom_flow_link_no				 IN NUMBER  DEFAULT NULL,
  p_leg_no						       IN NUMBER  DEFAULT NULL,
  p_asset_hdr_id				       IN NUMBER  DEFAULT NULL,
  p_tax_point_dt                  IN DATE    DEFAULT NULL,
  p_is_first_settle_bank_info     IN NUMBER  DEFAULT NULL,
  p_termination_quote_id			 IN NUMBER  DEFAULT NULL,
  p_payment_confirmation_status	 IN NUMBER  DEFAULT NULL,
  p_payment_confirmation_dt		 IN DATE    DEFAULT NULL,
  p_payment_confirmation_user_id  IN NUMBER  DEFAULT NULL,
  p_payment_credited_dt				 IN DATE    DEFAULT NULL,
  p_amt_matched_netted				 IN NUMBER  DEFAULT NULL,
  p_pp_nett_no							 IN NUMBER  DEFAULT NULL,
  p_is_funding_transfer				 IN NUMBER  DEFAULT NULL,
  p_stamp                         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO flowi (
    image_key,
    flow_id,
    contract_id,
    image_no,
    input_dt,
    calc_dt,
    actual_dt,
    expected_dt,
    currency_id,
    amount,
    flow_type,
    is_cash,
    status,
    owner_id,
    beneficiary_id,
    nett_no,
    bank_account_id,
    is_set,
    amt_principal,
    amt_interest,
    installment_no,
    flow_method_id,
    payee_ref,
    payment_ref,
    party_account_id,
    amt_gross,
    reversal_status,
    flow_link_id,
    amt_matched,
    rate,
    invoice_id,
    amt_contingent_rental,
    bank_interface_run_id,
    custom_flow_hdr_id,
    rejected_reason,
    settlement_bank_info_id,
    amt_gross_netted,
    collection_state,
    late_fee_upto,
    is_overdue_interest_pending,
    last_overdue_interest_dt,
    statement_id,
    amt_rental,
    release_dt,
    settled_dt,
    rejected_dt,
    amt_matched_principal,
    penalty_grace_days,
    amt_matched_interest,
    amt_matched_tax,
    exclude_from_account_bal,
    settle_count,
    exclude_from_late_fees,
    exclude_from_overdue_interest,
    split_no,
    last_user_id,
    in_recovery,
    split_flow_variation_id,
    gross_rec_amt,
    is_shadow_copy,
    grp_link_id,
    reserve_id,
    reserve_statement_id,
    gl_account_id,
    purchase_invoice_id,
    amt_invoice,
    can_process,
    custom_flow_link_no,
    leg_no,
    asset_hdr_id,
    tax_point_dt,
    is_first_settlement_bank_info,
    termination_quote_id,
    payment_confirmation_status,
	 payment_confirmation_dt,
	 payment_confirmation_user_id,
	 payment_credited_dt,
    amt_matched_netted,
    pp_nett_no,
	 is_funding_transfer,
    stamp)
  VALUES (
    p_image_key,
    p_flow_id,
    p_contract_id,
    p_image_no,
    p_input_dt,
    p_calc_dt,
    p_actual_dt,
    p_expected_dt,
    p_currency_id,
    p_amount,
    p_flow_type,
    p_is_cash,
    p_status,
    p_owner_id,
    p_beneficiary_id,
    p_nett_no,
    p_bank_account_id,
    p_is_set,
    p_amt_principal,
    p_amt_interest,
    p_installment_no,
    p_flow_method_id,
    p_payee_ref,
    p_payment_ref,
    p_party_account_id,
    p_amt_gross,
    p_reversal_status,
    p_flow_link_id,
    p_amt_matched,
    p_rate,
    p_invoice_id,
    p_amt_contingent_rental,
    p_bank_interface_run_id,
    p_custom_flow_hdr_id,
    p_rejected_reason,
    p_settlement_bank_info_id,
    p_amt_gross_netted,
    p_collection_state,
    p_late_fee_upto,
    p_is_overdue_interest_pending,
    p_last_overdue_interest_dt,
    p_statement_id,
    p_amt_rental,
    p_release_dt,
    p_settled_dt,
    p_rejected_dt,
    p_amt_matched_principal,
    p_penalty_grace_days,
    p_amt_matched_interest,
    p_amt_matched_tax,
    p_exclude_from_account_bal,
    p_settle_count,
    p_exclude_from_late_fees,
    p_exclude_from_overdue_int,
    p_split_no,
    p_last_user_id,
    p_in_recovery,
    p_split_flow_variation_id,
    p_gross_rec_amt,
    p_is_shadow_copy,
    p_grp_link_id,
    p_reserve_id,
    p_reserve_statement_id,
    p_gl_account_id,
    p_purchase_invoice_id,
    p_amt_invoice,
    p_can_process,
    p_custom_flow_link_no,
    p_leg_no,
    p_asset_hdr_id,
    p_tax_point_dt,
    p_is_first_settle_bank_info,
    p_termination_quote_id,
    p_payment_confirmation_status,
	 p_payment_confirmation_dt,
	 p_payment_confirmation_user_id,
	 p_payment_credited_dt,
    p_amt_matched_netted,
    p_pp_nett_no,
	 p_is_funding_transfer,
    p_stamp);
END AXSP_FLOWI_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_DEL(
  flow_id_1   IN NUMBER  DEFAULT NULL,
  is_pr_link_2 IN NUMBER DEFAULT 0) AS
--
  StoO_error     INTEGER;
  StoO_errmsg    VARCHAR2(255);
  e_fk_violation EXCEPTION;
  PRAGMA EXCEPTION_INIT(e_fk_violation, -2292);
BEGIN
  StoO_error   := 0;

  IF (is_pr_link_2 = 1) THEN
    DELETE FROM vendor_receipt_match WHERE flow_id = flow_id_1;
  END IF;

  UPDATE bank_flow_match SET flow_id = 0, recovery_flow_id = 0 WHERE flow_id = flow_id_1 OR recovery_flow_id = flow_id_1;
  UPDATE bank_flow SET source_flow_id = 0 WHERE source_flow_id = flow_id_1;
  DELETE tax_flow WHERE source_flow_id = flow_id_1;
  DELETE tax_flow WHERE flow_id = flow_id_1;
  DELETE tax_calc_flow WHERE flow_id = flow_id_1;
  DELETE flow WHERE flow_id = flow_id_1;
  EXCEPTION
    WHEN e_fk_violation THEN
      BEGIN
		  IF (is_pr_link_2 = 1) THEN
			 DELETE FROM vendor_receipt_match WHERE flow_id = flow_id_1;
		  END IF;

		UPDATE bank_flow_match SET flow_id = 0, recovery_flow_id = 0 WHERE flow_id = flow_id_1;
		UPDATE bank_flow SET source_flow_id = 0 WHERE source_flow_id = flow_id_1;
		DELETE tax_flow WHERE flow_id = flow_id_1;
      DELETE tax_calc_flow WHERE flow_id = flow_id_1;
      DELETE flow WHERE flow_id = flow_id_1;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          NULL;
        WHEN OTHERS THEN
          StoO_error := SQLCODE;
          StoO_errmsg := SQLERRM;
          raise_application_error(SQLCODE, SQLERRM,true);
      END;
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
END AXSP_FLOW_DEL;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_CALC_DEL(
p_flow_calc_id   IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  DELETE  flow_calc
  WHERE flow_calc_id = p_flow_calc_id;
END AXSP_FLOW_CALC_DEL;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_CALC_INS(
	p_flow_calc_id      IN NUMBER  DEFAULT NULL,
	p_contract_id      IN NUMBER  DEFAULT NULL,
	p_asset_id          IN NUMBER  DEFAULT NULL,
	p_asset_hdr_id          IN NUMBER  DEFAULT NULL,
	p_image_no         IN NUMBER  DEFAULT NULL,
	p_custom_flow_hdr_id  IN NUMBER  DEFAULT NULL,
	p_flow_schedule_type IN NUMBER  DEFAULT NULL,
	p_flow_type IN NUMBER  DEFAULT NULL,
	p_calc_dt                       IN DATE    DEFAULT NULL,
	p_amount           IN NUMBER  DEFAULT NULL,
	p_amt_principal 	   IN NUMBER  DEFAULT NULL,
	p_amt_interest 	   IN NUMBER  DEFAULT NULL,
	p_amt_contingent 	   IN NUMBER  DEFAULT NULL,
	p_amt_cof 	   IN NUMBER  DEFAULT NULL,
	p_amt_margin 	   IN NUMBER  DEFAULT NULL,
	p_amt_balance	   IN NUMBER  DEFAULT NULL,
	p_amt_balance2	   IN NUMBER  DEFAULT NULL,
	p_amt_commission	   IN NUMBER  DEFAULT NULL,
	p_amt_grant	   IN NUMBER  DEFAULT NULL,
	p_amt_subsidy	   IN NUMBER  DEFAULT NULL,
	p_rate				IN NUMBER  DEFAULT NULL,
	p_amt_rate_adjustment IN NUMBER  DEFAULT NULL,
	p_amt_nco_funding	IN NUMBER  DEFAULT NULL,
	p_amt_ape_funding	IN NUMBER  DEFAULT NULL,
	p_amt_interest_only	IN NUMBER  DEFAULT NULL,
	p_stamp            IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO flow_calc (
  flow_calc_id,
  contract_id,
  asset_id,
  asset_hdr_id,
  image_no,
  custom_flow_hdr_id,
  flow_schedule_type,
  flow_type,
  calc_dt,
  amount,
  amt_principal,
  amt_interest,
  amt_contingent,
  amt_cof,
  amt_margin,
  amt_balance,
  amt_balance2,
  amt_commission,
  amt_grant,
  amt_subsidy,
  rate,
  amt_rate_adjustment,
  amt_nco_funding,
  amt_ape_funding,
  amt_interest_only,
  stamp)
  VALUES (
  p_flow_calc_id,
  p_contract_id,
  p_asset_id,
  p_asset_hdr_id,
  p_image_no,
  p_custom_flow_hdr_id,
  p_flow_schedule_type,
  p_flow_type,
  p_calc_dt,
  p_amount,
  p_amt_principal,
  p_amt_interest,
  p_amt_contingent,
  p_amt_cof,
  p_amt_margin,
  p_amt_balance,
  p_amt_balance2,
  p_amt_commission,
  p_amt_grant,
  p_amt_subsidy,
  p_rate,
  p_amt_rate_adjustment,
  p_amt_nco_funding,
  p_amt_ape_funding,
  p_amt_interest_only,
  p_stamp);
END AXSP_FLOW_CALC_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_CALCI_INS(
	p_flow_calci_id      IN NUMBER  DEFAULT NULL,
	p_image_key      IN NUMBER  DEFAULT NULL,
	p_flow_calc_id      IN NUMBER  DEFAULT NULL,
	p_contract_id      IN NUMBER  DEFAULT NULL,
	p_asset_id          IN NUMBER  DEFAULT NULL,
	p_asset_hdr_id          IN NUMBER  DEFAULT NULL,
	p_image_no         IN NUMBER  DEFAULT NULL,
	p_custom_flow_hdr_id  IN NUMBER  DEFAULT NULL,
	p_flow_schedule_type IN NUMBER  DEFAULT NULL,
	p_flow_type IN NUMBER  DEFAULT NULL,
	p_calc_dt          IN DATE    DEFAULT NULL,
	p_amount           IN NUMBER  DEFAULT NULL,
	p_amt_principal 	   IN NUMBER  DEFAULT NULL,
	p_amt_interest 	   IN NUMBER  DEFAULT NULL,
	p_amt_contingent 	   IN NUMBER  DEFAULT NULL,
	p_amt_cof 	   IN NUMBER  DEFAULT NULL,
	p_amt_margin 	   IN NUMBER  DEFAULT NULL,
	p_amt_balance	   IN NUMBER  DEFAULT NULL,
	p_amt_balance2	   IN NUMBER  DEFAULT NULL,
	p_amt_commission	   IN NUMBER  DEFAULT NULL,
	p_amt_grant	   IN NUMBER  DEFAULT NULL,
	p_amt_subsidy	   IN NUMBER  DEFAULT NULL,
	p_rate				IN NUMBER  DEFAULT NULL,
	p_amt_rate_adjustment IN NUMBER  DEFAULT NULL,
	p_amt_nco_funding	IN NUMBER  DEFAULT NULL,
	p_amt_ape_funding	IN NUMBER  DEFAULT NULL,
	p_amt_interest_only	IN NUMBER  DEFAULT NULL,
	p_stamp            IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO flow_calci (
  flow_calci_id,
  image_key,
  flow_calc_id,
  contract_id,
  asset_id,
  asset_hdr_id,
  image_no,
  custom_flow_hdr_id,
  flow_schedule_type,
  flow_type,
  calc_dt,
  amount,
  amt_principal,
  amt_interest,
  amt_contingent,
  amt_cof,
  amt_margin,
  amt_balance,
  amt_balance2,
  amt_commission,
  amt_grant,
  amt_subsidy,
  rate,
  amt_rate_adjustment,
  amt_nco_funding,
  amt_ape_funding,
  amt_interest_only,
  stamp)
  VALUES (
  p_flow_calci_id,
  p_image_key,
  p_flow_calc_id,
  p_contract_id,
  p_asset_id,
  p_asset_hdr_id,
  p_image_no,
  p_custom_flow_hdr_id,
  p_flow_schedule_type,
  p_flow_type,
  p_calc_dt,
  p_amount,
  p_amt_principal,
  p_amt_interest,
  p_amt_contingent,
  p_amt_cof,
  p_amt_margin,
  p_amt_balance,
  p_amt_balance2,
  p_amt_commission,
  p_amt_grant,
  p_amt_subsidy,
  p_rate,
  p_amt_rate_adjustment,
  p_amt_nco_funding,
  p_amt_ape_funding,
  p_amt_interest_only,
  p_stamp);
END AXSP_FLOW_CALCI_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_CALC_UPD(
	p_flow_calc_id		IN NUMBER  DEFAULT NULL,
	p_contract_id		IN NUMBER  DEFAULT NULL,
	p_asset_id			IN NUMBER  DEFAULT NULL,
	p_asset_hdr_id		IN NUMBER  DEFAULT NULL,
	p_image_no			IN NUMBER  DEFAULT NULL,
	p_custom_flow_hdr_id IN NUMBER  DEFAULT NULL,
	p_flow_schedule_type IN NUMBER  DEFAULT NULL,
	p_flow_type			IN NUMBER  DEFAULT NULL,
	p_calc_dt			IN DATE    DEFAULT NULL,
	p_amount				IN NUMBER  DEFAULT NULL,
	p_amt_principal 	IN NUMBER  DEFAULT NULL,
	p_amt_interest 	IN NUMBER  DEFAULT NULL,
	p_amt_contingent 	IN NUMBER  DEFAULT NULL,
	p_amt_cof			IN NUMBER  DEFAULT NULL,
	p_amt_margin 	   IN NUMBER  DEFAULT NULL,
	p_amt_balance	   IN NUMBER  DEFAULT NULL,
	p_amt_balance2	   IN NUMBER  DEFAULT NULL,
	p_amt_commission	IN NUMBER  DEFAULT NULL,
	p_amt_grant			IN NUMBER  DEFAULT NULL,
	p_amt_subsidy	   IN NUMBER  DEFAULT NULL,
	p_rate				IN NUMBER  DEFAULT NULL,
	p_amt_rate_adjustment IN NUMBER  DEFAULT NULL,
	p_amt_nco_funding	IN NUMBER  DEFAULT NULL,
	p_amt_ape_funding	IN NUMBER  DEFAULT NULL,
	p_amt_interest_only	IN NUMBER  DEFAULT NULL,
	p_stamp				IN NUMBER  DEFAULT NULL) AS
--
BEGIN
	UPDATE flow_calc
	SET contract_id = p_contract_id,
	asset_id = p_asset_id,
	asset_hdr_id = p_asset_hdr_id,
   image_no = p_image_no,
   custom_flow_hdr_id = p_custom_flow_hdr_id,
   flow_schedule_type = p_flow_schedule_type,
   flow_type = p_flow_type,
   calc_dt = p_calc_dt,
   amount = p_amount,
   amt_principal = p_amt_principal,
   amt_interest = p_amt_interest,
   amt_contingent = p_amt_contingent,
   amt_cof = p_amt_cof,
   amt_margin = p_amt_margin,
   amt_balance = p_amt_balance,
   amt_balance2 = p_amt_balance2,
   amt_commission = p_amt_commission,
   amt_grant = p_amt_grant,
   amt_subsidy = p_amt_subsidy,
   rate = p_rate,
   amt_rate_adjustment = p_amt_rate_adjustment,
	amt_nco_funding = p_amt_nco_funding,
	amt_ape_funding = p_amt_ape_funding,
	amt_interest_only = p_amt_interest_only,
   stamp = p_stamp
   WHERE flow_calc_id = p_flow_calc_id;
END AXSP_FLOW_CALC_UPD;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_INS(
  p_flow_id                       IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_image_no                      IN NUMBER  DEFAULT NULL,
  p_input_dt                      IN DATE    DEFAULT NULL,
  p_calc_dt                       IN DATE    DEFAULT NULL,
  p_actual_dt                     IN DATE    DEFAULT NULL,
  p_expected_dt                   IN DATE    DEFAULT NULL,
  p_currency_id                   IN NUMBER  DEFAULT NULL,
  p_amount                        IN NUMBER  DEFAULT NULL,
  p_flow_type                     IN NUMBER  DEFAULT NULL,
  p_is_cash                       IN NUMBER  DEFAULT NULL,
  p_status                        IN NUMBER  DEFAULT NULL,
  p_owner_id                      IN NUMBER  DEFAULT NULL,
  p_beneficiary_id                IN NUMBER  DEFAULT NULL,
  p_nett_no                       IN NUMBER  DEFAULT NULL,
  p_bank_account_id               IN NUMBER  DEFAULT NULL,
  p_is_set                        IN NUMBER  DEFAULT NULL,
  p_amt_principal                 IN NUMBER  DEFAULT NULL,
  p_amt_interest                  IN NUMBER  DEFAULT NULL,
  p_installment_no                IN NUMBER  DEFAULT NULL,
  p_flow_method_id                IN NUMBER  DEFAULT NULL,
  p_payee_ref                     IN NVARCHAR2  DEFAULT NULL,
  p_payment_ref                   IN NVARCHAR2  DEFAULT NULL,
  p_party_account_id              IN NUMBER  DEFAULT NULL,
  p_amt_gross                     IN NUMBER  DEFAULT NULL,
  p_reversal_status               IN NUMBER  DEFAULT NULL,
  p_flow_link_id                  IN NUMBER  DEFAULT NULL,
  p_amt_matched                   IN NUMBER  DEFAULT NULL,
  p_rate                          IN NUMBER  DEFAULT NULL,
  p_invoice_id                    IN NUMBER  DEFAULT NULL,
  p_amt_contingent_rental         IN NUMBER  DEFAULT NULL,
  p_bank_interface_run_id         IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id            IN NUMBER  DEFAULT NULL,
  p_rejected_reason               IN NUMBER  DEFAULT NULL,
  p_settlement_bank_info_id       IN NUMBER  DEFAULT NULL,
  p_amt_gross_netted              IN NUMBER  DEFAULT NULL,
  p_collection_state              IN NUMBER  DEFAULT NULL,
  p_late_fee_upto                 IN NUMBER  DEFAULT NULL,
  p_is_overdue_interest_pending   IN NUMBER  DEFAULT NULL,
  p_last_overdue_interest_dt      IN DATE    DEFAULT NULL,
  p_statement_id                  IN NUMBER  DEFAULT NULL,
  p_amt_rental                    IN NUMBER  DEFAULT NULL,
  p_release_dt                    IN DATE    DEFAULT NULL,
  p_settled_dt                    IN DATE    DEFAULT NULL,
  p_rejected_dt                   IN DATE    DEFAULT NULL,
  p_amt_matched_principal         IN NUMBER  DEFAULT NULL,
  p_penalty_grace_days            IN NUMBER  DEFAULT NULL,
  p_amt_matched_interest          IN NUMBER  DEFAULT NULL,
  p_amt_matched_tax               IN NUMBER  DEFAULT NULL,
  p_exclude_from_account_bal      IN NUMBER  DEFAULT NULL,
  p_settle_count                  IN NUMBER  DEFAULT NULL,
  p_exclude_from_late_fees        IN NUMBER  DEFAULT NULL,
  p_exclude_from_overdue_int      IN NUMBER  DEFAULT NULL,
  p_split_no                      IN NUMBER  DEFAULT NULL,
  p_last_user_id                  IN NUMBER  DEFAULT NULL,
  p_in_recovery						IN NUMBER  DEFAULT NULL,
  p_split_flow_variation_id		IN NUMBER  DEFAULT NULL,
  p_gross_rec_amt       			IN NUMBER  DEFAULT NULL,
  p_is_shadow_copy               IN NUMBER  DEFAULT NULL,
  p_grp_link_id						IN NUMBER  DEFAULT NULL,
  p_reserve_id							IN NUMBER  DEFAULT NULL,
  p_reserve_statement_id			IN NUMBER  DEFAULT NULL,
  p_gl_account_id                IN NUMBER  DEFAULT NULL,
  p_purchase_invoice_id          IN NUMBER  DEFAULT NULL,
  p_amt_invoice                  IN NUMBER  DEFAULT NULL,
  p_can_process                  IN NUMBER  DEFAULT NULL,
  p_custom_flow_link_no			   IN NUMBER  DEFAULT NULL,
  p_leg_no						      IN NUMBER  DEFAULT NULL,
  p_asset_hdr_id				      IN NUMBER  DEFAULT NULL,
  p_tax_point_dt                 IN DATE    DEFAULT NULL,
  p_is_first_settle_bank_info    IN NUMBER  DEFAULT NULL,
  p_termination_quote_id			IN NUMBER  DEFAULT NULL,
  p_payment_confirmation_status	IN NUMBER  DEFAULT NULL,
  p_payment_confirmation_dt		IN DATE    DEFAULT NULL,
  p_payment_confirmation_user_id IN NUMBER  DEFAULT NULL,
  p_payment_credited_dt				IN DATE    DEFAULT NULL,
  p_amt_matched_netted				IN NUMBER  DEFAULT NULL,
  p_pp_nett_no							IN NUMBER  DEFAULT NULL,
  p_is_funding_transfer				IN NUMBER  DEFAULT NULL,
  p_stamp								IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO flow (
     flow_id,
     contract_id,
     image_no,
     input_dt,
     calc_dt,
     actual_dt,
     expected_dt,
     currency_id,
     amount,
     flow_type,
     is_cash,
     status,
     owner_id,
     beneficiary_id,
     nett_no,
     bank_account_id,
     is_set,
     amt_principal,
     amt_interest,
     installment_no,
     flow_method_id,
     payee_ref,
     payment_ref,
     party_account_id,
     amt_gross,
     reversal_status,
     flow_link_id,
     amt_matched,
     rate,
     invoice_id,
     amt_contingent_rental,
     bank_interface_run_id,
     custom_flow_hdr_id,
     rejected_reason,
     settlement_bank_info_id,
     amt_gross_netted,
     collection_state,
     late_fee_upto,
     is_overdue_interest_pending,
     last_overdue_interest_dt,
     statement_id,
     amt_rental,
     release_dt,
     settled_dt,
     rejected_dt,
     amt_matched_principal,
     penalty_grace_days,
     amt_matched_interest,
     amt_matched_tax,
     exclude_from_account_bal,
     settle_count,
     exclude_from_late_fees,
     exclude_from_overdue_interest,
     split_no,
     last_user_id,
     in_recovery,
     split_flow_variation_id,
     gross_rec_amt,
     is_shadow_copy,
     grp_link_id,
     reserve_id,
     reserve_statement_id,
     gl_account_id,
     purchase_invoice_id,
     amt_invoice,
     can_process,
     custom_flow_link_no,
     leg_no,
     asset_hdr_id,
     tax_point_dt,
     is_first_settlement_bank_info,
	  termination_quote_id,
	  payment_confirmation_status,
	  payment_confirmation_dt,
	  payment_confirmation_user_id,
	  payment_credited_dt,
	  amt_matched_netted,
	  pp_nett_no,
	  is_funding_transfer,
     stamp)
  VALUES (
     p_flow_id,
     p_contract_id,
     p_image_no,
     p_input_dt,
     p_calc_dt,
     p_actual_dt,
     p_expected_dt,
     p_currency_id,
     p_amount,
     p_flow_type,
     p_is_cash,
     p_status,
     p_owner_id,
     p_beneficiary_id,
     p_nett_no,
     p_bank_account_id,
     p_is_set,
     p_amt_principal,
     p_amt_interest,
     p_installment_no,
     p_flow_method_id,
     p_payee_ref,
     p_payment_ref,
     p_party_account_id,
     p_amt_gross,
     p_reversal_status,
     p_flow_link_id,
     p_amt_matched,
     p_rate,
     p_invoice_id,
     p_amt_contingent_rental,
     p_bank_interface_run_id,
     p_custom_flow_hdr_id,
     p_rejected_reason,
     p_settlement_bank_info_id,
     p_amt_gross_netted,
     p_collection_state,
     p_late_fee_upto,
     p_is_overdue_interest_pending,
     p_last_overdue_interest_dt,
     p_statement_id,
     p_amt_rental,
     p_release_dt,
     p_settled_dt,
     p_rejected_dt,
     p_amt_matched_principal,
     p_penalty_grace_days,
     p_amt_matched_interest,
     p_amt_matched_tax,
     p_exclude_from_account_bal,
     p_settle_count,
     p_exclude_from_late_fees,
     p_exclude_from_overdue_int,
     p_split_no,
     p_last_user_id,
     p_in_recovery,
     p_split_flow_variation_id,
     p_gross_rec_amt,
	  p_is_shadow_copy,
	  p_grp_link_id,
     p_reserve_id,
     p_reserve_statement_id,
     p_gl_account_id,
     p_purchase_invoice_id,
     p_amt_invoice,
     p_can_process,
     p_custom_flow_link_no,
     p_leg_no,
     p_asset_hdr_id,
     p_tax_point_dt,
     p_is_first_settle_bank_info,
     p_termination_quote_id,
     p_payment_confirmation_status,
     p_payment_confirmation_dt,
     p_payment_confirmation_user_id,
     p_payment_credited_dt,
     p_amt_matched_netted,
     p_pp_nett_no,
	  p_is_funding_transfer,
     p_stamp);
END AXSP_FLOW_INS;
/
create or replace PROCEDURE AXSP_FLOW_SETTLEMENT_INFO_DEF(
	p_from_dt        IN DATE DEFAULT NULL,
	p_run_dt         IN DATE DEFAULT NULL,
	p_user_id        IN NUMBER DEFAULT NULL,
	p_bankacc_id     IN NUMBER DEFAULT NULL,
	p_flow_method_id IN NUMBER DEFAULT NULL,
	p_flow_id        IN NUMBER DEFAULT NULL,
	p_run_mode       IN INTEGER DEFAULT NULL)
AS
	v_cur_dt DATE;
	v_min_effect_dt DATE;
	v_gbp_currency_id INT;
	v_has_system_rv INT;
	v_has_system_payout INT;
BEGIN
	v_cur_dt := axsp_get_datetime();
	v_min_effect_dt := TO_DATE('01-jan-1900','dd-mon-yyyy');
	SELECT currency_id INTO v_gbp_currency_id FROM currency WHERE code = 'GBP';

	--General Cursors
	--Select data for Update RV flows
	IF (p_flow_id = 0) THEN
		-- Run Mode:
		-- 1) Both
		-- 2) CHAPS/FPS Only
		-- 3) Normal Only (Not CHAPS/FPS)
		IF (p_run_mode = 1 or p_run_mode = 3) THEN
			IF (p_from_dt = v_min_effect_dt) THEN -- If this is not an intraday run
				--Update contract where it is due to be rateset, set is_set flag to Not Set
				UPDATE contract
				SET
					is_set = 7800,
					stamp = stamp + 1
				WHERE contract_id >= 0 -- contract valid
				AND
					EXISTS
					(
						SELECT *
						FROM flow
						WHERE flow.contract_id = contract.contract_id
						AND is_set = 7800
						AND actual_dt <= v_cur_dt
						AND (p_bankacc_id = -1 or bank_account_id = p_bankacc_id)
						AND (p_flow_method_id = -1 or flow_method_id = p_flow_method_id )
					);
			END IF;

			-- When we have effective dated SBIs that will come into play, then the statement
			-- below we reset is_default to 0 on existing is_default = 1 records
			UPDATE settlement_bank_info
			SET
				is_default = 0,
				settlement_dt_pending = 0,
				stamp = stamp + 1
			WHERE
				EXISTS
				(
					SELECT sb_old.settlement_bank_info_id
					FROM settlement_bank_info sb_old, settlement_bank_info sb_new
					WHERE sb_old.settlement_bank_info_id = settlement_bank_info.settlement_bank_info_id
					AND sb_old.is_active = 1
					AND sb_old.is_default = 1
					AND sb_old.effect_dt = v_min_effect_dt
					AND (p_flow_method_id = -1 or sb_old.flow_method_id = p_flow_method_id)
					-- old 2 new
					AND sb_old.party_id = sb_new.party_id
					AND sb_old.currency_id = sb_new.currency_id
					AND sb_old.flow_direction = sb_new.flow_direction
					-- new effective date
					AND sb_new.effect_dt > v_min_effect_dt
					AND sb_new.effect_dt <= p_run_dt
					-- new valid
					AND sb_new.is_active = 1
					AND sb_new.approval_status != 6502 --status != Rejected
				);

			-- This statement is making the appropriate effective dated SBIs is_default = 1
			-- We find the record with the effect_dt closest to the run date and make that the default
			-- For example, record 1 effect_dt = 01/01/2011, record 2 effect_dt = 02/01/2011 and run date = 03/01/2011
			-- In this case, we would make record 2 the default and set its effect_dt to 01/01/1900
			UPDATE settlement_bank_info s
			SET
				s.is_default = 1,
				s.effect_dt = v_min_effect_dt,
				s.stamp = s.stamp + 1
			WHERE s.is_active = 1
			AND s.approval_status != 6502 --status != Rejected
			AND (p_flow_method_id = -1 or s.flow_method_id = p_flow_method_id)
			AND s.effect_dt > v_min_effect_dt
			AND s.effect_dt <= p_run_dt
			AND
				s.effect_dt =
				(
					SELECT nvl(MAX(s2.effect_dt), v_min_effect_dt)
					FROM settlement_bank_info s2
					WHERE s2.is_active = 1
					AND s2.approval_status != 6502 --status != Rejected
					AND s2.effect_dt > v_min_effect_dt
					AND s2.effect_dt <= p_run_dt
					AND (p_flow_method_id  = -1 or s2.flow_method_id = p_flow_method_id)
					AND s2.party_id = s.party_id
					AND s2.currency_id = s.currency_id
					AND s2.flow_direction = s.flow_direction
				);

			-- This statement is here to reset any other remaining records with an earlier effect_dt than the
			-- records updated above.  These are records that effectively never got activated and never will
			-- unless the user resets their effective date again.
			-- Using the example above, record 1 would have its effect_dt reset to 01/01/1900 and would remain
			-- a non-default
			UPDATE settlement_bank_info
			SET
				is_default = 0,
				effect_dt = v_min_effect_dt,
				stamp = stamp + 1
			WHERE is_active = 1
			AND approval_status != 6502 --status != Rejected
			AND effect_dt > v_min_effect_dt
			AND effect_dt <= p_run_dt
			AND (p_flow_method_id = -1 or flow_method_id = p_flow_method_id);

			/*
			8406 Residual Value Cheque
				- 1007 Residual Value

			8407 Payout Cheque
				- 1006 Payout
				- 1010 Custom
				- 1012 Termination Penalty
			*/

			--Update RV flows flow method and settlement bank info
			--Update Payout flows flow method and settlement bank info
			--1007
			SELECT CASE WHEN EXISTS (SELECT * FROM system_setting WHERE system_setting_type = 8406 AND system_setting_value LIKE '1') THEN 1
				ELSE 0 END
			INTO v_has_system_rv FROM dual;

			-- If the Payout Cheque system_setting exists and is true
			-- then set Payout flows to cheque method where they are not yet set
			-- 1006,1010,1012
			SELECT CASE WHEN EXISTS (SELECT * FROM system_setting WHERE system_setting_type = 8407 AND system_setting_value LIKE '1') THEN 1
				ELSE 0 END
			INTO v_has_system_payout FROM dual;

			IF (v_has_system_rv = 1 OR v_has_system_payout = 1) THEN
				MERGE INTO flow
				USING
				(
					SELECT flow_id, new_settlement_bank_info_id, sbi.flow_method_id
					FROM
						(
							SELECT
								fl.flow_id,
								min(sbi.settlement_bank_info_id) as new_settlement_bank_info_id /*sbi.flow_method_id*/
							FROM
								flow fl,
								party_account pa,
								settlement_bank_info sbi,
								flow_method fm
							WHERE ( (v_has_system_rv = 1 AND fl.flow_type = 1007) OR (v_has_system_payout = 1 AND fl.flow_type IN (1006,1010,1012)) )
							-- flow type
							AND
								(
									fl.flow_type in (1006,1007,1012) --Payout, Termination Penalty, Termination Timed CF, RV-1007
									OR
									(
										fl.flow_type = 1010
										AND
										fl.custom_flow_hdr_id <> 0
										AND
										(select hdr.timing from custom_flow_hdr hdr where hdr.custom_flow_hdr_id = fl.custom_flow_hdr_id) = 14002
									)
								)
							-- not set!
							AND (fl.flow_method_id = 0 OR fl.settlement_bank_info_id = 0)
							-- flow valid
							AND fl.contract_id >= 0
							AND fl.can_process = 1
							AND fl.is_shadow_copy = 0
							AND fl.reversal_status IN (4200, 4202)
							AND fl.status BETWEEN 2100 AND 2103 --pending, released, settled, rejected
							-- flow expected date
							AND fl.expected_dt >= p_from_dt
							AND fl.expected_dt <= p_run_dt --get flows <= run_dt
							-- flow bankacc(s)
							AND (p_bankacc_id = -1 or fl.bank_account_id = p_bankacc_id)
							-- flow 2 party account
							AND fl.party_account_id = pa.party_account_id
							-- party account 2 sbi
							AND pa.party_id = sbi.party_id
							-- flow currency 2 sbi
							AND fl.currency_id = sbi.currency_id
							-- flow direction 2 sbi (including logic for flow type)
							AND
								(
									(fl.amt_gross_netted - fl.amt_matched_netted >= 0 AND sbi.flow_direction = 5901) --inflow (payout can be in or out flow)
									OR
									(fl.amt_gross_netted - fl.amt_matched_netted < 0 AND sbi.flow_direction = 5900 AND fl.flow_type <> 1007) --outflow (not allowed for RV-1007)
								)
							-- sbi valid
							AND sbi.is_active = 1
							AND sbi.approval_status != 6502 --status != Rejected
							-- sbi effective date
							AND sbi.effect_dt = v_min_effect_dt --min date i.e. current
							-- sbi 2 flow method
							AND sbi.flow_method_id = fm.flow_method_id --select initial flow_method table to get initial_period days
							-- cheque flow_method(s)
							AND
								(
									fm.payment_method = 5100
									OR
									(fm.payment_method = 5109 AND fl.flow_type <> 1007)
								)
							GROUP BY fl.flow_id
						) reqin
						INNER JOIN settlement_bank_info sbi ON (reqin.new_settlement_bank_info_id = sbi.settlement_bank_info_id)
				) req ON (flow.flow_id = req.flow_id)
				WHEN MATCHED THEN UPDATE
				SET
					flow.settlement_bank_info_id = req.new_settlement_bank_info_id,
					flow.flow_method_id = req.flow_method_id,
					flow.last_user_id = p_user_id;
			END IF; -- (v_has_system_rv = 1 OR v_has_system_payout = 1)

			MERGE INTO flow
			USING
			(
				SELECT flow_id, new_flow_method_id, new_settlement_bank_info_id
				FROM
					(
						SELECT
							flow_id,
							-- New Values
							--case 47972 - if this is a purchase invoice flow then use the contract settlement info rather than the default
							case when reqin.purchase_invoice_id > 0 and updt_method.sbi_flow_method_id is not null then updt_method.sbi_flow_method_id 
								else reqin.new_flow_method_id end as new_flow_method_id,
							case when reqin.purchase_invoice_id > 0 and updt_method.settlement_bank_info_id is not null then updt_method.settlement_bank_info_id 
								else reqin.new_settlement_bank_info_id end as new_settlement_bank_info_id,
							RANK() OVER (PARTITION BY flow_id ORDER BY Nvl(updt_method.contract_id, -1) DESC, new_settlement_bank_info_id ASC) updt_lvl
						FROM
							(
								SELECT
									-- Fields
									flow.flow_id,
									flow.contract_id,
									flow.purchase_invoice_id,
									flow.flow_method_id,

									-- Defaults
									sbi.is_default,
									sbi.flow_method_id def_flow_method_id,
									sbi.flow_direction,
									sbi.party_id,

									-- New Values
									CASE
										WHEN (flow.nett_no + 0 > 0 OR (flow.expected_dt > c.calc_dt + fmi.initial_period)) THEN sbi.flow_method_id
										ELSE sbi.initial_flow_method_id
									END new_flow_method_id,
									sbi.settlement_bank_info_id new_settlement_bank_info_id
								FROM
									flow,
									contract c,
									party_account pa,
									settlement_bank_info sbi,
									flow_method fmi,
									flow_method fm,
									party_bankacc pb
								WHERE (flow.flow_method_id = 0 OR flow.settlement_bank_info_id = 0) -- not set!
								-- flow valid
								AND flow.contract_id >= 0
								AND flow.can_process = 1
								AND flow.is_shadow_copy = 0
								AND flow.reversal_status IN (4200, 4202)
								AND flow.status BETWEEN 2100 AND 2103 --pending, released, settled, rejected
								-- flow expected date
								AND flow.expected_dt >= p_from_dt
								AND flow.expected_dt <= p_run_dt
								-- flow bankacc(s)
								AND (p_bankacc_id = -1 or flow.bank_account_id = p_bankacc_id)
								-- flow 2 contract
								AND flow.contract_id = c.contract_id
								AND c.contract_id >= 0
								-- flow 2 party_account
								AND flow.party_account_id = pa.party_account_id
								-- party_account 2 sbi
								AND pa.party_id = sbi.party_id
								-- flow currency 2 sbi
								AND flow.currency_id = sbi.currency_id
								-- flow direction 2 sbi
								AND
									(
										(flow.amt_gross_netted - flow.amt_matched_netted >= 0 AND sbi.flow_direction = 5901) --inflow
										OR
										(flow.amt_gross_netted - flow.amt_matched_netted < 0 AND sbi.flow_direction = 5900) --outflow
									)
								-- sbi valid
								AND sbi.is_active = 1
								AND sbi.approval_status != 6502 --status != Rejected
								-- sbi effective date
								AND sbi.effect_dt = v_min_effect_dt --min date i.e. current
								-- sbi 2 flow_method(s)
								AND sbi.initial_flow_method_id = fmi.flow_method_id --select initial flow_method table to get initial_period days
								AND sbi.flow_method_id = fm.flow_method_id
								-- sbi 2 party_bankacc
								AND sbi.party_bankacc_id = pb.bankacc_id
								AND
									(
										pb.credit_card_association_id = 0
										OR
										EXISTS
										(
											SELECT flow_method_association_id
											FROM flow_method_association fma
											WHERE fma.credit_card_association_id = pb.credit_card_association_id
											AND
												(
													fma.flow_method_id = (CASE WHEN (fmi.payment_method in (5111, 5112) AND fmi.bank_flag = 1) THEN fmi.flow_method_id ELSE fm.flow_method_id END)
													OR
													fma.flow_method_id = (CASE WHEN (fm.payment_method in (5111, 5112) AND fm.bank_flag = 1) THEN fm.flow_method_id ELSE fmi.flow_method_id END)
												)
										)
									)
								-- flow expected date vs. contract + flow methods + bankacc
								AND
									flow.expected_dt <=
									(
										CASE
											WHEN ((flow.nett_no + 0 = 0 AND flow.expected_dt <= c.calc_dt + fmi.initial_period) AND fmi.payment_method in (5111, 5112) AND fmi.bank_flag = 1) THEN pb.credit_card_expiry_dt
											WHEN ((flow.nett_no + 0 > 0 OR flow.expected_dt > c.calc_dt + fmi.initial_period) AND fm.payment_method in (5111, 5112) AND fm.bank_flag = 1) THEN pb.credit_card_expiry_dt
											ELSE flow.expected_dt
										END
									)
							) reqin
							LEFT JOIN
							(
								SELECT
									DISTINCT
									csi.contract_id,
									csi.party_id,
									csi.settlement_bank_info_id, -- if there is a record then it's the first pass
									sbi.flow_method_id AS sbi_flow_method_id
								FROM contract_settlement_info csi
								INNER JOIN settlement_bank_info sbi ON (csi.settlement_bank_info_id = sbi.settlement_bank_info_id)
							) updt_method ON (reqin.new_settlement_bank_info_id = updt_method.settlement_bank_info_id 
										AND ((reqin.purchase_invoice_id > 0 AND reqin.party_id = updt_method.party_id) 
											OR reqin.contract_id = updt_method.contract_id))
						WHERE
							--Update contract specific settlement details to flow and should not be overriden by flow method id
							--For flows with an expected date:
							--outside the contract calc_dt + (initial flow method)flow_method initial period or;
							--inside the contract calc_dt + (initial flow method)flow_method initial period
							(
								updt_method.contract_id IS NOT NULL
								AND
								(p_flow_method_id = -1 OR updt_method.sbi_flow_method_id = p_flow_method_id)
							)
							OR
							--Update default settlement details
							--For flows with an expected date:
							--outside the contract calc_dt + flow_method initial period or;
							--inside the contract calc_dt + flow_method initial period
							(
								(reqin.is_default = 1 or (reqin.purchase_invoice_id > 0 and updt_method.contract_id is not null))
								AND
								(p_flow_method_id = -1 OR reqin.def_flow_method_id = p_flow_method_id)
								AND -- case 47388 ensure there is no contract specific instruction with a different flow method when running for a specific flow method
								(p_flow_method_id = -1 OR 
								 (p_flow_method_id != -1 and NOT EXISTS (SELECT 1 FROM contract_settlement_info csi2 
									INNER JOIN settlement_bank_info sbi2 ON csi2.settlement_bank_info_id = sbi2.settlement_bank_info_id
									where csi2.settlement_bank_info_id != reqin.new_settlement_bank_info_id 
									and csi2.contract_id = reqin.contract_id 
									and sbi2.party_id = reqin.party_id 
									and sbi2.flow_direction = reqin.flow_direction)))
							)
						) reqin
					WHERE reqin.updt_lvl = 1
			) req ON (flow.flow_id = req.flow_id)
			WHEN MATCHED THEN UPDATE
			SET
				flow.flow_method_id = req.new_flow_method_id,
				flow.settlement_bank_info_id = req.new_settlement_bank_info_id,
				flow.last_user_id = p_user_id;
		END IF; -- (p_run_mode = 1 or p_run_mode = 3)

		-- Run Mode:
		-- 1) Both
		-- 2) CHAPS/FPS Only
		-- 3) Normal Only (Not CHAPS/FPS)
		IF (p_run_mode = 1 or p_run_mode = 2) THEN
			-- Update Payment method for FPS Payment method
			-- Payment Method = CHAPS (set above)
			-- Payment Amount is under Max Amount as defined on FPS Payment Method (or is special sort codes **** HSBC specific ***)
			-- Currency is GBP
			UPDATE
				(
					SELECT
						flow.flow_method_id,
						CASE
							WHEN (ABS(flow.amt_gross_netted - flow.amt_matched_netted) <= fmALT.max_amount) THEN fmALT.flow_method_id
							ELSE 5128
						END new_flow_method_id
					FROM
						flow,
						settlement_bank_info sbi,
						flow_method fmALT
					WHERE flow.flow_method_id in (5128, 5129) --CHAPS/FPS (already Allocated)
					-- flow valid
					AND flow.contract_id >= 0
					AND flow.can_process = 1
					AND flow.is_shadow_copy = 0
					AND flow.reversal_status IN (4200, 4202)
					AND flow.status BETWEEN 2100 AND 2103 --pending, released, settled, rejected
					-- flow expected date
					AND flow.expected_dt >= p_from_dt
					AND flow.expected_dt <= p_run_dt
					-- flow bankacc(s)
					AND (p_bankacc_id = -1 or flow.bank_account_id = p_bankacc_id)
					-- flow currency = GBP
					AND flow.currency_id = v_gbp_currency_id
					-- flow direction 2 sbi
					AND
						(
							(flow.amt_gross_netted - flow.amt_matched_netted >= 0 AND sbi.flow_direction = 5901) --inflow
							OR
							(flow.amt_gross_netted - flow.amt_matched_netted < 0 AND sbi.flow_direction = 5900) --outflow
						)
					-- flow 2 sbi DIRECT
					AND flow.settlement_bank_info_id = sbi.settlement_bank_info_id
					-- sbi valid
					AND sbi.is_active = 1
					AND sbi.approval_status != 6502 --status != Rejected
					-- sbi effective date
					AND sbi.effect_dt = v_min_effect_dt --min date i.e. current
					-- sbi alternate_flow_method = FPS
					AND sbi.alternate_flow_method_id = 5129
					-- sbi 2 alternate flow_method
					AND sbi.alternate_flow_method_id = fmALT.flow_method_id
					-- flow method vs alternate flow method
					AND
						flow.flow_method_id !=
						CASE
							WHEN (ABS(flow.amt_gross_netted - flow.amt_matched_netted) <= fmALT.max_amount) THEN fmALT.flow_method_id
							ELSE 5128
						END
				)
			SET
				flow_method_id = new_flow_method_id;
		END IF; -- (p_run_mode = 1 or p_run_mode = 2)
	ELSE -- (@flow_id != 0)
		-- Run Mode:
		-- 1) Both
		-- 2) CHAPS/FPS Only
		-- 3) Normal Only (Not CHAPS/FPS)
		IF (p_run_mode = 1 or p_run_mode = 3) THEN
			MERGE INTO flow
			USING
			(
				SELECT flow_id, new_flow_method_id, new_settlement_bank_info_id
				FROM
					(
						SELECT
							flow_id,
							-- New Values
							--case 47972 - if this is a purchase invoice flow then use the contract settlement info rather than the default
							case when reqin.purchase_invoice_id > 0 and updt_method.sbi_flow_method_id is not null then updt_method.sbi_flow_method_id 
								else reqin.new_flow_method_id end as new_flow_method_id,
							case when reqin.purchase_invoice_id > 0 and updt_method.settlement_bank_info_id is not null then updt_method.settlement_bank_info_id 
								else reqin.new_settlement_bank_info_id end as new_settlement_bank_info_id,
							RANK() OVER (PARTITION BY flow_id ORDER BY Nvl(updt_method.contract_id, -1) DESC, new_settlement_bank_info_id ASC) updt_lvl
						FROM
							(
								SELECT
									-- Fields
									flow.flow_id,
									flow.contract_id,
									flow.purchase_invoice_id,
									flow.flow_method_id,

									-- Defaults
									sbi.is_default,
									sbi.flow_method_id def_flow_method_id,
									sbi.flow_direction,
									sbi.party_id,

									-- New Values
									CASE
										WHEN (flow.nett_no + 0 > 0 OR (flow.expected_dt > c.calc_dt + fmi.initial_period)) THEN sbi.flow_method_id
										ELSE sbi.initial_flow_method_id
									END new_flow_method_id,
									sbi.settlement_bank_info_id new_settlement_bank_info_id
								FROM
									flow,
									contract c,
									party_account pa,
									settlement_bank_info sbi,
									flow_method fmi,
									flow_method fm,
									party_bankacc pb
								WHERE flow.flow_id = p_flow_id
								-- not set!
								AND (flow.flow_method_id = 0 OR flow.settlement_bank_info_id = 0)
								-- flow 2 contract
								AND flow.contract_id = c.contract_id --match flow to contract
								AND c.contract_id >= 0 --contract valid
								-- flow 2 party_account
								AND flow.party_account_id = pa.party_account_id
								-- party_account 2 sbi
								AND pa.party_id = sbi.party_id
								-- flow currency 2 sbi
								AND flow.currency_id = sbi.currency_id
								-- flow direction 2 sbi
								AND
									(
										(flow.amt_gross_netted - flow.amt_matched_netted >= 0 AND sbi.flow_direction = 5901) --inflow
										OR
										(flow.amt_gross_netted - flow.amt_matched_netted < 0 AND sbi.flow_direction = 5900) --outflow
									)
								-- sbi valid
								AND sbi.is_active = 1
								AND sbi.approval_status != 6502 --status != Rejected
								-- sbi effective date
								AND sbi.effect_dt = v_min_effect_dt --min date i.e. current
								-- sbi 2 flow_method(s)
								AND sbi.initial_flow_method_id = fmi.flow_method_id --select initial flow_method table to get initial_period days
								AND sbi.flow_method_id = fm.flow_method_id
								-- sbi 2 party_bankacc
								AND sbi.party_bankacc_id = pb.bankacc_id
								AND
									(
										pb.credit_card_association_id = 0
										OR
										EXISTS
										(
											SELECT flow_method_association_id
											FROM flow_method_association fma
											WHERE
												fma.credit_card_association_id = pb.credit_card_association_id
												AND
													(
														fma.flow_method_id = (CASE WHEN (fmi.payment_method in (5111, 5112) AND fmi.bank_flag = 1) THEN fmi.flow_method_id ELSE fm.flow_method_id END)
														OR
														fma.flow_method_id = (CASE WHEN (fm.payment_method in (5111, 5112) AND fm.bank_flag = 1) THEN fm.flow_method_id ELSE fmi.flow_method_id END)
													)
										)
									)
								-- flow expected date vs. contract + flow methods + bankacc
								AND
									flow.expected_dt <=
									(
										CASE
											WHEN ((flow.nett_no + 0 = 0 AND flow.expected_dt <= c.calc_dt + fmi.initial_period) AND fmi.payment_method IN (5111, 5112) AND fmi.bank_flag = 1) THEN pb.credit_card_expiry_dt
											WHEN ((flow.nett_no + 0 > 0 OR flow.expected_dt > c.calc_dt + fmi.initial_period) AND fm.payment_method IN (5111, 5112) AND fm.bank_flag = 1) THEN pb.credit_card_expiry_dt
											ELSE flow.expected_dt
										END
									)
							) reqin
							LEFT JOIN
							(
								SELECT
									DISTINCT
									contract_id,
									csi.party_id,
									csi.settlement_bank_info_id, -- if there is a record then it's the first pass
									sbi.flow_method_id AS sbi_flow_method_id
								FROM
									contract_settlement_info csi
								INNER JOIN
									settlement_bank_info sbi ON csi.settlement_bank_info_id = sbi.settlement_bank_info_id
							) updt_method ON (reqin.new_settlement_bank_info_id = updt_method.settlement_bank_info_id 
									AND ((reqin.purchase_invoice_id > 0 AND reqin.party_id = updt_method.party_id) 
										OR reqin.contract_id = updt_method.contract_id))
						WHERE
							--Update contract specific settlement details to flow and should not be overriden by flow method id
							--For flows with an expected date
							--outside the contract calc_dt + (initial flow method)flow_method initial period or;
							--inside the contract calc_dt + (initial flow method)flow_method initial period
							(
								updt_method.contract_id IS NOT NULL
								AND
								(p_flow_method_id = -1 or updt_method.sbi_flow_method_id = p_flow_method_id)
							)
							OR
							--Update default settlement details
							--For flows with an expected
							--outside the contract calc_dt + flow_method initial period or;
							--inside the contract calc_dt + flow_method initial period
							(
								(reqin.is_default = 1 or (reqin.purchase_invoice_id > 0 and updt_method.contract_id is not null))
								AND
								(p_flow_method_id = -1 or reqin.def_flow_method_id = p_flow_method_id)
								AND -- case 47388 ensure there is no contract specific instruction with a different flow method when running for a specific flow method
								(p_flow_method_id = -1 OR 
								 (p_flow_method_id != -1 and NOT EXISTS (SELECT 1 FROM contract_settlement_info csi2 
									INNER JOIN settlement_bank_info sbi2 ON csi2.settlement_bank_info_id = sbi2.settlement_bank_info_id
									where csi2.settlement_bank_info_id != reqin.new_settlement_bank_info_id 
									and csi2.contract_id = reqin.contract_id 
									and sbi2.party_id = reqin.party_id 
									and sbi2.flow_direction = reqin.flow_direction)))
							)
					) reqin
				WHERE reqin.updt_lvl = 1
			) req ON (flow.flow_id = req.flow_id)
			WHEN MATCHED THEN UPDATE
			SET
				flow.flow_method_id = req.new_flow_method_id,
				flow.settlement_bank_info_id = req.new_settlement_bank_info_id,
				flow.last_user_id = p_user_id;
		END IF; -- (p_run_mode = 1 or p_run_mode = 3)

		-- Run Mode:
		-- 1) Both
		-- 2) CHAPS/FPS Only
		-- 3) Normal Only (Not CHAPS/FPS)
		IF (p_run_mode = 1 or p_run_mode = 2) THEN
			-- Update Payment method for FPS Payment method
			-- Payment Method = CHAPS (set above)
			-- Payment Amount is under Max Amount as defined on FPS Payment Method (or is special sort codes **** HSBC specific ***)
			-- Currency is GBP
			UPDATE
				(
					SELECT
						flow.flow_method_id,
						CASE
							WHEN (ABS(flow.amt_gross_netted - flow.amt_matched_netted) <= fmALT.max_amount) THEN fmALT.flow_method_id
							ELSE 5128
						END new_flow_method_id
					FROM
						flow,
						settlement_bank_info sbi,
						flow_method fmALT
					WHERE flow.flow_id = p_flow_id
					AND flow.flow_method_id in (5128, 5129) --CHAPS/FPS (already Allocated)
					-- flow valid
					AND flow.reversal_status IN (4200, 4202)
					AND flow.status BETWEEN 2100 AND 2103 --pending, released, settled, rejected
					-- flow bankacc(s)
					AND (p_bankacc_id = -1 or flow.bank_account_id = p_bankacc_id)
					-- flow currency = GBP
					AND flow.currency_id = v_gbp_currency_id
					-- flow direction 2 sbi
					AND
						(
							(flow.amt_gross_netted - flow.amt_matched_netted >= 0 AND sbi.flow_direction = 5901) --inflow
							OR
							(flow.amt_gross_netted - flow.amt_matched_netted < 0 AND sbi.flow_direction = 5900) --outflow
						)
					-- flow 2 sbi DIRECT
					AND flow.settlement_bank_info_id = sbi.settlement_bank_info_id
					-- sbi valid
					AND sbi.is_active = 1
					AND sbi.approval_status != 6502 --status != Rejected
					-- sbi effective date
					AND sbi.effect_dt = v_min_effect_dt --min date i.e. current
					-- sbi alternate_flow_method = FPS
					AND sbi.alternate_flow_method_id = 5129
					-- sbi 2 alternate flow_method
					AND fmALT.flow_method_id = sbi.alternate_flow_method_id
					-- flow method vs alternate flow method
					AND
						flow.flow_method_id !=
						(
							CASE
								WHEN (ABS(flow.amt_gross_netted - flow.amt_matched_netted) <= fmALT.max_amount) THEN fmALT.flow_method_id
								ELSE 5128
							END
						)
				)
			SET
				flow_method_id = new_flow_method_id;
		END IF; -- (p_run_mode = 1 or p_run_mode = 2)

	END IF; -- (@flow_id != 0)
END AXSP_FLOW_SETTLEMENT_INFO_DEF;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_UPD(
  p_flow_id                       IN NUMBER  DEFAULT NULL,
  p_contract_id                   IN NUMBER  DEFAULT NULL,
  p_image_no                      IN NUMBER  DEFAULT NULL,
  p_input_dt                      IN DATE    DEFAULT NULL,
  p_calc_dt                       IN DATE    DEFAULT NULL,
  p_actual_dt                     IN DATE    DEFAULT NULL,
  p_expected_dt                   IN DATE    DEFAULT NULL,
  p_currency_id                   IN NUMBER  DEFAULT NULL,
  p_amount                        IN NUMBER  DEFAULT NULL,
  p_flow_type                     IN NUMBER  DEFAULT NULL,
  p_is_cash                       IN NUMBER  DEFAULT NULL,
  p_status                        IN NUMBER  DEFAULT NULL,
  p_owner_id                      IN NUMBER  DEFAULT NULL,
  p_beneficiary_id                IN NUMBER  DEFAULT NULL,
  p_nett_no                       IN NUMBER  DEFAULT NULL,
  p_bank_account_id               IN NUMBER  DEFAULT NULL,
  p_is_set                        IN NUMBER  DEFAULT NULL,
  p_amt_principal                 IN NUMBER  DEFAULT NULL,
  p_amt_interest                  IN NUMBER  DEFAULT NULL,
  p_installment_no                IN NUMBER  DEFAULT NULL,
  p_flow_method_id                IN NUMBER  DEFAULT NULL,
  p_payee_ref                     IN NVARCHAR2  DEFAULT NULL,
  p_payment_ref                   IN NVARCHAR2  DEFAULT NULL,
  p_party_account_id              IN NUMBER  DEFAULT NULL,
  p_amt_gross                     IN NUMBER  DEFAULT NULL,
  p_reversal_status               IN NUMBER  DEFAULT NULL,
  p_flow_link_id                  IN NUMBER  DEFAULT NULL,
  p_amt_matched                   IN NUMBER  DEFAULT NULL,
  p_rate                          IN NUMBER  DEFAULT NULL,
  p_invoice_id                    IN NUMBER  DEFAULT NULL,
  p_amt_contingent_rental         IN NUMBER  DEFAULT NULL,
  p_bank_interface_run_id         IN NUMBER  DEFAULT NULL,
  p_custom_flow_hdr_id            IN NUMBER  DEFAULT NULL,
  p_rejected_reason               IN NUMBER  DEFAULT NULL,
  p_settlement_bank_info_id       IN NUMBER  DEFAULT NULL,
  p_amt_gross_netted              IN NUMBER  DEFAULT NULL,
  p_collection_state              IN NUMBER  DEFAULT NULL,
  p_late_fee_upto                 IN NUMBER  DEFAULT NULL,
  p_is_overdue_interest_pending   IN NUMBER  DEFAULT NULL,
  p_last_overdue_interest_dt      IN DATE    DEFAULT NULL,
  p_statement_id                  IN NUMBER  DEFAULT NULL,
  p_amt_rental                    IN NUMBER  DEFAULT NULL,
  p_release_dt                    IN DATE    DEFAULT NULL,
  p_settled_dt                    IN DATE    DEFAULT NULL,
  p_rejected_dt                   IN DATE    DEFAULT NULL,
  p_amt_matched_principal         IN NUMBER  DEFAULT NULL,
  p_penalty_grace_days            IN NUMBER  DEFAULT NULL,
  p_amt_matched_interest          IN NUMBER  DEFAULT NULL,
  p_amt_matched_tax               IN NUMBER  DEFAULT NULL,
  p_exclude_from_account_bal      IN NUMBER  DEFAULT NULL,
  p_settle_count                  IN NUMBER  DEFAULT NULL,
  p_exclude_from_late_fees        IN NUMBER  DEFAULT NULL,
  p_exclude_from_overdue_int      IN NUMBER  DEFAULT NULL,
  p_split_no                      IN NUMBER  DEFAULT NULL,
  p_last_user_id                  IN NUMBER  DEFAULT NULL,
  p_in_recovery                   IN NUMBER  DEFAULT NULL,
  p_split_flow_variation_id       IN NUMBER  DEFAULT NULL,
  p_gross_rec_amt                 IN NUMBER  DEFAULT NULL,
  p_is_shadow_copy                IN NUMBER  DEFAULT NULL,
  p_grp_link_id                   IN NUMBER  DEFAULT NULL,
  p_reserve_id                    IN NUMBER  DEFAULT NULL,
  p_reserve_statement_id          IN NUMBER  DEFAULT NULL,
  p_gl_account_id                 IN NUMBER  DEFAULT NULL,
  p_purchase_invoice_id           IN NUMBER  DEFAULT NULL,
  p_amt_invoice                   IN NUMBER  DEFAULT NULL,
  p_can_process                   IN NUMBER  DEFAULT NULL,
  p_stamp                         IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  UPDATE flow
  SET contract_id = p_contract_id,
    image_no = p_image_no,
    input_dt = p_input_dt,
    calc_dt = p_calc_dt,
    actual_dt = p_actual_dt,
    expected_dt = p_expected_dt,
    currency_id = p_currency_id,
    amount = p_amount,
    flow_type = p_flow_type,
    is_cash = p_is_cash,
    status = p_status,
    owner_id = p_owner_id,
    beneficiary_id = p_beneficiary_id,
    nett_no = p_nett_no,
    bank_account_id = p_bank_account_id,
    is_set = p_is_set,
    amt_principal = p_amt_principal,
    amt_interest = p_amt_interest,
    installment_no = p_installment_no,
    flow_method_id = p_flow_method_id,
    payee_ref = p_payee_ref,
    payment_ref = p_payment_ref,
    party_account_id = p_party_account_id,
    amt_gross = p_amt_gross,
    reversal_status = p_reversal_status,
    flow_link_id = p_flow_link_id,
    amt_matched = p_amt_matched,
    rate = p_rate,
    invoice_id = p_invoice_id,
    amt_contingent_rental = p_amt_contingent_rental,
    bank_interface_run_id = p_bank_interface_run_id,
    custom_flow_hdr_id = p_custom_flow_hdr_id,
    rejected_reason = p_rejected_reason,
    settlement_bank_info_id = p_settlement_bank_info_id,
    amt_gross_netted = p_amt_gross_netted,
    collection_state = p_collection_state,
    late_fee_upto = p_late_fee_upto,
    is_overdue_interest_pending = p_is_overdue_interest_pending,
    last_overdue_interest_dt = p_last_overdue_interest_dt,
    statement_id = p_statement_id,
    amt_rental = p_amt_rental,
    release_dt = p_release_dt,
    settled_dt = p_settled_dt,
    rejected_dt = p_rejected_dt,
    amt_matched_principal = p_amt_matched_principal,
    penalty_grace_days = p_penalty_grace_days,
    amt_matched_interest = p_amt_matched_interest,
    amt_matched_tax = p_amt_matched_tax,
    exclude_from_account_bal = p_exclude_from_account_bal,
    settle_count = p_settle_count,
    exclude_from_late_fees = p_exclude_from_late_fees,
    exclude_from_overdue_interest = p_exclude_from_overdue_int,
    split_no = p_split_no,
    last_user_id = p_last_user_id,
    in_recovery = p_in_recovery,
    split_flow_variation_id = p_split_flow_variation_id,
    gross_rec_amt = p_gross_rec_amt,
    is_shadow_copy = p_is_shadow_copy,
    grp_link_id = p_grp_link_id,
    reserve_id = p_reserve_id,
    reserve_statement_id = p_reserve_statement_id,
    gl_account_id = p_gl_account_id,
    purchase_invoice_id = p_purchase_invoice_id,
    amt_invoice = p_amt_invoice,
    can_process = p_can_process,
    stamp = p_stamp
    WHERE flow_id = p_flow_id;
END AXSP_FLOW_UPD;
/
CREATE OR REPLACE PROCEDURE AXSP_GETOBJECTWITHCUSTOMFIELDS(
  p_id                           IN NUMBER    DEFAULT NULL,
  p_usesubtype                   IN NUMBER    DEFAULT NULL,
  p_filter                       IN VARCHAR2  DEFAULT NULL,
  p_maintable_name               IN VARCHAR2  DEFAULT NULL,
  p_maintable_columnname_id      IN VARCHAR2  DEFAULT NULL,
  p_maintable_columnname_subtype IN VARCHAR2  DEFAULT NULL,
  p_cfvtable_name                IN VARCHAR2  DEFAULT NULL,
  p_cfvtable_columnname_fieldid  IN VARCHAR2  DEFAULT NULL,
  p_cfvtable_columnname_entityid IN VARCHAR2  DEFAULT NULL,
  p_cfvtable_columnname_value    IN VARCHAR2  DEFAULT NULL,
  p_cfdtable_name                IN VARCHAR2  DEFAULT NULL,
  p_cfdtable_columnname_id       IN VARCHAR2  DEFAULT NULL,
  p_cfdtable_columnname_caption  IN VARCHAR2  DEFAULT NULL,
  p_cfdtable_columnname_length   IN VARCHAR2  DEFAULT NULL,
  p_cfdtable_columnname_subtype  IN VARCHAR2  DEFAULT NULL,
  RC1                            IN OUT globalPkg.RCT1) AS
--
  issinglevalue             NUMBER(1,0);
  singlevalueid             NUMBER;
  v_sqlstring               VARCHAR2(4000);
  v_sqlstring_selecttable   VARCHAR2(4000);
  usesubtype                NUMBER;
  v_session_id              VARCHAR2(20) := userenv('SESSIONID');
  v_tp_getdata_entity_table VARCHAR2(30) := 'tp_getdata_entity_'||v_session_id;
  cursor udf_defn_cursor is
    select * from tp_getdata_fieldlist;
BEGIN
--
-- decide if a single value is requested or an entire table.
-- decide if sub-types are going to be used.
--
  IF p_id > 0 THEN
    issinglevalue := 1;
    singlevalueid := p_id;
    usesubtype := NVL(p_usesubtype, 1);
  ELSE
    issinglevalue := 0;
    singlevalueid := NULL;
    usesubtype := 0;
  END IF;
--
--
-- build a temporary table to host the list of user-defined
-- fields that are going to be processed.
-- insert the user-defined field data into the temporary table.
--
  delete from tp_getdata_fieldlist; -- clean existing table
--
  v_sqlstring := 'insert into tp_getdata_fieldlist select ' ||
                  p_cfdtable_columnname_id || ', ' ||
                  'replace(substr('||p_cfdtable_columnname_caption||',1,30),'' '',''_'')' || ', ' ||
                  p_cfdtable_columnname_length || ')' ||
                  ' from ' || p_cfdtable_name;
  IF usesubtype = 1 THEN
    v_sqlstring := v_sqlstring ||
                   ' where ' ||
                   p_cfdtable_columnname_subtype ||
                   ' = (select  ' || p_maintable_columnname_subtype ||
                   ' from ' || p_maintable_name ||
                   ' where ' || p_maintable_columnname_id || ' = 1)';
  END IF;
  execute immediate v_sqlstring;
--
-- build the output table declaration and initialization sql
-- strings, the output table containes a column for the entity
-- id and another column for each user-defined fields which
-- are defaulted to null.
-- Recreate the temporary table
--
  begin
    execute immediate 'truncate table '||v_tp_getdata_entity_table;
    execute immediate 'drop table '||v_tp_getdata_entity_table;
  exception
    when others then
      null;
  end;
  execute immediate 'create global temporary table '||v_tp_getdata_entity_table||'(id integer) on commit preserve rows';
--
  v_sqlstring_selecttable := '';
  for i in (select * from tp_getdata_fieldlist) loop
    --
    -- Add user defined columns to temp table
    --
    v_sqlstring := 'alter table '||v_tp_getdata_entity_table||' add (' ||
                               i.caption ||
                               ' nvarchar2(' ||
                               to_char(i.field_length) || '))';
    execute immediate v_sqlstring;
    v_sqlstring_selecttable := v_sqlstring_selecttable || ', ' || i.caption;
  END LOOP;
--
  --
  -- Now insert just populate the ID column in the temp table
  --
  v_sqlstring := 'insert into '||v_tp_getdata_entity_table||'(id) select ' ||
                  p_maintable_columnname_id ||
                  ' from ' || p_maintable_name;
  IF issinglevalue = 1 THEN
    v_sqlstring := v_sqlstring ||
                   ' where ' || p_maintable_columnname_id || ' = ' ||
                   ROUND(singlevalueid);
  END IF;
  execute immediate v_sqlstring;
--
  for i in udf_defn_cursor loop
    -- build a sql string to populate the output table from the
    -- user-defined data, an update statement is used to insert
    -- the data of each user-defined fields
    v_sqlstring := 'update '||v_tp_getdata_entity_table||' tge set ' ||
                    i.caption || ' = (select ' || p_cfvtable_columnname_value ||
                    ' from ' || p_maintable_name || ' a, ' ||
                                p_cfvtable_name || ' b ' ||
                    'where a.' || p_maintable_columnname_id ||
                    ' = b.' || p_cfvtable_columnname_entityid ||
                    ' and b.' || p_cfvtable_columnname_fieldid ||
                    ' = ' || TO_CHAR(i.id) || ' and a.' ||
                    p_maintable_columnname_id || ' = tge.id)'||
                    'where exists (select ' || p_cfvtable_columnname_value ||
                    ' from ' || p_maintable_name || ' a, ' ||
                                p_cfvtable_name || ' b ' ||
                    'where a.' || p_maintable_columnname_id ||
                    ' = b.' || p_cfvtable_columnname_entityid ||
                    ' and b.' || p_cfvtable_columnname_fieldid ||
                    ' = ' || TO_CHAR(i.id) || ' and a.' ||
                    p_maintable_columnname_id || ' = tge.id)';
    execute immediate(v_sqlstring);
  END LOOP;
  --Select table
  v_sqlstring := 'select a.*' || v_sqlstring_selecttable || ' from ' || p_maintable_name || ' a, '||v_tp_getdata_entity_table||' b where a.' || p_cfvtable_columnname_entityid || ' = b.id';
  IF p_filter IS NOT NULL THEN
    v_sqlstring := v_sqlstring || ' and ' || p_filter;
  END IF;
  OPEN RC1 for v_sqlstring;
END;
/
CREATE OR REPLACE PROCEDURE AXSP_GETASSETCUSTFLDS_NOPRM(
  RC1           IN OUT       globalPkg.RCT1) AS
--
BEGIN
  axsp_getobjectwithcustomfields
  (p_id => NULL,
   p_usesubtype=>1,
   p_filter => NULL,
   p_maintable_name=>'ASSET',
   p_maintable_columnname_id=>'ASSET_ID',
   p_maintable_columnname_subtype=>'ASSET_TYPE_ID',
   p_cfvtable_name=>'ASSET_CFV',
   p_cfvtable_columnname_fieldid=>'ASSET_CFD_ID',
   p_cfvtable_columnname_entityid=>'ASSET_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'ASSET_CFD',
   p_cfdtable_columnname_id=>'ASSET_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype=>'ASSET_TYPE_ID',
   RC1 => RC1);
END AXSP_GETASSETCUSTFLDS_NOPRM;
/
CREATE OR REPLACE PROCEDURE AXSP_GETASSETWITHCUSTOMFIELDS(
  p_assetid     IN NUMBER    DEFAULT NULL,
  p_assetfilter IN VARCHAR2  DEFAULT NULL,
  RC1           IN OUT       globalPkg.RCT1)
AS
--id of the required entity, use null, zero or negative numbers for all
--filter used to process the output table
BEGIN
  axsp_getobjectwithcustomfields
  (p_id=>p_assetid,
   p_usesubtype=>1,
   p_filter=>p_assetfilter,
   p_maintable_name=>'ASSET',
   p_maintable_columnname_id=>'ASSET_ID',
   p_maintable_columnname_subtype=>'ASSET_TYPE_ID',
   p_cfvtable_name=>'ASSET_CFV',
   p_cfvtable_columnname_fieldid=>'ASSET_CFD_ID',
   p_cfvtable_columnname_entityid=>'ASSET_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'ASSET_CFD',
   p_cfdtable_columnname_id=>'ASSET_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype=>'ASSET_TYPE_ID',
   RC1 => RC1);
END AXSP_GETASSETWITHCUSTOMFIELDS;
/
CREATE OR REPLACE PROCEDURE AXSP_GETCONTRACTCUSTFLDS(
  p_contractid     IN NUMBER   DEFAULT NULL,
  p_contractfilter IN VARCHAR2 DEFAULT NULL,
  RC1              IN OUT      globalPkg.RCT1) AS
--
--id of the required entity, use null, zero or negative numbers for all
--filter used to process the output table
BEGIN
  axsp_getobjectwithcustomfields
  (p_id=>p_contractid,
   p_usesubtype=>0,
   p_filter=>p_contractfilter,
   p_maintable_name=>'CONTRACT',
   p_maintable_columnname_id=>'CONTRACT_ID',
   p_maintable_columnname_subtype => NULL,
   p_cfvtable_name=>'CONTRACT_CFV',
   p_cfvtable_columnname_fieldid=>'CONTRACT_CFD_ID',
   p_cfvtable_columnname_entityid=>'CONTRACT_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'CONTRACT_CFD',
   p_cfdtable_columnname_id=>'CONTRACT_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype => NULL,
   RC1 => RC1);
END AXSP_GETCONTRACTCUSTFLDS;
/
CREATE OR REPLACE PROCEDURE AXSP_GETCONTRACTCUSTFLDS_NOPRM(
  RC1              IN OUT      globalPkg.RCT1) AS
--
BEGIN
  axsp_getobjectwithcustomfields
  (p_id => NULL,
   p_usesubtype=>0,
   p_filter => NULL,
   p_maintable_name=>'CONTRACT',
   p_maintable_columnname_id=>'CONTRACT_ID',
   p_maintable_columnname_subtype => NULL,
   p_cfvtable_name=>'CONTRACT_CFV',
   p_cfvtable_columnname_fieldid=>'CONTRACT_CFD_ID',
   p_cfvtable_columnname_entityid=>'CONTRACT_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'CONTRACT_CFD',
   p_cfdtable_columnname_id=>'CONTRACT_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype => NULL,
   RC1 => RC1);
END AXSP_GETCONTRACTCUSTFLDS_NOPRM;
/
CREATE OR REPLACE PROCEDURE AXSP_GETPARTYCUSTFLDS_NOPRMS(
  RC1              IN OUT      globalPkg.RCT1) AS
--
BEGIN
  axsp_getobjectwithcustomfields
  (p_id => NULL,
   p_usesubtype=>0,
   p_filter => NULL,
   p_maintable_name=>'PARTY',
   p_maintable_columnname_id=>'PARTY_ID',
   p_maintable_columnname_subtype => NULL,
   p_cfvtable_name=>'PARTY_CFV',
   p_cfvtable_columnname_fieldid=>'PARTY_CFD_ID',
   p_cfvtable_columnname_entityid=>'PARTY_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'PARTY_CFD',
   p_cfdtable_columnname_id=>'PARTY_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype => NULL,
   RC1 => RC1);
END AXSP_GETPARTYCUSTFLDS_NOPRMS;
/
CREATE OR REPLACE PROCEDURE AXSP_GETPARTYWITHCUSTOMFIELDS(
  p_partyid     IN NUMBER   DEFAULT NULL,
  p_partyfilter IN VARCHAR2 DEFAULT NULL,
  RC1           IN OUT      globalPkg.RCT1) AS
--
--id of the required entity, use null, zero or negative numbers for all
--filter used to process the output table
BEGIN
  axsp_getobjectwithcustomfields
  (p_id=>p_partyid,
   p_usesubtype=>0,
   p_filter=>p_partyfilter,
   p_maintable_name=>'PARTY',
   p_maintable_columnname_id=>'PARTY_ID',
   p_maintable_columnname_subtype => NULL,
   p_cfvtable_name=>'PARTY_CFV',
   p_cfvtable_columnname_fieldid=>'PARTY_CFD_ID',
   p_cfvtable_columnname_entityid=>'PARTY_ID',
   p_cfvtable_columnname_value=>'FIELD_VALUE',
   p_cfdtable_name=>'PARTY_CFD',
   p_cfdtable_columnname_id=>'PARTY_CFD_ID',
   p_cfdtable_columnname_caption=>'NAME',
   p_cfdtable_columnname_length=>'MAX_LENGTH',
   p_cfdtable_columnname_subtype => NULL,
   RC1 => RC1);
END AXSP_GETPARTYWITHCUSTOMFIELDS;
/
CREATE OR REPLACE PROCEDURE AXSP_GET_CONTRACT_OVERDUE_BAL(
p_contract_id IN NUMBER  DEFAULT NULL,
p_dt          IN DATE  DEFAULT NULL,
RC1         IN OUT globalPkg.RCT1) AS
--
BEGIN
  OPEN RC1 FOR
    SELECT f.contract_id, SUM(f.amt_gross - f.amt_matched)
    FROM flow f, contract c
    WHERE f.contract_id = c.contract_id
      and f.party_account_id = c.party_account_id
      and f.flow_type != 1001
      and f.reversal_status = 4200
      and f.is_cash = 1
	  and f.status not in (2104, 2105)
	  and f.is_shadow_copy = 0
      and f.expected_dt < p_dt
      and f.contract_id = p_contract_id
    GROUP BY f.contract_id;
END AXSP_GET_CONTRACT_OVERDUE_BAL;
/
CREATE OR REPLACE PROCEDURE AXSP_GET_CONTRACT_SUMMARY(
p_contract_id IN NUMBER  DEFAULT NULL,
p_as_at_dt    IN DATE  DEFAULT NULL,
RC1           IN OUT globalPkg.RCT1) AS
--
  StoO_error   INTEGER;
  StoO_errmsg  VARCHAR2(255);
  adv_initial   NUMBER;
  adv_final   NUMBER;
  adv_initial_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  adv_initial_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
  adv_final_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  adv_final_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
  next_installment_dt   DATE;
  installment_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  installment_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
  reg_installment_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  reg_installment_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
  avg_installment_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  avg_installment_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
  remaining_installments   NUMBER;
  tempVar1   VARCHAR2(255);
  overdue_installments   NUMBER;
  tempVar2   VARCHAR2(255);
  rec_drawdown_amt   NUMBER(18,4); /* User Defined Type : axdt_amount */
  rec_drawdown_amt_gross   NUMBER(18,4); /* User Defined Type : axdt_amount */
--get advanced installment count
BEGIN
  FOR rec IN ( SELECT   adv_installments_initial,  adv_installments_final
               FROM contract
               WHERE contract_id = p_contract_id) LOOP
   adv_initial := rec.adv_installments_initial ;
   adv_final := rec.adv_installments_final ;
  END LOOP;
--get initial advanced amount
  IF  adv_initial = 0 THEN
    adv_initial_amt := 0;
    adv_initial_amt_gross := 0;
  ELSE
    SELECT SUM(amount), SUM(amt_gross)
      INTO adv_initial_amt, adv_initial_amt_gross
      FROM flow
      WHERE contract_id = p_contract_id
      and reversal_status = 4200
      and is_cash = 1
      and flow_type = 1003
		and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
      and actual_dt <= calc_dt
      and installment_no <= adv_initial;
--current installment
--for advanced contracts the 1st installment has actual_dt = calc_dt else actual_dt < calc_dt means it is advanced
--get final advanced amount
  END IF;
  IF  adv_final = 0 THEN
    adv_final_amt := 0;
    adv_final_amt_gross := 0;
  ELSE
    SELECT SUM(amount),  SUM(amt_gross)
      INTO adv_final_amt, adv_final_amt_gross
      FROM flow
      WHERE contract_id = p_contract_id
      and reversal_status = 4200
      and is_cash = 1
      and flow_type = 1003
		and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
      and actual_dt < calc_dt
      and installment_no > adv_initial;
--current installment
--exclude initial installments
--get next installment date
  END IF;
  SELECT NVL(MIN(expected_dt), p_as_at_dt)
    INTO next_installment_dt
    FROM flow
    WHERE contract_id = p_contract_id
    and reversal_status = 4200
    and is_cash = 1
    and flow_type = 1003
	 and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
    and expected_dt >= p_as_at_dt;
--current installment
--get current installment amounts
  FOR rec IN ( SELECT   NVL(amount, 0) tmpAlias1,  NVL(amt_gross, 0) tmpAlias2
               FROM flow
               WHERE contract_id = p_contract_id
               and reversal_status = 4200
               and is_cash = 1
               and flow_type = 1003
					and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
               and expected_dt = next_installment_dt) LOOP
    installment_amt := rec.tmpAlias1 ;
    installment_amt_gross := rec.tmpAlias2 ;
  END LOOP;
--current installment
--get current regular installment amounts
  SELECT NVL(SUM(amount), 0), NVL(SUM(amt_gross), 0)
    INTO reg_installment_amt, reg_installment_amt_gross
    FROM flow f, custom_flow_hdr h
    WHERE f.contract_id = p_contract_id
    and f.custom_flow_hdr_id = h.custom_flow_hdr_id
    and reversal_status = 4200
    and is_cash = 1
    and ( (flow_type = 1010 and (h.timing = 14003 or h.timing = 14011)) or flow_type = 1003)
	 and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
    and expected_dt = next_installment_dt;
--installment timing custom flow or current installment
--get avg installment amounts
  SELECT AVG(amount), AVG(amt_gross)
    INTO avg_installment_amt, avg_installment_amt_gross
    FROM flow
    WHERE contract_id = p_contract_id
    and reversal_status = 4200
    and is_cash = 1
    and flow_type = 1003
	 and is_funding_transfer = 0;
--current installment
--get remaining installments
  BEGIN
  StoO_error := 0;
  SELECT count(*)
  INTO tempVar1
   FROM flow
    WHERE contract_id = p_contract_id
     and flow_type = 1003
	  and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
     and reversal_status = 4200
     and actual_dt > axsp_dateonly(p_as_at_dt);
  EXCEPTION
    WHEN TOO_MANY_ROWS THEN
      null;
    WHEN NO_DATA_FOUND THEN
      null;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  remaining_installments := tempVar1;
--remaining installments
--get avg overdue installments
  BEGIN
  StoO_error   := 0;
  SELECT count(*)
  INTO tempVar2
   FROM flow
    WHERE contract_id = p_contract_id
     and flow_type = 1003
	  and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
     and reversal_status = 4200
     and collection_state = 14802;
  EXCEPTION
    WHEN TOO_MANY_ROWS THEN
      null;
    WHEN NO_DATA_FOUND THEN
      null;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  overdue_installments := tempVar2;
--overdue installments
-- get receivables drawdown
  FOR rec IN ( SELECT   amount,  amt_gross
               FROM flow
                  WHERE contract_id = p_contract_id
                   and flow_type = 1001
                   and is_cash = 1
                   and reversal_status = 4200
						 and is_funding_transfer = 0 --exclude any funding transfer installment flows from the amount
                   and calc_dt = (
                  SELECT  rec_drawdown_dt
                   FROM contract
                  WHERE contract_id = p_contract_id))
  LOOP
    rec_drawdown_amt := rec.amount ;
    rec_drawdown_amt_gross := rec.amt_gross ;
  END LOOP;
--return the result
  OPEN RC1 FOR
  SELECT  p_contract_id contract_id
  ,  NVL(adv_initial_amt, 0) adv_initial_amt
  ,  NVL(adv_initial_amt_gross, 0) adv_initial_amt_gross
  ,  NVL(adv_final_amt, 0) adv_final_amt
  ,  NVL(adv_final_amt_gross, 0) adv_final_amt_gross
  ,  NVL(installment_amt, 0) installment_amt
  ,  NVL(installment_amt_gross , 0) installment_amt_gross
  ,  NVL(reg_installment_amt , 0) reg_installment_amt
  ,  NVL(reg_installment_amt_gross , 0) reg_installment_amt_gross
  ,  NVL(avg_installment_amt , 0) avg_installment_amt
  ,  NVL(avg_installment_amt_gross , 0) avg_installment_amt_gross
  ,  NVL(remaining_installments, 0) remaining_installments
  ,  NVL(overdue_installments, 0) overdue_installments
  ,  NVL(rec_drawdown_amt , 0) rec_drawdown_amt
  ,  NVL(rec_drawdown_amt_gross, 0) rec_drawdown_amt_gross
  FROM DUAL;
END AXSP_GET_CONTRACT_SUMMARY;
/
CREATE OR REPLACE PROCEDURE AXSP_GET_NEXT_NO(
p_table_name   IN NVARCHAR2  DEFAULT NULL,
p_increment    IN NUMBER  DEFAULT 1,
p_log_id       IN NUMBER  DEFAULT 0,
RC1            IN OUT globalPkg.RCT1) AS
--
  next_no       NUMBER;
  StoO_error    INTEGER;
  StoO_errmsg   VARCHAR2(255);
BEGIN
  next_no := axsp_get_next_no_internal(p_table_name, p_increment, p_log_id);
  OPEN RC1 FOR SELECT next_no FROM DUAL;
END AXSP_GET_NEXT_NO;
/
CREATE OR REPLACE PROCEDURE AXSP_GET_PARTY_ACC_OPEN_BAL(
p_party_account_id IN NUMBER  DEFAULT NULL,
p_currency_id      IN NUMBER  DEFAULT NULL,
p_run_dt           IN DATE  DEFAULT NULL,
RC1                IN OUT globalPkg.RCT1) AS
--
  v_balance            NUMBER(18,4);
  v_temp	       NUMBER;
  v_temp2              NUMBER;
  v_balance_dt         DATE;
--initialize balance and balance dt
BEGIN
  v_balance := 0;
  v_balance_dt := axsp_datemin();
--first get the closest balance from statements
  FOR rec IN ( SELECT   balance,  balance_dt
               FROM statement_bal
               WHERE party_account_id = p_party_account_id
               and currency_id = p_currency_id
               and balance_dt = (
                  SELECT  MAX(balance_dt)
                  FROM statement_bal
                  WHERE party_account_id = p_party_account_id
                  and currency_id = p_currency_id
                  and balance_dt <= p_run_dt )) LOOP
    v_balance := rec.balance ;
    v_balance_dt := rec.balance_dt ;
  END LOOP;
--get the opening balance for the run date for given party account and ccy
--
  select nvl(sum(amt_gross), 0) into v_temp
  from axvw_party_account_flow
  where actual_dt < p_run_dt and actual_dt >= v_balance_dt
  and statement_id = 0
  and party_account_id = p_party_account_id
  and currency_id = p_currency_id
  and flow_source = 'C';
--
  select nvl(sum(amt_gross), 0) into v_temp2
  from axvw_party_account_flow
  where actual_dt < p_run_dt and actual_dt >= v_balance_dt
  and statement_id = 0
  and party_account_id = p_party_account_id
  and currency_id = p_currency_id
  and flow_source = 'B';
--
  v_balance := v_balance + (v_temp - v_temp2);
--
  OPEN RC1 FOR SELECT v_balance FROM DUAL;
END AXSP_GET_PARTY_ACC_OPEN_BAL;
/
CREATE OR REPLACE PROCEDURE AXSP_GET_PARTY_CREDIT_SUMMARY(
p_party_id IN NUMBER  DEFAULT NULL,
RC1       IN OUT globalPkg.RCT1) AS
--
  FXRate         NUMBER := 0;
  CreditCurrency NUMBER;
  CreditLimit    NUMBER(18,4);
  CreditUtilized NUMBER(18,4);
  CreditAvail    NUMBER(18,4);
  CurrentDate    DATE := axsp_dateonly(axsp_get_datetime());
BEGIN
  FOR rec in (select system_setting_value
  from system_setting where system_setting_type = 8402
  and rownum = 1) LOOP
  FxRate := to_number(rec.system_setting_value);
  END LOOP;
--
  for rec in (SELECT amount, currency_id
              FROM credit_info WHERE party_id = p_party_id) loop
    CreditLimit := rec.amount;
    CreditCurrency := rec.currency_id;
  end loop;
  execute immediate 'truncate table TP_FLOW_SUMMARY';
  INSERT INTO TP_FLOW_SUMMARY
    SELECT  SUM(f.amount), f.currency_id
    FROM flow f, party_account pa, party p
    WHERE f.party_account_id = pa.party_account_id
    and pa.party_id = p.party_id
    and flow_type  IN (1003, 1007)
    and is_cash = 1
	and f.is_shadow_copy = 0
    and reversal_status = 4200
    and status != 2099
    and f.contract_id >= 0
    and p.party_id = p_party_id
    and f.amt_gross != f.amt_matched
    GROUP BY currency_id;
  --get the cross rates
  SELECT SUM(fs.amount * xr.rate) INTO CreditUtilized
   FROM TP_FLOW_SUMMARY fs, TABLE(AXSP_GET_CROSS_RATES (FXRate, CurrentDate)) xr
   WHERE currency_id2 = CreditCurrency
   and fs.currency_id = xr.currency_id1;
  CreditAvail :=  ( NVL(CreditLimit, 0) - NVL(CreditUtilized, 0) );
  OPEN RC1 FOR
    SELECT p_party_id party_id,
           NVL(CreditLimit, 0) credit_limit,
           NVL(CreditUtilized, 0) credit_utilised,
           NVL(CreditAvail, 0) credit_avail
    FROM DUAL;
END AXSP_GET_PARTY_CREDIT_SUMMARY;
/
CREATE OR REPLACE PROCEDURE AXSP_GL_BAL_GET_ACCOUNT(
p_account_id IN NUMBER  DEFAULT NULL,
asAtDt       IN DATE  DEFAULT NULL,
RC1          IN OUT globalPkg.RCT1) AS
--
  tempVar1    NUMBER;
  tempVar2    NUMBER;
  baseFXRate  NUMBER := 0;
  baseCcyId   NUMBER := 0;
BEGIN
--
 FOR rec in (select system_setting_value
  from system_setting where system_setting_type = 8402
  and rownum = 1) LOOP
  baseFxRate := to_number(rec.system_setting_value);
  END LOOP;
--
  FOR rec in (select l.base_ccy_id
  from ledger l, gl_account a
  where l.ledger_id = a.ledger_id
  and a.gl_account_id = p_account_id
  and rownum = 1) LOOP
  baseCcyId := rec.base_ccy_id;
  END LOOP;
--
  DELETE FROM TP_GL_ACCOUNT_BALANCE_TABLE;
-- get and store latest balances into a table variable
  INSERT INTO TP_GL_ACCOUNT_BALANCE_TABLE
    SELECT DISTINCT  a.gl_account_id,
      CASE WHEN b.balance IS null THEN 0 ELSE b.balance END,
      0,
      CASE WHEN b.balance_dt IS null THEN to_date('01-jan-1900', 'dd-mon-yyyy')
        ELSE b.balance_dt END
   from gl_account a
   left outer join gl_account_balance b on a.gl_account_id = b.gl_account_id and b.balance_dt < asAtDt
   where (b.balance_dt is null or
          b.balance_dt = (select max(i.balance_dt)
                          from gl_account_balance i
                          where i.gl_account_id = a.gl_account_id
                          and i.balance_dt < asAtDt))
   and a.gl_account_id = p_account_id; --extra filter to axsp_get_all_gl_bal
--extra filter to axsp_get_all_gl_bal
--select balances
  tempVar1 := 0;
  tempVar2 := 0;
  for rec in (SELECT t.balance, t.base_balance
    FROM TP_GL_ACCOUNT_BALANCE_TABLE t
    WHERE t.gl_account_id = p_account_id and rownum = 1) LOOP
    tempVar1 := rec.balance;
    tempVar2 := rec.base_balance;
    END LOOP;
 --
  OPEN RC1 FOR
    SELECT a.gl_account_id,
           asAtDt as_at_dt,
           CASE WHEN SUM(e.amount) IS null THEN 0
             ELSE SUM(e.amount) END + tempVar1 amt_bal,
           CASE WHEN SUM(e.amount) IS null THEN 0
             ELSE SUM(e.amount) * r.rate  END + (tempVar2 * r.rate) base_amt_bal,
           0 stamp
    from table(cast(axsp_get_cross_rates(baseFXRate, asAtDt) as CROSS_RATES_TBLTAB)) r, gl_account a
    inner join ledger l on l.ledger_id = a.ledger_id
    left outer join TP_GL_ACCOUNT_BALANCE_TABLE t on a.gl_account_id = t.gl_account_id
    left outer join axvw_gl_entry e on a.gl_account_id = e.gl_account_id
    and e.post_dt <= asAtDt
    and e.post_dt > t.balance_dt
    where a.gl_account_id = p_account_id --extra filter to axsp_get_all_gl_bal
    and a.currency_id = r.currency_id1
    and r.currency_id2 = baseCcyId
    group by a.gl_account_id, r.rate;
--extra filter to axsp_get_all_gl_bal
END AXSP_GL_BAL_GET_ACCOUNT;
/
CREATE OR REPLACE PROCEDURE AXSP_GL_BAL_GET_ALL(
asAtDt IN DATE  DEFAULT NULL,
RC1    IN OUT globalPkg.RCT1) AS
--
BEGIN
  DELETE FROM TP_GL_ACCOUNT_BALANCE_TABLE_1;
--get and store latest balances into a table variable
  INSERT INTO TP_GL_ACCOUNT_BALANCE_TABLE_1
       SELECT DISTINCT  a.gl_account_id,
           CASE WHEN b.balance IS null THEN 0 ELSE b.balance END,
           CASE WHEN b.balance_dt IS null
             THEN to_date('01-jan-1900', 'dd-mon-yyyy')
             ELSE b.balance_dt END
    FROM gl_account a inner join ledger l on l.ledger_id = a.ledger_id
      left outer join gl_account_balance b on a.gl_account_id = b.gl_account_id and b.balance_dt < asAtDt
    WHERE b.balance_dt is null or b.balance_dt = (
        SELECT MAX(i.balance_dt)
        FROM gl_account_balance i
        WHERE i.gl_account_id = a.gl_account_id
        and i.balance_dt < asAtDt);
--select balances
 OPEN RC1 FOR
    SELECT a.gl_account_id,
           asAtDt as_at_dt,
           CASE WHEN SUM(e.amount) IS null THEN 0
             ELSE SUM(e.amount) END + nvl(t.balance, 0) amt_bal,
           0 base_amt_bal,
           0 stamp
    from gl_account a inner join ledger l on l.ledger_id = a.ledger_id
    left outer join TP_GL_ACCOUNT_BALANCE_TABLE_1 t on a.gl_account_id = t.gl_account_id
    left outer join axvw_gl_entry e on a.gl_account_id = e.gl_account_id
    and e.post_dt <= asAtDt
    and e.post_dt > t.balance_dt
    group by a.gl_account_id, asAtDt, nvl(t.balance, 0), 0;
END AXSP_GL_BAL_GET_ALL;
/
CREATE OR REPLACE PROCEDURE AXSP_GL_BAL_GET_LEDGER(
p_ledger_id IN NUMBER  DEFAULT NULL,
asAtDt      IN DATE  DEFAULT NULL,
RC1         IN OUT globalPkg.RCT1) AS
--
BEGIN
  DELETE FROM TP_GL_ACCOUNT_BALANCE_TABLE_2;
--get and store latest balances into a table variable
  INSERT INTO TP_GL_ACCOUNT_BALANCE_TABLE_2
    SELECT DISTINCT a.gl_account_id,
           CASE WHEN b.balance IS null THEN 0 ELSE b.balance END,
           CASE WHEN b.balance_dt IS null
             THEN to_date('01-jan-1900', 'dd-mon-yyyy')
             ELSE b.balance_dt END
    from gl_account a INNER JOIN ledger l ON l.ledger_id = a.ledger_id
		 LEFT OUTER JOIN gl_account_balance b ON a.gl_account_id = b.gl_account_id and b.balance_dt < asAtDt
    where (b.balance_dt is null
           or b.balance_dt = (select max(i.balance_dt)
                              from gl_account_balance i
                              where i.gl_account_id = a.gl_account_id
                              and i.balance_dt < asAtDt))
    and l.ledger_id = p_ledger_id;
--extra filter to axsp_get_all_gl_bal
--select balances
  OPEN RC1 FOR
    SELECT a.gl_account_id,
           asAtDt as_at_dt,
           CASE WHEN SUM(e.amount) IS null THEN 0
             ELSE SUM(e.amount) END +  nvl(t.balance, 0) amt_bal,
           0 base_amt_bal,
           0 stamp
    from gl_account a inner join ledger l on l.ledger_id = a.ledger_id
    left outer join TP_GL_ACCOUNT_BALANCE_TABLE_2 t on a.gl_account_id = t.gl_account_id
    left outer join axvw_gl_entry e on a.gl_account_id = e.gl_account_id
    and e.post_dt <= asAtDt
    and e.post_dt > t.balance_dt
    where l.ledger_id = p_ledger_id --extra filter to axsp_get_all_gl_bal
    GROUP BY a.gl_account_id, nvl(t.balance, 0), 0;
END AXSP_GL_BAL_GET_LEDGER;
/
CREATE OR REPLACE PROCEDURE AXSP_GL_BAL_UPD(
p_period_det_id IN NUMBER  DEFAULT NULL,
RC1             IN OUT globalPkg.RCT1) AS
--
  StoO_error        INTEGER;
  StoO_errmsg       VARCHAR2(255);
  asAtDt            DATE;
  tempVar1          VARCHAR2(255);
  status_code       NUMBER;
  v_date_min		DATE;
  TYPE NumTab IS TABLE OF TP_GL_ACCOUNT_BALANCE_TABLE_3.gl_account_id%TYPE;
  TYPE DateTab IS TABLE OF DATE;
  v_gl_account_id_tab NumTab;   -- No need to initialize the collections.
  v_date_tab DateTab;  -- Values will be filled in by the SELECT INTO.
--use the given period end date as the balance calculation date
BEGIN
  v_date_min := axsp_datemin();
  --
  FOR rec IN (SELECT d.end_dt
              FROM actg_period_det d
              WHERE d.actg_period_det_id = p_period_det_id) LOOP
    asAtDt := rec.end_dt;
  END LOOP;
  --
  execute immediate 'truncate table TP_GL_ACCOUNT_BALANCE_TABLE_3';
--get and store latest balances into a table variable
  INSERT INTO TP_GL_ACCOUNT_BALANCE_TABLE_3
    SELECT gl_account_id, balance, balance_dt
    FROM (SELECT DISTINCT  a.gl_account_id, b.balance, b.balance_dt, rank() over (partition by a.gl_account_id order by balance_dt desc) row_no
          FROM actg_period_det d, actg_period p,
               gl_account a INNER JOIN ledger l ON (l.ledger_id = a.ledger_id)
               INNER JOIN gl_account_balance b ON (a.gl_account_id = b.gl_account_id and b.balance_dt < asAtDt)
          WHERE d.actg_period_id = p.actg_period_id
          AND p.actg_period_id = l.actg_period_id
          AND b.balance_dt < asAtDt
          AND d.actg_period_det_id = p_period_det_id)
    WHERE row_no = 1
    UNION
    SELECT DISTINCT  a.gl_account_id, 0, v_date_min
    FROM actg_period_det d, actg_period p,
         gl_account a INNER JOIN ledger l ON (l.ledger_id = a.ledger_id)
    WHERE d.actg_period_id = p.actg_period_id
    AND p.actg_period_id = l.actg_period_id
    AND NOT EXISTS (select * from gl_account_balance b
      where a.gl_account_id = b.gl_account_id AND b.balance_dt < asAtDt)
    AND d.actg_period_det_id = p_period_det_id;
--delete existing balances
  select distinct gl_account_id, asAtDt bulk collect into v_gl_account_id_tab, v_date_tab
  FROM TP_GL_ACCOUNT_BALANCE_TABLE_3;
  --
  FORALL j IN v_gl_account_id_tab.FIRST..v_gl_account_id_tab.LAST
    DELETE FROM gl_account_balance WHERE gl_account_id = v_gl_account_id_tab(j) AND balance_dt = v_date_tab(j);
--insert new balances
  INSERT INTO gl_account_balance (
    balance_dt,
    gl_account_id,
    balance,
    stamp)
  SELECT asAtDt, a.gl_account_id, nvl(SUM(e.amount),0) + nvl(t.balance, 0), 1
  FROM actg_period_det d,
    actg_period p,
    gl_account a INNER JOIN
    ledger l ON (l.ledger_id = a.ledger_id) LEFT OUTER JOIN
    TP_GL_ACCOUNT_BALANCE_TABLE_3 t ON (a.gl_account_id = t.gl_account_id) LEFT OUTER JOIN
    axvw_gl_entry e ON (a.gl_account_id = e.gl_account_id AND e.post_dt <= asAtDt AND e.post_dt > t.balance_dt)
  WHERE  d.actg_period_id = p.actg_period_id
   AND p.actg_period_id = l.actg_period_id
   AND d.actg_period_det_id = p_period_det_id
  GROUP BY a.gl_account_id, nvl(t.balance, 0);
  --
--
--Clear temp table and insert latest balances to check sum for that period only
  execute immediate 'truncate table TP_GL_ACCOUNT_BALANCE_TABLE_3';
  INSERT INTO TP_GL_ACCOUNT_BALANCE_TABLE_3
    SELECT gl_account_id, balance, balance_dt
    FROM (SELECT DISTINCT  a.gl_account_id, b.balance, b.balance_dt, rank() over (partition by a.gl_account_id order by balance_dt desc) row_no
          FROM gl_account a INNER JOIN ledger l ON (l.ledger_id = a.ledger_id)
               INNER JOIN gl_account_balance b ON (a.gl_account_id = b.gl_account_id and b.balance_dt < asAtDt)
          WHERE b.balance_dt < asAtDt)
    WHERE row_no = 1
    UNION
    SELECT DISTINCT  a.gl_account_id, 0, v_date_min
    FROM gl_account a INNER JOIN ledger l ON (l.ledger_id = a.ledger_id)
    WHERE NOT EXISTS (select * from gl_account_balance b
      where a.gl_account_id = b.gl_account_id AND b.balance_dt < asAtDt);
--
  execute immediate 'truncate table TP_GL_SUM_BALANCE_TABLE';
  INSERT INTO TP_GL_SUM_BALANCE_TABLE
    SELECT l.ledger_id, l.base_ccy_id, SUM(b.balance)
    FROM gl_account a inner join ledger l on (l.ledger_id = a.ledger_id)
         inner join TP_GL_ACCOUNT_BALANCE_TABLE_3 b on (a.gl_account_id = b.gl_account_id)
    GROUP BY l.ledger_id, l.base_ccy_id;
--
  status_code := 0;
  SELECT case when count(*) = 0 then 0 else 1 end into status_code
  FROM TP_GL_SUM_BALANCE_TABLE
  WHERE balance <> 0 ;
--
  OPEN RC1 FOR SELECT  status_code status_code FROM DUAL;
END AXSP_GL_BAL_UPD;
/

CREATE OR REPLACE PROCEDURE AXSP_GL_ENTRY_INS(
p_gl_entry_id   IN NUMBER  DEFAULT NULL,
p_contract_id   IN NUMBER  DEFAULT NULL,
p_image_no   IN NUMBER  DEFAULT NULL,
p_flow_id   IN NUMBER  DEFAULT NULL,
p_tax_flow_id   IN NUMBER  DEFAULT NULL,
p_input_dt   IN DATE  DEFAULT NULL,
p_post_dt   IN DATE  DEFAULT NULL,
p_ledger_id   IN NUMBER  DEFAULT NULL,
p_business_unit_id   IN NUMBER  DEFAULT NULL,
p_gl_account_id   IN NUMBER  DEFAULT NULL,
p_currency_id   IN NUMBER  DEFAULT NULL,
p_amount   IN NUMBER  DEFAULT NULL,
p_base_currency_id   IN NUMBER  DEFAULT NULL,
p_base_amount   IN NUMBER  DEFAULT NULL,
p_fx_rate   IN NUMBER  DEFAULT NULL,
p_fx_rate_dt   IN DATE  DEFAULT NULL,
p_gl_interface_id   IN NUMBER  DEFAULT NULL,
p_gl_tag_id   IN NUMBER  DEFAULT NULL,
p_narration   IN NVARCHAR2  DEFAULT NULL,
p_reversal_status   IN NUMBER  DEFAULT NULL,
p_flow_type   IN NUMBER  DEFAULT NULL,
p_gl_entry_type   IN NUMBER  DEFAULT NULL,
p_bank_flow_id   IN NUMBER  DEFAULT NULL,
p_calc_dt   IN DATE  DEFAULT NULL,
p_asset_hdr_id   IN NUMBER  DEFAULT NULL,
p_purchase_invoice_id   IN NUMBER  DEFAULT NULL,
p_contract_actg_cf_id   IN NUMBER  DEFAULT NULL,
p_cost_centre   IN NUMBER  DEFAULT NULL,
p_branch_id   IN NUMBER  DEFAULT NULL,
p_gl_journal_id   IN NUMBER  DEFAULT NULL,
p_nett_no   IN NUMBER  DEFAULT NULL,
p_stamp   IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO gl_entry (
    gl_entry_id,
    contract_id,
    image_no,
    flow_id,
    tax_flow_id,
    input_dt,
    post_dt,
    ledger_id,
    business_unit_id,
    gl_account_id,
    currency_id,
    amount,
    base_currency_id,
    base_amount,
    fx_rate,
    fx_rate_dt,
    gl_interface_id,
    gl_tag_id,
    narration,
    reversal_status,
    flow_type,
    gl_entry_type,
    bank_flow_id,
    calc_dt,
    asset_hdr_id,
    purchase_invoice_id,
    contract_actg_cf_id,
    cost_centre,
    branch_id,
    gl_journal_id,
	 nett_no,
    stamp)
  VALUES (
    p_gl_entry_id,
    p_contract_id,
    p_image_no,
    p_flow_id,
    p_tax_flow_id,
    p_input_dt,
    p_post_dt,
    p_ledger_id,
    p_business_unit_id,
    p_gl_account_id,
    p_currency_id,
    p_amount,
    p_base_currency_id,
    p_base_amount,
    p_fx_rate,
    p_fx_rate_dt,
    p_gl_interface_id,
    p_gl_tag_id,
    p_narration,
    p_reversal_status,
    p_flow_type,
    p_gl_entry_type,
    p_bank_flow_id,
    p_calc_dt,
    p_asset_hdr_id,
    p_purchase_invoice_id,
    p_contract_actg_cf_id,
    p_cost_centre,
	 p_branch_id,
	 p_gl_journal_id,
	 p_nett_no,
    p_stamp);
END AXSP_GL_ENTRY_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_PARTY_SEARCH(
TextFind               IN NVARCHAR2  DEFAULT NULL,
TextFindNameFields     IN NUMBER  DEFAULT NULL,
TextFindEmailAddresses IN NUMBER  DEFAULT NULL,
TextFindCustomFields   IN NUMBER  DEFAULT NULL,
TextFindReference      IN NUMBER  DEFAULT NULL,
NumberFind             IN NVARCHAR2  DEFAULT NULL,
NumberFindPartyNumber  IN NUMBER  DEFAULT NULL,
NumberFindPhoneNumbers IN NUMBER  DEFAULT NULL,
AddressFind            IN NVARCHAR2  DEFAULT NULL,
MoreFind               IN NVARCHAR2  DEFAULT NULL,
PartyType              IN NUMBER DEFAULT NULL,
ProgramId              IN INTEGER DEFAULT 0,
PartyRole              IN INTEGER DEFAULT 0,
PartyClassification    IN INTEGER DEFAULT 0,
ContainFlag            IN NUMBER DEFAULT NULL,
DealerId               IN INTEGER DEFAULT 0,
PartyDetFilter         IN NVARCHAR2  DEFAULT NULL,
RC1                    IN OUT globalPkg.RCT1) AS
--
  TextFind_                NVARCHAR2(1024) := TextFind;
  TextFindNameFields_      NUMBER(1,0) := TextFindNameFields;
  TextFindEmailAddresses_  NUMBER(1,0) := TextFindEmailAddresses;
  TextFindCustomFields_    NUMBER(1,0) := TextFindCustomFields;
  TextFindReference_       NUMBER(1,0) := TextFindReference;
  NumberFind_              NVARCHAR2(1024) := NumberFind;
  NumberFindPartyNumber_   NUMBER(1,0) := NumberFindPartyNumber;
  NumberFindPhoneNumbers_  NUMBER(1,0) := NumberFindPhoneNumbers;
  AddressFind_             NVARCHAR2(1024) := AddressFind;
  MoreFind_                NVARCHAR2(1024) := MoreFind;
  PartyType_               NUMBER := PartyType;
  PartyClassification_	   NUMBER := PartyClassification;
  ContainFlag_					NUMBER(1,0) := ContainFlag;
  StoO_error               INTEGER;
  StoO_errmsg              VARCHAR2(255);
  SqlQuery                 VARCHAR2(10000);
  TextFindFlag             NUMBER(1,0);
  NumberFindFlag           NUMBER(1,0);
  PartyTypeIn              NVARCHAR2(100);
  partyClassificationIn	   NVARCHAR2(100);
  PartyId				      INTEGER;
  ProgramSql			      NVARCHAR2(3000);
  ChildList				      NVARCHAR2(2000);
  IndividualSearchText_	   NVARCHAR2(255);
  IndividualTextSearchSql_	      NVARCHAR2(3000);
  v_string_tbl             TEMP_SPLIT_TBL := TEMP_SPLIT_TBL();
  ContainWildcard		      NVARCHAR2(10);
  v_party_id_csv           NVARCHAR2(1000);

  CURSOR cur_program_party_w_children(p_programId INTEGER, p_partyRole INTEGER) is
	SELECT party_id FROM program_party
	WHERE program_id = p_programId AND party_role = p_partyRole and is_include_children = 1;

	PARTY_ID_TBL PARTY_CHILD_TBLOBJ := PARTY_CHILD_TBLOBJ (null);

--TODO
  v_pos number;
  row_limit CONSTANT INTEGER := 500;
	pm_count NUMBER := 0;
BEGIN
--
  SqlQuery := 'select * from (select party.party_id,
party.name,
party.ext_name as full_name,
party_address.street as full_address,
party_address.suburb as suburb,
city_location.name city_name,
county_location.name county_name,
state_province_location.name state_province_name,
party_address.zip_code zip_code,
country_region_location.name country_region_name,
party.date_of_birth,
party.business_ref_no,
party_address.address_type,
party_address.is_current,
party.party_type,
party.business_individual,
party.reference,
party.party_no,
party.tax_no,
party.ext_name,
party.gender,
party.trading_as,
lookup.value as entity_type,
party.business_ref_no2,
party.business_ref_no3,
case when email_business is not null and email_business <> '''' then email_business else email_home end as email_address,
''None'' status,
0 external_system,
party.stamp
from party inner join party_det on party.party_id = party_det.party_id
left outer join party_address on party.party_id = party_address.party_id
left outer join location city_location on party_address.city_id = city_location.location_Id
left outer join location county_location on party_address.county_id = county_location.location_Id
left outer join location state_province_location on party_address.state_province_id = state_province_location.location_Id
inner join lookupset lookup on party.entity_type = lookup.lookupset_Id
left outer join location country_region_location on party_address.country_region_id = country_region_location.location_Id';
--
  ContainWildCard := '';
  IF (ContainFlag_ = 1) THEN
    ContainWildCard := '%';
  END IF;
--
--
  IF ( PartyClassification_ > 0) THEN
	PartyClassificationIn := cast(PartyClassification_ as NVARCHAR2);
	SqlQuery := SqlQuery || ' where party.business_individual = ' || PartyClassification_ || ' and party.party_id != 0 and party.is_deleted = 0 and rownum <= '||row_limit;
  ELSE
	SqlQuery := SqlQuery || ' where party.party_id != 0 and party.is_deleted = 0 and rownum <= '||row_limit;
  END IF;
 --
  IF ( PartyType_ > 0) THEN
    PartyTypeIn := cast(PartyType_ as NVARCHAR2);
    IF (isnull(len(rtrim(ltrim(PartyDetFilter))), 0) != 0) THEN
      SqlQuery := SqlQuery || ' and ( ' || PartyDetFilter || ' ) ';
    END IF;
  END IF;
--
  IndividualSearchText_ := TextFind_;
  TextFind_ := AXSP_NORMALIZE_SEARCH_TERM(TextFind_);
  NumberFind_ := AXSP_NORMALIZE_SEARCH_TERM(NumberFind_);
  AddressFind_ := AXSP_NORMALIZE_SEARCH_TERM(AddressFind_);
  MoreFind_ := AXSP_NORMALIZE_SEARCH_TERM(MoreFind_);
-- Begin Text Find Code
  TextFindFlag := 0;
  IF  ( TextFindNameFields_ = 1) THEN
    IF  ( TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || ' (lower(party.ext_name) like ''' || ContainWildcard || TextFind_ || '%'' or lower(party.name) like ''' || ContainWildcard || TextFind_ || '%'' or lower(party.first_names) like '''|| ContainWildcard || TextFind_ || '%'' or lower(party.trading_as) like ''' || ContainWildcard || TextFind_ || '%''';
    IF ( ContainFlag = 1 AND INSTR(IndividualSearchText_, ' -') > 0) THEN
	  v_string_tbl := axsp_split(IndividualSearchText_,' -');
	  IF v_string_tbl.COUNT > 0 then
	    FOR j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
		  IndividualTextSearchSql_ := IndividualTextSearchSql_ || ' or lower(party.ext_name) like ''%' || v_string_tbl(j)  || '%'' or lower(party.name) like ''%' || v_string_tbl(j) || '%'' or lower(party.first_names) like ''%'|| v_string_tbl(j) || '%'' or lower(party.trading_as) like ''%' || v_string_tbl(j) || '%''';
		END LOOP;
		v_string_tbl.delete;
	  END IF;
	  SqlQuery := SqlQuery || ' ' || IndividualTextSearchSql_ ;
	END IF;
	SqlQuery := SqlQuery || ') ';
  END IF;
--
  IF  ( TextFindEmailAddresses_ = 1) THEN
    IF  ( TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || ' (lower(party.email_home) like '''|| ContainWildcard || TextFind_ || '%'' or lower(party.email_business) like ''' || ContainWildCard || TextFind_ || '%'')';
  END IF;
--
--
  IF  ( TextFindCustomFields_ = 1) THEN
    IF  ( TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || ' EXISTS (SELECT 1 FROM party_cfv cfv WHERE cfv.party_id = party.party_id AND lower(cfv.field_value) like ''' || ContainWildcard || TextFind_ || '%'')';
  END IF;
  -- End Text Find Code
  --Reference Find
  IF  ( TextFindReference_ = 1) THEN
    IF  ( TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || '  (lower(party.reference) like  ''' || ContainWildcard || TextFind_ || '%'' or lower(party.business_ref_no) like ''' || ContainWildcard || TextFind_ || '%'' or lower(party.business_ref_no2) like ''' || ContainWildcard || TextFind_ || '%'')';
  END IF;
    --End of Reference Find
  IF  ( TextFindFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
-- Begin Number Find Code
  END IF;
--
--
  NumberFindFlag := 0;
  IF  ( NumberFindPartyNumber_ = 1) THEN
    IF  ( NumberFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      NumberFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || ' (lower(party.party_no) like ''' || ContainWildcard || NumberFind_ || '%'')';
  END IF;
--
--
  IF  ( NumberFindPhoneNumbers_ = 1) THEN
    IF  ( NumberFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      NumberFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
	SqlQuery := SqlQuery || ' (lower(party.phone_home) like ''' || ContainWildcard || NumberFind_ || '%'' or lower(party.phone_business) like ''' || ContainWildcard || NumberFind_ ||
		'%'' or lower(party.phone_mobile) like '''|| ContainWildcard || NumberFind_ || '%'' or lower(party.fax_home) like '''|| ContainWildcard || NumberFind_ || 
		'%'' or lower(party.fax_business) like ''' || ContainWildcard || NumberFind_ || '%'' or lower(party.phone_mobile_search) like '''|| ContainWildcard || NumberFind_ || 
		'%'' or lower(party.phone_home_search) like '''|| ContainWildcard || NumberFind_ || '%'' or lower(party.phone_business_search) like '''|| ContainWildcard || NumberFind_ || '%'')';
  END IF;
--
--
  IF  ( NumberFindFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
-- End Number Find Code
-- Begin Address Find Code
  END IF;
--
--
  IF  ( LEN(LTRIM(RTRIM(AddressFind_))) > 0) THEN
    SqlQuery := SqlQuery || ' and (';
	SqlQuery := SqlQuery || ' (lower(party_address.zip_code) like ''' || ContainWildcard || AddressFind_ || '%'' or lower(party_address.street) like ''' || ContainWildcard || AddressFind_
					|| '%'' or lower(party_address.suburb) like ''' || ContainWildcard || AddressFind_
					|| '%'' or lower(city_location.ext_name) like ''' || ContainWildcard || AddressFind_
					|| '%'' or lower(county_location.ext_name) like ''' || ContainWildcard || AddressFind_
					|| '%'' or lower(state_province_location.ext_name) like ''' || ContainWildcard || AddressFind_
					|| '%'' or lower(country_region_location.ext_name) like ''' || ContainWildcard || AddressFind_ || '%'')';
    SqlQuery := SqlQuery || ')';
  ELSE
    SqlQuery := SqlQuery || ' and (party_address.is_current = 1 or party_address.is_current is null)';
-- End Address Find Code
-- Begin More Find Code
  END IF;
--
--
  IF  ( LEN(LTRIM(RTRIM(MoreFind_))) > 0) THEN
    SqlQuery := SqlQuery || ' and (';
	SqlQuery := SqlQuery || ' (lower(city_location.ext_name) like ''' || ContainWildcard || MoreFind_
					|| '%'' or lower(county_location.ext_name) like ''' || ContainWildcard || MoreFind_
					|| '%'' or lower(state_province_location.ext_name) like ''' || ContainWildcard || MoreFind_
					|| '%'' or lower(country_region_location.ext_name) like ''' || ContainWildcard || MoreFind_ || '%'')';
    SqlQuery := SqlQuery || ')';
-- End More Find Code
  END IF;
--
--
--Programs filter
    SELECT count(*)
    INTO pm_count
    FROM program_party
    WHERE program_id = ProgramId
					AND party_role = PartyRole
					AND is_include = 1;
	ProgramSql := ' ';
	IF (ProgramId != 0
		AND axsp_isbitand(PartyTypeIn, 8192) = 0 -- Permit Holder does not have Members filtering.  BUGZID:18875
		AND PartyRole != 27300 -- Search for all parties (outside the programm) if party role is not specified (BugzId: 22491)
		AND pm_count > 0)  -- Search for particular party's role and type within program membership if there is any.
								 -- Otherwise, search for any party of given type. (BugzId: 22491)
	THEN
		ProgramSql := 'party.party_id IN (SELECT party_id FROM program_party WHERE is_include = 1 AND party_role = ' || cast(PartyRole as nvarchar2) || ' AND program_id = ' || cast(ProgramId as nvarchar2) || ')';
		FOR rec IN cur_program_party_w_children(ProgramId, PartyRole) LOOP
			INSERT INTO PARTY_ID_TBL(party_id) (SELECT party_id FROM TABLE(CAST(axsp_get_party_child_tbl(rec.party_id) AS PARTY_CHILD_TBLTAB)));
		END LOOP;
		v_party_id_csv := '';
		FOR rec IN ( SELECT DISTINCT party_id  FROM PARTY_ID_TBL) LOOP
			v_party_id_csv := v_party_id_csv||CAST(rec.party_id as VARCHAR2) || ',';
		END LOOP;
		IF (v_party_id_csv IS NOT NULL) THEN
			v_party_id_csv := SUBSTRING(v_party_id_csv, 1, length(v_party_id_csv) - 1);
			ProgramSql := ProgramSql || ' or party.party_id  IN ('||v_party_id_csv||')';
		END IF;
	END IF;
--
	IF (ProgramSql != ' ') THEN
		SqlQuery := SqlQuery||' and ('||ProgramSql||')';
	END IF;


-- Dealer
	IF (DealerId > 0) THEN
   SqlQuery := SqlQuery ||
             ' and EXISTS (SELECT 1 FROM contract c WHERE c.dealer_id = ' || cast(DealerId as nvarchar2) ||
             ' and (party.party_id = c.cparty_id OR ' ||
             ' EXISTS (SELECT 1 FROM guarantee g WHERE g.contract_id = c.contract_id AND g.guarantor_party_id = party.party_id)))' ;
	END IF;

  SqlQuery := SqlQuery||') where rownum <= '||row_limit;
--
--

  BEGIN
  	OPEN RC1 FOR SqlQuery;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
END AXSP_PARTY_SEARCH;
/
CREATE OR REPLACE PROCEDURE AXSP_PURCHASE_INVOICE_SEARCH(
TextFind               IN NVARCHAR2  DEFAULT NULL,
InvoiceDtFrom          IN DATE DEFAULT NULL,
InvoiceDtTo            IN DATE DEFAULT NULL,
TextFindNameFields     IN NUMBER  DEFAULT NULL,
TextFindEmailAddresses IN NUMBER  DEFAULT NULL,
TextFindCustomFields   IN NUMBER  DEFAULT NULL,
TextFindReference      IN NUMBER  DEFAULT NULL,
RC1                    IN OUT globalPkg.RCT1) AS
--
  TextFind_                NVARCHAR2(1024) := TextFind;
  InvoiceDtFrom_           DATE := InvoiceDtFrom;
  InvoiceDtTo_             DATE := InvoiceDtTo;
  TextFindNameFields_      NUMBER(1,0) := TextFindNameFields;
  TextFindEmailAddresses_  NUMBER(1,0) := TextFindEmailAddresses;
  TextFindCustomFields_    NUMBER(1,0) := TextFindCustomFields;
  TextFindReference_       NUMBER(1,0) := TextFindReference;
  StoO_error               INTEGER;
  StoO_errmsg              VARCHAR2(255);
  SqlQuery                 VARCHAR2(4000);
  TextFindFlag             NUMBER(1,0);
BEGIN
--
  SqlQuery := 'select pi.purchase_invoice_id, pi.reference, pi.supplier_id, pi.invoice_dt, pi.business_unit_id, pi.branch_id, pi.currency_id, 
	(NVL(pir.amt_row,0) + NVL(pir_cf.amt_row_cf,0)) amt_row,
	(NVL(pir.amt_row,0) + NVL(pir_cf.amt_row_cf,0) + NVL(pir.amt_row_tax, 0) + NVL(pir_cf.amt_row_cf_tax, 0)) amt_row_gross,
	NVL(f.amt_flow,0) amt_flow,
	NVL(f.amt_flow_gross,0) amt_flow_gross,
	pi.purchase_invoice_type, pi.is_active
	from purchase_invoice pi
	left outer join (
		select purchase_invoice_row.purchase_invoice_id, sum(purchase_invoice_row.amount) amt_row, sum(pi_tax.amt_row_tax) amt_row_tax
		from purchase_invoice_row
		left outer join (select purchase_invoice_tax.purchase_invoice_row_id, sum(purchase_invoice_tax.amount) amt_row_tax from purchase_invoice_tax group by purchase_invoice_tax.purchase_invoice_row_id) pi_tax on purchase_invoice_row.purchase_invoice_row_id = pi_tax.purchase_invoice_row_id
		group by purchase_invoice_row.purchase_invoice_id) pir on pi.purchase_invoice_id = pir.purchase_invoice_id
	left outer join (
		select purchase_invoice_row_cf.purchase_invoice_id, sum(purchase_invoice_row_cf.amount) amt_row_cf, sum(pi_tax_cf.amt_row_cf_tax) amt_row_cf_tax
		from purchase_invoice_row_cf
		left outer join (select purchase_invoice_tax.purchase_invoice_row_cf_id, sum(purchase_invoice_tax.amount) amt_row_cf_tax from purchase_invoice_tax group by purchase_invoice_tax.purchase_invoice_row_cf_id) pi_tax_cf on purchase_invoice_row_cf.purchase_invoice_row_cf_id = pi_tax_cf.purchase_invoice_row_cf_id
		group by purchase_invoice_row_cf.purchase_invoice_id) pir_cf on pi.purchase_invoice_id = pir_cf.purchase_invoice_id
	left outer join (
		select flow.purchase_invoice_id, sum(flow.amount) amt_flow, sum(flow.amt_gross) amt_flow_gross
		from flow
		where flow.purchase_invoice_id != 0 and is_funding_transfer = 0 group by flow.purchase_invoice_id) f on pi.purchase_invoice_id = f.purchase_invoice_id
	inner join party on pi.supplier_id = party.party_id';
--
  SqlQuery := SqlQuery || ' where pi.invoice_dt between to_date('''||To_Char(InvoiceDtFrom_,'YYYY-MM-DD HH24:MI:SS') || ''',''YYYY-MM-DD HH24:MI:SS'')
  and to_date('''||To_Char(InvoiceDtTo_,'YYYY-MM-DD HH24:MI:SS')||''',''YYYY-MM-DD HH24:MI:SS'')
  and party.party_id != 0 and party.is_deleted = 0 and pi.purchase_invoice_id != 0';
--
  TextFind_ := AXSP_NORMALIZE_SEARCH_TERM(TextFind_);
  -- Begin Text Find Code
  TextFindFlag := 0;
  IF  (TextFindNameFields_ = 1) THEN
    IF  (TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(party.ext_name) like ''%' || TextFind_ || '%'' or lower(party.name) like ''%' || TextFind_ || '%'' or lower(party.first_names) like ''%' || TextFind_ || '%'')';
  END IF;
--
  IF  (TextFindEmailAddresses_ = 1) THEN
    IF  (TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(party.email_home) like ''%' || TextFind_ || '%'' or lower(party.email_business) like ''%' || TextFind_ || '%'')';
  END IF;
--
--
  IF  (TextFindCustomFields_ = 1) THEN
    IF  (TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || ' (lower(party_cfv.field_value) like ''%' || TextFind_ || '%'')';
  END IF;
  -- End Text Find Code
  --Reference Find
  IF  ( TextFindReference_ = 1) THEN
    IF  ( TextFindFlag = 0) THEN
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag := 1;
    ELSE
      SqlQuery := SqlQuery || ' or';
    END IF;
    SqlQuery := SqlQuery || '  (lower(party.reference) like ''%' || TextFind_ || '%'')';
  END IF;
    --End of Reference Find
  IF  ( TextFindFlag = 1) THEN
    SqlQuery := SqlQuery || ')';
-- Begin Number Find Code
  END IF;
--
-- We need to ensure the order by purchase_invoice_id as the C# depends on it when showing the end results
  SqlQuery := Sqlquery || ' order by pi.purchase_invoice_id ';
--
  BEGIN
 	OPEN RC1 FOR SqlQuery;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
END AXSP_PURCHASE_INVOICE_SEARCH;
/
create or replace PROCEDURE axsp_update_party_det (p_party_id IN number) AS
--
BEGIN
  DELETE FROM party_det WHERE party_id = p_party_id;
  INSERT INTO party_det (party_id,
                         curr_street_address_id,
                         curr_mail_address_id,
                         party_type_csv,
                         is_business_unit,
                         is_direct_customer,
                         is_vendor,
                         is_bank,
                         is_insurance,
                         is_manufacturer,
                         is_tax_authority,
                         is_end_customer,
                         is_broker,
                         is_dealer,
                         is_registration_authority,
                         is_funder,
                         is_branch,
                         is_permit_holder,
                         is_repossession_agency,
						 is_courier,
						 is_searches_agent,
                         is_life_company,
                         is_medical_assessor,
                         is_staff,
                         primary_party_contact_id,
                         stamp)
  SELECT DISTINCT p.party_id,
          nvl(pas.party_address_id, Nvl(pam.party_address_id, 0)),
          nvl(pam.party_address_id, Nvl(pas.party_address_id, 0)),
          nvl(axsp_party_type_csv(p.party_type),' '),
          axsp_isbitand(p.party_type, 1),
          axsp_isbitand(p.party_type, 2),
          axsp_isbitand(p.party_type, 4),
          axsp_isbitand(p.party_type, 8),
          axsp_isbitand(p.party_type, 16),
          axsp_isbitand(p.party_type, 32),
          axsp_isbitand(p.party_type, 64),
          axsp_isbitand(p.party_type, 128),
          axsp_isbitand(p.party_type, 256),
          axsp_isbitand(p.party_type, 512),
          axsp_isbitand(p.party_type, 1024),
          axsp_isbitand(p.party_type, 2048),
          axsp_isbitand(p.party_type, 4096),
          axsp_isbitand(p.party_type, 8192),
          axsp_isbitand(p.party_type, 16384),
          axsp_isbitand(p.party_type, 32768),
		  axsp_isbitand(p.party_type, 65536),
          axsp_isbitand(p.party_type, 131072),
		  axsp_isbitand(p.party_type, 262144),
		  axsp_isbitand(p.party_type, 524288),
          nvl(pc.party_contact_id, 0),
          0
  FROM party p
  LEFT OUTER JOIN party_address pas ON p.party_id = pas.party_id and pas.is_current = 1 and pas.address_type = 5000
  LEFT OUTER JOIN party_address pam ON p.party_id = pam.party_id and pam.is_current = 1 and pam.address_type = 5001
  LEFT OUTER JOIN party_contact pc  ON p.party_id = pc.party_id and pc.is_primary_contact = 1
  WHERE p.party_id = p_party_id;
END axsp_update_party_det;
/
CREATE OR REPLACE PROCEDURE AXSP_SELECT_ASSETS_FOR_DEPR(
p_run_dt IN DATE  DEFAULT NULL,
p_do_gl  IN NUMBER DEFAULT NULL,
RC1      IN OUT globalPkg.RCT1) AS
--
BEGIN
  OPEN RC1 FOR
select distinct ah.asset_hdr_id from asset_hdr ah, asset_depr ad where ah.asset_hdr_id = ad.asset_hdr_id and ah.asset_status > 43000
and ad.depreciation_state != 35105 -- dont include inactive
and ((p_do_gl != 1 and (( ad.depreciation_method != 3800 and ad.last_depreciation_calc_dt <= p_run_dt
and (ad.last_depreciation_calc_dt != ad.stop_depreciation_dt or ad.depreciation_state = 35104)) or
( ad.tax_depreciation_method != 3800 and ad.tax_last_depreciation_calc_dt <= p_run_dt
and (ad.tax_last_depreciation_calc_dt != ad.stop_depreciation_dt or ad.depreciation_state = 35104))))
or (p_do_gl > 0 and is_post_to_gl = 1 and ((ad.depreciation_state = 35102 and ad.save_status<2) or
( ad.depreciation_method != 3800 and ad.last_depreciation_post_dt <= p_run_dt
and ad.last_depreciation_post_dt != ad.stop_depreciation_dt))));
END AXSP_SELECT_ASSETS_FOR_DEPR;
/
CREATE OR REPLACE PROCEDURE AXSP_SELECT_ASSETS_FOR_XFER(
p_run_dt IN DATE  DEFAULT NULL,
RC1      IN OUT globalPkg.RCT1) AS
--
BEGIN
  OPEN RC1 FOR
  select distinct ah.asset_hdr_id from asset_hdr ah , asset_depr ad where ah.asset_hdr_id = ad.asset_hdr_id
  and ah.asset_status > 43000 and ad.is_post_to_gl = 1  and
  ((ad.depreciation_state = 35102 or ad.amt_reval_adj != 0 or ad.sec_amt_reval_adj != 0) and  (ad.save_status < 1 or (ad.save_status = 1 and ad.stop_depreciation_dt <= p_run_dt)));
END AXSP_SELECT_ASSETS_FOR_XFER;

/
CREATE OR REPLACE PROCEDURE AXSP_SELECT_CONTRACTS_FOR_ACCR(
p_run_dt IN DATE  DEFAULT NULL,
p_ledger_id IN INT DEFAULT NULL,
RC1      IN OUT globalPkg.RCT1) AS
--
  suspensionAccrualAction_ INTEGER;
--
BEGIN
  SELECT suspension_accrual_action INTO suspensionAccrualAction_ FROM system_defs;
  OPEN RC1 FOR
   select distinct ca.contract_id
	from contract c, contract_actg ca, contract_suspension cs
	where ca.accrual_actg_flag = 1 and
	c.contract_id = ca.contract_id and
	cs.contract_id = c.contract_id and
	c.save_status = 2203
   and c.contract_state >= 11130  --only complete activated or above contracts are valid for accruals
   and c.contract_id > 0
   and p_run_dt >= ca.last_accrual_dt
   and (ca.ledger_id = p_ledger_id or p_ledger_id = 0)
	and ( exists
			(select * from contract_actg_cf caf where caf.contract_id = ca.contract_id  and
           p_run_dt > caf.last_accrual_dt and ((caf.last_accrual_dt < caf.accrual_end_dt and not( ca.early_end_dt > TO_DATE('01-jan-1900','dd-mon-yyyy') and caf.custom_flow_hdr_id =0 and caf.last_accrual_dt > ca.early_end_dt )) or caf.last_accrual_amt_np != 0)
           and not (c.suspension_state >= 22502 and caf.custom_flow_hdr_id =0 and 
                caf.last_accrual_dt = (select cwo.calc_dt from contract_write_off cwo where cwo.contract_id = ca.contract_id  )))
		or ((c.suspension_state >= 22502 or (ca.early_end_dt > TO_DATE('01-jan-1900','dd-mon-yyyy') ) )
			 and exists (select * from contract_actg_cf caf where caf.contract_id = ca.contract_id
			and caf.last_accrual_dt < caf.accrual_end_dt and caf.custom_flow_hdr_id >0 ))) --custom flows accrue after termination	and writeoff
	and not ((c.suspension_state = 22501 OR c.intercept_state = 40801) and ca.last_accrual_dt = cs.suspended_from_dt and suspensionAccrualAction_ = 40950)
   and (c.product_style = 2007 or contract_grp_style != 18201);
END AXSP_SELECT_CONTRACTS_FOR_ACCR;
/
CREATE OR REPLACE PROCEDURE AXSP_SELECT_NOTE_VIEW_PARTNERS(
p_partyId      IN NUMBER  DEFAULT NULL,
p_activityArea IN NUMBER  DEFAULT NULL,
p_activityType IN NUMBER  DEFAULT NULL,
RC1            IN OUT globalPkg.RCT1) AS
--
BEGIN
  OPEN RC1 FOR
    SELECT *
    FROM axvw_note_view
    WHERE party_id = p_partyId
    and (p_activityArea = 0 or activity_area = p_activityArea)
    and (p_activityType = 0 or activity_type = p_activityType)
    UNION ALL
    SELECT a.*
	  from party_contact c
	  inner join axvw_note_view a ON c.party_id = a.party_id
    where c.contact_id = p_partyId and c.contact_type = 5609
    and (p_activityArea = 0 or a.activity_area = p_activityArea) 
    and (p_activityType = 0 or a.activity_type = p_activityType) 
    ORDER BY 12 DESC;
END AXSP_SELECT_NOTE_VIEW_PARTNERS;
/
create or replace
PROCEDURE axsp_select_tax_type (RC1      IN OUT globalPkg.RCT1)
AS
BEGIN
/****** Used from India Tax Web Service ******/
OPEN RC1 FOR
SELECT
	TAX_TYPE_HDR.TAX_TYPE_HDR_ID,
	TAX_TABLE_HDR.CODE,
	TAX_TYPE_HDR.REFERENCE,
	TAX_TYPE_DET.EFFECT_DT,
	TAX_TABLE_DET.RATE
FROM
	TAX_TABLE_HDR
	INNER JOIN
      TAX_TABLE_DET
ON
      TAX_TABLE_HDR.TAX_TABLE_HDR_ID = TAX_TABLE_DET.TAX_TABLE_HDR_ID
INNER JOIN
	TAX_TYPE_HDR
ON
	TAX_TYPE_HDR.TAX_TABLE_HDR_ID = TAX_TABLE_HDR.TAX_TABLE_HDR_ID
INNER JOIN
	TAX_TYPE_DET
ON
	TAX_TYPE_HDR.TAX_TYPE_HDR_ID = TAX_TYPE_DET.TAX_TYPE_HDR_ID
ORDER BY   TAX_TABLE_HDR.CODE;
END axsp_select_tax_type;
/
create or replace
PROCEDURE axsp_select_tax_type_filtered (RC1      IN OUT globalPkg.RCT1)
AS
BEGIN
/****** Used from India Tax Web Service ******/
OPEN RC1 FOR
SELECT
TAX_TABLE_HDR.CODE,
      TAX_TYPE_HDR.REFERENCE,
      TAX_TABLE_DET.RATE,
      TAX_TYPE_HDR.TAX_TYPE_HDR_ID,
      ASSET_TYPE.NAME AS ASSET_NAME,
      ASSET_TYPE.CODE AS ASSET_CODE

FROM  TAX_TYPE_DET

INNER JOIN TAX_TYPE_HDR
ON TAX_TYPE_HDR.TAX_TYPE_HDR_ID = TAX_TYPE_DET.TAX_TYPE_HDR_ID

INNER JOIN TAX_TABLE_HDR
ON TAX_TYPE_HDR.TAX_TABLE_HDR_ID = TAX_TABLE_HDR.TAX_TABLE_HDR_ID

INNER JOIN TAX_TABLE_DET
ON TAX_TABLE_DET.TAX_TABLE_HDR_ID = TAX_TABLE_HDR.TAX_TABLE_HDR_ID

INNER JOIN TAX_ASSET_TYPE_FILTER
ON TAX_ASSET_TYPE_FILTER.TAX_TYPE_DET_ID = TAX_TYPE_DET.TAX_TYPE_DET_ID

INNER JOIN ASSET_TYPE
ON TAX_ASSET_TYPE_FILTER.ASSET_TYPE_ID = ASSET_TYPE.ASSET_TYPE_ID

WHERE TAX_TYPE_HDR.REFERENCE LIKE '%_FILTERED';
END axsp_select_tax_type_filtered;
/
CREATE OR REPLACE PROCEDURE AXSP_SET_SETTLEMENT_BANK_INFO(
p_settlement_bank_info_id   IN NUMBER  DEFAULT NULL) AS
--
  StoO_error                INTEGER;
  StoO_errmsg               VARCHAR2(255);
--Reset existing default settlement instructions if there is a new instruction effective on run date
BEGIN
  BEGIN
  StoO_error   := 0;
  UPDATE settlement_bank_info
    SET is_default = 0,
        stamp = stamp + 1
    WHERE settlement_bank_info_id IN
      (
        SELECT  sb_old.settlement_bank_info_id
        FROM settlement_bank_info sb_old, settlement_bank_info sb_new
        WHERE sb_new.settlement_bank_info_id = p_settlement_bank_info_id
        AND sb_new.is_active = 1
        AND sb_old.flow_direction = sb_new.flow_direction
        AND sb_old.currency_id = sb_new.currency_id
        AND sb_old.party_id = sb_new.party_id
        AND sb_old.is_active = 1
        AND sb_old.is_default = 1
        AND sb_old.effect_dt = TO_DATE('01-jan-1900','dd-mon-yyyy')
      );
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--Reset effective dated default settlement instructions
  BEGIN
  StoO_error   := 0;
  UPDATE settlement_bank_info
    SET is_default = 1,
        effect_dt = TO_DATE('01-jan-1900','dd-mon-yyyy'),
        stamp = stamp + 1
    WHERE is_active = 1
    AND settlement_bank_info_id = p_settlement_bank_info_id;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
END AXSP_SET_SETTLEMENT_BANK_INFO;
/
CREATE OR REPLACE PROCEDURE AXSP_STATEMENT_RERUN(
p_statement_id IN NUMBER  DEFAULT NULL,
RC1            IN OUT globalPkg.RCT1) AS
--
--flows
BEGIN
  OPEN RC1 FOR
    SELECT f.*
    FROM flow f
    WHERE f.statement_id = p_statement_id;
--bank flows
  OPEN RC1 FOR
    SELECT f.*
    FROM bank_flow f
    WHERE f.statement_id = p_statement_id;
--openning balance
  OPEN RC1 FOR
    SELECT *
    FROM (
           SELECT b2.*
           FROM statement_bal b1 inner JOIN statement_bal b2
           ON b1.party_account_id = b2.party_account_id
           WHERE b1.currency_id = b2.currency_id
           and b1.balance_dt > b2.balance_dt
           and b1.statement_id = p_statement_id
           ORDER BY b2.balance_dt DESC)
    WHERE ROWNUM <= 1 ;
--closing balance
  OPEN RC1 FOR
    SELECT * FROM (
                    SELECT *
                    FROM statement_bal b
                    WHERE b.statement_id = p_statement_id
                  )
    WHERE ROWNUM <= 1 ;
END AXSP_STATEMENT_RERUN;
/
CREATE OR REPLACE PROCEDURE AXSP_STATEMENT_RUN
(asAtDt  IN DATE  DEFAULT NULL,
 RC1     IN OUT globalPkg.RCT1) AS
--
  StoO_error       INTEGER;
  StoO_errmsg      VARCHAR2(255);
  current_date     DATE;
  v_statement_run_no NUMBER;
  tempVar1         VARCHAR2(255) :='statement_run_no';
  tempVar2         NUMBER;
--
  CURSOR UF1_cursor IS
    SELECT f.ROWID, s.statement_id
    FROM party_account a, actg_period_det d, flow f, statement s
    WHERE a.statement_schedule_id = d.actg_period_id
          AND a.party_account_id = f.party_account_id
          AND a.party_account_id = s.party_account_id
    AND (d.end_dt > a.last_statement_dt
         and d.end_dt <= asAtDt
         and f.contract_id >= 0
	     and f.status not in (2099, 2104, 2105) --not Projected, WrittenOff
		 and f.is_shadow_copy = 0
         and f.is_cash = 1
         and f.statement_id = 0
         and d.start_dt = s.statement_start_dt
         and d.end_dt = s.statement_end_dt
         and f.exclude_from_account_bal = 0 --exclude recovery flows
         and
        (
        (f.actual_dt >= d.start_dt
         and f.actual_dt <= d.end_dt)
         or
        (f.actual_dt <= (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )
         and d.start_dt = (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    ))))
    FOR UPDATE OF f.statement_id;
--
  CURSOR UF2_cursor IS
    SELECT f.ROWID, s.statement_id
    FROM party_account a, actg_period_det d, bank_flow f, statement s
    WHERE (a.statement_schedule_id = d.actg_period_id)
    AND (a.party_account_id = f.party_account_id)
    AND (a.party_account_id = s.party_account_id)
    AND (d.end_dt > a.last_statement_dt
         and d.end_dt <= asAtDt
         and f.contract_id >= 0
         and f.statement_id = 0
         and f.is_deleted = 0
         and f.is_shadow_copy = 0
         and d.start_dt = s.statement_start_dt
         and d.end_dt = s.statement_end_dt
         and
        (
        (f.actual_dt >= d.start_dt
         and f.actual_dt <= d.end_dt)
         or
        (f.actual_dt <= (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )
         and d.start_dt = (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    ))))
    FOR UPDATE OF f.statement_id;
--
  CURSOR UF3_cursor IS
    SELECT a.ROWID, t1.end_dt_4 last_statement_dt, s.statement_no + 1 next_statement_no
     FROM party_account a, TP_TMP_TABLE_4 t1, statement s
        WHERE (a.party_account_id = t1.party_account_id_4) AND (a.party_account_id
    = s.party_account_id) AND
      (t1.end_dt_4 = (
        SELECT  MAX(end_dt_4)
         FROM TP_TMP_TABLE_4 t2
        WHERE a.party_account_id = t2.party_account_id_4    )
         and s.statement_no = (
        SELECT  MAX(statement_no)
         FROM statement s2
        WHERE a.party_account_id = s2.party_account_id    ))
    FOR UPDATE OF a.last_statement_dt, a.next_statement_no;
--
  CURSOR UF4_cursor IS
    SELECT TP_TMP_TABLE_1_1.ROWID, ( CASE
  WHEN b.balance IS null THEN 0
  ELSE b.balance
  END ) opening_balance_1
     FROM statement_bal b, TP_TMP_TABLE_1_1
        WHERE party_account_id_1 = b.party_account_id
         and currency_id_1 = b.currency_id
         and b.balance_dt = (
        SELECT  MAX(balance_dt)
         FROM statement_bal b1
        WHERE b.party_account_id = b1.party_account_id
         and b.currency_id = b1.currency_id    )
    FOR UPDATE OF TP_TMP_TABLE_1_1.opening_balance_1;
--
  CURSOR UF5_cursor IS
    SELECT TP_TMP_TABLE_1_1.ROWID, balance_2 balance_1
     FROM TP_TMP_TABLE_2_1, TP_TMP_TABLE_1_1
        WHERE party_account_id_1 = party_account_id_2
         and currency_id_1 = currency_id_2
         and start_dt_1 = start_dt_2
         and end_dt_1 = end_dt_2
    FOR UPDATE OF TP_TMP_TABLE_1_1.balance_1;
--
  CURSOR UF6_cursor IS
    SELECT TP_TMP_TABLE_1_1.ROWID, balance_1 - balance_3 balance_1
     FROM TP_TMP_TABLE_3, TP_TMP_TABLE_1_1
        WHERE party_account_id_1 = party_account_id_3
         and currency_id_1 = currency_id_3
         and start_dt_1 = start_dt_3
         and end_dt_1 = end_dt_3
    FOR UPDATE OF TP_TMP_TABLE_1_1.balance_1;
--
  CURSOR UF7_cursor IS
    SELECT TP_TMP_TABLE_1_1.ROWID, opening_balance_1 + balance_0 running_balance_1
     FROM TP_TMP_TABLE_0, TP_TMP_TABLE_1_1
        WHERE party_account_id_1 = party_account_id_0
         and currency_id_1 = currency_id_0
         and start_dt_1 = start_dt_0
         and end_dt_1 = end_dt_0
    FOR UPDATE OF TP_TMP_TABLE_1_1.running_balance_1;
BEGIN
  DELETE FROM TP_TMP_TABLE_0;
  DELETE FROM TP_TMP_TABLE_1_1;
  DELETE FROM TP_TMP_TABLE_2_1;
  DELETE FROM TP_TMP_TABLE_3;
  DELETE FROM TP_TMP_TABLE_4;
--
  current_date := axsp_dateonly(axsp_get_datetime());
--insure an entry exists for all accounts per currency per period
--==========================================================================
--the resulting table contains a record for each account for each currency for
--each accounting periond with starting statement number and zero balance
--only accounts with a previous balance or new flows are included
--
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_TMP_TABLE_1_1 (
    party_account_id_1,
    currency_id_1,
    start_dt_1,
    end_dt_1,
    next_no_1,
    balance_1)
  SELECT
    a.party_account_id,
    (CASE WHEN f.currency_id IS null THEN b.currency_id ELSE f.currency_id END),
    d.start_dt,
    d.end_dt,
    a.next_statement_no,
    0
  FROM party_account a inner JOIN actg_period_det d
  ON a.statement_schedule_id = d.actg_period_id left JOIN flow f
  ON a.party_account_id = f.party_account_id left JOIN statement_bal b
  ON a.party_account_id = b.party_account_id
  WHERE d.end_dt > a.last_statement_dt
  and d.end_dt <= asAtDt
  and f.contract_id >= 0
  and (f.currency_id IS NOT NULL or b.currency_id IS NOT NULL)
  GROUP BY a.party_account_id,
           (CASE WHEN f.currency_id IS null THEN b.currency_id
              ELSE f.currency_id END ),
           d.start_dt,
           d.end_dt,
           a.next_statement_no;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--selects accounting period between the the last statement date and the current date
--get sum of expected flows per accoutnt per currency since the last statement
--=============================================================================
--sums flows since last account statement till accounting period date
--
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_TMP_TABLE_2_1 (
    party_account_id_2,
    currency_id_2,
    start_dt_2,
    end_dt_2,
    next_no_2,
    balance_2)
  SELECT
    a.party_account_id,
    f.currency_id,
    d.start_dt,
    d.end_dt,
    a.next_statement_no,
    SUM(f.amt_gross)
  FROM party_account a inner JOIN actg_period_det d
  ON a.statement_schedule_id = d.actg_period_id inner JOIN flow f
  ON a.party_account_id = f.party_account_id
  WHERE d.end_dt > a.last_statement_dt
  and d.end_dt <= asAtDt
  and f.statement_id = 0
  and f.status not in (2099, 2104, 2105) --not Projected, WrittenOff
  and f.is_shadow_copy = 0
  and f.is_cash = 1
  and f.exclude_from_account_bal = 0 --exclude recovery flows
  and f.contract_id >= 0
  and (
        (f.actual_dt >= d.start_dt
         and f.actual_dt <= d.end_dt)
         or
        (f.actual_dt <= (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )
         and d.start_dt = (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )))
  GROUP BY a.party_account_id, f.currency_id, d.start_dt, d.end_dt,
           a.next_statement_no;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--selects accounting period between the the last statement date and the current date
--not Projected
--selects cash flows including reversals
--selects flows that are expected within the duration of the accounting period
--select flows that are expected before the accounting period
--get sum of bank flows per accoutnt per currency since the last statement
--===============================================================================================================
--sums flows since last account statement till accounting period date
--
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_TMP_TABLE_3 (
    party_account_id_3,
    currency_id_3,
    start_dt_3,
    end_dt_3,
    next_no_3,
    balance_3)
  SELECT
    a.party_account_id,
    f.currency_id,
    d.start_dt,
    d.end_dt,
    a.next_statement_no,
    SUM(f.amount)
  FROM party_account a inner JOIN actg_period_det d
  ON a.statement_schedule_id = d.actg_period_id inner JOIN bank_flow f
  ON a.party_account_id = f.party_account_id
  WHERE d.end_dt > a.last_statement_dt
  and d.end_dt <= asAtDt
  and f.statement_id = 0 and f.is_deleted = 0 and f.is_shadow_copy = 0
  and (
        (f.actual_dt >= d.start_dt
         and f.actual_dt <= d.end_dt)
         or
        (f.actual_dt <= (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )
         and d.start_dt = (
        SELECT  MIN(d1.start_dt)
         FROM actg_period_det d1
        WHERE d1.actg_period_id = d.actg_period_id    )))
  GROUP BY a.party_account_id, f.currency_id, d.start_dt, d.end_dt,
           a.next_statement_no;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--selects accounting period between the the last statement date and the current date
--selects flows that are expected within the duration of the accounting period
--select flows that are expected before the accounting period
--calculate statement numbers
--==============================================================================
--insert statements and calculate their statement number
--==============================================================================
  BEGIN
  v_statement_run_no:=axsp_get_next_no_internal(tempVar1);
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
  StoO_error   := 0;
  INSERT INTO TP_TMP_TABLE_4 (
    party_account_id_4,
    currency_id_4,
    start_dt_4,
    end_dt_4,
    next_no_4,
    balance_4)
  SELECT
    t1.party_account_id_1,
    0,
    t1.start_dt_1,
    t1.end_dt_1,
    MAX(t1.next_no_1),
    0
  FROM TP_TMP_TABLE_1_1 t1
  GROUP BY t1.party_account_id_1, t1.start_dt_1, t1.end_dt_1
  ORDER BY t1.party_account_id_1 , t1.start_dt_1 , t1.end_dt_1;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
    StoO_error   := 0;
    SELECT MIN(t1.id_4)
    INTO tempVar2
    FROM TP_TMP_TABLE_4 t1, TP_TMP_TABLE_4 t2
    WHERE t1.party_account_id_4 = t2.party_account_id_4;
  EXCEPTION
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
    StoO_error   := 0;
    INSERT INTO statement
    SELECT
      S_STATEMENT.nextval,
      t1.next_no_4 + rownum - 1,
      v_statement_run_no,
      current_date,
      8500,
      t1.start_dt_4,
      t1.end_dt_4,
      t1.party_account_id_4,
      1
    FROM TP_TMP_TABLE_4 t1;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--update flows data
--==============================================================================
--update flow statement id
  FOR i in UF1_cursor LOOP
    UPDATE flow f
      SET statement_id = i.statement_id
      WHERE CURRENT OF UF1_cursor;
  END LOOP;
--join on account id
--selects accounting period between the the last statement date and the current date
--not Projected
--selects cash flows
--selects cash flows that have not been attached to a statement
--selects flows that are expected within the duration of the accounting period
--select flows that are expected before the accounting period
--update bank flow statement id
  FOR i in UF2_cursor LOOP
    UPDATE bank_flow f
      SET statement_id = i.statement_id
      WHERE CURRENT OF UF2_cursor;
  END LOOP;
--join on account id
--selects accounting period between the the last statement date and the current date
--selects cash flows that have not been attached to a statement
--selects flows that are expected within the duration of the accounting period
--select flows that are expected before the accounting period
--update accounts statement no and last statement date
--============================================================================
  FOR i in UF3_cursor LOOP
    UPDATE party_account a
      SET last_statement_dt = i.last_statement_dt,
          next_statement_no = i.next_statement_no
      WHERE CURRENT OF UF3_cursor;
  END LOOP;
--update balances
--==============================================================================
  FOR i in UF4_cursor LOOP
    UPDATE TP_TMP_TABLE_1_1
      SET opening_balance_1 = i.opening_balance_1
      WHERE CURRENT OF UF4_cursor;
  END LOOP;
--
  FOR i in UF5_cursor LOOP
    UPDATE TP_TMP_TABLE_1_1
      SET balance_1 = i.balance_1
      WHERE CURRENT OF UF5_cursor;
  END LOOP;
--
  FOR i in UF6_cursor LOOP
    UPDATE TP_TMP_TABLE_1_1
      SET balance_1 = i.balance_1
      WHERE CURRENT OF UF6_cursor;
  END LOOP;
--
  BEGIN
    StoO_error   := 0;
    INSERT INTO TP_TMP_TABLE_0 (
      party_account_id_0,
      currency_id_0,
      start_dt_0,
      end_dt_0,
      balance_0)
    SELECT
      a.party_account_id_1,
      a.currency_id_1,
      a.start_dt_1,
      a.end_dt_1,
      SUM(b.balance_1)
    FROM TP_TMP_TABLE_1_1 a, TP_TMP_TABLE_1_1 b
    WHERE b.end_dt_1 <= a.end_dt_1
    and a.party_account_id_1 = b.party_account_id_1
    and a.currency_id_1 = b.currency_id_1
    GROUP BY a.party_account_id_1, a.currency_id_1, a.start_dt_1,
             a.end_dt_1;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--
  FOR i in UF7_cursor LOOP
    UPDATE TP_TMP_TABLE_1_1
      SET running_balance_1 = i.running_balance_1
      WHERE CURRENT OF UF7_cursor;
  END LOOP;
--
  BEGIN
    StoO_error   := 0;
    INSERT INTO statement_bal
    SELECT
      S_STATEMENT_BAL.nextval,
      s.statement_id,
      party_account_id_1,
      currency_id_1,
      running_balance_1,
      end_dt_1,
      1
    FROM TP_TMP_TABLE_1_1, statement s
    WHERE party_account_id_1 = s.party_account_id
    AND start_dt_1 = s.statement_start_dt
    and end_dt_1 = s.statement_end_dt;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
  END;
--feedback to caller
--
--==============================================================================
  StoO_error  := 0;
  OPEN RC1 FOR SELECT  v_statement_run_no run_no FROM DUAL;
END AXSP_STATEMENT_RUN;
/
CREATE OR REPLACE PROCEDURE AXSP_STMT_RERUN_BY_PARTY_ACC(
p_party_account_id  IN NUMBER  DEFAULT NULL,
p_statement_no      IN NUMBER  DEFAULT NULL,
RC1_CALL            IN OUT globalPkg.RCT1 ) AS
--
  v_statement_id     NUMBER;
BEGIN
  FOR rec IN ( SELECT s.statement_id
               FROM statement s
               WHERE s.party_account_id = p_party_account_id
               and s.statement_no = p_statement_no) LOOP
    v_statement_id := rec.statement_id ;
  END LOOP;
  axsp_statement_rerun(v_statement_id, RC1 => RC1_CALL);
END AXSP_STMT_RERUN_BY_PARTY_ACC;
/
CREATE OR REPLACE PROCEDURE AXSP_TAX_FLOWI_INS(
  image_key_1        IN NUMBER  DEFAULT NULL,
  tax_flow_id_2      IN NUMBER  DEFAULT NULL,
  contract_id_3      IN NUMBER  DEFAULT NULL,
  flow_id_4          IN NUMBER  DEFAULT NULL,
  image_no_5         IN NUMBER  DEFAULT NULL,
  tax_type_hdr_id_6  IN NUMBER  DEFAULT NULL,
  tax_authority_id_7 IN NUMBER  DEFAULT NULL,
  amount_8           IN NUMBER  DEFAULT NULL,
  amt_matched_9      IN NUMBER  DEFAULT NULL,
  amt_invoice_10     IN NUMBER  DEFAULT NULL,
  amt_financed_11    IN NUMBER  DEFAULT NULL,
  tax_rate_12        IN NUMBER  DEFAULT NULL,
  source_flow_id_13  IN NUMBER  DEFAULT NULL,
  stamp_14           IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO tax_flowi (
    tax_flowi_id,
    image_key,
    tax_flow_id,
    contract_id,
    flow_id,
    image_no,
    tax_type_hdr_id,
    tax_authority_id,
    amount,
    amt_matched,
    amt_invoice,
    amt_financed,
    tax_rate,
    source_flow_id,
    stamp)
  VALUES (
    s_tax_flowi.nextval,
    image_key_1,
    tax_flow_id_2,
    contract_id_3,
    flow_id_4,
    image_no_5,
    tax_type_hdr_id_6,
    tax_authority_id_7,
    amount_8,
    amt_matched_9,
    amt_invoice_10,
    amt_financed_11,
    tax_rate_12,
    source_flow_id_13,
    stamp_14);
END AXSP_TAX_FLOWI_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_TAX_FLOW_DEL(
tax_flow_id_1   IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  DELETE  tax_flow
  WHERE tax_flow_id = tax_flow_id_1;
END AXSP_TAX_FLOW_DEL;
/
CREATE OR REPLACE PROCEDURE AXSP_TAX_FLOW_INS(
  contract_id_1      IN NUMBER  DEFAULT NULL,
  flow_id_2          IN NUMBER  DEFAULT NULL,
  image_no_3         IN NUMBER  DEFAULT NULL,
  tax_type_hdr_id_4  IN NUMBER  DEFAULT NULL,
  tax_authority_id_5 IN NUMBER  DEFAULT NULL,
  amount_6           IN NUMBER  DEFAULT NULL,
  amt_matched_7 	   IN NUMBER  DEFAULT NULL,
  amt_invoice_8 	   IN NUMBER  DEFAULT NULL,
  amt_financed_9 	   IN NUMBER  DEFAULT NULL,
  tax_rate_10 	      IN NUMBER  DEFAULT NULL,
  source_flow_id_11  IN NUMBER  DEFAULT NULL,
  stamp_12           IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  INSERT INTO tax_flow (
    tax_flow_id,
    contract_id,
    flow_id,
    image_no,
    tax_type_hdr_id,
    tax_authority_id,
    amount,
    amt_matched,
    amt_invoice,
    amt_financed,
    tax_rate,
    source_flow_id,
    stamp)
  VALUES (
    s_tax_flow.nextval,
    contract_id_1,
    flow_id_2,
    image_no_3,
    tax_type_hdr_id_4,
    tax_authority_id_5,
    amount_6,
    amt_matched_7,
    amt_invoice_8,
    amt_financed_9,
    tax_rate_10,
    source_flow_id_11,
    stamp_12);
END AXSP_TAX_FLOW_INS;
/
CREATE OR REPLACE PROCEDURE axsp_location_search
(
  TextFind IN NVARCHAR2 DEFAULT NULL,
  TextFindNameFields IN NUMBER DEFAULT NULL,
  TextFindDescription IN NUMBER DEFAULT NULL,
  TextFindCode IN NUMBER DEFAULT NULL,
  ProgramId IN INTEGER DEFAULT 0,
  LocationType IN INTEGER DEFAULT 0,
  ContainFlag IN INTEGER DEFAULT 0,
  RC1     IN OUT globalPkg.RCT1
) AS
--
  SqlQuery VARCHAR2(4000);
  TextFindFlag_ NUMBER(1,0);
  TextFind_ NVARCHAR2(1024) := TextFind;
  Count_ INTEGER;
  ProgramSql VARCHAR2(3000);
  row_limit CONSTANT INTEGER := 500;
  IndividualSearchText_	   NVARCHAR2(255);
  IndividualTextSearchSql_	      NVARCHAR2(3000);
  v_string_tbl             TEMP_SPLIT_TBL := TEMP_SPLIT_TBL();
  LocationType_     NVARCHAR2(100);
  ContainWildcard   NVARCHAR2(10);

  CURSOR cur_program_list_with_children(p_program_id INTEGER) IS
	SELECT value_id FROM program_list
	WHERE list_type = 27507 AND program_id = p_program_id and has_inherited_children = 1;

BEGIN
  SqlQuery := 'select location.location_id, location.name, loc_owner.name owner, loc_type.value loc_type_name, ccy.code currency, location.location_type, location.stamp
from location
inner join lookupset loc_type on location.location_type = loc_type.lookupset_id inner join currency ccy on location.currency_id = ccy.currency_id
inner join location loc_owner on location.owner_id = loc_owner.location_id';
--
  SqlQuery := SqlQuery || ' where location.location_id > 0 and location.is_active = 1
and location.is_draft = 0 and location.location_type in (';
  IF (LocationType = 0) THEN
    SqlQuery := SqlQuery || '4402,4403,4404,4405) and rownum <= '||row_limit;
  ELSE
    LocationType_ := cast(LocationType as NVARCHAR2);
	SqlQuery := SqlQuery || LocationType_ ||') and rownum <= '||row_limit;
  END IF;

--
  IndividualSearchText_ := TextFind_;
  TextFind_ := AXSP_NORMALIZE_SEARCH_TERM(TextFind_);

  ContainWildCard := '';
  IF (ContainFlag = 1) THEN
    ContainWildCard := '%';
  END IF;
--
-- Begin Text Find Code
  TextFindFlag_ := 0;
  if (TextFindNameFields = 1) then
    if (TextFindFlag_ = 0) then
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag_ := 1;
    else
      SqlQuery := SqlQuery || ' or';
    end if;
    SqlQuery := SqlQuery || ' (lower(location.name) like ''' || ContainWildCard || TextFind_ || '%''';
	if (ContainFlag = 1) then
	  v_string_tbl := axsp_split(IndividualSearchText_,' -');
	  if v_string_tbl.COUNT > 0 then
	    FOR j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
		  IndividualTextSearchSql_ := IndividualTextSearchSql_ || ' or lower(location.name) like ''%' || v_string_tbl(j)  || '%''';
		END LOOP;
		v_string_tbl.delete;
	  end if;
	  SqlQuery := SqlQuery || ' ' || IndividualTextSearchSql_ ;
	end if;
	SqlQuery := SqlQuery || ') ';
  end if;
  if (TextFindDescription = 1 ) then
    if (TextFindFlag_ = 0) then
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag_ := 1;
    else
      SqlQuery := SqlQuery || ' or';
    end if;
    SqlQuery := SqlQuery || ' (lower(location.ext_name) like ''' || ContainWildCard || TextFind_  || '%'')';
  end if;
  if (TextFindCode = 1) then
    if (TextFindFlag_ = 0) then
      SqlQuery := SqlQuery || ' and (';
      TextFindFlag_ := 1;
    else
      SqlQuery := SqlQuery || ' or';
    end if;
    SqlQuery := SqlQuery || ' (lower(location.code) like ''' || ContainWildCard || TextFind_ || '%'')';
  end if;

  if (TextFindFlag_ = 1) then
    SqlQuery := SqlQuery || ')';
  end if;

  ProgramSql := ' ';
  SELECT count(*) INTO Count_ FROM program_list WHERE list_type = 27507 AND value_id = 0 AND program_id = ProgramId;
  IF (ProgramId != 0 AND Count_ = 0) THEN
    ProgramSql :=  'location.location_id in (select value_id from program_list where list_type = 27507 and program_id = ' || to_char(ProgramId) || ')';
    FOR rec IN cur_program_list_with_children(ProgramId) LOOP
			ProgramSql := ProgramSql||' OR location.location_id IN (SELECT location_id FROM TABLE(CAST(axsp_get_location_child_tbl('||To_Char(rec.value_id)||') AS LOCATION_CHILD_TBLTAB)))';
  	END LOOP;
	END IF;

	IF (ProgramSql != ' ') THEN
		SqlQuery := SqlQuery || ' and (' || ProgramSql || ')';
	END IF;

  OPEN RC1 FOR SqlQuery;
end AXSP_LOCATION_SEARCH;
/
CREATE OR REPLACE PROCEDURE AXSP_REFERENCE_LOCATION_SEARCH(
Country 		IN NVARCHAR2 DEFAULT NULL,
PostalZipCode 	IN NVARCHAR2 DEFAULT NULL,
City 			IN NVARCHAR2 DEFAULT NULL,
StateProvince 	IN NVARCHAR2 DEFAULT NULL,
isZipCodeMode	IN NUMBER DEFAULT NULL,
RC1     		IN OUT globalPkg.RCT1
) AS
--
  Country_			NVARCHAR2(512) := Country;
  PostalZipCode_    NVARCHAR2(512) := PostalZipCode;
  City_  			NVARCHAR2(512) := City;
  StateProvince_    NVARCHAR2(512) := StateProvince;
  isZipCodeMode_    NUMBER(1,0) := isZipCodeMode;
  SqlQuery          VARCHAR2(4000);
BEGIN
  Country_ := lower(Country);
	SqlQuery := 'select zip_code, street, city, county, state_province, country from location_reference';
	SqlQuery := SqlQuery || ' where lower(country) = ''' || Country_ || '''';
	IF (isZipCodeMode_ = 1) THEN
		PostalZipCode_ := lower(PostalZipCode_);
		SqlQuery := SqlQuery || ' and lower(zip_code) = ''' || PostalZipCode_ || '''';
	ELSE
		City_ := lower(City_);
		StateProvince_ := lower(StateProvince_);
		SqlQuery := SqlQuery || ' and lower(city) like ''%' || City_  || '%''';
		SqlQuery := SqlQuery || ' and lower(state_province) like ''%' || StateProvince_  || '%''' || ' and rownum <= 1000';
	END IF;

  OPEN RC1 FOR SqlQuery;
END AXSP_REFERENCE_LOCATION_SEARCH;
/
CREATE OR REPLACE PROCEDURE axsp_address_search(
TextFindEndCustomer IN NUMBER DEFAULT NULL,
EndCustomerIds IN NVARCHAR2 DEFAULT NULL,
CustomerId IN INTEGER DEFAULT NULL,
CustomerSlotType IN INTEGER DEFAULT NULL,
DealerId IN INTEGER DEFAULT NULL,
DealerSlotType IN INTEGER DEFAULT NULL,
VendorId IN INTEGER DEFAULT NULL,
VendorSlotType IN INTEGER DEFAULT NULL,
buzModelId IN NVARCHAR2 DEFAULT NULL,
RC1     IN OUT globalPkg.RCT1
) AS
--
	locationCols 			VARCHAR2(400);
	locationTable 			VARCHAR(400);
	locationWhere 			VARCHAR(400);
	SqlQuery 				VARCHAR(4000);
	sourceName 				VARCHAR(400);
	partyRole	 			VARCHAR(400);
	queryTable 				VARCHAR(400);
BEGIN
	locationCols := ' ad.party_address_id as partyAddrId, ad.street as street_name, ad.suburb as suburb_name, adcity.name as city_name, adcounty.name as county_name, adstate.name as state_name, ad.zip_code as zip_code, adcountry.name as country_name, adcountry.location_id as location_id ';
	locationTable := ' location adcity, location adcounty, location adstate, location adcountry ';
	locationWhere := ' AND ad.city_id = adcity.location_id AND ad.county_id = adcounty.location_id AND ad.state_province_id = adstate.location_id AND ad.country_region_id = adcountry.location_id ';
	sourceName := 'case when bmp.caption_override = '' '' then sysDefsPartyRole.caption || '' (''||lukset.value||'')'' else bmp.caption_override || '' (''||lukset.value||'')'' end as source, ';
	partyRole := ' AND bmp.system_defs_party_role_id = sysDefsPartyRole.system_defs_party_role_id AND sysDefsPartyRole.slot_type = ';
	queryTable := ' party_address ad, lookupset lukset, business_model_party bmp, system_defs_party_role sysDefsPartyRole, ';

	if (TextFindEndCustomer = 1) then
		-- End Customer
		SqlQuery := '
		SELECT ''End Customer ('' || lukset.value || '')'' as source, ' || locationCols || '
		FROM party_address ad, lookupset lukset, ' || locationTable || '
		WHERE ad.party_id in (' || EndCustomerIds || ') and ad.party_id != 0 and lukset.lookupset_id = ad.address_type' || locationWhere;
	end if;

	if (CustomerId > 0) then
		-- Customer
		SqlQuery := case when length(SqlQuery) != 0 then SqlQuery || ' union all ' else '' end || '
		SELECT ' || sourceName || locationCols || '
		FROM ' || queryTable || locationTable || '
		WHERE ad.party_id = ' || cast(CustomerId as NVARCHAR2) || ' and lukset.lookupset_id = ad.address_type and bmp.business_model_id = ' || buzModelId || partyRole || cast(CustomerSlotType as NVARCHAR2) || locationWhere;
	end if;

	if (DealerId > 0) then
		-- Dealer
		SqlQuery := case when length(SqlQuery) != 0 then SqlQuery || ' union all ' else '' end || '
		SELECT ' || sourceName || locationCols || '
		FROM ' || queryTable || locationTable || '
		WHERE ad.party_id = ' || cast(DealerId as NVARCHAR2) || ' and lukset.lookupset_id = ad.address_type and bmp.business_model_id = ' || buzModelId || partyRole || cast(DealerSlotType as NVARCHAR2) || locationWhere;
	end if;

	if (VendorId > 0) then
		-- Vendor
		SqlQuery := case when length(SqlQuery) != 0 then SqlQuery || ' union all ' else '' end || '
		SELECT ' || sourceName || locationCols || '
		FROM ' || queryTable || locationTable || '
		WHERE ad.party_id = ' || cast(VendorId as NVARCHAR2) || ' and lukset.lookupset_id = ad.address_type and bmp.business_model_id = ' || buzModelId || partyRole || cast(VendorSlotType as NVARCHAR2) || locationWhere;
	end if;

	OPEN RC1 FOR SqlQuery;
END axsp_address_search;
/
CREATE OR REPLACE PROCEDURE axsp_get_cross_rates_proc
(rate_basis_id IN NUMBER,
 thedate       IN DATE,
 RC1 IN OUT globalPkg.RCT1) AS
--
begin
   open RC1 for
     select xr.*,0 stamp
     from table(cast(axsp_get_cross_rates(rate_basis_id, thedate) as CROSS_RATES_TBLTAB)) xr;
end axsp_get_cross_rates_proc;
/
CREATE OR REPLACE PROCEDURE axsp_get_fx_cross_rate_proc
(rate_basis_id IN NUMBER,
 thedate       IN DATE,
 ccy1          IN NUMBER,
 ccy2          IN NUMBER ,
 RC1 IN OUT globalPkg.RCT1) AS
--
begin
   open RC1 for
     select AXSP_GET_FX_CROSS_RATE(rate_basis_id, thedate, ccy1, ccy2) FROM dual;
end axsp_get_fx_cross_rate_proc;
/
CREATE OR REPLACE PROCEDURE axsp_party_account_summary
(p_party_id IN NUMBER,
 RC1 IN OUT globalPkg.RCT1) AS
--
-- temp table to hold flows for party
--this includes reversals/reversed flows as well
  v_sql varchar2(32000);
BEGIN
  execute immediate 'truncate table tp_party_account_summary';
--
  insert into tp_party_account_summary (flow_id, party_account_id, collection_state, reversal_status, flow_type,
	  contract_id, currency_id, currency_code, c_party_account_id, contract_grp_style,
	  expected_dt, amt_gross, custom_flow_hdr_id, timing, invoice_id, amt_matched, exclude_from_account_bal)
  select f.flow_id, f.party_account_id, f.collection_state, f.reversal_status, f.flow_type,
	  f.contract_id, f.currency_id, ccy.code currency_code, c.party_account_id c_party_account_id, c.contract_grp_style,
	  f.expected_dt, f.amt_gross, f.custom_flow_hdr_id, cfh.timing, f.invoice_id, f.amt_matched, f.exclude_from_account_bal
	  from flow f
	  inner join currency ccy on f.currency_id = ccy.currency_id
	  inner join contract c on f.contract_id = c.contract_id
	  inner join party_account pa on f.party_account_id = pa.party_account_id
      inner join custom_flow_hdr cfh on f.custom_flow_hdr_id = cfh.custom_flow_hdr_id
	  where pa.party_id = p_party_id
	  and f.is_cash = 1
	  and f.is_shadow_copy = 0
	  and f.status != 2099
	  and c.contract_id >= 0;
v_sql := 'select pa.party_account_id,
	pa.account_no,
	pa.name,
	pa.party_id,
	case (select count(*) from tp_party_account_summary where
		  collection_state = 14802 -- overdue
		  and reversal_status = 4200 --current
		  and currency_id = cf.currency_id
		  and party_account_id = cf.party_account_id
		  )
    	  when 0 then cast(0 AS NUMBER) else cast(1 AS NUMBER) end is_overdue,
	nvl(cf.currency_id, 0) currency_id,
	nvl(cf.currency_code, '' '') currency_code,
	pa.last_invoice_dt,
	pa.last_statement_dt,
	pa.next_statement_no-1 last_statement_no,
 	pa.input_dt,
	nvl(last_contract_id, 0) last_contract_id,
	nvl((select reference from contract where contract_id = last_contract_id), '' '') last_contract_ref,
 	-- select amt_due (sum of flows prev expected - sum of actual bankflows)
	-- is total amount due....amount overdue is calculated elsewhere
 	nvl((SELECT SUM(amt_gross)
		FROM tp_party_account_summary
		WHERE party_account_id = cf.party_account_id AND
		expected_dt <= trunc(dt.server_dt) AND
		currency_id = cf.currency_id AND
		exclude_from_account_bal = 0), 0) -
		nvl((SELECT SUM(amount)
		FROM bank_flow bf
		WHERE party_account_id = cf.party_account_id AND
		actual_dt <= trunc(dt.server_dt) AND
		currency_id = cf.currency_id and is_deleted = 0 and is_shadow_copy = 0), 0) amt_due,
	nvl((select sum(amt_financed) from contract where
		currency_id = cf.currency_id and party_account_id = cf.party_account_id
		and save_status >= 2201 and is_active = 1 and contract_id > 0), 0) amt_financed,
	nvl((select sum(f.amt_gross) amt_gross
		from tp_party_account_summary f
		where f.currency_id = cf.currency_id and
		f.party_account_id = cf.party_account_id and
		f.expected_dt =
			(select min(expected_dt) from tp_party_account_summary f1
				where f1.expected_dt >= trunc(dt.server_dt)
				and f1.reversal_status = 4200
				and f1.flow_type = 1003
				and f1.currency_id = cf.currency_id
				and f1.party_account_id = cf.party_account_id) and
		(f.flow_type = 1003 or f.timing = 14003 or f.timing = 14011) and
		f.reversal_status = 4200), 0) reg_installment,
	nvl((select sum(amt_gross) from tp_party_account_summary f
		where f.party_account_id = pa.party_account_id
		and f.currency_id = cf.currency_id
		and f.reversal_status = 4200 --none/current
		and f.invoice_id in (select invoice_id from invoice i
				where i.party_account_id = pa.party_account_id
				and i.invoice_end_dt = pa.last_invoice_dt)), 0) last_invoice_bal,
	nvl((select balance from statement_bal sb
		where sb.party_account_id = pa.party_account_id
		and sb.balance_dt = pa.last_statement_dt
		and sb.currency_id = cf.currency_id), 0) last_statement_bal,
	nvl((select min(expected_dt) from tp_party_account_summary f
		where f.expected_dt >= trunc(dt.server_dt)
		and f.party_account_id = pa.party_account_id
		and f.flow_type = 1003 -- Installment
		and f.reversal_status = 4200 --current
		and f.currency_id = cf.currency_id), axsp_datemin()) next_installment_dt,
	nvl((select sum(amt_gross-amt_matched) from tp_party_account_summary f
		where f.party_account_id = pa.party_account_id
		and f.currency_id = cf.currency_id
		and f.collection_state = 14802 -- overdue
		and f.reversal_status = 4200), 0) overdue_bal,
	pa.stamp
  from axvw_get_datetime dt,
	party_account pa left outer join
	(select max(contract_id) last_contract_id, party_account_id, currency_id, currency_code from tp_party_account_summary
	where reversal_status = 4200
	group by party_account_id, currency_id, currency_code) cf
	on pa.party_account_id = cf.party_account_id
  where
	pa.party_id = '||p_party_id;
--
  open RC1 for v_sql;
END;
/
create or replace procedure compile_objs(p_is_upgrade boolean default false) as
  cursor c1 is
    select object_name, object_type
    from user_objects a, order_object_by_dependency b
    where A.OBJECT_ID = B.OBJECT_ID(+)
    and   STATUS = 'INVALID'
    order by dlevel desc, object_type, object_name;
  v_sql varchar2(200);
  v_invalid_count number;
begin
  for i in c1 loop
  begin
    if i.object_type = 'PACKAGE BODY' then
      v_sql := 'alter package '||i.object_name||' compile body';
    else
      v_sql := 'alter '||i.object_type||' '||i.object_name||' compile';
    end if;
    execute immediate v_sql;
  exception
    when others then
      null;
  end;
  end loop;
  --
  if p_is_upgrade then
    select count(*) into v_invalid_count
    from user_objects where status='INVALID' and object_name not like 'BIN$%';
    if v_invalid_count > 0 then
      RAISE_APPLICATION_ERROR(-20000,'There are '||v_invalid_count||' invalid objects.  Please resolve manually.');
    end if;
  end if;
end;
/
--TODO Sean Clifford BugzId:21239 Description:This may need to be changed depending on what MAC decides on suspensions
create or replace PROCEDURE AXSP_CONTRACT_SUSPENSION_UPD
(p_run_dt  IN DATE   DEFAULT NULL,
 p_contract_id_spec IN NUMBER DEFAULT NULL,
 p_user_id IN NUMBER DEFAULT NULL) AS
--
  v_min_dt                      DATE;
  v_max_dt                      DATE;
  v_no_suspension_defs          NUMBER;
  v_rate_basis_id               NUMBER := 0;
  v_base_ccy_id                 NUMBER := 0;
  v_contract_id                 NUMBER;
  v_party_id                    NUMBER;
  v_business_unit_id            NUMBER;
  v_program_id                  NUMBER;
  v_product_id                  NUMBER;
  v_installment_frequency       NUMBER;
  v_oldest_oustanding           DATE;
  v_overdue_days                NUMBER;
  v_base_amt_overdue            NUMBER(18,4);
  v_contract_suspension_def_id  NUMBER;
  v_suspension_type             NUMBER;
  v_trigger_days                NUMBER;
  v_trigger_threshold           NUMBER(18,4);
  v_trigger_threshold_rate      NUMBER(18,12);
  v_future_payment              NUMBER(18,4);
  v_suspension_state            NUMBER;
  v_suspended_to_dt             DATE;
  v_prevent_suspension_dt       DATE;
  v_suspended_from_dt           DATE;
  v_is_auto_set_suspense_state  NUMBER;
  v_is_installment_only         NUMBER;
  v_is_trigger_or               NUMBER;
  v_is_prevent_susp_reinstate   NUMBER;
  v_is_suspended                BOOLEAN;
  v_is_threshold_trigger        BOOLEAN;
--
  --create cursor for contracts with overdue flows
  --and suspended contracts that are not overdue
  --join to cross rates proc to get base amount overdue
  CURSOR cur_contracts IS
    SELECT c.contract_id,
        c.cparty_id, c.business_unit_id, c.program_id, c.product_id, c.installment_frequency,
        MIN(f.expected_dt) oldest_oustanding,
        trunc(p_run_dt) - MIN(f.expected_dt) overdue_days,
        SUM((f.amt_gross-f.amt_matched)*r.rate) base_amt_overdue,
        suspension_state, cs.suspended_to_dt, cs.prevent_suspension_dt
    FROM contract c, flow f, cross_rates_tbl r, contract_suspension cs
    WHERE ((c.contract_id+0 = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0))
        AND cs.contract_id = c.contract_id
        AND f.contract_id = c.contract_id AND cs.allow_suspension = 1
        AND c.currency_id = r.currency_id1 AND r.currency_id2 = v_base_ccy_id
        AND f.collection_state = 14802 AND f.reversal_status = 4200
        AND f.is_cash = 1 AND f.amt_gross >= 0 AND f.status != 2099 AND contract_grp_style != 18201
    GROUP BY c.contract_id, c.cparty_id, c.business_unit_id, c.program_id, c.product_id, c.currency_id, c.installment_frequency,
        c.suspension_state, cs.suspended_to_dt, cs.prevent_suspension_dt
    UNION --select contracts that are suspended, but not overdue
    SELECT c.contract_id, c.cparty_id, c.business_unit_id, c.program_id, c.product_id, c.installment_frequency,
        axsp_datemin() oldest_oustanding, 0 overdue_days, 0 base_amt_overdue,
        suspension_state, cs.suspended_to_dt, cs.prevent_suspension_dt
    FROM contract c, contract_suspension cs
    WHERE c.suspension_state = 22501
	     AND cs.contract_id = c.contract_id
        AND ((c.contract_id+0 = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0))
        AND NOT EXISTS (
            SELECT c2.contract_id
            FROM contract c2, flow f, cross_rates_tbl r, contract_suspension cs2
            WHERE c2.contract_id = c.contract_id AND f.contract_id = c2.contract_id
                AND cs2.contract_id = c2.contract_id
                AND cs2.allow_suspension = 1 AND c2.currency_id = r.currency_id1
                AND r.currency_id2 = v_base_ccy_id AND f.collection_state = 14802
                AND f.reversal_status = 4200 AND f.is_cash = 1 AND f.amt_gross >= 0
                AND f.status != 2099);
--
BEGIN
--
  v_min_dt := axsp_datemin();
  v_max_dt := axsp_datemax();
--
  --update allow_suspension on contracts
  UPDATE contract_suspension SET stamp = stamp + 1, allow_suspension = 1, prevent_suspension_dt = v_min_dt
  WHERE allow_suspension = 0 AND prevent_suspension_dt < axsp_dateonly(p_run_dt)
  AND contract_id in (SELECT contract_id FROM contract WHERE contract_grp_style != 18201
		AND ((contract_id+0 = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0)));
  --
  --dont go any further if there are no suspension rules
  select count(*) INTO v_no_suspension_defs from contract_suspension_def
   where contract_suspension_def_id > 0 and is_auto_set_suspense_state = 1
   and is_deleted = 0 and is_active = 1;
  if V_no_suspension_defs = 0 THEN
   GOTO exit_proc;
  END IF;
  --
  --fetch fx rate basis id and base ccy id
  FOR rec in (select system_setting_value
              from system_setting where system_setting_type = 8402
              and rownum = 1) LOOP
    v_rate_basis_id := to_number(rec.system_setting_value);
  END LOOP;
  --
  FOR rec in (select currency_id
              from currency where is_base = 1 and rownum = 1) LOOP
    v_base_ccy_id := rec.currency_id;
  END LOOP;
  --
  -- Populate temp table with cross rate information
  delete from cross_rates_tbl;
  insert into cross_rates_tbl
  select * from TABLE(CAST(axsp_get_cross_rates(v_rate_basis_id, p_run_dt) AS cross_rates_tbltab));
  --
  -- default contracts selected in cursor
  <<suspend_reinstate_contracts>>
  --get from from cursor into local vriables
  for cur_contracts_rec in cur_contracts LOOP
    v_contract_id := cur_contracts_rec.contract_id;
    v_party_id := cur_contracts_rec.cparty_id;
    v_business_unit_id := cur_contracts_rec.business_unit_id;
    v_program_id := cur_contracts_rec.program_id;
    v_product_id := cur_contracts_rec.product_id;
    v_oldest_oustanding := cur_contracts_rec.oldest_oustanding;
    v_overdue_days := cur_contracts_rec.overdue_days;
    v_base_amt_overdue := cur_contracts_rec.base_amt_overdue;
    v_suspension_state := cur_contracts_rec.suspension_state;
    v_suspended_to_dt := cur_contracts_rec.suspended_to_dt;
    v_prevent_suspension_dt := cur_contracts_rec.prevent_suspension_dt;
    v_installment_frequency := cur_contracts_rec.installment_frequency;
    --
    -- get suspension def id for this contract (party, bus_unit, product)
    v_contract_suspension_def_id := axsp_contract_susp_get_def(v_party_id, v_business_unit_id, v_program_id, v_product_id,v_installment_frequency);
    --
    -- get suspension def details. ONLY SUSPEND/REINSTATE if there is a RULE
    IF  v_contract_suspension_def_id > 0 THEN
      FOR rec IN (SELECT d.suspension_type, d.trigger_period_days, d.trigger_threshold_amt, d.trigger_threshold_rate, d.is_auto_set_suspense_state, d.is_installment_only, d.is_trigger_or, d.is_prevent_suspense_reinstate
                  FROM contract_suspension_def d
                  WHERE d.contract_suspension_def_id = v_contract_suspension_def_id) LOOP
        v_suspension_type := rec.suspension_type;
        v_trigger_days := rec.trigger_period_days;
        v_trigger_threshold := rec.trigger_threshold_amt;
        v_trigger_threshold_rate := rec.trigger_threshold_rate;
        v_is_auto_set_suspense_state := rec.is_auto_set_suspense_state;
        v_is_installment_only := rec.is_installment_only;
        v_is_trigger_or := rec.is_trigger_or;
		  v_is_prevent_susp_reinstate := rec.is_prevent_suspense_reinstate;
      END LOOP;
      ---
      IF (v_is_auto_set_suspense_state = 1) THEN
        --set values incase null
        IF  v_suspension_type is NULL THEN
          v_suspension_type := 0;
        END IF;
        IF  v_trigger_days is NULL THEN
          v_trigger_days := 0;
        END IF;
        IF  v_trigger_threshold is NULL THEN
          v_trigger_threshold := 0;
        END IF;
        IF v_is_installment_only is NULL THEN
          v_is_installment_only := 0;
        END IF;
        IF v_is_trigger_or is NULL THEN
          v_is_trigger_or := 0;
        END IF;
        IF v_trigger_threshold_rate is NULL THEN
          v_trigger_threshold_rate := 0;
        END IF;

        IF (v_is_installment_only = 1) THEN -- Installment, Interest Only, Final in Advance
          FOR rec IN (select 	MIN(f.expected_dt) oldest_oustanding, trunc(p_run_dt) - MIN(f.expected_dt) overdue_days, SUM((f.amt_gross-f.amt_matched)*r.rate) base_amt_overdue
                  FROM contract c, flow f, cross_rates_tbl r
                  WHERE (c.contract_id = v_contract_id) AND
                        f.contract_id = c.contract_id AND
                        c.currency_id = r.currency_id1 AND r.currency_id2 = v_base_ccy_id AND
                        f.collection_state = 14802 AND f.reversal_status = 4200 AND
                        f.is_cash = 1 AND f.amt_gross >= 0 AND f.status != 2099 AND
                        f.flow_type in (1003,1043,1015)) LOOP
              v_oldest_oustanding := rec.oldest_oustanding;
              v_overdue_days := rec.overdue_days;
              v_base_amt_overdue := rec.base_amt_overdue;
          END LOOP;
        END IF;

        v_future_payment:= 0;

        IF v_trigger_threshold_rate = 0 THEN -- use threshold amount
          v_is_threshold_trigger := (v_base_amt_overdue >= v_trigger_threshold);
        ELSE -- use percentage
          -- calculate future payment
		FOR rec IN (select 	SUM((f.amt_gross-f.amt_matched)*r.rate) future_amount
	        	FROM contract c, flow f, cross_rates_tbl r
	          	WHERE (c.contract_id = v_contract_id) AND
	                	f.contract_id = c.contract_id AND
	                	c.currency_id = r.currency_id1 AND r.currency_id2 = v_base_ccy_id AND
	                	f.reversal_status = 4200 AND
	                	f.is_cash = 1 AND f.amt_gross >= 0 AND f.status != 2099 AND
	                	(f.flow_type in (1003,1043,1015) or v_is_installment_only = 0) AND f.expected_dt > p_run_dt) LOOP
	      		v_future_payment := rec.future_amount;
	    	END LOOP;
          	v_is_threshold_trigger := ((v_trigger_threshold_rate * 0.01 * (v_base_amt_overdue + v_future_payment)) < v_base_amt_overdue);
        END IF;

        IF v_is_trigger_or = 1 THEN
          v_is_suspended := (v_overdue_days >= v_trigger_days) or (v_is_threshold_trigger);
        ELSE
          v_is_suspended := (v_overdue_days >= v_trigger_days) and (v_is_threshold_trigger);
        END IF;

        --test if this contract is overdue enough to be suspended
        IF (v_is_suspended) THEN
        --if contract is not suspended and not prevented from being suspended
        -- it can then be suspended
          IF (v_suspension_state = 22500 and v_prevent_suspension_dt < axsp_dateonly(p_run_dt)) THEN
            --set suspended from dt
            IF v_suspension_type = 22001 THEN --oldest oustanding date type
            v_suspended_from_dt := v_oldest_oustanding;
            ELSE
            v_suspended_from_dt := p_run_dt; --processing date type
            END IF;
            --
            --do update (suspend)
            -- is_supended = 1 (suspended)
            -- suspended_from_dt (either oldest oustanding or run dt)
            -- suspended_to_dt = min_dt (needs to be this so it can be automatically reinstated)
            -- prevent_suspension_dt = min_dt (reset this)
            -- suspension_change_user_id = passed in user and suspension_change_dt = run dt
            -- save status = gl updates pending
            UPDATE contract SET stamp = stamp + 1, suspension_state = 22501,
            save_status = 2201 where contract_id = v_contract_id;
            UPDATE contract_suspension SET stamp = stamp + 1, suspended_from_dt = v_suspended_from_dt,
            suspended_to_dt = v_min_dt, suspension_change_user_id = p_user_id,
            suspension_change_dt = p_run_dt, prevent_suspension_dt = v_min_dt,
            trigger_period_days = v_trigger_days
            where contract_id = v_contract_id;
          END IF;
        --else make sure contract is not suspended, if it is reinstate it
        ELSE
          --if contract is suspended, but only until now then reinstate, and not prevented from auto reinstating
          IF v_suspension_state = 22501 and v_suspended_to_dt < axsp_dateonly(p_run_dt) and v_is_prevent_susp_reinstate = 0 THEN
            --update reinstate
            -- is_supended = 0 (not suspended)
            -- suspended_to_dt = run_dt (contract (was) suspended to this (new) date)
            -- prevent_suspension_dt = min_dt (reset this / dont prevent resuspension)
            -- suspension_change_user_id = passed in user and suspension_change_dt = passed in dt
            -- save status = gl updates pending
            update contract set stamp = stamp + 1, suspension_state = 22500,
            save_status = 2201 where contract_id = v_contract_id;
            update contract_suspension set stamp = stamp + 1, suspended_to_dt = p_run_dt,
            suspension_change_user_id = p_user_id, suspension_change_dt = p_run_dt,
            prevent_suspension_dt = v_min_dt where contract_id = v_contract_id;
          END IF;
        END IF;
      END IF; --end v_is_auto_set_suspense_state = 1
    END IF; --end only suspend/reinstate if there is a rule
  END LOOP;
--
--
<<exit_proc>>
--
RETURN;
--
END axsp_contract_suspension_upd;
/

CREATE OR REPLACE PROCEDURE axsp_create_note(
	p_contract_id		INTEGER,
	p_subject			NVARCHAR2,
	p_comments			NVARCHAR2,
	p_user_id			INTEGER,
	p_activity_type	INTEGER,
	p_activity_area	INTEGER)
AS
	v_party_id				  INTEGER;
	v_party_account_id  INTEGER;
	v_today_dt				  DATE;
BEGIN
	v_today_dt := axsp_get_datetime();

	--if dealer mode then get dealer party
	SELECT case when business_model = 13403 then dealer_id else cparty_id end,
			 case when business_model = 13403 then dealer_party_account_id else party_account_id end
	INTO v_party_id, v_party_account_id
	FROM contract
	WHERE contract_id = p_contract_id;

	INSERT INTO note (
		party_id,
		party_account_id,
		activity_type,
		activity_area,
		activity_dt,
		user_id,
		subject,
		comments,
		contract_id,
		duration,
		task_id,
		flow_id,
		currency_id,
		asset_hdr_id,
		reserve_id,
		doc_id,
		purchase_invoice_id,
		orig_asset_hdr_id,
		contract_restructure_id,
		hidden_links,
		bank_flow_id,
		is_manual,
		credit_line_id,
		report_id,
		custom_action_id,
		inertia_id,
		portfolio_id,
		response_id,
		sub_response_id,
		input_dt,
		last_updated_dt,
		last_updated_by_id,
		bank_statement_det_id,
		opportunity_id,
		bank_flow_batch_no,
		secondary_id,
		stamp
    )
	VALUES (
		v_party_id,
		v_party_account_id,
		p_activity_type,
		p_activity_area,
		v_today_dt,
		p_user_id,
		p_subject,
		p_comments,
		p_contract_id,
		0,					--duration,
		0,					--task_id,
		0,					--flow_id,
		0,					--currency_id,
		0,					--asset_hdr_id,
		0,					--reserve_id,
		0,					--doc_id,
		0,					--purchase_invoice_id,
		0,					--orig_asset_hdr_id,
		0,					--contract_restructure_id,
		' ',				--hidden_links,
		0,					--bank_flow_id,
		0,					--is_manual,
		0,					--credit_line_id,
		0,					--report_id,
		0,					--custom_action_id,
		0,					--inertia_id,
		0,					--portfolio_id,
		0,					--response_id,
		0,					--sub_response_id,
		v_today_dt,		--input_dt,
		v_today_dt,		--last_updated_dt,
		p_user_id,		--last_updated_by_id,
		0,					--bank_statement_det_id,
		0,                  --opportunity_id,
		0,					--bank_flow_batch_no
		0,             --secondary_id
		0					--stamp
	);
END;
/

CREATE OR REPLACE PROCEDURE AXSP_CONTRACT_INTERCEPT_UPD
(
	p_run_dt				IN DATE   DEFAULT NULL,
	p_contract_id_spec	IN NUMBER DEFAULT NULL,
	p_user_id				IN NUMBER DEFAULT NULL
)
AS
--
v_min_dt                          DATE;
v_no_suspension_defs              NUMBER;
v_contract_id                     NUMBER;
v_party_id                        NUMBER;
v_business_unit_id                NUMBER;
v_program_id                      NUMBER;
v_product_id                      NUMBER;
v_installment_frequency           NUMBER;
v_overdue_days                    NUMBER;
v_contract_suspension_def_id      NUMBER;
v_intercept_trigger_days          NUMBER;
v_intercept_trigger_dt            DATE;
v_intercept_state                 NUMBER;
v_intercepted_to_dt               DATE;
v_prevent_intercept_dt            DATE;
v_is_prevent_intercept_reinst     NUMBER;
v_note_subject                    NVARCHAR2(80);
v_is_auto_set_intercept_state		 NUMBER;
--

CURSOR cur_contracts IS
SELECT c.contract_id, c.cparty_id, c.business_unit_id, c.program_id, c.product_id, c.installment_frequency,
    c.intercept_state, c.intercept_trigger_dt, cs.intercepted_to_dt, cs.prevent_intercept_dt
FROM contract c, contract_suspension cs
WHERE c.intercept_state = 40800
	AND ((c.contract_id = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0))
	AND cs.contract_id = c.contract_id
	AND cs.allow_intercept = 1
	AND c.contract_grp_style != 18201
	AND c.intercept_trigger_dt != v_min_dt
GROUP BY c.contract_id, c.cparty_id, c.business_unit_id, c.program_id, c.product_id, c.installment_frequency,
	c.intercept_state, c.intercept_trigger_dt, cs.intercepted_to_dt, cs.prevent_intercept_dt;

--currently is_prevent_intercept_reinstate is alwasy True, so no auto removal of intercept yet, may be for the future development
--UNION --select contracts that are intercepted but have intercept_trigger_dt reset so they can be reinstated
--SELECT c.contract_id, c.cparty_id, c.business_unit_id, c.program_id, c.product_id,
--	c.intercept_state, c.intercept_trigger_dt, cs.intercepted_to_dt, cs.prevent_intercept_dt
--FROM contract c, contract_suspension cs
--WHERE c.intercept_state = 40801
--	AND ((c.contract_id = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0))
--	AND c.contract_grp_style != 18201
--	AND cs.contract_id = c.contract_id
--	AND c.intercept_trigger_dt = v_min_dt;
--
BEGIN
--
	v_min_dt := axsp_datemin();

	--update allow_intercept on contracts
	UPDATE contract_suspension SET stamp = stamp + 1, allow_intercept = 1, prevent_intercept_dt = v_min_dt
	WHERE allow_intercept = 0 AND prevent_intercept_dt < p_run_dt
		AND contract_id IN (SELECT contract_id FROM contract
								WHERE contract_grp_style != 18201
								AND ((contract_id = p_contract_id_spec AND p_contract_id_spec > 0) OR (p_contract_id_spec = 0)));

	--dont go any further if there are no suspension rules
	SELECT count(*) INTO v_no_suspension_defs FROM contract_suspension_def
	WHERE contract_suspension_def_id > 0 AND is_auto_set_intercept_state = 1
		AND is_deleted = 0 AND is_active = 1;

	if V_no_suspension_defs = 0 THEN
		GOTO exit_proc;
	END IF;

	v_note_subject := ' ';

	-- default contracts selected in cursor
	FOR cur_contracts_rec in cur_contracts LOOP
		v_contract_id           := cur_contracts_rec.contract_id;
		v_party_id              := cur_contracts_rec.cparty_id;
		v_business_unit_id      := cur_contracts_rec.business_unit_id;
		v_program_id            := cur_contracts_rec.program_id;
		v_product_id            := cur_contracts_rec.product_id;
		v_intercept_state       := cur_contracts_rec.intercept_state;
		v_intercept_trigger_dt  := cur_contracts_rec.intercept_trigger_dt;
		v_intercepted_to_dt     := cur_contracts_rec.intercepted_to_dt;
		v_prevent_intercept_dt  := cur_contracts_rec.prevent_intercept_dt;
		v_installment_frequency := cur_contracts_rec.installment_frequency;

		-- get suspension def id for this contract (party, bus_unit, product)
		v_contract_suspension_def_id := axsp_contract_susp_get_def(v_party_id, v_business_unit_id, v_program_id, v_product_id,v_installment_frequency);

		-- get suspension def details. ONLY INTERCEPT/REINSTATE if there is a RULE
		IF  v_contract_suspension_def_id > 0 THEN

			FOR rec IN (SELECT d.intercept_trigger_days, d.is_prevent_intercept_reinstate, d.is_auto_set_intercept_state
							FROM contract_suspension_def d
							WHERE d.contract_suspension_def_id = v_contract_suspension_def_id) LOOP
				v_intercept_trigger_days       := rec.intercept_trigger_days;
				v_is_prevent_intercept_reinst  := rec.is_prevent_intercept_reinstate;
				v_is_auto_set_intercept_state	 := rec.is_auto_set_intercept_state;
			END LOOP;

			IF (v_is_auto_set_intercept_state = 1) THEN

				IF  v_intercept_trigger_days is NULL THEN
					v_intercept_trigger_days := 0;
				END IF;
				IF  v_is_prevent_intercept_reinst is NULL THEN
					v_is_prevent_intercept_reinst := 0;
				END IF;

				v_overdue_days := trunc(p_run_dt) - v_intercept_trigger_dt;

				--test if this contract is ovedue enough to be iIntercepted
				IF (v_overdue_days >= v_intercept_trigger_days) THEN
					--if contract is not intercepted and not prevented from being intercepted
					-- it can then be intercepted
					IF (v_intercept_state = 40800 and v_prevent_intercept_dt < axsp_dateonly(p_run_dt)) THEN
						-- intercept_state = 40801 (Intercepted)
						-- intercepted_from_dt (run dt)
						-- intercepted_to_dt = min_dt (needs to be this so it can be automatically reinstated)
						-- prevent_intercept_dt = min_dt (reset this)
						-- intercept_change_user_id = passed in user and intercept_change_dt = run dt
						-- save status = gl updates pending
						v_note_subject := 'Contract ' || CAST(v_contract_id AS NVARCHAR2) || ': Intercept state changed from None to Intercepted';
						UPDATE contract SET stamp = stamp + 1, intercept_state = 40801,
							save_status = 2201, intercept_trigger_dt = v_min_dt
						WHERE contract_id = v_contract_id;

						UPDATE contract_suspension SET stamp = stamp + 1, intercepted_from_dt = p_run_dt,
							intercepted_to_dt = v_min_dt, intercept_change_user_id = p_user_id,
							intercept_change_dt = p_run_dt, prevent_intercept_dt = v_min_dt
						WHERE contract_id = v_contract_id;
					END IF;
	--currently is_prevent_intercept_reinstate is alwasy True, so no auto removal of intercept yet, may be for the future development
	--			ELSE
	--				--if contract is intercepted, but only until now then reinstate
	--				IF v_intercept_state = 40801 AND v_is_prevent_intercept_reinst = 0 AND v_intercepted_to_dt < axsp_dateonly(p_run_dt) THEN
	--					-- intercept_state = 40800 (not intercepted)
	--					-- intercepted_to_dt = run_dt (contract (was) intercepted to this (new) date)
	--					-- prevent_intercept_dt = min_dt (reset this / dont prevent re-intercept)
	--					-- intercept_change_user_id = passed in user and intercept_change_dt = passed in dt
	--					-- save status = gl updates pending
	--					v_note_subject := 'Contract ' || CAST(v_contract_id AS NVARCHAR2) || ': Intercept state changed from Intercepted to None';
	--					UPDATE contract SET stamp = stamp + 1, intercept_state = 40800,
	--						save_status = 2201, intercept_trigger_dt = v_min_dt
	--					WHERE contract_id = v_contract_id;
	--
	--					UPDATE contract_suspension SET stamp = stamp + 1, intercepted_to_dt = p_run_dt,
	--						intercept_change_user_id = p_user_id, intercept_change_dt = p_run_dt,
	--						prevent_intercept_dt = v_min_dt
	--					WHERE contract_id = v_contract_id;
	--				END IF;
				END IF;

				IF v_note_subject != ' ' THEN
					axsp_create_note(v_contract_id, v_note_subject, ' ', p_user_id, 8022, 8104);
				END IF;

			END IF; --end only intercept/reinstate if there is a rule
		END IF;
	END LOOP;
--
--
<<exit_proc>>
--
RETURN;
--
END AXSP_CONTRACT_INTERCEPT_UPD;
/

CREATE OR REPLACE PROCEDURE AXSP_UPD_CREDIT_UTILISATION2(p_creditLinePartyId IN NUMBER, p_takedownPartyId IN NUMBER, p_creditLineId IN NUMBER) AS

CURSOR cur_CLT IS
  SELECT clt.credit_line_takedown_id, cr.calc_type
  FROM credit_line_takedown clt
  INNER JOIN credit_line cr ON clt.credit_line_id = cr.credit_line_id
  WHERE
    clt.credit_line_takedown_id > 0
    AND (

        (
          cr.owner_id in (select owner_id from credit_line where credit_line_id = p_creditLineId)
          and cr.owner_id > 0
        )
        OR
        (
          (p_creditLinePartyId = 0 OR cr.party_id = p_creditLinePartyId)
          AND
          (p_creditLineId = 0 OR p_creditLineId = cr.credit_line_id)
        )
      )
  FOR UPDATE OF clt.amt_utilisation;

CURSOR cur_CL IS
  SELECT cr.credit_line_id
  FROM
    credit_line cr
  WHERE
    cr.credit_line_id > 0
    and cr.is_group = 0
    and (
        (
          cr.owner_id in (select owner_id from credit_line where credit_line_id = p_creditLineId)
          and cr.owner_id > 0
        )
        or
        (
          (p_creditLinePartyId = 0 or cr.party_id = p_creditLinePartyId) and (p_creditLineId = 0 or p_creditLineId = cr.credit_line_id)
        )
      )
  FOR UPDATE OF amt_utilisation;

  -- NOTE the order by clause to ensure that child groups are updated prior to top level groups
  -- Assumes that there cannot be more than Group-SubGroup-CreditLine levels
CURSOR cur_CLG IS
  SELECT crg.credit_line_id
  FROM credit_line crg, credit_line cr
  WHERE cr.credit_line_id > 0
	 AND crg.credit_line_id = cr.owner_id
    AND crg.is_group = 1
    AND cr.owner_id > 0
    AND (p_creditLinePartyId = 0 or cr.party_id = p_creditLinePartyId)
  ORDER BY crg.owner_id DESC
  FOR UPDATE OF cr.amt_utilisation;

BEGIN

  --Update the utilisation
  FOR i IN cur_CLT loop

    UPDATE credit_line_takedown clt
    SET clt.amt_utilisation =
	   NVL(axsp_get_credit_utilisation(clt.party_id, clt.credit_line_id, clt.contract_id, clt.contract_party_role, i.calc_type) - clt.amt_reinstated ,0),
      clt.stamp = clt.stamp+1
      WHERE clt.credit_line_takedown_id = i.credit_line_takedown_id;

  END LOOP;

  --Update the credit line.
  FOR i IN cur_CL loop

    UPDATE credit_line cl
    SET cl.amt_utilisation = (select coalesce(SUM(crt.amt_utilisation),0) from credit_line_takedown crt where crt.credit_line_id = cl.credit_line_id),
        cl.stamp = cl.stamp+1
    WHERE cl.credit_line_id = i.credit_line_id;

  END LOOP;

   --Update the credit line group.
  FOR i IN cur_CLG loop

    UPDATE credit_line clg
    SET clg.amt_utilisation = (select coalesce(SUM(cri.amt_utilisation),0) from credit_line cri where cri.owner_id = clg.credit_line_id),
      clg.stamp = clg.stamp+1
    WHERE clg.credit_line_id = i.credit_line_id;

  END LOOP;

END AXSP_UPD_CREDIT_UTILISATION2;
/

CREATE OR REPLACE PROCEDURE AXSP_PARTY_DUP_BUCKETS(p_filter_party_id int) AS
  cursor c_party_fields is
  select party_id, name, first_names, middle_name, trading_as, maiden_name
    from party where party_id > 0 and party_type != 1 and p_filter_party_id = 0
  union
  select party_id, name, first_names, middle_name, trading_as, maiden_name
    from party where party_id > 0 and party_type != 1 and party_id = p_filter_party_id;
  v_party_id integer;
  v_tolerance INTEGER;
  v_is_ignore_noise_words NUMBER(1,0);
  v_noise_word_matches INTEGER;
  v_string_tbl TEMP_SPLIT_TBL := TEMP_SPLIT_TBL();
BEGIN
  select tolerance INTO v_tolerance from party_dup_check_setting where party_dup_check_setting_id = 1;
  select is_ignore_noise_words INTO v_is_ignore_noise_words from party_dup_check_setting where party_dup_check_setting_id = 1;
  --
  if p_filter_party_id = 0 THEN
    delete from party_dup_bucket;
  else
    delete from party_dup_bucket WHERE party_id = p_filter_party_id;
  end if;
  --
  for i in c_party_fields loop
--
    v_string_tbl := axsp_split(i.name,' -');
    if v_string_tbl.COUNT > 0 then
      for j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
		if length(v_string_tbl(j)) > 3 then
			select count(*) INTO v_noise_word_matches from noise_word where word = Lower(v_string_tbl(j));
			if v_is_ignore_noise_words = 0 or v_noise_word_matches = 0 then
			  insert into tp_party_dup_bucket(party_id, name, sdx, bucket)
			  values (i.party_id, v_string_tbl(j), soundex(v_string_tbl(j)),
					  substr(soundex(v_string_tbl(j)), 1, 1) || substr(to_char(floor(to_number(substr(soundex(v_string_tbl(j)), -3))/v_tolerance)),1,3));
			end if;
		end if;
      end loop;
      v_string_tbl.delete;
    end if;
--
    v_string_tbl := axsp_split(i.first_names,' -');
    if v_string_tbl.COUNT > 0 then
      for j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
        if length(v_string_tbl(j)) > 3 then
			select count(*) INTO v_noise_word_matches from noise_word where word = Lower(v_string_tbl(j));
			if v_is_ignore_noise_words = 0 or v_noise_word_matches = 0 then
			  insert into tp_party_dup_bucket(party_id, name, sdx, bucket)
			  values (i.party_id, v_string_tbl(j), soundex(v_string_tbl(j)),
					  substr(soundex(v_string_tbl(j)), 1, 1) || substr(to_char(floor(to_number(substr(soundex(v_string_tbl(j)), -3))/v_tolerance)),1,3));
			end if;
		end if;
      end loop;
      v_string_tbl.delete;
    end if;
--
    v_string_tbl := axsp_split(i.middle_name,' -');
    if v_string_tbl.COUNT > 0 then
      for j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
        if length(v_string_tbl(j)) > 3 then
			select count(*) INTO v_noise_word_matches from noise_word where word = Lower(v_string_tbl(j));
			if v_is_ignore_noise_words = 0 or v_noise_word_matches = 0 then
			  insert into tp_party_dup_bucket(party_id, name, sdx, bucket)
			  values (i.party_id, v_string_tbl(j), soundex(v_string_tbl(j)),
					  substr(soundex(v_string_tbl(j)), 1, 1) || substr(to_char(floor(to_number(substr(soundex(v_string_tbl(j)), -3))/v_tolerance)),1,3));
			end if;
		end if;
      end loop;
      v_string_tbl.delete;
    end if;
--
    v_string_tbl := axsp_split(i.trading_as,' -');
    if v_string_tbl.COUNT > 0 then
      for j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
        if length(v_string_tbl(j)) > 3 then
			select count(*) INTO v_noise_word_matches from noise_word where word = Lower(v_string_tbl(j));
			if v_is_ignore_noise_words = 0 or v_noise_word_matches = 0 then
			  insert into tp_party_dup_bucket(party_id, name, sdx, bucket)
			  values (i.party_id, v_string_tbl(j), soundex(v_string_tbl(j)),
					  substr(soundex(v_string_tbl(j)), 1, 1) || substr(to_char(floor(to_number(substr(soundex(v_string_tbl(j)), -3))/v_tolerance)),1,3));
			end if;
		end if;
      end loop;
      v_string_tbl.delete;
    end if;
--
    v_string_tbl := axsp_split(i.maiden_name,' -');
    if v_string_tbl.COUNT > 0 then
      for j in v_string_tbl.FIRST..v_string_tbl.LAST LOOP
        if length(v_string_tbl(j)) > 3 then
			select count(*) INTO v_noise_word_matches from noise_word where word = Lower(v_string_tbl(j));
			if v_is_ignore_noise_words = 0 or v_noise_word_matches = 0 then
			  insert into tp_party_dup_bucket(party_id, name, sdx, bucket)
			  values (i.party_id, v_string_tbl(j), soundex(v_string_tbl(j)),
					  substr(soundex(v_string_tbl(j)), 1, 1) || substr(to_char(floor(to_number(substr(soundex(v_string_tbl(j)), -3))/v_tolerance)),1,3));
			end if;
		end if;
      end loop;
    end if;
  end loop;
--
  insert into party_dup_bucket(party_id, word, soundex_value, bucket_value, stamp)
  SELECT distinct party_id, lower(name), sdx, bucket, 0 from tp_party_dup_bucket where sdx != '0000';
END AXSP_PARTY_DUP_BUCKETS;
/
CREATE OR REPLACE PROCEDURE AXSP_SUM_AMT_FINANCED(
  contract_id_in  IN NUMBER DEFAULT NULL,
  fx_rate_id  IN NUMBER DEFAULT NULL,
  ccy_id_in IN NUMBER DEFAULT NULL,
  RC1  IN OUT globalPkg.RCT1) AS
--
-- Get the sum of amt_financed  (converted for ccy)
-- for use in contract group calcs */
--
p_dt date :=axsp_dateonly(axsp_get_datetime());

   cursor amt_cur  is
	   select sum(c.amt_financed) amt_financed, c.currency_id , c.calc_dt from contract c
          where c.product_style != 2007 and  c.calc_dt <= p_dt and c.mature_dt1 > p_dt and ( c.contract_id in (select cl1.linked_contract_id from contract_grp_link cl1 where cl1.contract_id = contract_id_in) or
   c.contract_id in (select cl2.linked_contract_id from contract_grp_link cl2 where cl2.contract_id in (
select cl3.linked_contract_id from contract_grp_link cl3 where cl3.contract_id = contract_id_in))) group by c.currency_id, c.calc_dt ;
--
p_total_amt number(18,4) :=0;
p_amt number(18,4);
--
BEGIN

  for i in amt_cur  loop

    p_amt := NVL( (i.amt_financed *axsp_get_fx_cross_rate(fx_rate_id, i.calc_dt, i.currency_id, ccy_id_in)),0);
    p_total_amt:= p_total_amt + p_amt;

  end LOOP;
    OPEN RC1 FOR
  SELECT NVL(p_total_amt,0) amt_financed from DUAL;
END AXSP_SUM_AMT_FINANCED;
/
CREATE OR REPLACE PROCEDURE AXSP_LOCATION_AUTOCOMPLETE(
p_NameStartsWith IN VARCHAR2,
p_LocationType IN NUMBER,
p_CountryId IN NUMBER,
p_StateProvinceId IN NUMBER,
p_CityId IN NUMBER,
p_RecordCount IN NUMBER,
RC1 IN OUT globalPkg.RCT1)
AS
BEGIN
  IF p_RecordCount = 0 THEN
    OPEN RC1 FOR
      SELECT * FROM location
      where search_name like upper(p_NameStartsWith || '%') and location_type = p_LocationType
      and (p_CountryId = 0 or country_id = p_CountryId)
      and (p_StateProvinceId = 0 or state_province_id = p_StateProvinceId)
      and (p_CityId = 0 or city_id = p_CityId)
      and is_active = 1 order by NAME;
  ELSE
    OPEN RC1 FOR
      select * from
      (SELECT * FROM location
       where search_name like upper(p_NameStartsWith || '%') and location_type = p_LocationType
       and (p_CountryId = 0 or country_id = p_CountryId)
       and (p_StateProvinceId = 0 or state_province_id = p_StateProvinceId)
       and (p_CityId = 0 or city_id = p_CityId)
       and is_active = 1 order by NAME) WHERE ROWNUM <= p_RecordCount;
  END IF;
END AXSP_LOCATION_AUTOCOMPLETE;
/
CREATE OR REPLACE PROCEDURE AXSP_LOCATION_SETUP
AS
--
p_AffectedRows NUMBER := 0;
--
BEGIN
--
  update location set search_name = upper(name), country_id = 0, state_province_id = 0, city_id = 0;
--
  /* Set country id on all immediate children of a country location */
  update location set country_id = owner_id
  where owner_id in (select location_id from location where location_type = 4402);
  /* Set state province id on all immediate children of a state province location */
  update location set state_province_id = owner_id
  where owner_id in (select location_id from location where location_type = 4403);
  /* Set city id on all immediate children of a city location */
  update location set city_id = owner_id
  where owner_id in (select location_id from location where location_type = 4405);
--
  /* Set country id on all non-immediate descendants of a country location
     by setting the country id of each generation to that of the parent
  */
  p_AffectedRows := 1;
--
  WHILE p_AffectedRows > 0 LOOP
    update location set country_id = (select inner_country_id from (select location_id inner_location_id, country_id inner_country_id from location) inner_location where inner_location_id = owner_id)
    where owner_id in (select location_id from location where country_id > 0) and country_id = 0;
--
    p_AffectedRows := sql%ROWCOUNT;
  END LOOP;
--
  /* Set state province id on all non-immediate descendants of a state province location
     by setting the state province id of each generation to that of the parent
  */
  p_AffectedRows := 1;
--
  WHILE p_AffectedRows > 0 LOOP
    update location set state_province_id = (select inner_state_province_id from (select location_id inner_location_id, state_province_id inner_state_province_id from location) inner_location where inner_location_id = owner_id)
    where owner_id in (select location_id from location where state_province_id > 0) and state_province_id = 0;
 --
    p_AffectedRows := sql%ROWCOUNT;
  END LOOP;
--
  /* Set city id on all non-immediate descendants of a city location
     by setting the city id of each generation to that of the parent
  */
  p_AffectedRows := 1;
--
  WHILE p_AffectedRows > 0 LOOP
    update location set city_id = (select inner_city_id from (select location_id inner_location_id, city_id inner_city_id from location) inner_location where inner_location_id = owner_id)
    where owner_id in (select location_id from location where city_id > 0) and city_id = 0;
--
    p_AffectedRows := sql%ROWCOUNT;
  END LOOP;
--
END AXSP_LOCATION_SETUP;
/
CREATE OR REPLACE PROCEDURE axsp_dsktp_pipeline_monitor(
eventType      INTEGER DEFAULT NULL,
startDt 	      DATE,
endDt 	      DATE,
productId 	   INTEGER DEFAULT NULL,
businessUnitId INTEGER DEFAULT NULL,
locationId     INTEGER DEFAULT NULL,
ccyId          INTEGER DEFAULT NULL,
dealerId       INTEGER DEFAULT NULL,
RC1            IN OUT globalPkg.RCT1)
AS
--
  SqlQuery     VARCHAR2(4000);
  SqlQuery2    VARCHAR2(4000);
BEGIN
  DELETE FROM TP_STATE_TABLE; -- clear temp table
  SqlQuery := 'INSERT INTO TP_STATE_TABLE
  SELECT contract_id,
         contract_state,
         NVL(DATEDIFF(''ss'', (SELECT Max(e.input_dt) FROM event_queue e
                         WHERE e.event_type = ' || eventType
                         || ' AND c.contract_id = e.contract_id
                         AND c.contract_state = e.event_state_after), axsp_get_datetime()), 0)
                  AS state_entered_elapsed_secs
   FROM contract c
   WHERE c.contract_id > 0
   AND c.input_dt between to_date('''||To_Char(startDt,'YYYY-MM-DD HH24:MI:SS')||''',''YYYY-MM-DD HH24:MI:SS'') AND to_date('''||To_Char(endDt,'YYYY-MM-DD HH24:MI:SS')||''',''YYYY-MM-DD HH24:MI:SS'')';

IF (productId > 0) THEN
   SqlQuery := SqlQuery || ' and (product_id = ' || to_char(productId) ||
   ' or product_id IN (SELECT product_id FROM product
     START WITH owner_id = ' || to_char(productId) || ' CONNECT BY PRIOR product_id = owner_id))';
END IF;

IF (businessUnitId > 0) THEN
   SqlQuery := SqlQuery || ' and (business_unit_id = ' || to_char(businessUnitId) ||
   ' or business_unit_id IN (SELECT party_id FROM party
     START WITH owner_id = ' || to_char(businessUnitId) || ' CONNECT BY PRIOR party_id = owner_id))';
END IF;

IF (locationId > 0) THEN
   SqlQuery := SqlQuery || ' and (location_id = ' || to_char(locationId) ||
   ' or location_id IN (SELECT location_id FROM location
     START WITH owner_id = ' || to_char(locationId) || ' CONNECT BY PRIOR location_id = owner_id))';
END IF;

IF (ccyId > 0) THEN
   SqlQuery := SqlQuery || ' and (currency_id = ' || to_char(ccyId) || ')';
END IF;

IF (dealerId > 0) THEN
   SqlQuery := SqlQuery || ' and (dealer_id = ' || to_char(dealerId) || ')';
END IF;

SqlQuery := SqlQuery || ' group by contract_id, contract_state';

EXECUTE IMMEDIATE SqlQuery;

SqlQuery2 := 'SELECT contract_state, axsp_get_lookupset_value(s.contract_state) as contract_state_str,
       count(s.contract_id) as count_in_state,
       cast(avg(s.state_entered_elapsed_secs) as INTEGER) as avg_time_in_state,
       cast(max(s.state_entered_elapsed_secs) as INTEGER) as max_time_in_state
FROM TP_STATE_TABLE s
GROUP BY contract_state
ORDER BY contract_state';

OPEN RC1 for SqlQuery2;
EXCEPTION
WHEN OTHERS THEN
  NULL;
END AXSP_DSKTP_PIPELINE_MONITOR;
/
create or replace procedure axsp_contract_summary_data
(p_party_id in number,
 RC1 IN OUT globalPkg.RCT1) as
--
-- Details for the party attached to the contract
  cursor c_contract_party(c_party_id number) is
    select p.business_individual, l.value title, trim(p.first_names) first_name, trim(p.name) last_name,
           p.phone_business, p.phone_mobile, p.phone_home,
           p.fax_business, p.fax_home,
           p.email_business, p.email_home, l.lookupset_id title_id
    from party p, lookupset l
    where p.party_id = c_party_id
    and   l.lookupset_id = p.title;
--
-- Details for the primary contact hanging of the contract party
  cursor c_primary_contact(c_party_id number) is
    select p.party_id,
           p.phone_business, p.phone_mobile, p.phone_home,
           p.fax_business, p.fax_home,
           p.email_business, p.email_home
    from party_contact pc, party p
    where pc.party_id = c_party_id
    and p.party_id = pc.contact_id
    and pc.is_primary_contact = 1;
--
-- Address lines for the party
  cursor c_address(c_party_id number) is
    SELECT decode(address_type, 5000, 1, 5002, 2, 5001, 3, 4) address_order,
           pa.is_current,
           trim(replace(pa.street,chr(13)||chr(10),N', ')) street,
           trim(replace(nvl(pa.suburb, N' '),chr(13)||chr(10),N', ')) suburb,
           trim(l1.name) city,
           trim(l2.name) state_province,
           trim(pa.zip_code) zip_code,
           trim(l3.name) country_region
    FROM  party_address pa, location l1, location l2, location l3
    WHERE pa.party_id = c_party_id
    AND   pa.city_id = l1.location_id
    AND   pa.state_province_id = l2.location_id
    AND   pa.country_region_id = l3.location_id
    AND   pa.street IS NOT NULL
    AND   pa.street != N' '
    order by 1, 2 desc;
--
--
  v_contract_cust_name nvarchar2(400);
  --
  v_primary_party_id number := 0;
  v_primary_phone_business party.phone_business%type;
  v_primary_phone_mobile party.phone_mobile%type;
  v_primary_phone_home party.phone_home%type;
  v_primary_fax_business party.fax_business%type;
  v_primary_fax_home party.fax_home%type;
  v_primary_email_business party.email_business%type;
  v_primary_email_home party.email_home%type;
  --
  v_address nvarchar2(1000) := N' ';
  v_phone party.phone_business%type := N'';
  v_fax party.fax_business%type := N'';
  v_email party.email_business%type := N'';
--
begin
--
-- Store details for primary contact
  for i in c_primary_contact(p_party_id) loop
    v_primary_party_id := i.party_id;
    v_primary_phone_business := i.phone_business;
    v_primary_phone_mobile := i.phone_mobile;
    v_primary_phone_home := i.phone_home;
    v_primary_fax_business := i.fax_business;
    v_primary_fax_home := i.fax_home;
    v_primary_email_business := i.email_business;
    v_primary_email_home := i.email_home;
  end loop;
--
  for i in c_contract_party(p_party_id) loop
    --
    -- Do the customer name
    if i.business_individual = 5201 then -- individual
      if i.title_id = 4499 then --None
        v_contract_cust_name := i.first_name||N' '||i.last_name;
     else     
        v_contract_cust_name := i.title||N' '||i.first_name||N' '||i.last_name;
     end if;
     else
      v_contract_cust_name := i.last_name;
     end if;
    --
    -- Phone
    if length(v_primary_phone_business) > 0 AND v_primary_phone_business != N' ' then
      v_phone := v_primary_phone_business;
    elsif length(i.phone_business) > 0 AND i.phone_business != N' ' then
      v_phone := i.phone_business;
    elsif length(v_primary_phone_mobile) > 0 AND v_primary_phone_mobile != N' ' then
      v_phone := v_primary_phone_mobile;
    elsif length(i.phone_mobile) > 0 AND i.phone_mobile != N' ' then
      v_phone := i.phone_mobile;
    elsif length(v_primary_phone_home) > 0 AND v_primary_phone_home != N' ' then
      v_phone := v_primary_phone_home;
    elsif length(i.phone_home) > 0 AND i.phone_home != N' ' then
      v_phone := i.phone_home;
    end if;
    --
    -- Fax
    if length(v_primary_fax_business) > 0 AND v_primary_fax_business != N' ' then
      v_fax := v_primary_fax_business;
    elsif length(i.fax_business) > 0 AND i.fax_business != N' ' then
      v_fax := i.fax_business;
    elsif length(v_primary_fax_home) > 0 AND v_primary_fax_home != N' ' then
      v_fax := v_primary_fax_home;
    elsif length(i.fax_home) > 0 AND i.fax_home != N' ' then
      v_fax := i.fax_home;
    end if;
    --
    -- Email
    if length(v_primary_email_business) > 0 AND v_primary_email_business != N' ' then
      v_email := v_primary_email_business;
    elsif length(i.email_business) > 0 AND i.email_business != N' ' then
      v_email := i.email_business;
    elsif length(v_primary_email_home) > 0 AND v_primary_email_home != N' ' then
      v_email := v_primary_email_home;
    elsif length(i.email_home) > 0 AND i.email_home != N' ' then
      v_email := i.email_home;
    end if;
  end loop;
  --
  -- Do the address
  for i in c_address(p_party_id) loop
    v_address := i.street;
    if i.suburb IS NOT NULL and i.suburb != N' ' then
      v_address := v_address||N', '||i.suburb;
    end if;
    if i.city != N'None' then
      v_address := v_address||N', '||i.city;
    end if;
    if i.state_province != N'None' then
      v_address := v_address||N', '||i.state_province;
    end if;
    if i.zip_code != N'None' then
      v_address := v_address||N', '||i.zip_code;
    end if;
    if i.country_region != N'None' then
      v_address := v_address||N', '||i.country_region;
    end if;
    exit; -- exit after first pass because we have ordered it this way
  end loop;
  --
  if v_address = N' ' AND v_primary_party_id != 0 then
    for i in c_address(v_primary_party_id) loop
      v_address := i.street;
      if i.suburb IS NOT NULL and i.suburb != N' ' then
			v_address := v_address||N', '||i.suburb;
		end if;
      if i.city != N'None' then
        v_address := v_address||N', '||i.city;
      end if;
      if i.state_province != N'None' then
        v_address := v_address||N', '||i.state_province;
      end if;
      if i.zip_code != N'None' then
        v_address := v_address||N', '||i.zip_code;
      end if;
      if i.country_region != N'None' then
        v_address := v_address||N', '||i.country_region;
      end if;
      exit; -- exit first pass because we have ordered that way
    end loop;
  end if;
  v_address := replace(v_address,N',,',N',');
  v_address := replace(v_address,N', ,',N',');
--
-- Now return the refcursor
  open RC1 FOR
    select 'customer_name', v_contract_cust_name from dual
    union all
    select 'address', v_address from dual
    union all
    select 'phone', v_phone from dual
    union all
    select 'fax', v_fax from dual
    union all
    select 'email', v_email from dual;
end axsp_contract_summary_data;
/
CREATE OR REPLACE PROCEDURE axsp_tax_calc_flow_ins
(
	p_asset_hdr_id		IN INTEGER DEFAULT NULL,
	p_flow_id 		IN INTEGER DEFAULT NULL,
	p_contract_id 		IN INTEGER DEFAULT NULL,
	p_tax_type_id		IN INTEGER DEFAULT NULL,
	p_tax_authority_id	IN INTEGER DEFAULT NULL,
	p_amount		IN NUMBER  DEFAULT NULL,
	p_amt_tax		IN NUMBER  DEFAULT NULL,
	p_rate			IN NUMBER  DEFAULT NULL,
	p_tax_area		IN NVARCHAR2 DEFAULT ' ',
	p_stamp 		IN INTEGER DEFAULT NULL
 )
AS
BEGIN
	INSERT INTO tax_calc_flow
	(
		asset_hdr_id,
		flow_id,
		contract_id,
		tax_type_id,
		tax_authority_id,
		amount,
		amt_tax,
		rate,
		tax_area,
		stamp
	)
	VALUES
	(
		p_asset_hdr_id,
		p_flow_id,
		p_contract_id,
		p_tax_type_id,
		p_tax_authority_id,
		p_amount,
		p_amt_tax,
		p_rate,
		p_tax_area,
		p_stamp
	);
END axsp_tax_calc_flow_ins;
/
CREATE OR REPLACE PROCEDURE axsp_asset_type_search(
  textfind IN nvarchar2 DEFAULT NULL,
  lookinname IN NUMBER DEFAULT NULL,
  lookinparent IN NUMBER DEFAULT NULL,
  lookindescription IN NUMBER DEFAULT NULL,
  lookinstockcode IN NUMBER DEFAULT NULL,
  programid IN INTEGER DEFAULT 0,
  rc1 IN OUT globalpkg.rct1,
  excludedAssetClassIds IN VARCHAR2 DEFAULT NULL
   ) AS
--
	textfind_ nvarchar2(1024) := textfind;
	sqlquery VARCHAR2(4000);
	sqlsearchfilter VARCHAR2(1024);
	parentfilter VARCHAR2(1024);
	stoo_error INTEGER;
	stoo_errmsg VARCHAR2(255);
	count_ INTEGER;
	programsql VARCHAR2(3000);
   SqlLicenseFilter VARCHAR2(3000);
	--
	CURSOR cur_program_list_with_children(p_program_id INTEGER) IS
		SELECT value_id
		FROM program_list
		WHERE list_type IN(27504, 27505) AND has_inherited_children = 1
		AND program_id = p_program_id;

BEGIN
  sqlquery := 'SELECT distinct at.asset_type_id,
												at.name,
												at.ext_name,
												at.code,
												at.owner_id,
												AXSP_GET_ASSET_TYPE_OWNER_TREE(at.asset_type_id) owner_hierarchy,
												o.name owner_name,
												at.stock_code,
												at.transaction_level,
												l.value transaction_level_value,
												at.asset_class_enum,
												at.is_obsolete
      FROM asset_type at, asset_type o, lookupset l
      WHERE at.asset_type_id != 0 AND at.owner_id = o.asset_type_id AND l.lookupset_id = at.transaction_level and at.is_deleted = 0 and at.is_active = 1';
  --
  SqlLicenseFilter := '';
   IF (excludedAssetClassIds != ' ') THEN
     SqlLicenseFilter := ' and at.asset_class_enum NOT IN (' || excludedAssetClassIds || ')';
     sqlquery := sqlquery || SqlLicenseFilter;
   END IF;
  --
  IF (textfind_ != ' ') THEN
    textfind_ := '''%' || LOWER(textfind) || '%''';
  END IF;
  --
  sqlsearchfilter := ' ';
  --
  IF ((lookinname = 1 OR lookinparent = 1) AND textfind_ != ' ') THEN
    sqlsearchfilter := ' Lower(at.name) LIKE ' || textfind_;
  END IF;
  --
  IF(lookindescription = 1 AND textfind_ != ' ') THEN
    IF(sqlsearchfilter = ' ') THEN
      sqlsearchfilter := 'Lower(at.ext_name) LIKE ' || textfind_;
    ELSE
      sqlsearchfilter := sqlsearchfilter || ' OR Lower(at.ext_name) LIKE ' || textfind_;
    END IF;
  END IF;
  --
  IF (lookinstockcode = 1 AND textfind_ != ' ') THEN
    IF (sqlsearchfilter = ' ') THEN
      sqlsearchfilter := 'Lower(at.stock_code) LIKE ' || textfind_;
    ELSE
      sqlsearchfilter := sqlsearchfilter || ' OR Lower(at.stock_code) LIKE ' || textfind_;
    END IF;
  END IF;
  --
  IF (sqlsearchfilter != ' ') THEN
    sqlquery := sqlquery || ' AND (' || sqlsearchfilter || ')';
  END IF;
  --
  programsql := ' ';
  --
  SELECT COUNT(*) INTO count_ FROM program_list
  WHERE list_type = 27505
  AND value_id = 0
  AND program_id = programid;
  --
  IF(programid != 0 AND count_ = 0) THEN
    programsql :=  'at.asset_type_id in (select value_id from program_list where list_type in (27504, 27505) and program_id = ' || to_char(programid) || ')';
    FOR rec IN cur_program_list_with_children(programid) LOOP
      programsql := programsql || ' OR at.asset_type_id IN (SELECT asset_type_id FROM TABLE(CAST(axsp_get_asset_type_child_tbl(' || to_char(rec.value_id) || ') AS ASSET_TYPE_CHILD_TBLTAB)))';
    END LOOP;
  END IF;
  --
  IF(programsql != ' ') THEN
    sqlquery := sqlquery || ' and (' || programsql || ')';
  END IF;
  --
  IF (lookinparent = 1 AND textfind_ != ' ') THEN
    parentfilter := ' union
SELECT distinct at.asset_type_id,
					at.name,
					at.ext_name,
					at.code,
					at.owner_id,
					AXSP_GET_ASSET_TYPE_OWNER_TREE(at.asset_type_id) owner_hierarchy,
					o.name owner_name,
					at.stock_code,
					at.transaction_level,
					l.value transaction_level_value,
					at.asset_class_enum,
					at.is_obsolete
FROM asset_type at, asset_type o, lookupset l
WHERE at.asset_type_id != 0
AND at.is_deleted = 0
AND at.is_active = 1
AND at.owner_id = o.asset_type_id
AND l.lookupset_id = at.transaction_level
AND lower(AXSP_GET_ASSET_TYPE_OWNER_TREE(at.asset_type_id)) like ' || textfind_;
    sqlquery := sqlquery || parentfilter || SqlLicenseFilter;
  END IF;
  --
  OPEN rc1 FOR sqlquery;
  --
  EXCEPTION
  WHEN no_data_found THEN
    NULL;
  WHEN others THEN
    stoo_error := SQLCODE;
    stoo_errmsg := sqlerrm;
    raise_application_error(SQLCODE,   sqlerrm,   TRUE);
END axsp_asset_type_search;
/
CREATE OR REPLACE PROCEDURE axsp_program_search(
TextFind                    IN NVARCHAR2  DEFAULT NULL,
IdTextFind                  IN NVARCHAR2  DEFAULT NULL,
ProductStyleIds             IN NVARCHAR2  DEFAULT NULL,
LookInProgramNo             IN NUMBER  DEFAULT NULL,
LookInProgramReference      IN NUMBER  DEFAULT NULL,
LookInProgramName           IN NUMBER  DEFAULT NULL,
LookInDescription           IN NUMBER  DEFAULT NULL,
LookInManagerId             IN NUMBER  DEFAULT NULL,
LookInMemberPartyNo         IN NUMBER  DEFAULT NULL,
LookInDefaultBusinessModel  IN NUMBER  DEFAULT NULL,
LookInDefaultProduct        IN NUMBER  DEFAULT NULL,
LookInDefaultLocation       IN NUMBER  DEFAULT NULL,
LookInDefaultAssetType      IN NUMBER  DEFAULT NULL,
LookInDefaultBranch         IN NUMBER  DEFAULT NULL,
IncludeCurrentPrograms      IN NUMBER  DEFAULT NULL,
IncludeFuturePrograms       IN NUMBER  DEFAULT NULL,
IncludeExpiredPrograms      IN NUMBER  DEFAULT NULL,
IncludeDeactivatedPrograms  IN NUMBER  DEFAULT NULL,
IncludeNonLocalPrograms     IN NUMBER  DEFAULT NULL,
IncludeGlobalBranchPrograms IN NUMBER  DEFAULT NULL,
AxUserId                    IN INTEGER DEFAULT NULL,
RC1                         IN OUT globalPkg.RCT1)
AS
--
  TextFind_              NVARCHAR2(1024) := TextFind;
  IdTextFind_            NVARCHAR2(1024) := IdTextFind;
  ProductStyleIds_       NVARCHAR2(1024) := ProductStyleIds;
  BusinessUnitsCsv       NVARCHAR2(1024);
  OwnerBizUnitsCsv		 NVARCHAR2(1024);
  TempBizUnitsCsv			 NVARCHAR2(1024);
  BusinessUnitId			 INTEGER;
  AccessRights				 INTEGER;
  BranchesCsv            NVARCHAR2(1024);
  OwnerBranchCsv			 NVARCHAR2(1024);
  BranchId					 INTEGER;
  StoO_error             INTEGER;
  StoO_errmsg            VARCHAR2(255);
  SqlQuery               VARCHAR2(4000);
  TodayDtText            VARCHAR2(255);
  SqlDatesFilter         VARCHAR2(1024);
  SqlSearchFilter        VARCHAR2(1024);

BEGIN
  SqlQuery := 'SELECT distinct program.program_id,program.name,program.ext_name,program.code,program.manager_id,ax_user.name manager_name,program.is_active FROM program' ||
                        ' INNER JOIN ax_user ON ax_user.ax_user_id = program.manager_id';
  TodayDtText := 'trunc(axsp_get_datetime())';

  IF (LookInMemberPartyNo = 1) THEN
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN program_party ON program_party.program_id = program.program_id';
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN party ON party.party_id = program_party.party_id';
  END IF;

  IF (LookInDefaultBusinessModel = 1) THEN
    SqlQuery := SqlQuery || ' INNER JOIN business_model ON business_model.business_model_id = program.business_model_id';
  END IF;

  IF (LookInDefaultProduct = 1) THEN
      SqlQuery := SqlQuery || ' INNER JOIN product ON product.product_id = program.product_id';
  END IF;

  IF (LookInDefaultLocation = 1) THEN
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN program_list location_list ON location_list.program_id = program.program_id AND location_list.list_type in (27506, 27507)';
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN location ON location.location_id = location_list.value_id';
  END IF;

  IF (LookInDefaultAssetType = 1) THEN
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN program_list asset_type_list ON asset_type_list.program_id = program.program_id AND asset_type_list.list_type = 27504';
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN asset_type ON asset_type.asset_type_id = asset_type_list.value_id';
  END IF;

  IF (LookInDefaultBranch = 1) THEN
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN program_list branch_list ON branch_list.program_id = program.program_id AND branch_list.list_type = 27513';
      SqlQuery := SqlQuery || ' LEFT OUTER JOIN party branch ON branch.party_id = branch_list.value_id';
  END IF;

  SqlQuery := SqlQuery || ' WHERE program.program_id != 0 AND program.is_deleted = 0 and program.credit_line_id = 0';

  IF (IncludeDeactivatedPrograms = 0) THEN
      SqlQuery := SqlQuery || ' AND program.is_active = 1';
  END IF;

  IF (ProductStyleIds_ != ' ') THEN
      SqlQuery := SqlQuery || ' AND program.product_id IN (SELECT product_id FROM product WHERE product_style IN ('||ProductStyleIds_||'))';
  END IF;

  SELECT access_rights INTO AccessRights FROM ax_user WHERE ax_user_id = AxUserId;
  IF (IncludeNonLocalPrograms = 0 AND AxUserId != 0 AND AccessRights != 10602) THEN -- not global
      SELECT business_units_csv INTO BusinessUnitsCsv FROM ax_user WHERE ax_user_id = AxUserId;
      SqlQuery := SqlQuery || ' AND (program.manager_id = ' || To_Char(AxUserId) || ' OR (SELECT COUNT(*) FROM program_list WHERE program_id = program.program_id and list_type = 27500) = 0';

      IF (BusinessUnitsCsv = ' ' ) THEN
        SqlQuery := SqlQuery || ')';
      ELSE
         SELECT business_unit_id INTO BusinessUnitId FROM ax_user WHERE ax_user_id = AxUserId;
         IF(AccessRights = 10601) THEN
            TempBizUnitsCsv  := axsp_get_party_child_list(BusinessUnitId);
		      IF (len(TempBizUnitsCsv) > 0) THEN
			      BusinessUnitsCsv := BusinessUnitsCsv || ',' || TempBizUnitsCsv;
			   END IF;
		  END IF;
        BusinessUnitsCsv := BusinessUnitsCsv || ',0';
        SqlQuery := SqlQuery || ' OR program.program_id IN (SELECT program_id FROM program_list WHERE list_type = 27500 AND (value_id IN (' || BusinessUnitsCsv || ')';
        OwnerBizUnitsCsv  := axsp_get_party_owner_list(BusinessUnitId);
        IF ( OwnerBizUnitsCsv = ' ' ) THEN
           SqlQuery := SqlQuery || ')))';
        ELSE
           OwnerBizUnitsCsv := OwnerBizUnitsCsv || ',0';
           SqlQuery := SqlQuery || ' OR value_id IN (' || OwnerBizUnitsCsv || ') AND has_inherited_children = 1 )))';
        END IF;
      END IF;
  END IF;

  IF (IncludeGlobalBranchPrograms = 0 AND AxUserId != 0) THEN
		SELECT branch_id INTO BranchId FROM ax_user WHERE ax_user_id = AxUserId;
      SELECT branches_csv INTO BranchesCsv FROM ax_user WHERE ax_user_id = AxUserId;
		OwnerBranchCsv := NVL(axsp_get_party_owner_list(BranchId),' ');
		IF (OwnerBranchCsv = ' ' ) THEN
		  OwnerBranchCsv := '0';
		ELSE
		  OwnerBranchCsv := OwnerBranchCsv || ',0';
		END IF;
      SqlQuery := SqlQuery || ' AND (program.manager_id = ' || To_Char(AxUserId) || ' OR program.branch_defaulting_type = 39402 OR (program.branch_defaulting_type = 39400 AND program.branch_id IN (' || OwnerBranchCsv || '))';
      IF (BranchesCsv = ' ' ) THEN
        BranchesCsv := '0';
      ELSE
        BranchesCsv := BranchesCsv || ',0';
		END IF;
      SqlQuery := SqlQuery || ' OR program.program_id IN (SELECT program_id FROM program_list WHERE list_type = 27513 AND value_id IN (' || BranchesCsv || ')';
		IF ( OwnerBranchCsv != '0' ) THEN
			SqlQuery := SqlQuery || ' OR value_id IN (' || OwnerBranchCsv || ') AND has_inherited_children = 1 ))';
		ELSE
			SqlQuery := SqlQuery || '))';
		END IF;
  END IF;

  SqlDatesFilter := ' ';
  IF (IncludeCurrentPrograms = 1) THEN
    SqlDatesFilter := '(program.effect_dt <= ' || TodayDtText || ' AND program.expiry_dt >= ' || TodayDtText || ')';
  END IF;

  IF (IncludeFuturePrograms = 1) THEN
      IF (SqlDatesFilter = ' ') THEN
          SqlDatesFilter := '(program.effect_dt > ' || TodayDtText || ')';
      ELSE
          SqlDatesFilter := SqlDatesFilter || ' OR (program.effect_dt > ' || TodayDtText || ')';
    END IF;
  END IF;

  IF (IncludeExpiredPrograms = 1) THEN
      IF (SqlDatesFilter = ' ') THEN
          SqlDatesFilter := '(program.expiry_dt < ' || TodayDtText || ')';
      ELSE
          SqlDatesFilter := SqlDatesFilter || ' OR (program.expiry_dt < ' || TodayDtText || ')';
    END IF;
  END IF;

  IF (SqlDatesFilter != ' ') THEN
      SqlQuery := SqlQuery || ' AND (' || SqlDatesFilter || ')';
  END IF;


  IF (TextFind_ != ' ') THEN
    TextFind_ := '''%' || Lower(TextFind_) || '%''';
  END IF;

  SqlSearchFilter := ' ';
  IF (LookInProgramNo = 1 AND IdTextFind_ != ' ') THEN
      SqlSearchFilter := ' program.program_id = ' || IdTextFind_;
  END IF;

  IF (LookInProgramReference = 1 AND TextFind_ != ' ') then
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := 'Lower(program.code) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(program.code) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInProgramName = 1 AND TextFind_ != ' ') THEN
    IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(program.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(program.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInDescription = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(program.ext_name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(program.ext_name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInManagerId = 1 AND TextFind_ != ' ') THEN
    IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(ax_user.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(ax_user.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInMemberPartyNo = 1 AND IdTextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' party.party_no = ' || IdTextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR party.party_no = ' || IdTextFind_;
    END IF;
  END IF;

  IF (LookInDefaultBusinessModel = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(business_model.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(business_model.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInDefaultProduct = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(product.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(product.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInDefaultLocation = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(location.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(location.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInDefaultAssetType = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
          SqlSearchFilter := ' Lower(asset_type.name) LIKE ' || TextFind_;
      ELSE
          SqlSearchFilter := SqlSearchFilter || ' OR Lower(asset_type.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (LookInDefaultBranch = 1 AND TextFind_ != ' ') THEN
      IF (SqlSearchFilter = ' ') THEN
        SqlSearchFilter := ' Lower(branch.name) LIKE ' || TextFind_;
      ELSE
	SqlSearchFilter := SqlSearchFilter || ' OR Lower(branch.name) LIKE ' || TextFind_;
    END IF;
  END IF;

  IF (SqlSearchFilter != ' ') THEN
      SqlQuery := SqlQuery || ' AND (' || SqlSearchFilter || ')';
  END IF;

  OPEN RC1 for SqlQuery;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);
END AXSP_PROGRAM_SEARCH;
/
CREATE OR REPLACE FUNCTION AXSP_GET_ASSET_TYPE_OWNER_TREE
(p_id IN NUMBER DEFAULT NULL) RETURN NVARCHAR2 AS
--
  v_owner_name   asset_type.name%TYPE;
  v_owner_tree   NVARCHAR2(2000);
  v_owner_id     number;
BEGIN
    v_owner_id := p_id;
    WHILE v_owner_id <> 0 LOOP
      FOR rec IN (SELECT owner_id, name
                  FROM asset_type
                  WHERE asset_type_id = v_owner_id)
      LOOP
        IF p_id != v_owner_id then
          v_owner_name := rec.name;
        ELSE
          v_owner_name := null;
        END IF;
        v_owner_id := rec.owner_id;
      END LOOP;
      IF v_owner_name IS NOT NULL then
        v_owner_tree := v_owner_name||N'\'|| v_owner_tree;
      END IF;
    END LOOP;
    --
    if v_owner_tree is not null then
      v_owner_tree := N'\'||v_owner_tree;
    end if;
    --
    RETURN v_owner_tree;
END AXSP_GET_ASSET_TYPE_OWNER_TREE;
/

CREATE OR REPLACE FUNCTION AXSP_GET_WF_STATUS
(		p_party_id INTEGER,
		p_contract_id INTEGER,
		p_asset_hdr_id INTEGER,
		p_task_id INTEGER,
		p_gl_journal_id INTEGER,
		p_opportunity_id INTEGER,
		p_slot INTEGER)
RETURN varchar AS
--
  v_result varchar(50);
BEGIN
  BEGIN
	  select ws.name
		into v_result
		from workflow w
			inner join wf_state_type ws on w.current_state_type = ws.wf_state_type_id
			inner join todo_hdr th on th.todo_hdr_id = w.wf_todo_hdr_id
			inner join wf_state_chain_hdr sh on th.wf_state_chain_hdr_id = sh.wf_state_chain_hdr_id
			inner join wf_type wt on wt.wf_type_id = sh.wf_type_id
		where
			w.party_id = p_party_id and
			w.contract_id = p_contract_id and
			w.asset_hdr_id = p_asset_hdr_id and
			w.task_id = p_task_id and
			w.gl_journal_id = p_gl_journal_id and
			w.opportunity_id = p_opportunity_id and
			wt.slot_id = p_slot and
			ROWNUM = 1;
	EXCEPTION
	WHEN NO_DATA_FOUND THEN
		null;
	END;
	return  v_result;
END AXSP_GET_WF_STATUS;
/
CREATE OR REPLACE FUNCTION AXSP_GET_WF_STATE_TYPE_ID
(		p_party_id INTEGER,
		p_contract_id INTEGER,
		p_asset_hdr_id INTEGER,
		p_task_id INTEGER,
		p_gl_journal_id INTEGER,
		p_opportunity_id INTEGER,
		p_slot INTEGER)
RETURN integer AS
--
  v_result integer;
BEGIN
  BEGIN
	  select ws.wf_state_type_id
		into v_result
		from workflow w
			inner join wf_state_type ws on w.current_state_type = ws.wf_state_type_id
			inner join todo_hdr th on th.todo_hdr_id = w.wf_todo_hdr_id
			inner join wf_state_chain_hdr sh on th.wf_state_chain_hdr_id = sh.wf_state_chain_hdr_id
			inner join wf_type wt on wt.wf_type_id = sh.wf_type_id
		where
			w.party_id = p_party_id and
			w.contract_id = p_contract_id and
			w.asset_hdr_id = p_asset_hdr_id and
			w.task_id = p_task_id and
			w.gl_journal_id = p_gl_journal_id and
			w.opportunity_id = p_opportunity_id and
			wt.slot_id = p_slot and
			ROWNUM = 1;
	EXCEPTION
	WHEN NO_DATA_FOUND THEN
		null;
	END;
	return v_result;
END AXSP_GET_WF_STATE_TYPE_ID;
/
CREATE OR REPLACE
FUNCTION AXSP_GET_CONTRACT_PARTY
(		p_role_id INTEGER,
		p_contract_id INTEGER)
RETURN varchar AS
--
  v_result varchar(50);
BEGIN
  BEGIN
	  select p.ext_name
		into v_result
	  from contract_party cp
		inner join party p on cp.party_id = p.party_id
	  where
		cp.system_defs_party_role_id = p_role_id
		and cp.contract_id = p_contract_id
		and ROWNUM = 1;
	EXCEPTION
	WHEN NO_DATA_FOUND THEN
		null;
	END;
	return  v_result;
END AXSP_GET_CONTRACT_PARTY;
/

CREATE OR REPLACE FUNCTION axsp_get_dir_exp_for_party
(
  -- Add the parameters for the function here
  v_party_id IN NUMBER
)
RETURN DIR_EXP_FOR_PARTY_TBLTAB PIPELINED IS
--
 X DIR_EXP_FOR_PARTY_TBLOBJ := DIR_EXP_FOR_PARTY_TBLOBJ (null,null,null,null,null,null,null,null,null,null);
--
   --Enum values
   v_NoneSlotType NUMBER(10,0);
   v_CustomerSlotType NUMBER(10,0);
   v_DealerSlotType NUMBER(10,0);
   v_VendorSlotType NUMBER(10,0);
   v_PermitHolderSlotType NUMBER(10,0);
   v_SalesPersonSlotType NUMBER(10,0);
   v_GuarantorSlotType NUMBER(10,0);
   v_NoneExposureCalculationType NUMBER(10,0);
   v_GrossUnpaidReceivableExposur NUMBER(10,0);
   v_GuaranteeTypeExposureCalcula NUMBER(10,0);
   v_MemorandumOnlyExposureCalcul NUMBER(10,0);
   v_GrossUnpaidRVExposureCalcula NUMBER(10,0);
   v_RVGuaranteeType NUMBER(10,0);
   v_BankGuaranteeType NUMBER(10,0);
   v_GrossUnpaidReceivableMem NUMBER(10,0);
   v_GrossUnpaidRVMemorandum NUMBER(10,0);
   v_GuaranteeGrossUnpaidRec NUMBER(10,0);
   v_GuaranteeGrossUnpaidRV NUMBER(10,0);
   v_GuaranteeBank NUMBER(10,0);
   v_time DATE;
   v_IsExpiredGuaranteeIncluded NUMBER(1,0);
BEGIN

   v_time := AXSP_GET_DATETIME;

   v_NoneSlotType := 36800;
   v_CustomerSlotType := 36801;
   v_DealerSlotType := 36802;
   v_VendorSlotType := 36803;
   v_PermitHolderSlotType := 36804;
   v_SalesPersonSlotType := 36805;
   v_GuarantorSlotType := 36810;
   v_NoneExposureCalculationType := 37900;
   v_GrossUnpaidReceivableExposur := 37901;
   v_GuaranteeTypeExposureCalcula := 37902;
   v_MemorandumOnlyExposureCalcul := 37903;
   v_GrossUnpaidRVExposureCalcula := 37904;
   v_GrossUnpaidReceivableMem := 37905;
	v_GrossUnpaidRVMemorandum := 37906;
	v_GuaranteeGrossUnpaidRec := 37907;
	v_GuaranteeGrossUnpaidRV := 37908;
	v_GuaranteeBank := 37909;
   v_RVGuaranteeType := 20003;
   v_BankGuaranteeType := 20004;

   SELECT is_expired_guarantee_included INTO v_IsExpiredGuaranteeIncluded FROM system_defs;

  FOR i IN
     --select stuff from asset balance
     (( SELECT contract_id,
                  product_id,
                  business_unit_id,
                  currency_id,
                  party_id,
                  amount_direct_exposure,
                  amount_memorandum,
                  party_role,
                  calculation_type,
                  contract_state
       FROM ( SELECT ab.contract_id,
                     c.product_id,
                     c.business_unit_id,
                     c.currency_id,
                     CASE
                          WHEN sdp.slot_type > v_NoneSlotType THEN
										CASE
                                    WHEN sdp.slot_type = v_CustomerSlotType THEN c.cparty_id
                                    WHEN sdp.slot_type = v_DealerSlotType THEN c.dealer_id
                                    WHEN sdp.slot_type = v_VendorSlotType THEN c.vendor_id
                                    WHEN sdp.slot_type = v_PermitHolderSlotType THEN c.permit_holder_id
										END
                     ELSE cp.party_id
                     END party_id,
                     sum(CASE
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableExposur THEN ab.gross_unpaid_receivable_amt
                          WHEN bmp.exposure_calculation_type = v_MemorandumOnlyExposureCalcul THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVExposureCalcula THEN ab.gross_unpaid_residual_amt
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableMem THEN 0
									WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVMemorandum THEN 0
                     END) amount_direct_exposure,
                     sum(CASE
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableExposur THEN 0
                          WHEN bmp.exposure_calculation_type = v_MemorandumOnlyExposureCalcul THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVExposureCalcula THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableMem THEN ab.gross_unpaid_receivable_amt
									WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVMemorandum THEN ab.gross_unpaid_residual_amt
                     END) amount_memorandum,
                     sdp.slot_type party_role,
                     bmp.exposure_calculation_type as calculation_type,
                     c.contract_state
              FROM asset_balance ab
                     INNER JOIN CONTRACT c ON ab.contract_id = c.contract_id
                     INNER JOIN business_model bm ON bm.business_model_id = c.business_model_id
                     INNER JOIN business_model_party bmp ON bm.business_model_id = bmp.business_model_id
                     INNER JOIN system_defs_party_role sdp ON sdp.system_defs_party_role_id = bmp.system_defs_party_role_id
                     LEFT JOIN contract_party cp ON cp.system_defs_party_role_id = sdp.system_defs_party_role_id AND cp.contract_id = ab.contract_id
              WHERE
					-- BugzId: 33219 - Exclude derived balances
					ab.is_derived_aggregate = 0 AND
					ab.effective_dt = (select max(effective_dt) from asset_balance ab2 where ab2.contract_id = ab.contract_id) AND
					bmp.exposure_calculation_type IN ( v_GrossUnpaidReceivableExposur,v_MemorandumOnlyExposureCalcul,v_GrossUnpaidRVExposureCalcula,v_GrossUnpaidReceivableMem,v_GrossUnpaidRVMemorandum ) AND
					sdp.slot_type <> v_SalesPersonSlotType
			  group by
					ab.contract_id,
					c.product_id,
					c.business_unit_id,
					c.currency_id,
					CASE
						WHEN sdp.slot_type > v_NoneSlotType THEN
							CASE
									WHEN sdp.slot_type = v_CustomerSlotType THEN c.cparty_id
									WHEN sdp.slot_type = v_DealerSlotType THEN c.dealer_id
									WHEN sdp.slot_type = v_VendorSlotType THEN c.vendor_id
									WHEN sdp.slot_type = v_PermitHolderSlotType THEN c.permit_holder_id
							END
					ELSE cp.party_id
					END,
					sdp.slot_type,
					bmp.exposure_calculation_type,
					c.contract_state
					) tbl
          WHERE
					party_id > 0 AND
					( party_id = v_party_id OR v_party_id = 0 ) )
   UNION
     --select stuff from guarantors
     ( SELECT g.contract_id,
                  c.product_id,
                  c.business_unit_id,
                  c.currency_id,
                  g.guarantor_party_id party_id,
                  SUM(CASE
                           WHEN g.guarantee_type = v_RVGuaranteeType THEN
											CASE
												WHEN COALESCE(ga.amount, g.amount) = 0 THEN ab.gross_unpaid_residual_amt
											ELSE
												CASE
													WHEN COALESCE(ga.amount, g.amount) < ab.gross_unpaid_residual_amt THEN COALESCE(ga.amount, g.amount)
													ELSE ab.gross_unpaid_residual_amt
												END
											END
                           WHEN g.guarantee_type = v_BankGuaranteeType THEN 0
									ELSE CASE
                                WHEN COALESCE(ga.amount, g.amount) = 0 THEN ab.gross_unpaid_receivable_amt
										  ELSE
												CASE
													WHEN COALESCE(ga.amount, g.amount) < ab.gross_unpaid_receivable_amt THEN COALESCE(ga.amount, g.amount)
													ELSE ab.gross_unpaid_receivable_amt
												END
											END
									END) amount_direct_exposure,
						0 amount_memorandum,
                  sdp.slot_type party_role,
						CASE
							WHEN g.guarantee_type = v_RVGuaranteeType THEN v_GuaranteeGrossUnpaidRV
							WHEN g.guarantee_type = v_BankGuaranteeType THEN v_GuaranteeBank
							ELSE v_GuaranteeGrossUnpaidRec
						END as calculation_type,
						c.contract_state
       FROM guarantee g
              LEFT JOIN guarantee_asset ga ON g.guarantee_id = ga.guarantee_id
              INNER JOIN asset_balance ab ON g.contract_id = ab.contract_id AND (COALESCE(ga.asset_id, ab.asset_id) = ab.asset_id or ab.asset_id = 0)
              INNER JOIN CONTRACT c ON ab.contract_id = c.contract_id
              INNER JOIN business_model bm ON bm.business_model_id = c.business_model_id
              INNER JOIN business_model_party bmp ON bm.business_model_id = bmp.business_model_id
              INNER JOIN system_defs_party_role sdp ON sdp.system_defs_party_role_id = bmp.system_defs_party_role_id
          WHERE ( g.guarantor_party_id = v_party_id OR v_party_id = 0 )
            AND g.effect_dt <= v_time
            AND ((v_IsExpiredGuaranteeIncluded = 0 and g.expiry_dt > v_time) or v_IsExpiredGuaranteeIncluded = 1)
            -- BugzId: 33219 - Exclude derived balances
            AND ab.is_derived_aggregate = 0
            AND ab.effective_dt = (select max(effective_dt) from asset_balance ab2 where ab2.contract_id = ab.contract_id)
            AND bmp.exposure_calculation_type = v_GuaranteeTypeExposureCalcula
            AND sdp.slot_type = v_GuarantorSlotType
         GROUP BY g.contract_id,c.product_id,c.business_unit_id,c.currency_id,g.guarantor_party_id,sdp.slot_type,g.guarantee_type,c.contract_state)
         ) LOOP
            X.contract_id := i.contract_id;
            X.product_id := i.product_id;
            X.business_unit_id := i.business_unit_id;
            X.currency_id := i.currency_id;
            X.party_id := i.party_id;
            X.amount_direct_exposure := i.amount_direct_exposure;
            X.amount_memorandum := i.amount_memorandum;
            X.party_role := i.party_role;
            X.calculation_type := i.calculation_type;
            X.contract_state := i.contract_state;
      PIPE ROW ( X );
		END LOOP;
END;
/

CREATE OR REPLACE FUNCTION axsp_get_dir_exp_for_parties
(
  -- Add the parameters for the function here
  p_party_ids varchar2
)
RETURN DIR_EXP_FOR_PARTY_TBLTAB PIPELINED IS
--
 X DIR_EXP_FOR_PARTY_TBLOBJ := DIR_EXP_FOR_PARTY_TBLOBJ (null,null,null,null,null,null,null,null,null,null);
--
   --Enum values
   v_NoneSlotType NUMBER(10,0);
   v_CustomerSlotType NUMBER(10,0);
   v_DealerSlotType NUMBER(10,0);
   v_VendorSlotType NUMBER(10,0);
   v_PermitHolderSlotType NUMBER(10,0);
   v_SalesPersonSlotType NUMBER(10,0);
   v_GuarantorSlotType NUMBER(10,0);
   v_NoneExposureCalculationType NUMBER(10,0);
   v_GrossUnpaidReceivableExposur NUMBER(10,0);
   v_GuaranteeTypeExposureCalcula NUMBER(10,0);
   v_MemorandumOnlyExposureCalcul NUMBER(10,0);
   v_GrossUnpaidRVExposureCalcula NUMBER(10,0);
   v_RVGuaranteeType NUMBER(10,0);
   v_BankGuaranteeType NUMBER(10,0);
   v_GrossUnpaidReceivableMem NUMBER(10,0);
   v_GrossUnpaidRVMemorandum NUMBER(10,0);
   v_GuaranteeGrossUnpaidRec NUMBER(10,0);
   v_GuaranteeGrossUnpaidRV NUMBER(10,0);
   v_GuaranteeBank NUMBER(10,0);

   v_party_ids VARCHAR2(32767);
   v_doAllParties NUMBER(1,0);
   v_time DATE;
   v_IsExpiredGuaranteeIncluded NUMBER(1,0);
BEGIN

   v_time := AXSP_GET_DATETIME;

--Store the partyIds in a table variable
IF (p_party_ids=' ') THEN
  v_party_ids:='';
ELSE
  v_party_ids:=p_party_ids;
END IF;
delete from PARTY_EXPOSURE_TBL;
insert into PARTY_EXPOSURE_TBL
select to_number(spl.column_value) from table(axsp_split(v_party_ids,',')) spl;

-- Check if we need to the direct exposure for all parties
select count(*) into v_doAllParties from PARTY_EXPOSURE_TBL;	-- if there is records then put 0 in PARTY_EXPOSURE_TBL
IF (v_doAllParties > 0) THEN
  v_doAllParties := 0; -- if there are records put 0
ELSE
  v_doAllParties := 1; -- if there are no records do the job for all parties
END IF;


   v_NoneSlotType := 36800;
   v_CustomerSlotType := 36801;
   v_DealerSlotType := 36802;
   v_VendorSlotType := 36803;
   v_PermitHolderSlotType := 36804;
   v_SalesPersonSlotType := 36805;
   v_GuarantorSlotType := 36810;
   v_NoneExposureCalculationType := 37900;
   v_GrossUnpaidReceivableExposur := 37901;
   v_GuaranteeTypeExposureCalcula := 37902;
   v_MemorandumOnlyExposureCalcul := 37903;
   v_GrossUnpaidRVExposureCalcula := 37904;
   v_GrossUnpaidReceivableMem := 37905;
	v_GrossUnpaidRVMemorandum := 37906;
	v_GuaranteeGrossUnpaidRec := 37907;
	v_GuaranteeGrossUnpaidRV := 37908;
	v_GuaranteeBank := 37909;
   v_RVGuaranteeType := 20003;
   v_BankGuaranteeType := 20004;

   SELECT is_expired_guarantee_included INTO v_IsExpiredGuaranteeIncluded FROM system_defs;

  FOR i IN
     --select stuff from asset balance
     (( SELECT contract_id,
                  product_id,
                  business_unit_id,
                  currency_id,
                  party_id,
                  amount_direct_exposure,
                  amount_memorandum,
                  party_role,
                  calculation_type,
                  contract_state
       FROM ( SELECT ab.contract_id,
                     c.product_id,
                     c.business_unit_id,
                     c.currency_id,
                     CASE
                          WHEN sdp.slot_type > v_NoneSlotType THEN
										CASE
                                    WHEN sdp.slot_type = v_CustomerSlotType THEN c.cparty_id
                                    WHEN sdp.slot_type = v_DealerSlotType THEN c.dealer_id
                                    WHEN sdp.slot_type = v_VendorSlotType THEN c.vendor_id
                                    WHEN sdp.slot_type = v_PermitHolderSlotType THEN c.permit_holder_id
										END
                     ELSE cp.party_id
                     END party_id,
                     sum(CASE
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableExposur THEN ab.gross_unpaid_receivable_amt
                          WHEN bmp.exposure_calculation_type = v_MemorandumOnlyExposureCalcul THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVExposureCalcula THEN ab.gross_unpaid_residual_amt
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableMem THEN 0
									WHEN bmp.exposure_calculation_type =  v_GrossUnpaidRVMemorandum THEN 0
                     END) amount_direct_exposure,
                     sum(CASE
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableExposur THEN 0
                          WHEN bmp.exposure_calculation_type = v_MemorandumOnlyExposureCalcul THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidRVExposureCalcula THEN 0
                          WHEN bmp.exposure_calculation_type = v_GrossUnpaidReceivableMem THEN ab.gross_unpaid_receivable_amt
									WHEN bmp.exposure_calculation_type =  v_GrossUnpaidRVMemorandum THEN ab.gross_unpaid_residual_amt
                     END) amount_memorandum,
                     sdp.slot_type party_role,
                     bmp.exposure_calculation_type as calculation_type,
                     c.contract_state
              FROM asset_balance ab
                     INNER JOIN CONTRACT c ON ab.contract_id = c.contract_id
                     INNER JOIN business_model bm ON bm.business_model_id = c.business_model_id
                     INNER JOIN business_model_party bmp ON bm.business_model_id = bmp.business_model_id
                     INNER JOIN system_defs_party_role sdp ON sdp.system_defs_party_role_id = bmp.system_defs_party_role_id
                     LEFT JOIN contract_party cp ON cp.system_defs_party_role_id = sdp.system_defs_party_role_id AND cp.contract_id = ab.contract_id
              WHERE
					-- BugzId: 33219 - Exclude derived balances
					ab.is_derived_aggregate = 0 AND
					ab.effective_dt = (select max(effective_dt) from asset_balance ab2 where ab2.contract_id = ab.contract_id) AND
					bmp.exposure_calculation_type IN ( v_GrossUnpaidReceivableExposur,v_MemorandumOnlyExposureCalcul,v_GrossUnpaidRVExposureCalcula,v_GrossUnpaidReceivableMem,v_GrossUnpaidRVMemorandum ) AND
					sdp.slot_type <> v_SalesPersonSlotType
			  group by
					ab.contract_id,
					c.product_id,
					c.business_unit_id,
					c.currency_id,
					CASE
						WHEN sdp.slot_type > v_NoneSlotType THEN
							CASE
									WHEN sdp.slot_type = v_CustomerSlotType THEN c.cparty_id
									WHEN sdp.slot_type = v_DealerSlotType THEN c.dealer_id
									WHEN sdp.slot_type = v_VendorSlotType THEN c.vendor_id
									WHEN sdp.slot_type = v_PermitHolderSlotType THEN c.permit_holder_id
							END
					ELSE cp.party_id
					END,
					sdp.slot_type,
					bmp.exposure_calculation_type,
					c.contract_state
					) tbl
          WHERE
					party_id > 0 AND
					(party_id in (select party_id from PARTY_EXPOSURE_TBL) OR v_doAllParties = 1) )
   UNION
     --select stuff from guarantors
     ( SELECT g.contract_id,
                  c.product_id,
                  c.business_unit_id,
                  c.currency_id,
                  g.guarantor_party_id party_id,
                  SUM(CASE
                           WHEN g.guarantee_type = v_RVGuaranteeType THEN
											CASE
												WHEN COALESCE(ga.amount, g.amount) = 0 THEN ab.gross_unpaid_residual_amt
											ELSE
												CASE
													WHEN COALESCE(ga.amount, g.amount) < ab.gross_unpaid_residual_amt THEN COALESCE(ga.amount, g.amount)
													ELSE ab.gross_unpaid_residual_amt
												END
											END
                           WHEN g.guarantee_type = v_BankGuaranteeType THEN 0
									ELSE CASE
                                WHEN COALESCE(ga.amount, g.amount) = 0 THEN ab.gross_unpaid_receivable_amt
										  ELSE
												CASE
													WHEN COALESCE(ga.amount, g.amount) < ab.gross_unpaid_receivable_amt THEN COALESCE(ga.amount, g.amount)
													ELSE ab.gross_unpaid_receivable_amt
												END
											END
									END) amount_direct_exposure,
						0 amount_memorandum,
                  sdp.slot_type party_role,
						CASE
							WHEN g.guarantee_type = v_RVGuaranteeType THEN v_GuaranteeGrossUnpaidRV
							WHEN g.guarantee_type = v_BankGuaranteeType THEN v_GuaranteeBank
							ELSE v_GuaranteeGrossUnpaidRec
						END as calculation_type,
						c.contract_state
       FROM guarantee g
              LEFT JOIN guarantee_asset ga ON g.guarantee_id = ga.guarantee_id
              INNER JOIN asset_balance ab ON g.contract_id = ab.contract_id AND (COALESCE(ga.asset_id, ab.asset_id) = ab.asset_id or ab.asset_id = 0)
              INNER JOIN CONTRACT c ON ab.contract_id = c.contract_id
              INNER JOIN business_model bm ON bm.business_model_id = c.business_model_id
              INNER JOIN business_model_party bmp ON bm.business_model_id = bmp.business_model_id
              INNER JOIN system_defs_party_role sdp ON sdp.system_defs_party_role_id = bmp.system_defs_party_role_id
          WHERE ( g.guarantor_party_id in (select party_id from PARTY_EXPOSURE_TBL) OR v_doAllParties = 1)
				AND g.effect_dt <= v_time
				AND ((v_IsExpiredGuaranteeIncluded = 0 and g.expiry_dt > v_time) or v_IsExpiredGuaranteeIncluded = 1)
				-- BugzId: 33219 - Exclude derived balances
				AND ab.is_derived_aggregate = 0
				AND ab.effective_dt = (select max(effective_dt) from asset_balance ab2 where ab2.contract_id = ab.contract_id)
				AND bmp.exposure_calculation_type = v_GuaranteeTypeExposureCalcula
				AND sdp.slot_type = v_GuarantorSlotType
         GROUP BY g.contract_id,c.product_id,c.business_unit_id,c.currency_id,g.guarantor_party_id,sdp.slot_type,g.guarantee_type,c.contract_state)
         ) LOOP
            X.contract_id := i.contract_id;
            X.product_id := i.product_id;
            X.business_unit_id := i.business_unit_id;
            X.currency_id := i.currency_id;
            X.party_id := i.party_id;
            X.amount_direct_exposure := i.amount_direct_exposure;
            X.amount_memorandum := i.amount_memorandum;
            X.party_role := i.party_role;
            X.calculation_type := i.calculation_type;
            X.contract_state := i.contract_state;
      PIPE ROW ( X );
		END LOOP;
END;
/

CREATE OR REPLACE PROCEDURE axsp_contract_search
(
	ContractId IN NUMBER DEFAULT NULL,
	ContractReference IN NVARCHAR2,
	CustomerNo IN NUMBER DEFAULT NULL,
	CustomerName IN NVARCHAR2,
	CustomFields IN NVARCHAR2,
	ProductStyle IN NUMBER DEFAULT NULL,
	ProductId IN NUMBER DEFAULT NULL,
	ProgramId IN NUMBER DEFAULT NULL,
	AssetTypeId IN NUMBER DEFAULT NULL,
	ContainFlag IN NUMBER DEFAULT NULL,
	ExcludeDeleted IN NUMBER DEFAULT NULL,
    RC1 IN OUT globalPkg.RCT1
)
AS
  ContractIdStr NVARCHAR2(120) := ContractId;
  ContractReferenceStr NVARCHAR2(50) := ContractReference;
  CustomerNoStr NVARCHAR2(120) := CustomerNo;
  CustomerNameStr NVARCHAR2(275) := CustomerName;
  CustomFieldsStr NVARCHAR2(2020) := CustomFields;
  ProductStyleStr NVARCHAR2(120) := ProductStyle;
  ProductIdStr NVARCHAR2(120) := ProductId;
  ProgramIdStr NVARCHAR2(120) := ProgramId;
  AssetTypeIdStr NVARCHAR2(120) := AssetTypeId;
  SqlQuery VARCHAR2(4000);
  SqlFilter VARCHAR2(4000);
  StoO_error             INTEGER;
  StoO_errmsg            VARCHAR2(255);
  ContainWildcard NVARCHAR2(10);
  row_limit CONSTANT INTEGER := 500;
BEGIN

SqlQuery :=
'select c.contract_id,
        c.reference as contract_reference,
        p.ext_name as party_ext_name,
        pd.name as product_name,
        pg.name as program_name,
        c.contract_state,
        c.calc_dt,
        c.mature_dt1,
        c.amt_financed,
        c.credit_state,
        c.is_active,
        c.save_status,
        c.suspension_state,
        c.intercept_state,
        c.business_unit_id
from contract c
inner join party p on c.cparty_id = p.party_id
inner join product pd on c.product_id = pd.product_id
inner join program pg on c.program_id = pg.program_id ';

IF (AssetTypeIdStr > 0) THEN
        SqlQuery := SqlQuery ||
                    'left outer join asset a on c.contract_id = a.contract_id
                     left outer join asset_hdr ah on a.asset_hdr_id = ah.asset_hdr_id ';
END IF;

IF (LEN(CustomFieldsStr) > 0) THEN
        SqlQuery := SqlQuery || 'left outer join contract_cfv cf on c.contract_id = cf.contract_id ';
END IF;

SqlQuery := SqlQuery || 'and rownum <= ' || row_limit || ' ';

SqlFilter := ' where c.contract_id > 0 ';

ContainWildCard := '';
IF (ContainFlag = 1) THEN
        ContainWildCard := '%';
END IF;

IF (ContractIdStr > 0) THEN
        SqlFilter := SqlFilter || 'and c.contract_id = ' || ContractIdStr || ' ';
END IF;

IF (LEN(ContractReferenceStr) > 0) THEN
        SqlFilter := SqlFilter || 'and c.reference like ''' || ContainWildCard || ContractReferenceStr || '%'' ';
END IF;

IF (CustomerNoStr > 0) THEN
        SqlFilter := SqlFilter || 'and p.party_no = ' || CustomerNoStr || ' ';
END IF;

IF (LEN(CustomerNameStr) > 0) THEN
        SqlFilter := SqlFilter || 'and (p.name like ''' || ContainWildCard || CustomerNameStr || '%'' or p.ext_name like ''' || ContainWildCard || CustomerNameStr || '%'' or p.first_names like ''' || ContainWildCard || CustomerNameStr || '%'') ';
END IF;

IF (LEN(CustomFieldsStr) > 0) THEN
        SqlFilter := SqlFilter || 'and (cf.field_value like ''' || ContainWildcard || CustomFieldsStr || '%'') ';
END IF;

IF (ProductStyleStr > 0) THEN
        SqlFilter := SqlFilter || 'and pd.product_style = ' || ProductStyleStr || ' ';
END IF;

IF (ProductIdStr > 0) THEN
        SqlFilter := SqlFilter || 'and pd.product_id = ' || ProductIdStr || ' ';
END IF;

IF (ProgramIdStr > 0) THEN
        SqlFilter := SqlFilter || 'and pg.program_id = ' || ProgramIdStr || ' ';
END IF;

IF (ExcludeDeleted = 1) THEN
        SqlFilter := SqlFilter || 'and c.is_active = 1 ';
END IF;

IF (AssetTypeIdStr > 0) THEN
        SqlFilter := SqlFilter || 'and ah.asset_type_id = ' || AssetTypeIdStr || ' ';
END IF;

SqlQuery := SqlQuery || SqlFilter;

  OPEN RC1 for SqlQuery;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      StoO_error := SQLCODE;
      StoO_errmsg := SQLERRM;
      raise_application_error(SQLCODE, SQLERRM,true);

END axsp_contract_search;
/
CREATE OR REPLACE PROCEDURE AXSP_FLOW_SET_FIRST_SBI
(
  p_from_dt         IN DATE  DEFAULT NULL,
  p_run_dt         IN DATE  DEFAULT NULL,
  p_settlement_bank_info_id	IN NUMBER DEFAULT NULL
) AS
--
  StoO_error   INTEGER;
  StoO_errmsg  VARCHAR2(255);
BEGIN
  BEGIN
  StoO_error := 0;
  DELETE FROM TP_FLOW_SBI_TABLE_1;
  DELETE FROM TP_FLOW_SBI_TABLE_2;

    INSERT INTO TP_FLOW_SBI_TABLE_1
    SELECT sql1.*,
      NVL(bfm.bank_flow_match_id, 0) as bfm_id, NVL(bf.bank_flow_type, 0) as bf_payment_method
    FROM (
      SELECT
			f.settlement_bank_info_id, f.flow_id, f.expected_dt,
			sbi.status as sbi_status, sbi.approval_status,
			f.is_cash, f.contract_id, f.flow_method_id, f.status, f.reversal_status
      FROM flow f
      INNER JOIN settlement_bank_info sbi
			ON f.settlement_bank_info_id = sbi.settlement_bank_info_id
			AND sbi.settlement_bank_info_id != 0
			AND sbi.is_set_first_settle_flow = 0 -- Includes locked instruction that does not have a first settle flow
			-- AND sbi.status IN (6700, 6702) --NotRequired/Loaded
			AND sbi.approval_status IN (6501, 6503) -- Approved/None
			AND sbi.is_active = 1
      INNER JOIN flow_method fm
			ON fm.flow_method_id = f.flow_method_id
			AND fm.payment_method = 5101 -- specific to direct debit
      WHERE f.flow_method_id != 0
         AND f.expected_dt >= p_from_dt
         AND f.expected_dt <= p_run_dt
      	AND f.settlement_bank_info_id = (case p_settlement_bank_info_id when -1 then f.settlement_bank_info_id else p_settlement_bank_info_id END)
			AND f.is_shadow_copy = 0
			AND f.amt_gross_netted != 0
         AND f.contract_id+0 >= 0
    ) sql1
    LEFT OUTER JOIN bank_flow_match bfm
		 on bfm.flow_id = sql1.flow_id
		 and bfm.is_deleted = 0
		 and bfm.is_rejected = 0
    LEFT OUTER JOIN bank_flow bf
		 on bf.bank_flow_id = bfm.bank_flow_id
		 and bf.is_deleted = 0
		 and bf.reversal_status != 4201; -- Bank flow is not Reversed/Rejected

  INSERT INTO TP_FLOW_SBI_TABLE_2
    SELECT settlement_bank_info_id, NVL(MIN(flow_id), 0) flow_id, NVL(MIN(expected_dt), axsp_get_datetime()) expected_dt
    FROM TP_FLOW_SBI_TABLE_1 tmp1a
    WHERE reversal_status IN (4200, 4202) --None/Reversal
      AND status IN (2101, 2102) --Released/Settled
      AND expected_dt = (SELECT MIN(tmp1b.expected_dt) FROM TP_FLOW_SBI_TABLE_1 tmp1b
         WHERE tmp1a.settlement_bank_info_id = tmp1b.settlement_bank_info_id
         AND tmp1b.reversal_status IN (4200, 4202) --None/Reversal
         AND tmp1b.status IN (2101, 2102) --Released/Settled
         AND ((tmp1b.bank_flow_match_id != 0 and tmp1b.bank_flow_payment_method = 5101 and tmp1b.status != 2101) or (tmp1b.bank_flow_match_id = 0)))
	  AND ((bank_flow_match_id != 0 and bank_flow_payment_method = 5101 and status != 2101) or (bank_flow_match_id = 0))
    GROUP BY settlement_bank_info_id;
    EXCEPTION
      WHEN TOO_MANY_ROWS THEN
        null;
      WHEN NO_DATA_FOUND THEN
        null;
      WHEN OTHERS THEN
        StoO_error := SQLCODE;
        StoO_errmsg := SQLERRM;
        raise_application_error(SQLCODE, SQLERRM,true);
  END;
  BEGIN
  StoO_error   := 0;

  FOR i IN (SELECT flow_id, settlement_bank_info_id FROM TP_FLOW_SBI_TABLE_2 WHERE flow_id != 0 AND settlement_bank_info_id != 0) LOOP
	UPDATE flow SET is_first_settlement_bank_info = 1
	WHERE flow_id = i.flow_id;

	UPDATE settlement_bank_info SET is_set_first_settle_flow = 1
	WHERE settlement_bank_info_id = i.settlement_bank_info_id;
  END LOOP;

  EXCEPTION
     WHEN NO_DATA_FOUND THEN
       NULL;
     WHEN OTHERS THEN
       StoO_error := SQLCODE;
       StoO_errmsg := SQLERRM;
       raise_application_error(SQLCODE, SQLERRM, true);
  END;
END AXSP_FLOW_SET_FIRST_SBI;
/
CREATE OR REPLACE PROCEDURE AXSP_XT_LOOKUPSET_LINK_INS(
response_set      IN VARCHAR2  DEFAULT NULL,
response_value      IN VARCHAR2  DEFAULT NULL,
response_code      IN VARCHAR2  DEFAULT NULL,
sub_response_set      IN VARCHAR2  DEFAULT NULL,
sub_response_value     IN VARCHAR2  DEFAULT NULL,
sub_response_code      IN VARCHAR2  DEFAULT NULL)
AS
--
rec_counter NUMBER := 0;
response_id INTEGER := 0;
sub_response_id INTEGER := 0;
link_id INTEGER := 0;
--
BEGIN

  /*  Insert Parent record if does not exist */
  /*  -------------------------------------- */

    SELECT Count(xt_lookupset_id) INTO rec_counter
    FROM    xt_lookupset
    WHERE code = response_code AND set_name = response_set;

    IF (rec_counter = 0) THEN
      INSERT INTO xt_lookupset (
        set_name,
        value,
        code)
      VALUES (
        response_set,
        response_value,
        response_code);
       SELECT s_xt_lookupset.currval INTO response_id  FROM DUAL;
    ELSE
      SELECT xt_lookupset_id
      INTO    response_id
      FROM    xt_lookupset
      WHERE code = response_code AND set_name = response_set;
    END IF;

  /*  Insert Child record if does not exist */
  /*  ------------------------------------- */

    SELECT Count(xt_lookupset_id) INTO rec_counter
    FROM    xt_lookupset
    WHERE code = sub_response_code AND set_name = sub_response_set;


    IF (rec_counter = 0) THEN
      INSERT INTO xt_lookupset (
        set_name,
        value,
        code)
      VALUES (
        sub_response_set,
        sub_response_value,
        sub_response_code);

       SELECT s_xt_lookupset.currval INTO sub_response_id  FROM DUAL;
    ELSE
      SELECT xt_lookupset_id
      INTO    sub_response_id
      FROM    xt_lookupset
      WHERE code = sub_response_code AND set_name = sub_response_set;
    END IF;

  /*  Insert Link record if does not exist */
  /*  ------------------------------------ */

    SELECT  COUNT(xt_lookupset_link_id)
    INTO    rec_counter
    FROM    xt_lookupset_link
    WHERE   xt_lookupset_parent_id = response_id AND xt_lookupset_child_id = sub_response_id;

     IF (rec_counter = 0) THEN
       INSERT INTO xt_lookupset_link (
        xt_lookupset_parent_id,
        xt_lookupset_child_id)
       VALUES (
        response_id,
        sub_response_id);
    END IF;

    COMMIT;

END AXSP_XT_LOOKUPSET_LINK_INS;
/
CREATE OR REPLACE PROCEDURE AXSP_CPU_CALC_DEL(
cpu_calc_id_1   IN NUMBER  DEFAULT NULL) AS
--
BEGIN
  DELETE cpu_calc
  WHERE cpu_calc_id = cpu_calc_id_1;
END AXSP_CPU_CALC_DEL;
/
CREATE OR REPLACE PROCEDURE AXSP_CPU_CALC_INS(
	 cpu_cbg_id_1              IN NUMBER  DEFAULT NULL,
	 contract_id_2             IN NUMBER  DEFAULT NULL,
	 flow_id_3                 IN NUMBER  DEFAULT NULL,
	 cpu_component_type_4      IN NUMBER  DEFAULT NULL,
	 cost_per_use_5            IN NUMBER  DEFAULT NULL,
	 usage_charged_6           IN NUMBER  DEFAULT NULL,
	 units_7                   IN NUMBER  DEFAULT NULL,
	 amount_8                  IN NUMBER  DEFAULT NULL,
	 amt_invoice_9	            IN NUMBER  DEFAULT NULL,
	 is_cash_10                IN NUMBER  DEFAULT NULL,
	 is_waived_11              IN NUMBER  DEFAULT NULL,
	 stamp_12                  IN NUMBER  DEFAULT NULL) AS
--
BEGIN
INSERT INTO cpu_calc (
    cpu_calc_id,
    cpu_cbg_id,
    contract_id,
    flow_id,
    cpu_component_type,
    cost_per_use,
    usage_charged,
    units,
    amount,
    amt_invoice,
    is_cash,
    is_waived,
    stamp)
 VALUES (
    s_cpu_calc.nextval,
    cpu_cbg_id_1,
    contract_id_2,
    flow_id_3,
    cpu_component_type_4,
    cost_per_use_5,
    usage_charged_6,
    units_7,
    amount_8,
    amt_invoice_9,
    is_cash_10,
    is_waived_11,
    stamp_12);
END AXSP_CPU_CALC_INS;
/

CREATE OR REPLACE TRIGGER axtr_flow
AFTER DELETE OR UPDATE OF status, is_set, reversal_status, amt_matched, invoice_id, bank_interface_run_id, 
					rejected_reason, settlement_bank_info_id, collection_state, amt_gross, release_dt, 
					settled_dt, rejected_dt, nett_no, expected_dt, bank_account_id, pp_nett_no, 
					amt_gross_netted,	amt_matched_netted ON flow FOR EACH ROW
BEGIN
	IF UPDATING THEN
		IF (:old.status != :new.status OR
			:old.is_set != :new.is_set OR
			:old.reversal_status != :new.reversal_status OR
			:old.amt_matched != :new.amt_matched OR
			:old.invoice_id != :new.invoice_id OR
			:old.bank_interface_run_id != :new.bank_interface_run_id OR
			:old.rejected_reason != :new.rejected_reason OR
			:old.settlement_bank_info_id != :new.settlement_bank_info_id OR
			:old.collection_state != :new.collection_state OR
			:old.amt_gross != :new.amt_gross OR
			:old.release_dt != :new.release_dt OR
			:old.settled_dt != :new.settled_dt OR
			:old.rejected_dt != :new.rejected_dt OR
			:old.nett_no != :new.nett_no OR
			:old.expected_dt != :new.expected_dt OR
			:old.bank_account_id != :new.bank_account_id OR
			:old.pp_nett_no != :new.pp_nett_no OR
			:old.amt_gross_netted != :new.amt_gross_netted OR
			:old.amt_matched_netted != :new.amt_matched_netted) THEN
		insert into flow_audit (
			flow_audit_id,
			flow_id,
			audit_dt,
			user_id,
			contract_id,
			image_no,
			status,
			is_set,
			reversal_status,
			amt_matched,
			invoice_id,
			bank_interface_run_id,
			rejected_reason,
			settlement_bank_info_id,
			collection_state,
			amt_gross,
			release_dt,
			settled_dt,
			rejected_dt,
			nett_no,
			expected_dt,
			bank_account_id,
			pp_nett_no,
			amt_gross_netted,
			amt_matched_netted,
			purchase_invoice_id,
			stamp)
		 values (
			s_flow_audit.nextval,
			:new.flow_id,
			axsp_get_datetime(),
			:new.last_user_id,
			:old.contract_id,
			:old.image_no,
			:old.status,
			:old.is_set,
			:old.reversal_status,
			:old.amt_matched,
			:old.invoice_id,
			:old.bank_interface_run_id,
			:old.rejected_reason,
			:old.settlement_bank_info_id,
			:old.collection_state,
			:old.amt_gross,
			:old.release_dt,
			:old.settled_dt,
			:old.rejected_dt,
			:old.nett_no,
			:old.expected_dt,
			:old.bank_account_id,
			:old.pp_nett_no,
			:old.amt_gross_netted,
			:old.amt_matched_netted,   
			:old.purchase_invoice_id,  
			:old.stamp);
		END IF;
	ELSE
		insert into flow_audit (
			flow_audit_id,
			flow_id,
			audit_dt,
			user_id,
			contract_id,
			image_no,
			status,
			is_set,
			reversal_status,
			amt_matched,
			invoice_id,
			bank_interface_run_id,
			rejected_reason,
			settlement_bank_info_id,
			collection_state,
			amt_gross,
			release_dt,
			settled_dt,
			rejected_dt,
			nett_no,
			expected_dt,
			bank_account_id,
			pp_nett_no,
			amt_gross_netted,
			amt_matched_netted,
			purchase_invoice_id,
			stamp)
		values (
			s_flow_audit.nextval,
			:old.flow_id,
			axsp_get_datetime(),
			0,
			:old.contract_id,
			:old.image_no,
			:old.status,
			:old.is_set,
			:old.reversal_status,
			:old.amt_matched,
			:old.invoice_id,
			:old.bank_interface_run_id,
			:old.rejected_reason,
			:old.settlement_bank_info_id,
			:old.collection_state,
			:old.amt_gross,
			:old.release_dt,
			:old.settled_dt,
			:old.rejected_dt,
			:old.nett_no,
			:old.expected_dt,
			:old.bank_account_id,
			:old.pp_nett_no,
			:old.amt_gross_netted,
			:old.amt_matched_netted,   
			:old.purchase_invoice_id,  
			:old.stamp);
	END IF;
END;
/

CREATE OR REPLACE TRIGGER axtr_asset_hdr_audit
AFTER INSERT OR UPDATE OF image_no, original_contract_id, current_contract_id, 
			serial_no, maturity_dt, reference, asset_status, amt_selling_price, amt_initial_book_value, amt_salvage_value,
			sec_amt_initial_book_value, sec_amt_salvage_value,
			tax_amt_initial_book_value, tax_amt_salvage_value, input_user_id, last_saved_by_id, ownership_type, orig_asset_hdr_id,
			branch_id, amt_retention, disposal_dt, owner_id, hdr_type, amt_base_repayment, pending_amt_base_repayment, parent_owner_id,
			reserved_contract_id, has_asset_children, initial_fx_rate, purchase_ccy_id, purchase_ccy_price, amt_inertia_salvage_value, 
			tax_payment_code, stamp ON asset_hdr FOR EACH ROW
BEGIN
	insert into asset_hdr_audit (
			audit_dt, asset_hdr_id, image_no, original_contract_id, current_contract_id, 
			serial_no, maturity_dt, reference, asset_status, amt_selling_price, amt_initial_book_value, amt_salvage_value,
			sec_amt_initial_book_value, sec_amt_salvage_value,
			tax_amt_initial_book_value, tax_amt_salvage_value, input_user_id, last_saved_by_id, ownership_type, orig_asset_hdr_id,
			branch_id, amt_retention, disposal_dt, owner_id, hdr_type, amt_base_repayment, pending_amt_base_repayment, parent_owner_id,
			reserved_contract_id, has_asset_children, initial_fx_rate, purchase_ccy_id, purchase_ccy_price, amt_inertia_salvage_value, 
			tax_payment_code, stamp)
		values (
			axsp_get_datetime(), :new.asset_hdr_id, :new.image_no, :new.original_contract_id, :new.current_contract_id, 
			:new.serial_no, :new.maturity_dt, :new.reference, :new.asset_status, :new.amt_selling_price, :new.amt_initial_book_value, :new.amt_salvage_value,
			:new.sec_amt_initial_book_value, :new.sec_amt_salvage_value,
			:new.tax_amt_initial_book_value, :new.tax_amt_salvage_value, :new.input_user_id, :new.last_saved_by_id, :new.ownership_type, :new.orig_asset_hdr_id,
			:new.branch_id, :new.amt_retention, :new.disposal_dt, :new.owner_id, :new.hdr_type, :new.amt_base_repayment, :new.pending_amt_base_repayment, :new.parent_owner_id,
			:new.reserved_contract_id, :new.has_asset_children, :new.initial_fx_rate, :new.purchase_ccy_id, :new.purchase_ccy_price, :new.amt_inertia_salvage_value, 
			:new.tax_payment_code, :new.stamp);
END;
/

--populate indirect exposure for party
CREATE OR REPLACE PROCEDURE axsp_popul8_indir_exp_4_party
(
  v_partyid IN NUMBER DEFAULT NULL
)
AS
 v_EnumDefaultFXRate int;
 v_DefaultRateBasis int :=0;
 v_dt date;

 CURSOR c_get_def_fx_rate IS
   SELECT to_number(system_setting_value) as def_fx_rate
     FROM system_setting where system_setting_type = 8402;

BEGIN
	if (v_partyid = 0) then
		execute immediate 'truncate table indirect_exposure';
	end if;

   --SET TRANSACTION READ WRITE;

   v_EnumDefaultFXRate := 8402;
   v_dt := axsp_get_datetime();

   -- Loop should only ever return max of one row
   FOR rec IN c_get_def_fx_rate LOOP
       v_DefaultRateBasis := rec.def_fx_rate;
   END LOOP;

   if (v_partyid <> 0) then
      DELETE FROM indirect_exposure WHERE party_id = v_partyid;
   end if;

   insert into indirect_exposure (party_id, currency_id, amount_indirect_exposure, calc_dt, stamp)
	select tbl.party_id, tbl.currency_id, max(tbl.amount_indirect_exposure) as amount_indirect_exposure, v_dt as calc_dt, 0 as stamp
	from
		(

			select
				de_rank1.party_id,
				CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END as currency_id,
				SUM(
						de_rank1.amount_direct_exposure *
						axsp_get_fx_cross_rate(
								v_DefaultRateBasis,
								v_dt,
								CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END,
								de_rank1.currency_id)
					)  as amount_indirect_exposure
			from

				(
					select party_id, party_role, contract_id, amount_direct_exposure, currency_id
					from (
						select
							pep.party_id,
							de.party_role,
							de.contract_id, de.amount_direct_exposure, de.currency_id,
							Rank() over (Partition BY de.contract_id, de.currency_id order by de.amount_direct_exposure DESC, de.party_id ASC) MyRank
						from direct_exposure de

						inner join (select distinct party_id,related_party_id FROM party_exposure_party) pep on de.party_id = pep.related_party_id

						where
						(pep.party_id = v_partyid OR v_partyid = 0)

						--do not count indirect exposure for contracts that the party already has direct exposure for.
						and not exists (select 1 from direct_exposure de_check where de_check.party_id = pep.party_id and de_check.contract_id = de.contract_id)

					) de_ranked
					where de_ranked.MyRank = 1
				) de_rank1


				inner join party p on p.party_id = de_rank1.party_id
				inner join party bu on p.party_business_unit_id = bu.party_id
				inner join location l1 on l1.location_id = CASE WHEN p.is_business_unit = 1 THEN p.location_id ELSE bu.location_id END
				inner join location l2 on l1.country_id = l2.location_id
				inner join currency c on c.currency_id = de_rank1.currency_id
			Group By
				de_rank1.party_id,
				CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END

		) tbl
	where
		tbl.party_id is not null
	Group By
		tbl.party_id, tbl.currency_id;
END;
/

--------------------------------------------------------------------------------------------------------------------------------
---------------------------------axsp_populate_direct_exposure_for_parties--------------------------------------------------------
CREATE OR REPLACE PROCEDURE axsp_popul8_dir_exp_4_parties
(
  p_party_ids VARCHAR2
)
AS
v_party_ids VARCHAR2(32767);
v_dt date;
v_doAllParties NUMBER(1,0);
BEGIN
   v_dt:=axsp_get_datetime();

   --Store the partyIds in a table variable
   IF (p_party_ids=' ') THEN
     v_party_ids:='';
   ELSE
     v_party_ids:=p_party_ids;
   END IF;
   delete from PARTY_EXPOSURE_TBL;
   insert into PARTY_EXPOSURE_TBL
   select to_number(spl.column_value) from table(axsp_split(v_party_ids,',')) spl;

   -- Check if we need to the direct exposure for all parties
   select count(*) into v_doAllParties from PARTY_EXPOSURE_TBL;	-- if there is records then put 0 in PARTY_EXPOSURE_TBL
   IF (v_doAllParties > 0) THEN
     v_doAllParties := 0; -- if there are records put 0
   ELSE
     v_doAllParties := 1; -- if there are no records do the job for all parties
   END IF;

   if (v_doAllParties = 1) then
      execute immediate 'truncate table direct_exposure';
   end if;

   if (v_doAllParties <> 1) then
      delete from direct_exposure where party_id in (select party_id from PARTY_EXPOSURE_TBL);
   end if;

   insert into direct_exposure (party_id, contract_id, product_id, business_unit_id,party_role,currency_id,amount_direct_exposure,amount_memorandum,calc_dt,calculation_type,contract_state,stamp)
   select party_id,contract_id,product_id,business_unit_id,party_role,currency_id,amount_direct_exposure,amount_memorandum,v_dt,calculation_type,contract_state,0
   from table(axsp_get_dir_exp_for_parties (p_party_ids));
END;
/

--populate direct exposure for party
CREATE OR REPLACE PROCEDURE axsp_popul8_direct_exp_4_party
(
  v_partyid IN NUMBER DEFAULT NULL
)
AS
BEGIN
   if (v_partyid = 0) then
      execute immediate 'truncate table direct_exposure';
   end if;

   --SET TRANSACTION READ WRITE;

   if (v_partyid <> 0) then
      DELETE FROM direct_exposure WHERE party_id = v_partyid;
   end if;

   INSERT INTO direct_exposure (party_id, contract_id, product_id, business_unit_id,party_role,currency_id,amount_direct_exposure,amount_memorandum,calc_dt,calculation_type,contract_state,stamp)
   (SELECT party_id,
              contract_id,
              product_id,
              business_unit_id,
              party_role,
              currency_id,
              amount_direct_exposure,
              amount_memorandum,
              axsp_get_datetime(),
              calculation_type,
              contract_state,
              0
    FROM TABLE(axsp_get_dir_exp_for_party(v_partyid))
    );
END;
/

--get direct exposure parties to link 1st pass
CREATE OR REPLACE PROCEDURE asxp_get_dir_exp_party_4_link
(
  cv_1 IN OUT SYS_REFCURSOR
)
AS
BEGIN
/** Get direct exposure for party link -- this is the first pass **/

--Store the partyIds in a table variable
delete from PARTY_EXPOSURE_TBL;
insert into PARTY_EXPOSURE_TBL
select distinct party_id
from direct_exposure;

   OPEN cv_1 FOR
--select parties with direct exposure
select distinct p.party_id, p.stamp
from
	PARTY_EXPOSURE_TBL de
		inner join party p on de.party_id = p.party_id
		left join party_exposure_party pep on pep.party_id = p.party_id
		left join party rp on rp.party_id = pep.related_party_id
where
	pep.party_id is null or pep.party_version <> p.stamp or pep.related_party_version <> rp.stamp;
END;
/

--get direct exposure parties to link 2nd pass
CREATE OR REPLACE PROCEDURE asxp_get_dir_exp_party_4_lnk_2
(
  p_party_ids VARCHAR2,
  cv_1 IN OUT SYS_REFCURSOR
)
AS
BEGIN
/** Get direct exposure for party link -- this is the second pass **/

--Store the partyIds in a table variable
delete from PARTY_EXPOSURE_TBL;
insert into PARTY_EXPOSURE_TBL
select distinct party_id
from direct_exposure d
where exists (select 1 from table(axsp_split(p_party_ids,',')) spl where to_number(spl.column_value) = d.party_id);

   OPEN cv_1 FOR
--select parties that may have indirect exposure (Case 19972)
select rp.party_id, rp.stamp
from
	PARTY_EXPOSURE_TBL de
		inner join party_exposure_party pep on pep.party_id = de.party_id
		inner join party rp on rp.party_id = pep.related_party_id
		left join party_exposure_party pep2 on pep2.party_id = rp.party_id
		left join party rp2 on rp2.party_id = pep2.related_party_id
where
	pep2.party_id is null or pep2.party_version <> rp.stamp or pep2.related_party_version <> rp2.stamp;
END;
/

--Get exposure contracts for a party
CREATE OR REPLACE PROCEDURE axsp_get_exp_contracts_4_party
(
  v_party_id IN NUMBER DEFAULT NULL ,
  v_run_dt IN DATE DEFAULT NULL ,
  cv_1 IN OUT SYS_REFCURSOR
)
AS
   v_GrossUnpaidReceivableExposur NUMBER(10,0);
   v_MemorandumOnlyExposureCalcul NUMBER(10,0);
   v_GrossUnpaidRVExposureCalcula NUMBER(10,0);
   v_NoneSlotType NUMBER(10,0);
   v_CustomerSlotType NUMBER(10,0);
   v_DealerSlotType NUMBER(10,0);
   v_VendorSlotType NUMBER(10,0);
   v_PermitHolderSlotType NUMBER(10,0);
   v_SalesPersonSlotType NUMBER(10,0);
   v_CompleteActivated NUMBER(10,0);
   v_PartiallyTerminatedPendingSa NUMBER(10,0);
   v_PartiallyTerminated NUMBER(10,0);
   v_TerminatedPendingSale NUMBER(10,0);
   v_WriteOffPending NUMBER(10,0);
   v_IsFutureContractIncluded NUMBER(1,0);
BEGIN
   v_GrossUnpaidReceivableExposur := 37901;
   v_MemorandumOnlyExposureCalcul := 37903;
   v_GrossUnpaidRVExposureCalcula := 37904;
   v_NoneSlotType := 36800;
   v_CustomerSlotType := 36801;
   v_DealerSlotType := 36802;
   v_VendorSlotType := 36803;
   v_PermitHolderSlotType := 36804;
   v_SalesPersonSlotType := 36805;
   v_CompleteActivated := 11130;
   v_PartiallyTerminatedPendingSa := 11135;
   v_PartiallyTerminated := 11140;
   v_TerminatedPendingSale := 11145;
   v_WriteOffPending := 11158;

   select is_future_contract_included into v_IsFutureContractIncluded from system_defs;

   OPEN cv_1 FOR
      SELECT DISTINCT contract_id
        FROM
				( SELECT c.contract_id,
                      CASE
                           WHEN sdp.slot_type > v_NoneSlotType THEN
										CASE
                                  WHEN sdp.slot_type = v_CustomerSlotType THEN c.cparty_id
                                  WHEN sdp.slot_type = v_DealerSlotType THEN c.dealer_id
                                  WHEN sdp.slot_type = v_VendorSlotType THEN c.vendor_id
                                  WHEN sdp.slot_type = v_PermitHolderSlotType THEN c.permit_holder_id
										END
                      ELSE cp.party_id
                      END party_id
               FROM CONTRACT c
                      INNER JOIN business_model bm ON bm.business_model_id = c.business_model_id
                      INNER JOIN business_model_party bmp ON bm.business_model_id = bmp.business_model_id
                      INNER JOIN system_defs_party_role sdp ON sdp.system_defs_party_role_id = bmp.system_defs_party_role_id
                      LEFT JOIN contract_party cp ON sdp.slot_type = v_NoneSlotType AND cp.system_defs_party_role_id = sdp.system_defs_party_role_id AND cp.contract_id = c.contract_id
                WHERE
                     bmp.exposure_calculation_type IN ( v_GrossUnpaidReceivableExposur,v_MemorandumOnlyExposureCalcul,v_GrossUnpaidRVExposureCalcula ) AND
                     sdp.slot_type <> v_SalesPersonSlotType AND
                     exists (select 1 from exposure_state es where es.include_in_asset_balance = 1 and es.contract_state = c.contract_state) and
                     -- Include future dated contracts when is_future_contract_included is set
                     (c.calc_dt <= v_run_dt or v_IsFutureContractIncluded = 1)
             ) tbl
         WHERE
	 	tbl.party_id = v_party_id OR exists (select 1 from party_exposure_party pep where pep.related_party_id = tbl.party_id and pep.party_id = v_party_id);
END;
/

create or replace  FUNCTION axsp_is_valid_credit_line(p_credit_line_id int)
RETURN NUMBER IS
--
  v_res1 NUMBER := 0;
BEGIN
	select count(*) into v_res1 from workflow wkf
      inner join wf_state_chain_item wsci on wsci.wf_state_type_id = wkf.current_state_type
			inner join wf_state_type wst on wst.wf_state_type_id = wsci.wf_state_type_id
			where wkf.credit_line_id = p_credit_line_id
        and wsci.wf_state_chain_hdr_id = 1
				and not exists (select * from xt_lookupset where set_name ='CrLnExclWorkFlowState' and value = wst.name);
	if (v_res1 = 1) then
    return 1;
	end if;

	return 0;
END;
/

create or replace
PROCEDURE axsp_get_total_exp_for_party
(
  p_party_id IN NUMBER DEFAULT NULL,
  p_applicant_id_to_ignore IN NUMBER default 0,
  RC1 IN OUT globalPkg.RCT1
)
AS
   v_amtTotalExposure NUMBER(18,4);
	v_amtGroupLimit NUMBER(18,4);
	v_amtUsedInGroup NUMBER(18,4);
	v_amtGroupLimitGuarantor NUMBER(18,4);
	v_amtUsedInGroupGuarantor NUMBER(18,4);
   v_offSystemExposure NUMBER(18,4);
	v_amtLineLimit NUMBER(18,4);
	v_time DATE;
   v_ignoreCount NUMBER(10,0);
BEGIN
   --Store the contract ids to ignore in a temporary table
   --It will be contract ids where the party id is the guarantor and the applicant id is either the customer or the dealer
   insert into TP_IGNORED_CONTRACTS(contract_id)
   select distinct c.contract_id
		from contract c
		inner join guarantee g on g.contract_id = c.contract_id
		where (c.cparty_id = p_applicant_id_to_ignore or c.dealer_id = p_applicant_id_to_ignore)
		and g.guarantor_party_id = p_party_id
		and p_applicant_id_to_ignore > 0;

	insert into TP_IGNORED_CONTRACTS(contract_id)
      select distinct c.contract_id
		from contract c
		inner join guarantee g on g.contract_id = c.contract_id
		where g.guarantor_party_id = p_applicant_id_to_ignore
		and p_applicant_id_to_ignore > 0;

   select count(*) into v_ignoreCount from TP_IGNORED_CONTRACTS;

   -- initialise the variables
   v_amtTotalExposure := 0;
   v_amtGroupLimit := 0;
   v_amtUsedInGroup := 0;
   v_amtGroupLimitGuarantor := 0;
   v_amtUsedInGroupGuarantor := 0;
   v_offSystemExposure := 0;
	v_amtLineLimit := 0;
   v_time := AXSP_GET_DATETIME;

  -- sum any direct exposures for the Party ..
  select coalesce(sum(amount_direct_exposure),0) into v_amtTotalExposure from direct_exposure d where party_id = p_party_id
    and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = d.contract_id);

  -- add sum of any indirect exposure for the Party ..
  select v_amtTotalExposure + coalesce(sum(amount_indirect_exposure),0) into v_amtTotalExposure from indirect_exposure where party_id = p_party_id;

   -- ***
   -- ***
   -- ***
   -- CREDIT LINE EXPOSURE CALCULATION
   -- ***
   -- ***
   -- ***
  -- add the amount of unused limit on current lines for the Party, where the line is not part of a group.
  if (v_ignoreCount = 0) then
    select v_amtTotalExposure + coalesce(sum(amt_limit - amt_utilisation),0) into v_amtTotalExposure
    from credit_line cl
    where party_id = p_party_id and owner_id = 0  and is_group = 0 and is_deleted = 0 and expiry_dt >= v_time and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);
  else
    select v_amtTotalExposure + coalesce(sum(amt_limit - amt_utilisation - coalesce(takedowns.td_utilisation,0)),0) into v_amtTotalExposure
    from credit_line cl
	    left join (
		  select credit_line_id, sum(amt_utilisation) as td_utilisation
			from credit_line_takedown t
			inner join TP_IGNORED_CONTRACTS ign on (t.contract_id = ign.contract_id)
		  group by credit_line_id) takedowns on (takedowns.credit_line_id = cl.credit_line_id)
    where party_id = p_party_id and owner_id = 0  and is_group = 0 and is_deleted = 0 and expiry_dt >= v_time and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);
  end if;

  -- ***
  -- *** now calculate the amount of unused credit available on 'current' groups ***
  -- ***

  -- select the 'group limit' for groups that belong to the party, and that have at least one 'current' line
  select coalesce(sum(cl.amt_limit),0) into v_amtGroupLimit
  from credit_line cl
  where cl.is_group = 1 and cl.party_id = p_party_id and cl.is_deleted = 0
    and exists (select 1 from credit_line cl1 where cl1.is_deleted = 0 and cl1.owner_id = cl.credit_line_id and cl1.expiry_dt >= v_time) and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);

  -- select the amount used for lines attached to groups that have at least one 'current' line
  if (v_ignoreCount = 0) then
    select coalesce(sum(amt_utilisation),0) into v_amtUsedInGroup
    from credit_line cl
    where party_id = p_party_id and is_group = 0 and is_deleted = 0
    and exists
	  (select 1 from credit_line gp where gp.is_group = 1 and gp.party_id = p_party_id and gp.is_deleted = 0 and cl.owner_id = gp.credit_line_id
	  and exists (select 1 from credit_line gp1 where gp1.is_deleted = 0 and gp1.owner_id = gp.credit_line_id and gp1.expiry_dt >= v_time)) and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);
  else
    select coalesce(sum(amt_utilisation - coalesce(takedowns.td_utilisation,0)),0) into v_amtUsedInGroup
    from credit_line cl
    left join (
		  select credit_line_id, sum(amt_utilisation) as td_utilisation
			from credit_line_takedown t
			inner join TP_IGNORED_CONTRACTS ign on (t.contract_id = ign.contract_id)
		  group by credit_line_id) takedowns on (takedowns.credit_line_id = cl.credit_line_id)
    where party_id = p_party_id and is_group = 0 and is_deleted = 0
    and exists
	  (select 1 from credit_line gp where gp.is_group = 1 and gp.party_id = p_party_id and gp.is_deleted = 0 and cl.owner_id = gp.credit_line_id
	  and exists (select 1 from credit_line gp1 where gp1.is_deleted = 0 and gp1.owner_id = gp.credit_line_id and gp1.expiry_dt >= v_time)) and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);
  end if;

  -- add the amount of unused limit for groups belonging to the party
  select coalesce(v_amtTotalExposure + (v_amtGroupLimit - v_amtUsedInGroup),0) into v_amtTotalExposure FROM dual;

  -- ***
  -- *** now calculate applicable Credit Line Guarantees ****
  -- ***
  -- if a Party is a Guarantor on a Line or Group, then they will have been copied to the Contract
  --  .. and therefore any utilised amount of the line will already be attributed to the guarantor party by way of a direct exposure
  -- this means we are only interested in the un-utilised amount
  -- Note: if the line has not guarantors, then the groups guarantors are used. if the line has a guarantor, then the group guarantors are not used

  -- ***
  -- firstly we get identify lines where the party is a guarantor, where the the line is current and is approved and not in a group!!!
  select v_amtTotalExposure +
	coalesce(sum(amt_limit - amt_utilisation),0) into v_amtTotalExposure from credit_line cl
  where is_deleted = 0 and exists
	  (select 1 from guarantee g where guarantor_party_id = p_party_id and credit_line_id <> 0
      and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = g.contract_id)
			and cl.credit_line_id = g.credit_line_id)
	and is_group = 0 and expiry_dt >= v_time
	and owner_id = 0
	and exists
		(select 1 from workflow wkf where wkf.credit_line_id = credit_line_id and wkf.credit_line_id = cl.credit_line_id)
		and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);

  -- ***
  -- *** now calculate applicable Credit Line Guarantees for Groups !! ****
  -- select the 'group limit' for groups that are guaranteed by the party, and that have at least one 'current' line
  select coalesce(sum(cl.amt_limit),0) INTO v_amtGroupLimitGuarantor from credit_line cl
  where cl.is_deleted = 0 and cl.is_group = 1 and exists
	(select credit_line_id from guarantee g where guarantor_party_id = p_party_id and credit_line_id <> 0
      and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = g.contract_id)
			and g.credit_line_id = cl.credit_line_id)
  and exists (select 1 from credit_line cl1 where cl1.is_deleted = 0 and  cl1.owner_id = cl.credit_line_id and cl1.expiry_dt >= v_time) and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);


  -- select the amount used for lines attached to groups that have at least one 'current' line -- where the group is guaranteed by the party
  select coalesce(sum(amt_utilisation),0) INTO v_amtUsedInGroupGuarantor from credit_line cl
  where is_deleted = 0 and party_id = p_party_id and is_group = 0 and
  exists (select 1  from credit_line gp where gp.is_group = 1 and cl.owner_id = gp.credit_line_id
	and
		exists (select 1 from guarantee gp2 where guarantor_party_id = p_party_id
            and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = gp2.contract_id)
            and credit_line_id <> 0 and gp2.credit_line_id = gp.credit_line_id)
	and exists (select 1 from credit_line gp1 where gp1.is_deleted = 0 and   gp1.owner_id = gp.credit_line_id and gp1.expiry_dt >= v_time)) and (axsp_is_valid_credit_line(cl.credit_line_id) = 1);

	  -- add the amount of unused limit for groups where the party is a guarantor 
  v_amtTotalExposure := coalesce(v_amtTotalExposure + (v_amtGroupLimitGuarantor - v_amtUsedInGroupGuarantor),0);   		
  --	*****************
  -- and now find the lines that are in groups that have at least one line guranteed by the party BUT the group is NOT guaranteed by the party
  --
  for i in       
		(select clx.credit_line_id clxGroupLineId, clx.amt_limit clxGroupLimit from credit_line clx where 
			clx.is_deleted = 0 and clx.is_group = 1 and not exists
				(select 1 from guarantee g where g.guarantor_party_id = p_party_id and g.credit_line_id <> 0
				and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = g.contract_id)
				and clx.credit_line_id = g.credit_line_id)
			and clx.credit_line_id in (select clxx.owner_id from credit_line clxx where
			clxx.is_deleted = 0 and clxx.is_group = 0 and clxx.owner_id <> 0 and exists
				(select 1 from guarantee g where g.guarantor_party_id = p_party_id and g.credit_line_id <> 0
				and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = g.contract_id)
				and clxx.credit_line_id = g.credit_line_id)and clxx.expiry_dt >= v_time and (axsp_is_valid_credit_line(clxx.credit_line_id) = 1))) loop
		--
				 -- select the amount used for lines owned by the group .. and guaranteed by the party
				select coalesce(sum(amt_utilisation),0) into v_amtUsedInGroupGuarantor from credit_line clx1
					where clx1.is_deleted = 0 and clx1.owner_id = i.clxGroupLineId and clx1.is_group = 0 and  
					exists 
					(select 1 from guarantee glx where glx.guarantor_party_id = p_party_id
					and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = glx.contract_id)
					and glx.credit_line_id <> 0 and clx1.credit_line_id = glx.credit_line_id)
					and clx1.expiry_dt >= v_time
					and (axsp_is_valid_credit_line(clx1.credit_line_id) = 1);
					
				--- Now get the line limit for the 
				select coalesce(sum(amt_limit),0) into v_amtLineLimit from credit_line clx1
					where clx1.is_deleted = 0 and clx1.owner_id = i.clxGroupLineId and clx1.is_group = 0 and  
					exists 
					(select 1 from guarantee glx where glx.guarantor_party_id = p_party_id
					and not exists (select 1 from TP_IGNORED_CONTRACTS ign where ign.contract_id = glx.contract_id)
					and glx.credit_line_id <> 0 and clx1.credit_line_id = glx.credit_line_id)
					and clx1.expiry_dt >= v_time
					and (axsp_is_valid_credit_line(clx1.credit_line_id) = 1);
				
				-- add the amount of unused limit for groups where the party is a guarantor 
				if v_amtLineLimit > i.clxGroupLimit THEN
					v_amtTotalExposure := coalesce(v_amtTotalExposure + (i.clxGroupLimit - v_amtUsedInGroupGuarantor),0);
				else
					v_amtTotalExposure := coalesce(v_amtTotalExposure + (v_amtLineLimit - v_amtUsedInGroupGuarantor),0);
				END IF;     
			END LOOP;

	  --****************************************************************
   -- add the amount of unused limit for groups WHERE the party is a guarantor
	v_amtTotalExposure := COALESCE(v_amtTotalExposure + (v_amtGroupLimitGuarantor - v_amtUsedInGroupGuarantor),0); 

  -- add the off system exposure
  select coalesce(SUM(cast(pcfv.field_value as decimal(18,4))),0) into v_offSystemExposure
  from party_cfv pcfv
  inner join party_cfd pcfd on (pcfv.party_cfd_id = pcfd.party_cfd_id)
  where pcfd.data_type in (5505,5504) and pcfd.is_applied_to_exposure = 1
  and pcfv.party_id = p_party_id;

  v_amtTotalExposure := coalesce((v_amtTotalExposure + v_offSystemExposure),0);

  OPEN RC1 FOR SELECT v_amtTotalExposure FROM DUAL;
END;
/

create or replace PROCEDURE axsp_get_direct_exp_for_party
(
  p_party_id IN NUMBER DEFAULT NULL,
  RC1 IN OUT globalPkg.RCT1
)
AS
	v_amtTotalExposure NUMBER(18,4);
BEGIN
	-- initialise the variables
	v_amtTotalExposure := 0;

	-- sum any direct exposures for the Party
	select coalesce(sum(amount_direct_exposure),0) into v_amtTotalExposure from direct_exposure where party_id = p_party_id;

	-- return the value
	OPEN RC1 FOR SELECT v_amtTotalExposure FROM DUAL;
END;
/

create or replace FUNCTION axsp_to_number(
in_string varchar2,
default_number float)
RETURN NUMBER IS
  testnumber float;
BEGIN
  testnumber:=TO_NUMBER(RTRIM(LTRIM(in_string,'$+-')));
  return testnumber;
EXCEPTION
  when others then return default_number;
END axsp_to_number;
/

CREATE OR REPLACE FUNCTION axsp_get_modUnit_List (
	p_asset_class_id int,
   p_asset_list_type_id int ) RETURN VARCHAR2
IS
     lv_csv varchar2(2000);
     CURSOR c_get_mod_list IS
       SELECT xl.value
       FROM asset_class_modular_list acml
	    INNER JOIN xt_lookupset xl ON xl.xt_lookupset_id = acml.value_id
	    WHERE acml.asset_class_id = p_asset_class_id
	    AND acml.asset_class_modular_list_type = p_asset_list_type_id;
BEGIN
-- This is not the prettiest way of doing this

    FOR rec IN c_get_mod_list LOOP
      lv_csv := lv_csv ||',' || rec.value;
    END LOOP;

    IF LENGTH(lv_csv) > 1 THEN
      lv_csv := SUBSTR(lv_csv,2);
    END IF;

	 RETURN lv_csv;
END;
/

CREATE OR REPLACE FUNCTION AXSP_NORMALIZE_SEARCH_TERM
(p_search_term IN NVARCHAR2) RETURN NVARCHAR2 AS
  v_search_term NVARCHAR2(1024);
begin
  v_search_term := p_search_term;
  --
  if substr(trim(p_search_term),1,1) = '"' AND substr(trim(p_search_term),-1,1) = '"' AND length(trim(p_search_term)) > 1 then
    -- search term is quoted, so trim any spaces and the first and last double quote
    -- do not subsitute spaces within the search term
    v_search_term := lower(trim(both '"' from trim(p_search_term)));
  else
    -- Substitute spaces in the search term and leave it at that
    v_search_term := lower(replace(p_search_term, ' ', '%'));
  end if;
  return v_search_term;
end AXSP_NORMALIZE_SEARCH_TERM;
/

CREATE OR REPLACE PROCEDURE axsp_create_index
(p_index_name varchar2,
 p_table_name varchar2,
 p_column_csv varchar2,
 p_is_unique number,
 p_exclude_oracle number default 0,
 p_inc_column_csv varchar2 default null) as
--
  v_column_csv VARCHAR2(1000);
  v_sql varchar2(1000);
  v_existing_count number;
BEGIN
	if p_exclude_oracle = 0 THEN
	  if trim(p_inc_column_csv) IS null THEN
		 v_column_csv := p_column_csv;
	  else
		 v_column_csv := p_column_csv || ',' || p_inc_column_csv;
	  end if;

	  if p_is_unique = 1 then
		 v_sql := 'create unique index '||p_index_name||' on '||p_table_name||'('||v_column_csv||')';
	  else
		 v_sql := 'create index '||p_index_name||' on '||p_table_name||'('||v_column_csv||')';
	  end if;
	  select count(*) into v_existing_count from user_indexes where index_name = upper(p_index_name);
	  if v_existing_count = 0 then
		 execute immediate v_sql;
	  end if;
	end if;
end axsp_create_index;
/

CREATE OR REPLACE PROCEDURE axsp_enable_fks
(tblName varchar2,
 colName varchar2,
 enable int) as
	enableDisable VARCHAR2(30);
BEGIN
  IF (ENABLE = 1) THEN
    enableDisable := 'ENABLE';
  ELSE
    enableDisable := 'DISABLE';
  END IF;

	for i in (
		select uc.table_name, uc.constraint_name
		from user_constraints uc
		inner join user_constraints ruc on (ruc.constraint_name=uc.r_constraint_name)
		inner join USER_CONS_COLUMNS ucc on (ucc.constraint_name=ruc.constraint_name)
		where uc.constraint_type ='R'
		AND UPPER(ruc.table_name)=UPPER(tblName)
		AND UPPER(ucc.COLUMN_NAME)=UPPER(colName)
	) LOOP
		execute immediate 'alter table '||i.table_name||' '||enableDisable||' constraint '||i.constraint_name||'';
	end loop;
end axsp_enable_fks;
/

CREATE OR REPLACE PROCEDURE axsp_contract_address_search
(
	p_contract_id int,
	p_street nvarchar2,
	p_suburb nvarchar2,
	p_city_id int,
	p_zip_code nvarchar2,
	p_county_id int,
	p_state_province_id int,
	p_country_region_id int,
  RC1 IN OUT globalPkg.RCT1
)
AS
  SqlQuery VARCHAR2(1000);
BEGIN
  SqlQuery := '
SELECT	*
FROM axvw_contract_address
WHERE
		contract_id = '||p_contract_id||'
		AND street = '''||nvl(p_street, ' ')||'''
		AND suburb = '''||nvl(p_suburb, ' ')||'''
		AND city_id = '||nvl(p_city_id, 0)||'
		AND zip_code = '''||nvl(p_zip_code, ' ')||'''
		AND county_id = '||nvl(p_county_id, 0)||'
		AND state_province_id = '||nvl(p_state_province_id, 0)||'
		AND country_region_id = '||nvl(p_country_region_id, 0);
--
OPEN RC1 for SqlQuery;
EXCEPTION
    WHEN NO_DATA_FOUND THEN
      NULL;
    WHEN OTHERS THEN
      raise_application_error(SQLCODE, SQLERRM,true);
END axsp_contract_address_search;
/

CREATE OR REPLACE PROCEDURE axsp_get_ancestor_info(
	p_predecessorId IN number,
	RC1 IN OUT globalPkg.RCT1
) AS
--
	v_status number;
	v_predecessorId number;
	SqlQuery VARCHAR(500);

BEGIN

	select status, predecessor_id into v_status, v_predecessorId
	from scheduler_process where scheduler_process_id = p_predecessorId;

	if v_status = 24300 then
		SqlQuery := 'select next_start_dt, last_end_dt from scheduler_process where scheduler_process_id = ' || p_predecessorId;
		OPEN RC1 FOR SqlQuery;
	else
		if v_predecessorId = 0 then
			SqlQuery := 'select next_start_dt, last_end_dt, scheduler_process_id from scheduler_process where scheduler_process_id = ' || p_predecessorId;
			OPEN RC1 FOR SqlQuery;
		else
			axsp_get_ancestor_info(v_predecessorId, RC1);
		end if;
	end if;

END axsp_get_ancestor_info;
/

CREATE OR REPLACE
FUNCTION AXSP_VALIDATE_PREDECESSOR(
	p_process_id IN NUMBER DEFAULT NULL,
	p_predecessor_id IN NUMBER DEFAULT NULL)
RETURN NUMBER AS
  CURSOR cur(c_id number) IS
	  SELECT rid FROM (select predecessor_id, scheduler_process_id as rid from scheduler_process where scheduler_process_id != 0) start  WITH rid = c_id CONNECT BY rid = PRIOR predecessor_id;
BEGIN
	for rec in cur(p_predecessor_id) loop
		if rec.rid = p_process_id then
			return 0;
		end if;
	end loop;

	return 1;

END AXSP_VALIDATE_PREDECESSOR;
/

CREATE OR REPLACE
FUNCTION AXSP_IS_PROCESS_QUALIFIED(
	p_predecessorId       IN NUMBER  DEFAULT NULL,
	p_child_last_start_dt IN DATE)
RETURN NUMBER AS
--
	v_last_end_dt DATE;
	v_predecessorId number;
	v_status NUMBER;
BEGIN

	select predecessor_id, last_end_dt, status into v_predecessorId, v_last_end_dt, v_status
	from scheduler_process where scheduler_process_id = p_predecessorId;

	if v_status = 24300 OR v_status = 24305 then
		if v_last_end_dt > p_child_last_start_dt then
			return 1;
		else
			return 0;
		end if;
	else
		if v_predecessorId = 0 then
			return 1;
		else
			return axsp_is_process_qualified(v_predecessorId, p_child_last_start_dt);
		end if;
	end if;

END AXSP_IS_PROCESS_QUALIFIED;
/
CREATE OR REPLACE FORCE VIEW axvw_coll_overdue_rating
AS
SELECT os.party_id, os.overdue_days, ot.credit_rating_id expected_rating_id, ci.rating_id actual_rating_id, os.stamp
FROM party_overdue_stats os
	LEFT OUTER JOIN party_overdue_stats os2 on os.party_id = os2.party_id AND os.task_type = os2.task_type AND (os.overdue_days < os2.overdue_days OR (os.overdue_days = os2.overdue_days AND os.party_overdue_stats_id < os2.party_overdue_stats_id))
	INNER JOIN overdues_timeband ot ON os.overdue_days >= ot.start_day AND os.overdue_days <= ot.end_day
	LEFT OUTER JOIN credit_info ci ON os.party_id = ci.party_id AND ci.is_current = 1
WHERE os2.party_overdue_stats_id IS NULL and os.task_type = 6205 AND (ci.rating_id IS NULL OR ot.credit_rating_id != ci.rating_id)
/

CREATE OR REPLACE FORCE VIEW axvw_dvp_flow_view
AS
SELECT f.flow_id,f.contract_id, acf.asset_id, pa.party_id, f.flow_type, f.custom_flow_hdr_id,f.amount,f.amt_gross,f.calc_dt,f.is_cash, acfh.is_direct_vendor_payment,f.stamp FROM flow f
	INNER JOIN asset_custom_flow acf on f.custom_flow_link_no = acf.link_no and f.contract_id = acf.contract_id
	INNER JOIN custom_flow_hdr acfh on acf.custom_flow_hdr_id = acfh.custom_flow_hdr_id
	INNER JOIN party_account pa on f.party_account_id = pa.party_account_id
	WHERE acf.is_deleted = 0
/

CREATE OR REPLACE FORCE VIEW axvw_wf_contract_summary as
SELECT contract_id, workflow_id, wft_name as workflow_type_name, wf_type_id, st_name as wf_state_type_Name,
wf_state_type_id, task_id, contract_restructure_id, wf_completed, 
todo_hdr_id, 0 as stamp
FROM (
-- Restructure
SELECT c.contract_id, 0 as workflow_id, wft.name as wft_name, wft.wf_type_id, st.name as st_name, st.wf_state_type_id, NVL(t.task_id,0) as task_id,
r.contract_restructure_id, CASE wsd.next_pos_state WHEN 0 THEN 1 ELSE 0 END as wf_completed, 0 as todo_hdr_id
FROM contract c
inner join contract_restructure r on r.contract_id = c.contract_id 
inner join workflow_state_link wsl on wsl.workflow_state_link_id = r.workflow_state_link
inner join workflow_state_det wsd on wsd.workflow_state_hdr_id = wsl.workflow_state_hdr_id and wsd.current_state = r.variation_state
inner join wf_state_type st on st.wf_state_type_id=r.variation_state
inner join wf_type wft on wft.wf_type_id = st.wf_type_id
left join task t on (t.contract_id=c.contract_id and task_type = 6209 and approval_status in (6500, 6504) and r.variation_state != 19670) -- NOT completed
UNION ALL
-- Contract State
SELECT c.contract_id, 0 as workflow_id, wft.name as wft_name, wft.wf_type_id, st.name as st_name, st.wf_state_type_id, NVL(t.task_id,0) as task_id,
0 as contract_restructure_id,0 as wf_completed, 0 as todo_hdr_id
FROM contract c
inner join wf_state_type st on st.wf_state_type_id=c.contract_state
inner join wf_type wft on wft.wf_type_id = st.wf_type_id
left join task t on (t.contract_id=c.contract_id and task_type = 6200 and approval_status in (6500, 6504))
UNION ALL
--Credit State
SELECT c.contract_id, 0 as workflow_id, wft.name as wft_name, wft.wf_type_id, st.name as st_name, st.wf_state_type_id, NVL(t.task_id,0) as task_id,
0 as contract_restructure_id, 0 as wf_completed, 0 as todo_hdr_id
FROM contract c
inner join wf_state_type st on st.wf_state_type_id=c.credit_state
inner join wf_type wft on wft.wf_type_id = st.wf_type_id
left join task t on (t.contract_id=c.contract_id and task_type = 6204 and approval_status in (6500, 6504))
UNION ALL 
-- generic workflow
SELECT w.contract_id, w.workflow_id, wft.name wft_name, wft.wf_type_id, st.name as st_name, st.wf_state_type_id, NVL(t.task_id, 0) task_id,
0 as contract_restructure_id, sci.is_end_state as wf_completed, th.todo_hdr_id
FROM workflow w
inner join wf_state_type st on st.wf_state_type_id=w.current_state_type
inner join wf_type wft on wft.wf_type_id = st.wf_type_id
inner join todo_hdr th on th.todo_hdr_id = w.wf_todo_hdr_id
inner join wf_state_chain_item sci on sci.wf_state_chain_hdr_id = th.wf_state_chain_hdr_id and sci.wf_state_type_id = w.current_state_type
left join task t on (t.workflow_id=w.workflow_id and task_type = 6226 and approval_status in (6500, 6504))
) 
/

CREATE OR REPLACE FORCE VIEW axvw_wf_generic_summary as
-- generic workflow
SELECT  w.workflow_id, w.contract_id, w.party_id, w.credit_line_id, w.asset_hdr_id, w.gl_journal_id, w.opportunity_id, w.secondary_id, w.termination_quote_id, 
w.financing_statement_id, wft.name as workflow_type_name, wft.wf_type_id, st.name as wf_state_type_Name, st.wf_state_type_id, NVL(t.task_id, 0) as task_id,
sci.is_end_state as wf_completed, 0 as contract_restructure_id, th.todo_hdr_id, 0 as stamp
FROM workflow w
inner join wf_state_type st on st.wf_state_type_id=w.current_state_type
inner join wf_type wft on wft.wf_type_id = st.wf_type_id
inner join todo_hdr th on th.todo_hdr_id = w.wf_todo_hdr_id
inner join wf_state_chain_item sci on sci.wf_state_chain_hdr_id = th.wf_state_chain_hdr_id and sci.wf_state_type_id = w.current_state_type
left join task t on (t.workflow_id=w.workflow_id and task_type = 6226 and approval_status in (6500, 6504))
/

-- Put this view after all the SP's and Views in order to make it compiled properly (Oracle Bug #2678809).
CREATE OR REPLACE FORCE VIEW axvw_contract_address
AS
SELECT	aa.contract_id, a.*
FROM		address a INNER JOIN
			(
				SELECT	address_id, contract_id
				FROM		axvw_asset_address av
					UNION
				SELECT	aa.address_id, a.contract_id
				FROM		asset a INNER JOIN asset_address aa ON a.asset_id = aa.asset_id
					UNION
				SELECT	aha.address_id, ah.current_contract_id
				FROM		asset_hdr ah INNER JOIN asset_hdr_address aha ON ah.asset_hdr_id = aha.asset_hdr_id
			) aa ON a.address_id = aa.address_id
/
--This is oracle only function and only needed for axvw_bail_asset view due to oracle limitations.
CREATE OR REPLACE FUNCTION axsp_get_max_bal_dt(p_asset_id NUMBER)
RETURN DATE AS
  v_ret DATE;
BEGIN
  SELECT Nvl(Max(effective_dt),axsp_datemax()) INTO v_ret FROM asset_balance WHERE asset_id = p_asset_id;
  RETURN v_ret;
END axsp_get_max_bal_dt;
/
CREATE OR REPLACE FORCE VIEW axvw_bail_asset
AS
SELECT
  ba.bail_asset_id,
  ba.bail_acc_id,
  ba.contract_id,
  ba.asset_id,
  ba.asset_hdr_id,
  ba.asset_usage,
  ba.balance_method,
  a.calc_dt bailment_dt,
  a.mature_dt,
  a.cost,
  a.amt_financed,
  c.interest_rate,
  acv.registration_no,
  acv.chassis_no,
  acv.vin_no,
  nvl(ab.interest_accrued,0) interest_accrued,
  nvl(ab.amt_total_interest,0) amt_total_interest,
  nvl(ab.fee_amt,0) fee_amt,
  nvl(ab.amt_sales_tax,0) amt_sales_tax,
  nvl(ab.amt_payments_received,0) amt_payments_received,
  nvl(ab.amt_subventions,0) amt_subventions,
  nvl(ab.amt_outstanding_balance,0) amt_outstanding_balance,
  nvl(ab.days_bailed,0) days_bailed,
  c.contract_state,
  c.save_status,
  c.contract_type,
  c.suspension_state,
  c.intercept_state,
  c.is_variation_pending,
  cs.allow_intercept,
  cs.allow_suspension,
  ba.stamp
 FROM
  bail_asset ba

 INNER JOIN contract c ON c.contract_id = ba.contract_id AND c.is_active = 1
 INNER JOIN asset a ON a.asset_id = ba.asset_id
 INNER JOIN asset_class_vehicle acv ON acv.asset_hdr_id = ba.asset_hdr_id
 LEFT OUTER JOIN asset_balance ab ON ab.asset_id = ba.asset_id
  AND ab.effective_dt = axsp_get_max_bal_dt(ba.asset_id)
 INNER JOIN contract_suspension cs ON c.contract_id = cs.contract_id
/
CREATE OR REPLACE FORCE VIEW axvw_property_tax_output
AS
select depr.asset_depr_id, depr.asset_hdr_id, depr.asset_id, depr.contract_id, depr.start_dt, depr.stop_dt, depr.stamp
from asset_hdr ah
	inner join asset_hdr_address on ah.asset_hdr_id = asset_hdr_address.asset_hdr_id and asset_hdr_address.asset_hdr_address_type = 45101
	inner join (
	  select ad.asset_depr_id,
			 ad.asset_hdr_id,
			 ad.asset_id,
			 ad.contract_id,
			 ad.start_dt,
			 ad.stop_depreciation_dt as stop_dt,
			 ad.stamp
	  from asset_depr ad
	) depr on depr.asset_hdr_id = ah.asset_hdr_id

union

-- asset_hdr_addresses with the relevant asset where the asset_hdr record doesn't have an asset_depr entry
select 0, asset.asset_hdr_id, asset.asset_id, asset.contract_id, asset_hdr_address.effective_dt start_dt, to_date('99981231', 'YYYYMMDD') stop_dt, 0 stamp
from asset
inner join asset_hdr_address on asset.asset_hdr_id = asset_hdr_address.asset_hdr_id and asset_hdr_address.asset_hdr_address_type = 45101
inner join (
	select asset_hdr.asset_hdr_id,
           (case asset_hdr.parent_owner_id when 0 then asset_hdr.asset_hdr_id else asset_hdr.parent_owner_id end) temp_asset_hdr_id,
           (case asset_hdr.current_contract_id when 0 then asset_hdr.original_contract_id else asset_hdr.current_contract_id end) temp_contract_id
    from asset_hdr
    where asset_hdr_id in (select asset_hdr_id from asset_hdr where not exists (select 1 from asset_depr where asset_depr.asset_hdr_id = asset_hdr.asset_hdr_id))
    and asset_hdr_id <> 0
    ) temp_asset_hdr
    on asset.asset_hdr_id = temp_asset_hdr.temp_asset_hdr_id
	and asset.contract_id = temp_asset_hdr.temp_contract_id
/
create or replace PROCEDURE AXSP_MODULAR_LIST_INS(
  p_asset_class_modlst_type IN NUMBER  DEFAULT NULL,
  p_asset_class_id IN NUMBER  DEFAULT NULL,
  p_asset_hdr_id IN NUMBER  DEFAULT NULL,
  p_asset_type_moddef_id IN NUMBER  DEFAULT NULL,
  p_asset_type_id IN NUMBER  DEFAULT NULL,
  p_value_id IN NUMBER  DEFAULT NULL,
  p_stamp   IN NUMBER  DEFAULT NULL)  AS
BEGIN
  INSERT INTO ASSET_CLASS_MODULAR_LIST(
    asset_class_modular_list_type
   ,asset_class_id
   ,asset_hdr_id
   ,asset_type_modular_def_id
   ,asset_type_id
   ,value_id
   ,stamp)
   VALUES (p_asset_class_modlst_type,
   p_asset_class_id,
   p_asset_hdr_id,
   p_asset_type_moddef_id,
   p_asset_type_id,
   p_value_id,
   p_stamp);
END AXSP_MODULAR_LIST_INS;
/
create or replace function axsp_purchase_invoice_comments(p_purchase_invoice_id number) return clob is
  cursor c_notes (c_purchase_invoice_id number) is
    select comments from note where purchase_invoice_id = c_purchase_invoice_id order by note_id asc;
  v_comments clob;
begin
  v_comments := to_clob(' ');
  if (p_purchase_invoice_id != 0) then
    for i in c_notes(p_purchase_invoice_id) loop
      v_comments := i.comments;
      exit;
    end loop;
  end if;
  return v_comments;
end;
/

CREATE OR REPLACE
  FUNCTION axsp_get_inv_period_end_date(
      months          IN NUMBER DEFAULT NULL,
      invoice_from_dt IN DATE DEFAULT NULL,
      adjusted_run_dt IN DATE DEFAULT NULL)
    RETURN DATE
  AS
    result DATE;
  BEGIN
    result      := invoice_from_dt;
    WHILE result < adjusted_run_dt
    LOOP
      result := DATEADD('MM', months, result);
    END LOOP;
    RETURN result;
  END axsp_get_inv_period_end_date;
/

create or replace
PROCEDURE AXSP_DOC_METADATA_DOC_LINK_INS(
-- Procedure only called as part of documentation upgrade
-- parameter to check doc_metadata record
paraReference IN VARCHAR2,
paraDocProviderId IN NUMBER,
--parameter to insert doc_metadata record
paraName  IN VARCHAR2,
paraFileSuffix  IN VARCHAR2,
paraDescription  IN VARCHAR2,
paraInputUserId IN NUMBER,
paraInputDt IN DATE,
paraLastUpdatedById IN NUMBER,
paraLastUpdatedDt IN DATE,
--parameter to insert doc_link record
paraDocLinkType IN NUMBER,
paraAssetHdrId IN NUMBER,
paraContractId IN NUMBER,
paraCreditLineId IN NUMBER,
paraGLJournalId IN NUMBER,
paraOpportunityId IN NUMBER,
paraPartyId IN NUMBER,
paraPortfolioId IN NUMBER,
paraProgramId IN NUMBER,
paraReserveId IN NUMBER,
paraDocId IN NUMBER,
paraDocCategoryId IN NUMBER
) AS
docMetadataId NUMBER;
docLinkCount NUMBER;
docSource NUMBER;
BEGIN

begin
SELECT doc_metadata_id
INTO docMetadataId
FROM doc_metadata
WHERE reference = paraReference AND doc_provider_id = paraDocProviderId;
exception
  when NO_DATA_FOUND
    then docMetadataId := 0;
end;

if (paraDocId > 0)
then
	docSource := 50001;
else
  docSource := 50000;
end if;

IF (docMetadataId = 0) THEN
--inserts docMetaData
--sets docMetadataId
INSERT INTO doc_metadata
           (name
           ,file_suffix
           ,description
           ,doc_provider_id
           ,reference
           ,doc_category_id
           ,is_active
           ,is_deleted
           ,input_user_id
           ,input_dt
           ,last_updated_by_id
           ,last_updated_dt
           ,stamp
           ,is_document_deleted
           ,doc_source)
     VALUES
           (paraName,
           paraFileSuffix,
           paraDescription,
           paraDocProviderId,
           paraReference,
           0,
           1,
           0,
           paraInputUserId,
           paraInputDt,
           paraLastUpdatedById,
           paraLastUpdatedDt,
           1,
           0,
           docSource);
           
SELECT globalPkg.lastidentity
INTO docMetadataId
FROM dual;

elsif (paraDocCategoryId > 0) then
UPDATE doc_metadata set doc_category_id = paraDocCategoryId where doc_metadata_id = docMetadataId;

END IF;

--insert docLinkRecord
if (paraDocId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where doc_id = paraDocId and doc_metadata_id = docMetadataId;

elsif (paraAssetHdrId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where asset_hdr_id = paraAssetHdrId and doc_metadata_id = docMetadataId;

elsif (paraContractId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where contract_id = paraContractId and doc_metadata_id = docMetadataId;

elsif (paraCreditLineId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where credit_line_id = paraCreditLineId and doc_metadata_id = docMetadataId;

elsif (paraGLJournalId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where gl_journal_id = paraGLJournalId and doc_metadata_id = docMetadataId;

elsif (paraOpportunityId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where opportunity_id = paraOpportunityId and doc_metadata_id = docMetadataId;

elsif (paraPartyId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where party_id = paraPartyId and doc_metadata_id = docMetadataId;

elsif (paraPortfolioId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where portfolio_id = paraPortfolioId and doc_metadata_id = docMetadataId;

elsif (paraProgramId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where program_id = paraProgramId and doc_metadata_id = docMetadataId;

elsif (paraReserveId > 0)
then
select COUNT(*) INTO docLinkCount from doc_link where reserve_id = paraReserveId and doc_metadata_id = docMetadataId;

end if;

if (docLinkCount = 0)
then
INSERT INTO doc_link
           (doc_metadata_id
           ,doc_link_type
           ,asset_hdr_id
           ,contract_id
           ,credit_line_id
           ,gl_journal_id
           ,opportunity_id
           ,party_id
           ,portfolio_id
           ,program_id
           ,reserve_id
           ,doc_id
           ,stamp)
     VALUES
           (docMetadataId
           ,paraDocLinkType
           ,paraAssetHdrId
           ,paraContractId
           ,paraCreditLineId
           ,paraGLJournalId
           ,paraOpportunityId
           ,paraPartyId
           ,paraPortfolioId
           ,paraProgramId
           ,paraReserveId
           ,paraDocId
           ,1);
end if;           

END AXSP_DOC_METADATA_DOC_LINK_INS;

/

CREATE OR REPLACE PROCEDURE axsp_party_depersonalisation 
(
	v_paraKey IN NVARCHAR2
)
AS
 v_first_names NVARCHAR2(20);
 v_middle_name NVARCHAR2(20);
 v_dob date;
 v_company_info NVARCHAR2(20);
 v_trading_as NVARCHAR2(5);
 v_phone NVARCHAR2(20);
 v_phone_search NVARCHAR2(20);
 v_fax_business NVARCHAR2(20);
 v_fax_home NVARCHAR2(20);
 v_web_page NVARCHAR2(20);
 v_email_business NVARCHAR2(30);
 v_tax_no NVARCHAR2(20);
 v_drivers_license_no NVARCHAR2(20);
 v_business_ref_no NVARCHAR2(20);
 v_invoice_name NVARCHAR2(20);
 v_key NVARCHAR2(50);
 v_national_no NVARCHAR2(20);
 v_passport_location_id int;
 v_passport_no NVARCHAR2(20);
 v_identification_ref1 NVARCHAR2(20);
 v_identification_ref2 NVARCHAR2(20);
 v_duns_number NVARCHAR2(20);
 v_business_ref_no2 NVARCHAR2(20);
 v_business_ref_no3 NVARCHAR2(20);
 v_gross_income decimal(18,4);
 v_job_title NVARCHAR2(20);
 v_employee_id int;
BEGIN
	/*
	WARNING: Do not run this script against the production/live database.
	NOTE   : Ensure a valid backup is taken of the database before this script is run against it.
   */
  
  select cast(lock_key as NVARCHAR2(50)) into v_key from role where role_id = 1;
  if v_paraKey != v_key then
    return;
  end if;
  dbms_output.put_line('Executing axsp_party_depersonalization ...');

  BEGIN
  
  v_first_names := 'John';
  v_middle_name := 'Peter';
  v_dob := '01-Jan-1900';
  v_company_info := '';
  v_trading_as := ' ';
  v_phone := '+31(99)1234567';
  v_phone_search := '1234567';
  v_fax_business := '+31(99)2345678';
  v_fax_home := '+31(99)1234567';
  v_web_page := 'localhost';
  v_email_business := 'everyone@internet.com';
  v_tax_no := 'XX000000000';
  v_drivers_license_no := 'XX0000';
  v_business_ref_no := 'KVK Earth';
  v_invoice_name := 'Dear Customer';
  v_national_no := '123';
  v_passport_location_id := 0;
  v_passport_no := '123';
  v_identification_ref1 := 'abc';
  v_identification_ref2 := 'abc';
  v_duns_number := '123';
  v_business_ref_no2 := 'KVK Earth';
  v_business_ref_no3 := 'KVK Earth';
  v_gross_income := 0;
  v_job_title := 'Head Chef';
  v_employee_id := 0;

update party set
name = TO_CHAR(party_no),
ext_name = TO_CHAR(party_no),
first_names = v_first_names,
middle_name = v_middle_name,
date_of_birth = v_dob,
company_info = v_company_info,
trading_as = v_trading_as,
phone_business = v_phone,
phone_business_search = v_phone_search,
fax_business = v_fax_business,
web_page = v_web_page,
email_business = v_email_business,
tax_no = CONCAT(v_tax_no, TO_CHAR(party_no)),
drivers_license_no = CONCAT(v_drivers_license_no, TO_CHAR(party_no)),
business_ref_no = v_business_ref_no,
invoice_name = v_invoice_name,
phone_home = v_phone,
phone_home_search = v_phone_search,
phone_mobile = v_phone,
phone_mobile_search = v_phone_search,
fax_home = v_fax_home,
national_no = v_national_no,
passport_location_id = v_passport_location_id,
passport_no = v_passport_no,
identification_ref1 = v_identification_ref1,
identification_ref2 = v_identification_ref2,
duns_number = v_duns_number,
business_ref_no2 = v_business_ref_no2,
business_ref_no3 = v_business_ref_no3,
email_home = v_email_business
where
party_id > 0
and is_business_unit = 0
and is_branch = 0;
commit;

/** Party Account **/
update party_account
set party_account.name = CONCAT('PartyAcc ', TO_CHAR(party_account_id))
where party_account_id > 0;
commit;

/** Party Bank Account **/
update party_bankacc
set
code = '0123456789',
name = 'Bank',
ext_name = 'Bank',
Branch_Code = '123456',
Branch_Address = 'Street1'
where bankacc_id > 0;
commit;

/** Party Duplicate Bucket **/
update party_dup_bucket
set word = CONCAT(soundex_value, bucket_value);
commit;

/** Party Address **/
update party_address
set
street = CONCAT('Sesame Street ', party_id),
zip_code = 'E14 5HQ'
where party_address_id > 0;
commit;

/** Address **/
update address
set
street = CONCAT('Sesame Street ', address_id),
zip_code = 'E14 5HQ';
commit;

update audit_fld 
set old_fld_value = '', new_fld_value = ''
where audit_hdr_id in 
(select audit_hdr_id from audit_hdr where tbl_name in ('party', 'party_account', 'party_address', 'party_bankacc', 'address'));
commit;

update audit_key 
set old_fld_value = '', new_fld_value = ''
where audit_hdr_id in 
(select audit_hdr_id from audit_hdr where tbl_name in ('party', 'party_account', 'party_address', 'party_bankacc', 'address'));
commit;

update note
set comments = ''
where subject like '%party%';
commit;

/** Bank flows **/
update bank_flow set
party_name = CONCAT('Party', bank_flow_id),
party_bank_name = CONCAT('Bank', bank_flow_id), 
party_bank_account_name = CONCAT('BankAccName', bank_flow_id),
party_bank_account_no = CONCAT('BankAccNo', bank_flow_id),
party_account_name = CONCAT('PartyAccName', bank_flow_id);
commit;

/** Employment Info  **/
update Employment_info set
gross_income = v_gross_income,
employee_id = v_employee_id,
job_title = v_job_title;
commit;

/** User Info - Script is added on 12/16/2013, we excluded these fields (name, codeand ext_name) because it will stuff the lock_key **/
update ax_user Set
email = v_email_business
where  external_user_type in (16801,16803);
commit;

/** Doc List Email Info - Script is added on 12/16/2013 **/
update doc_list set
list_value = v_email_business
where list_type in (50701, 50702, 50703, 50704, 50705, 50706, 50710) and length(list_value) > 0;
commit;

--select to_char(sysdate,'dd/mm/yyyy hh24:mi:ss') end_time from dual;
--spool off;
--exit;
	END;
END axsp_party_depersonalisation;
/
CREATE OR REPLACE PROCEDURE AXSP_UPDATE_AMT_ABV
AS
  v_today_dt DATE;
BEGIN
  v_today_dt := axsp_get_datetime();
  
  INSERT INTO asset_hdr_balance (amt_abv,input_dt,effective_dt,asset_hdr_id,stamp)
    SELECT 0, v_today_dt, v_today_dt ,asset_hdrs_to_add.asset_hdr_id,0
  FROM 
    ( --Find asset_hdr_id that are not in asset_hdr_balnce
      SELECT 
        ah.asset_hdr_id 
      FROM 
        asset_hdr ah
      LEFT JOIN 
        asset_hdr_balance ahb
      ON 
        ah.asset_hdr_id = ahb.asset_hdr_id
      WHERE  
        ah.asset_hdr_id > 0 and ahb.asset_hdr_id IS NULL
    ) asset_hdrs_to_add;
    
  
  --Updates the amt_abv of all asset_hdr that are not groups
  --Only assets that were not sold will get updated as well as assets that were sold after the last update (one time update).
  MERGE INTO asset_hdr_balance
  using
  (
      select 
        ahb.asset_hdr_balance_id, amt_book_value
      FROM 
        asset_hdr_balance ahb 
      inner join
      (
        select 
          amt_book_value, ahb.asset_hdr_id
        FROM 
          asset_hdr_balance ahb
        INNER JOIN 
          --Find the latest depreciation record for asset_hdr
          (
            SELECT 
              amt_book_value, last_depreciation_calc_dt, asset_hdr_id as ad1_asset_hdr_id, asset_depr_id as ad1_depr_id,start_dt, RANK() over (PARTITION by asset_hdr_id order by last_depreciation_calc_dt desc, start_dt desc, stop_depreciation_dt desc, asset_depr_id desc) rank_num
            FROM 
              asset_depr
            WHERE 
              depreciation_state != 35105 --Not inactive
          ) latest_depr_record  
        ON 
          latest_depr_record.ad1_asset_hdr_id = ahb.asset_hdr_id
        INNER JOIN 
          asset_hdr ah
        ON 
          latest_depr_record.ad1_asset_hdr_id = ah.asset_hdr_id
        WHERE rank_num = 1
         and ah.hdr_type != 33402 -- Not Group 
          and (ah.asset_status != 43060 --NOT SOLD
         OR (ah.asset_status = 43060 
			and 
				(
				CAST(ahb.effective_dt AS DATE) <= CAST(latest_depr_record.last_depreciation_calc_dt as DATE) --or sold after last ABV update
				OR
				CAST(ahb.effective_dt AS DATE) = CAST(v_today_dt as DATE) -- when running twice in the same day need to intialise values as assets with sub assets will not have the correct values for the parent asset as we will just add to the existing parent ABV values
				)
			)
		) 
      )latest
      on 
        ahb.asset_hdr_id = latest.asset_hdr_id
  ) q2 
  on (q2.asset_hdr_balance_id = asset_hdr_balance.asset_hdr_balance_id)
  WHEN MATCHED THEN UPDATE 
  set asset_hdr_balance.amt_abv = q2.amt_book_value,
  asset_hdr_balance.effective_dt = v_today_dt;
  
  
  --Update amt_abv of parnets that were not sold or sold after the last ABV calculation (one time update)
  MERGE INTO asset_hdr_balance
  using
  (
    select 
      ahb.asset_hdr_balance_id, amt_abv + sum_of_children as total
      FROM --Calculate sum of all children related to a parent
        (SELECT 
          owner_id, SUM(amt_abv) sum_of_children FROM asset_hdr_balance ahb
        INNER JOIN asset_hdr ah
          ON ahb.asset_hdr_id = ah.asset_hdr_id
        WHERE 
          ah.hdr_type = 33401 --only look at sub assets
        GROUP BY 
          owner_id
        ) sum_of_children_per_parent
      INNER JOIN asset_hdr_balance ahb
        on ahb.asset_hdr_id = sum_of_children_per_parent.owner_id
      INNER JOIN asset_hdr ah on 
        ah.asset_hdr_id = sum_of_children_per_parent.owner_id
      WHERE 
        ah.hdr_type = 33400 --Asset
      AND
      (
        ah.asset_status != 43060 --not sold
        OR (ah.asset_status = 43060 and 
          exists
          (
            select 0 from asset_depr adep
            where 
            ah.asset_hdr_id = adep.asset_hdr_id 
            AND 
				(
					CAST(ahb.effective_dt AS DATE) <= CAST(adep.last_depreciation_calc_dt as DATE) -- sold after last ABV update
					OR
					CAST(ahb.effective_dt AS DATE) = CAST(v_today_dt as DATE) -- effective date may be greater than last_depreciation_calc_dt so this is required to ensure summing up of children
				)
          )
        )
      )
  )q2 
  on (q2.asset_hdr_balance_id = asset_hdr_balance.asset_hdr_balance_id)
  WHEN MATCHED THEN UPDATE 
  set asset_hdr_balance.amt_abv = q2.total,
  asset_hdr_balance.effective_dt = v_today_dt;
  
  --update amt_abv for groups
  --that were not sold or groups that were sold after the last ABV update
  --update abv when "Depreciation on Group when present" checked 
  --in this case the group abv = book value of group
  DECLARE itemExists INT;
  BEGIN
    begin
    SELECT COUNT(*) INTO itemExists 
        FROM system_defs
        WHERE is_depreciate_on_group = 1;
    end;
    IF 
      itemExists > 0
    THEN 
      MERGE INTO asset_hdr_balance
      using
      (
          select 
            ahb1.asset_hdr_balance_id, amt_book_value 
          from 
            asset_hdr_balance ahb1
          INNER JOIN
          (
            select 
            amt_book_value, ahb.asset_hdr_id from
          --Find the latest depreciation record for asset_hdr
            (
              select 
                amt_book_value, asset_hdr_id as ad1_asset_hdr_id, asset_depr_id as ad1_depr_id,start_dt,last_depreciation_calc_dt, 
              RANK() over (PARTITION by asset_hdr_id order by last_depreciation_calc_dt desc, start_dt desc, stop_depreciation_dt desc, asset_depr_id desc) rank_num
              from 
                asset_depr
              where 
                depreciation_state != 35105 --Not inactive
            ) latest_depr_record  
            INNER JOIN asset_hdr_balance ahb 
              ON latest_depr_record.ad1_asset_hdr_id = ahb.asset_hdr_id
            INNER JOIN asset_hdr ah on
              ah.asset_hdr_id = ahb.asset_hdr_id
            WHERE
              rank_num = 1
            AND ah.hdr_type = 33402 --Group
            AND 
              (
              ah.asset_status != 43060 --not sold
              OR (ah.asset_status = 43060 and 
                exists
                (
                  select 0 from asset_depr adep
                  where 
                  ah.asset_hdr_id = adep.asset_hdr_id 
                  AND 
						(
							CAST(ahb.effective_dt AS DATE) <= CAST(adep.last_depreciation_calc_dt as DATE) -- sold after last ABV update
							OR
							CAST(ahb.effective_dt AS DATE) = CAST(v_today_dt as DATE) -- effective date may be greater than last_depreciation_calc_dt so this is required to ensure summing up of children
						)
                )
                )
              )
          ) latest
          ON 
            ahb1.asset_hdr_id = latest.asset_hdr_id
      ) q2
      on (q2.asset_hdr_balance_id = asset_hdr_balance.asset_hdr_balance_id)
      WHEN MATCHED THEN UPDATE 
      set asset_hdr_balance.amt_abv = q2.amt_book_value,
      asset_hdr_balance.effective_dt = v_today_dt;
  ELSE
      MERGE INTO asset_hdr_balance
      using
      (
          SELECT 
            ahb.asset_hdr_balance_id, group_sum_of_children.sum_of_children
          FROM
            --Get the sum of all ABV in each group
            (
            SELECT 
              owner_id, SUM(amt_abv) sum_of_children
             FROM 
              asset_hdr_balance ahb
            INNER JOIN asset_hdr ah
              ON ahb.asset_hdr_id = ah.asset_hdr_id
            WHERE
              ah.hdr_type = 33400 --Asset 
            GROUP BY 
              owner_id
            ) group_sum_of_children
            INNER JOIN 
              asset_hdr_balance ahb
            ON
              group_sum_of_children.owner_id = ahb.asset_hdr_id
            INNER JOIN 
              asset_hdr ah 
            ON 
              ah.asset_hdr_id = group_sum_of_children.owner_id 
            WHERE
                ah.hdr_type = 33402 --Group
                AND 
                (
                  ah.asset_status != 43060 --not sold
                  OR 
                  (ah.asset_status = 43060 
                  and 
                      exists
                      (
                        select 0 from asset_depr adep
                        where 
                        ah.asset_hdr_id = adep.asset_hdr_id
                       AND 
								(
									CAST(ahb.effective_dt AS DATE) <= CAST(adep.last_depreciation_calc_dt as DATE) -- sold after last ABV update
									OR
									CAST(ahb.effective_dt AS DATE) = CAST(v_today_dt as DATE) -- effective date may be greater than last_depreciation_calc_dt so this is required to ensure summing up of children
								)
                      )
                    )
              )
        ) q2
        on (q2.asset_hdr_balance_id = asset_hdr_balance.asset_hdr_balance_id)
        WHEN MATCHED THEN UPDATE 
        set asset_hdr_balance.amt_abv = q2.sum_of_children,
        asset_hdr_balance.effective_dt = v_today_dt;
    END IF;
  END;
END AXSP_UPDATE_AMT_ABV;
/
CREATE OR REPLACE FUNCTION axsp_get_indr_exp_for_party
	(partyId IN NUMBER)
RETURN party_indirect_exp_tbltab PIPELINED IS
--
  v_default_fx_rate NUMBER;
  v_default_rate_basis NUMBER;
  tbl party_indirect_exp_tblobj := party_indirect_exp_tblobj(null, null, null, null, null, null, null, null, null, null, null);
BEGIN

  v_default_fx_rate := 8402;
  SELECT ISNULL(to_number(system_setting_value), 0) INTO v_default_rate_basis
  FROM system_setting WHERE system_setting_type = v_default_fx_rate;
  
  FOR i IN
	  (SELECT de_rank1.contract_id,
			de_rank1.product_id,
			de_rank1.business_unit_id,
			CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END AS currency_id,
			de_rank1.party_id,
			(de_rank1.amount_direct_exposure * axsp_get_fx_cross_rate(v_default_rate_basis, de_rank1.calc_dt, CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END, de_rank1.currency_id)) AS amount_direct_exposure,
			(de_rank1.amount_memorandum * axsp_get_fx_cross_rate(v_default_rate_basis,de_rank1.calc_dt, CASE WHEN l1.currency_id > l2.currency_id THEN l1.currency_id ELSE l2.currency_id END, de_rank1.currency_id)) AS amount_memorandum,
			de_rank1.party_role,
			de_rank1.calc_dt,
			de_rank1.calculation_type,
			de_rank1.contract_state
		FROM axvw_party_indirect_exp de_rank1
			inner join party p ON p.party_id = de_rank1.party_id
			inner join party bu ON p.party_business_unit_id = bu.party_id
			inner join location l1 ON l1.location_id = CASE WHEN p.is_business_unit = 1 THEN p.location_id ELSE bu.location_id END
			inner join location l2 ON l1.country_id = l2.location_id
		WHERE
			(de_rank1.party_id = partyId OR partyId = 0)
		ORDER BY de_rank1.contract_id
		) LOOP
			tbl.contract_id := i.contract_id;
			tbl.product_id := i.product_id;
			tbl.business_unit_id := i.business_unit_id;
			tbl.currency_id := i.currency_id;
			tbl.party_id := i.party_id;
			tbl.amount_direct_exposure := i.amount_direct_exposure;
			tbl.amount_memorandum := i.amount_memorandum;
			tbl.party_role := i.party_role;
			tbl.calc_dt := i.calc_dt;
			tbl.calculation_type := i.calculation_type;
			tbl.contract_state := i.contract_state;
		PIPE ROW(tbl);
	END LOOP;
END axsp_get_indr_exp_for_party;
/
