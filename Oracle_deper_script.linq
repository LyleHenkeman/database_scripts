<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--Oracle depersonalisation

create or replace 
PROCEDURE axsp_party_depersonalisation 
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
  v_email_business := 'everyone:internet.com';
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