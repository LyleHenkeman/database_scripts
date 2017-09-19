<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

USE [DB_NAME]
GO
/****** Object:  StoredProcedure [dbo].[axsp_party_depersonalisation]    Script Date: 11/14/2013 10:22:05 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [dbo].[axsp_party_depersonalisation] 
(
	@paraKey nvarchar(50)
)
AS
BEGIN
/*
	WARNING: Do not run this stored procedure against the production/live database.
	NOTE   : Ensure a valid backup is taken of the database before this stored procedure is run against it.
*/

 --if ((select isnull(cast(lock_key as nvarchar(50)), 0) from role where role_id = 1) != @paraKey)
	--return;

 PRINT 'Executing axsp_party_depersonalization ...'

 SET NOCOUNT ON

 declare @v_first_names nvarchar(20);
 declare @v_middle_name nvarchar(20);
 declare @v_dob datetime;
 declare @v_company_info nvarchar(20);
 declare @v_trading_as nvarchar(5);
 declare @v_phone nvarchar(20);
 declare @v_phone_search nvarchar(20);
 declare @v_fax_business nvarchar(20);
 declare @v_fax_home nvarchar(20);
 declare @v_web_page nvarchar(20);
 declare @v_email_business nvarchar(30);
 declare @v_tax_no nvarchar(20);
 declare @v_drivers_license_no nvarchar(20);
 declare @v_business_ref_no nvarchar(20);
 declare @v_invoice_name nvarchar(20);
 
 set @v_first_names = 'John';
 set @v_middle_name = 'Peter';
 set @v_dob = '01-Jan-1900';
 set @v_company_info = '';
 set @v_trading_as = ' ';
 set @v_phone = '+31(99)1234567';
 set @v_phone_search = '1234567';
 set @v_fax_business = '+31(99)2345678';
 set @v_fax_home = '+31(99)1234567';
 set @v_web_page = 'localhost';
 set @v_email_business = 'everyone@internet.com';
 set @v_tax_no = 'XX000000000';
 set @v_drivers_license_no = 'XX0000';
 set @v_business_ref_no = 'KVK Earth';
 set @v_invoice_name = 'Dear Customer';
 
/** Customer **/
update party set
name = 'C_Customer' + cast(party_no as nvarchar),
ext_name = 'C_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where
party_id > 0
and is_business_unit = 0
and is_branch = 0
and party_type not in (1,4,16,32,64,1024,4096,8,520,40,256,258,262,272,512,
                       513,514,516,518,528,530,544,546,768,770,786,784,2818,
                       2816,2562,2560,2304);

--/** BANK **/
--update party
--set
--name = 'B_Customer' + CAST(party_no AS NVARCHAR),
--ext_name = 'Customer' + CAST(party_no AS NVARCHAR),
--first_names = 'John',
--middle_name = 'Peter',
--Date_of_birth = '01-Jan-1900',
--company_info = '',
--trading_as = ' ',
--phone_business = '+31(99)1234567',
--phone_business_search = '1234567',
--fax_business = '+31(99)2345678',
--web_page = 'localhost',
--email_business = 'everyone@internet.com',
--tax_no = 'XX000000000' + CAST(party_no AS NVARCHAR),
--drivers_license_no = 'XX0000' + CAST(party_no AS NVARCHAR),
--business_ref_no = 'KVK Earth',
--invoice_name = 'Dear Customer',
--phone_home = '+31(99)1234567',
--phone_home_search = '1234567',
--phone_mobile = '+31(99)1234567',
--phone_mobile_search = '1234567',
--fax_home = '+31(99)1234567'
--where party_type in (8, 520, 40)
--and party_id > 0
--and is_business_unit = 0
--and is_branch = 0;

/** VENDOR ONLY **/
update party
set
name = 'V_Customer' + cast(party_no as nvarchar),
ext_name = 'V_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type = 4
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** INSURANCE ONLY **/
update party
set
name = 'I_Customer' + cast(party_no as nvarchar),
ext_name = 'I_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type = 16
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** Manufacture ONLY **/
update party
set
name = 'M_Customer' + cast(party_no as nvarchar),
ext_name = 'M_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type = 32
and party_id > 0;

/** Tax Authority ONLY **/
update party
set
name = 'T_Customer' + cast(party_no as nvarchar),
ext_name = 'T_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type = 64
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** Broker **/
update party
set
name = 'BR_Customer' + cast(party_no as nvarchar),
ext_name = 'B_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type in (256,258,262,272)
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** Dealer **/
update party
set
name = 'D_Customer' + cast(party_no as nvarchar),
ext_name = 'D_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type in (512,513,514,516,518,528,530,544,546,768,770,786,784,2818,2816,2562,2560,2304)
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** Registration Authority ONLY **/
update party
set
name = 'R_Customer' + cast(party_no as nvarchar),
ext_name = 'R_Customer' + cast(party_no as nvarchar),
first_names = @v_first_names,
middle_name = @v_middle_name,
date_of_birth = @v_dob,
company_info = @v_company_info,
trading_as = @v_trading_as,
phone_business = @v_phone,
phone_business_search = @v_phone_search,
fax_business = @v_fax_business,
web_page = @v_web_page,
email_business = @v_email_business,
tax_no = @v_tax_no + cast(party_no as nvarchar),
drivers_license_no = @v_drivers_license_no + cast(party_no as nvarchar),
business_ref_no = @v_business_ref_no,
invoice_name = @v_invoice_name,
phone_home = @v_phone,
phone_home_search = @v_phone_search,
phone_mobile = @v_phone,
phone_mobile_search = @v_phone_search,
fax_home = @v_fax_home
where party_type = 1024
and party_id > 0
and is_business_unit = 0
and is_branch = 0;

/** Party Account **/
update party_account
set party_account.name = 'PartyAcc ' + cast(party_account_id as nvarchar)
where party_account_id > 0;

/** Party Bank Account **/
update party_bankacc
set
code = '0123456789',
name = 'Bank',
ext_name = 'Bank',
Branch_Code = '123456',
Branch_Address = 'Street1'
where bankacc_id > 0;

/** Party Duplicate Bucket **/
update party_dup_bucket
set word = soundex_value + bucket_value;

/** Party Address **/
update party_address
set
street = 'Sesame Street ' + cast(party_id as nvarchar),
zip_code = 'E14 5HQ'
where party_address_id > 0;

/** Address **/
update address
set
street = 'Sesame street ' + cast(address_id as nvarchar),
zip_code = 'E14 5HQ';

update audit_fld 
set old_fld_value = '', new_fld_value = ''
where audit_hdr_id in 
(select audit_hdr_id from audit_hdr where tbl_name in ('party', 'party_account', 'party_address', 'party_bankacc', 'address'))

update audit_key 
set old_fld_value = '', new_fld_value = ''
where audit_hdr_id in 
(select audit_hdr_id from audit_hdr where tbl_name in ('party', 'party_account', 'party_address', 'party_bankacc', 'address'))

update note
set comments = ''
where subject like '%party%'

/** Bank flows **/
update bank_flow set
party_name = 'Party' + cast(bank_flow_id as nvarchar),
party_bank_name = 'Bank' + cast(bank_flow_id as nvarchar), 
party_bank_account_name = 'BankAccName' + cast(bank_flow_id as nvarchar),
party_bank_account_no = 'BankAccNo' + cast(bank_flow_id as nvarchar),
party_account_name = 'PartyAccName' + cast(bank_flow_id as nvarchar);

--select to_char(sysdate,'dd/mm/yyyy hh24:mi:ss') end_time from dual;
--spool off;
--exit;

SET NOCOUNT OFF
END