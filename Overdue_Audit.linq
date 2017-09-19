<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

/*
Date:	 14/02/2014
Created: 	Lynden Sherriff
Modified:	Lyle Henkeman
Case:	 44122 - Script to help with decoding the 'Audit' tables

General Information:

	This script is to help decode the 'Audit' tables to more user friendly format.

	The script is broken in to 2 sections 'Parameters' and 'Script'.
	Update the parameters section by:
			a. Entering the table that you are interest in seeing the audit changes.
			b. Enter the Start/End dates of interest.
			c. optional setup the '@primaryValue' of interest.
	Then run the script.
	
	The result will show you the in the following layout:
	
			General Audit Information					|			Audit Key to Identify Original Record			|		Extra Fields that have changed
	[Audit_Hdr_id	audit_dt	Type	UserName]		|	[PrimaryFieldName	PrimaryOldValue	PrimaryNewValue]	|	[fld_name	old_fld_value	new_fld_value]
	----------------------------------------------------------------------------------------------------------------------------------------------------------------
	53775			2013-07-26	Insert	Administrator	|	asset_hdr_id						657					|	NULL		NULL			NULL
	
	Some tables require multiple Audit Keys to identify the original object, in which case it will have multiple rows.
	[Most have only 1 primary key]
		
	The script combines the following 3 Audit tables
		Audit_Hdr	- General Information about the Audit Add/Delete/Change
		Audit_key	- Primary keys to help Id the record changing
		Audit_Fld	- Extra fields that are changing other than primary Key Fields
	Including 'ax_user' to provide the actual user name for display.
*/

-----------------------------------------------------------------------------
-- Parameters
DECLARE @tableName varchar(32)		-- The Table of interest	[eg: 'asset']
set @tableName = ''					-- use '' to show all

DECLARE @startDate DateTime			-- The first date of interest
set @startDate = '1900-01-01'		-- Year-Month-Day

DECLARE @endDate DateTime			-- The last date of interest
set @endDate = '2999-01-01'			-- Year-Month-Day

DECLARE @primaryValue nvarchar(255)
set @primaryValue = ''				-- The primary key of interest [eg if scanning the 'Asset' table use the actual asset_hdr_Id value of interest]
									-- use '' to show all, other wise it used this to scan or the new/old values of hte primary key.
-----------------------------------------------------------------------------
-- Script
select 
		-- General Audit Information
		ah.Audit_Hdr_id, ah.audit_dt, 
			CASE ah.change_type
				WHEN 0 THEN 'None'
				WHEN 1 THEN 'Insert'
				WHEN 2 THEN 'Update'
				WHEN 3 THEN 'Delete'
				WHEN 4 THEN 'Queued'
			END Type, 
		u.Name UserName, 
		
		'' '-',	-- Audit Key to Identify Original Record
		ak.fld_name PrimaryFieldName,
		ak.old_fld_value PrimaryOldValue, ak.new_fld_value PrimaryNewValue,
		
		'' '-',	-- Extra Fields that have changed
		af.fld_name, 
		af.old_fld_value, af.new_fld_value
		
from Audit_Hdr ah						-- Audit Header

inner join Audit_key ak					-- Audit Key
	on ah.Audit_Hdr_id = ak.Audit_Hdr_id
	
inner join ax_user u					-- Use AX_User to get nice 'Name' to display
	on u.ax_user_id = ah.user_id		
	
left join Audit_Fld af					-- Audit Fields - Other fields that have changed not primary key.
	on ah.Audit_Hdr_id = af.Audit_Hdr_id-- Might have 0 or more fields.
	
where 
	(ah.tbl_name = @tableName or @tableName = '')			-- Use Parameters to limit results to table and date range
	and ah.audit_dt >= @startDate 
	and ah.audit_dt <= @endDate
	and (@primaryValue = '' or ak.old_fld_value = @primaryValue or ak.new_fld_value = @primaryValue)
order by ah.audit_dt;					-- Place in Audit Date order
