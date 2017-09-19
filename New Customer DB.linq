<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

/* These are DRAFT instructions when loading new customer DB.
	1. Restore DB
	2. Check SQL
*/
	-- Check date
		-- MS-SQL - (order in code is SP -> View -> sys fn)
		select 'axsp_get_datetime', myDt = dbo.axsp_get_datetime()
		  union all select 'axvw_get_datetime', * from axvw_get_datetime 
		  union all select 'System Date', mySysDt = getdate();
  
		-- Oracle - (order in code is View -> SP -> sys fn)
		select 'axvw_get_datetime', to_char(server_dt, 'Dy DD-Mon-YYYY HH24:MI:SS') as myDt from axvw_get_datetime 
		  union all select 'axsp_get_datetime', to_char(axsp_get_datetime(), 'Dy DD-Mon-YYYY HH24:MI:SS') as myDt from dual
		  union all select 'getdate', to_char(getdate(), 'Dy DD-Mon-YYYY HH24:MI:SS') as myDt from dual
		  union all select 'Sysdate', to_char(sysdate, 'Dy DD-Mon-YYYY HH24:MI:SS') as myDt from dual;		  
		  
		  
	-- Event log (recent activity)
		-- MS-SQL
		select top 20 * from event_log
		where  src_machine_name not like '%chc%'
		--	and description like 'remote service start%'
		order by event_log_id desc;
		
		-- Oracle
		select * from
			(select to_char(input_dt, 'Dy DD-Mon-YYYY HH24:MI:SS') as myDt, event_log.* from event_log
				where  lower(src_machine_name) not like '%chc%'
            and lower(description) like 'remote service start%'
				order by event_log_id desc
			)
		where rownum <=20;

	-- Last attempted logins
		select top 200 * from sec_audit_log order by sec_audit_log_id desc;
		/* optionally look at ax_user_logon_det */


	-- DB Version
		select * from db_ver;
		select * from db_upgrade_step order by db_upgrade_step_id desc;		--v4.2x+

		-- pre v4.2x only
		select top 10 * from db_patch_ver order by db_patch_ver desc;	-- MS-SQL
		select * from db_patch_ver order by db_patch_ver desc;     -- Oracle


	-- Triggers
		/* find all triggers by parent object */
		select b.name, b.type_desc, a.name, a.type_desc from sys.all_objects as a 
			join sys.all_objects as b on a.parent_object_id = b.object_id
		where a.type = 'TR'
		order by b.name, a.name;

	
	-- Identify whether login uses name or code
		/* At username prompt use: 0 = Name, 1=Code */
		select logon_method from system_security_defs;

		/* Change logon method to name, only required for v4.40 */
		update system_security_defs
		set logon_method=0 where  logon_method=1;
		


	-- Reset Administrator password
		/* Identify Administrator's Business Unit, need to change it later */
		select party.ext_name, ax_user.business_unit_id, ax_user.* from ax_user
			join party on party.party_id = ax_user.business_unit_id
		where ax_user.ext_name = 'Administrator';
 

		/* set admin user password to GreenFrog100 */
		UPDATE    ax_user
		SET
			password = '2292FCB0D076FA90AEAA9AC5A89D2D9AF456EC06',	-- GreenFrog100
			password_dt = '02-May-2007',
			lock_key = -490052201,
			stamp = 12,
			password_hist = '1852851148,-654946257,1620798902,-100888914,883963751,-517328653',
			is_locked = 0,
			is_active = 1,
			business_unit_id = 0,
			business_units_csv =' ',
			is_deleted = 0
		WHERE ext_name = 'Administrator';

		/* set admin user password to passw0rd */
		UPDATE ax_user SET
			password = '2BDBB5005204C3453EF2A5C8155FFD0427FAB9DE',	-- passw0rd
			password_dt = '26-Sep-2028',
			lock_key = 1259766693,
			stamp = 2,
			password_hist = ' ',
			is_locked = 0,
			is_active = 1,
			business_unit_id = 0,
			business_units_csv = ' '
	/* The following fields are checked by the lock_key code as well, however they
		should never change. If there is a problem with the ax_user table then try
		updating these too.
			,
			version_no = 1,					-- See case 42778, non Unicode
			ignore_inactivity_lock_days = 1,
			is_group = 0,
			access_rights = 10602,
			external_user_type = 16800,
			external_party_id = 0
	--*/
		WHERE (name = 'Administrator');





  /* Check logon detail linkage, master only */
    select * from ax_user_logon_det
    where ax_user_id = (select ax_user_id from ax_user where ax_user.ext_name = 'Administrator');

  /* Delete entry for Administrator - seems to work */
  delete ax_user_logon_det where ax_user_id = (select ax_user_id from ax_user where ax_user.ext_name = 'Administrator'); 

  /* Changes entries for Administrator - DRAFT doesn't work */
    update ax_user_logon_det
      set cum_logon_fail = 0,
        consec_logon_fail = 0
        -- ax_user_id=999
    where ax_user_id = (select ax_user_id from ax_user where ax_user.ext_name = 'Administrator');


	-- Stop all scheduled processes
		select * from scheduler_process where scheduler_Process_id > 0 and status != 24304;
		update scheduler_process set status = 24304 where scheduler_Process_id > 0 and status != 24304;


	-- Truncate log file as required.


	-- Identify if Prevent Concurrent logins is enabled
		/* If true then after loading licence remote service must be restarted, see case 39871 */
		select prevent_concurrent_sessions from system_security_defs


/*
3. Install correction version/build of 42
4. Configure .config files
5. Start service
6. Review Windows Event Log - expect security tampering alert
7. Update licence
8. Reset Administrators Business Unit.
9. Check Default Path
*/


--11. Reports
		/* All report definitions that have been used */
		select * from report_fmt
		where report_fmt_id in (
			select report_fmt_id from report
			);


		/* All Document/template definitions that have been used */
		select * from template_fmt
		where template_fmt_id in (
			select template_fmt_id from doc
			)order by name;


--12. Plugins


--13.  Approval Rules
		/* List all active approval rules */
		select * from approval_rule_hdr where is_active != 0;
		/* Turn off all active approval rules */
		Update approval_rule_hdr set is_active = 0 where is_active != 0;
		
--14. Workflow Security Restrictions
		/* turn off all restrictions */
		update wf_state_chain_item set is_restricted_access = 0, is_move_back_from_restricted = 0, is_move_back_into_restricted = 0, 
			is_move_fwd_from_restricted = 0, is_move_fwd_into_restricted = 0;
--(Backup DB)
