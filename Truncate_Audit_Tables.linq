<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

IF EXISTS (SELECT * FROM sysobjects WHERE id = OBJECT_ID(N'[dbo].[axsp_db_size_reduction]')
    AND objectproperty( id, N'IsProcedure' ) = 1)
DROP PROCEDURE [dbo].[axsp_db_size_reduction]
GO

CREATE PROCEDURE [dbo].[axsp_db_size_reduction] 
(
	@deleteFromImageTables bit,
	@logDaysToKeep int,
	@shrinkDB bit
)
AS
BEGIN
	/*
		WARNING: Do not run this stored procedure against the production/live database.
		NOTE   : Ensure a valid backup is taken of the database before this stored procedure is run against it.
	*/

	PRINT 'Executing axsp_db_size_reduction ...'
				
	/* 
		Description:

		Run this script on large client databases to remove bloat in some core tables.

		The first part of this script will entirely empty the following tables:
		Audit_fld
		audit_key
		audit_hdr
		flow_audit

	5	asset_cfvi
		asset_custom_flowi
		asset_hdr_cfvi
		asset_hdri
		asset_price_schedulei
	10	asset_price_segmenti
		asseti

		bail_asset_ctlmt_plani
		bail_asseti

		contract_actgi
	15	contract_cfvi
		contract_deti
		contract_custom_flowi
		contract_grp_linki
		contract_indexationi
	20	contract_price_schedulei
		contract_price_segmenti
		contract_svtni
		contracti

		flow_calci
	25	flowi

		gl_journali

		po_row_cfi
		po_rowi

		policyi

	30	purch_inv_split_flowi
		purch_inv_tax_varni

		purchase_invoice_depositi
		purchase_invoice_depositi
		purchase_invoice_rowi
	35	purchase_invoice_taxi
		purchase_invoicei
		purchase_order_depositi

	38	tax_flowi


		The second part of the script will remove all records older than 3 months from the following tables:
		- event_log
		- grid_queue
		- aii_event_queue_response
		- contract_write_off
		- event_queue
		- workflow_task_todo

		- note
		  - contract_write_off
		  - note_attachment

		- bank_interface_run_temp
		- calculation_log
		- asset_balance

		- doc
		  - doc_list
		  - doc_link
		  - note
			 - contract_write_off
			 - note_attachment

		-- IGNORING TASK FOR THE TIME BEING.
		--	 This is because it has a cicular reference to itself 
		--  and so becomes quite hard to do a delete cleanly

		-- TASK
		--  - financing_filing_method
		--    - financing_statement
		--      - TASK

	
	*/

	DECLARE @DBName as VARCHAR(255) = DB_Name();
	-- Used for running custom exec scripts
	DECLARE @SQL AS VARCHAR(255);

	-- Set recovery mode to simple
	DECLARE @origRecoveryMode AS varchar(60);
	SELECT @origRecoveryMode = recovery_model_desc FROM sys.databases WHERE name = @DBName;

	PRINT 'RecoveryMode: ' + @origRecoveryMode

	IF ( @origRecoveryMode <> 'SIMPLE' )
	BEGIN
		SET @SQL = 'ALTER DATABASE ' + @DBName + ' SET RECOVERY SIMPLE'
		EXEC(@SQL)
	
		PRINT 'Recovery Mode set to SIMPLE'
	END

	exec sp_spaceused

	IF (@deleteFromImageTables = 1)
	BEGIN

		-- -----------------------------------------

		INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, task_id, stamp)
		VALUES ('Script Run', dbo.axsp_get_datetime(), 'Deleting contents of Image Tables', 0, 'Script' , 3 , ' ' , 0, 0);

		-- -----------------------------------------

		PRINT CONVERT(VARCHAR(50), GETDATE()) + ' - Deleting contents of Image Tables'
			
		-- --------------------------------------------------------------------
		-- --------------------------------------------------------------------
		--               STAGE 1 - CLEAR ALL DATA FROM TABLES
		-- --------------------------------------------------------------------
		-- --------------------------------------------------------------------

		-- Clear ALL data FROM core tables

		TRUNCATE TABLE audit_fld

		TRUNCATE TABLE audit_key

		-- Now Audit_fld and audit_key are empty, drop the FKs referencing audit_hdr so we can truncate
		ALTER TABLE audit_fld
		DROP CONSTRAINT fk_audit_fld_audit_hdr

		ALTER TABLE audit_key
		DROP CONSTRAINT fk_audit_key_audit_hdr

		TRUNCATE TABLE audit_hdr

		-- Re-add foreign keys
		ALTER TABLE audit_fld
		ADD CONSTRAINT fk_audit_fld_audit_hdr
		FOREIGN KEY (audit_hdr_id)
		REFERENCES audit_hdr(audit_hdr_id)

		ALTER TABLE audit_key
		ADD CONSTRAINT fk_audit_key_audit_hdr
		FOREIGN KEY (audit_hdr_id)
		REFERENCES audit_hdr(audit_hdr_id)

		TRUNCATE TABLE flow_audit

		TRUNCATE TABLE asset_cfvi
		TRUNCATE TABLE asset_custom_flowi
		TRUNCATE TABLE asset_hdr_cfvi
		TRUNCATE TABLE asset_hdri
		TRUNCATE TABLE asset_price_schedulei
		TRUNCATE TABLE asset_price_segmenti
		TRUNCATE TABLE asseti

		TRUNCATE TABLE bail_asset_ctlmt_plani
		TRUNCATE TABLE bail_asseti

		TRUNCATE TABLE contract_actgi
		TRUNCATE TABLE contract_cfvi
		TRUNCATE TABLE contract_deti
		TRUNCATE TABLE contract_custom_flowi
		TRUNCATE TABLE contract_grp_linki
		TRUNCATE TABLE contract_indexationi
		TRUNCATE TABLE contract_price_schedulei
		TRUNCATE TABLE contract_price_segmenti
		TRUNCATE TABLE contract_svtni
		TRUNCATE TABLE contracti

		TRUNCATE TABLE flow_calci
		TRUNCATE TABLE flowi

		TRUNCATE TABLE gl_journali

		TRUNCATE TABLE po_row_cfi
		TRUNCATE TABLE po_rowi

		TRUNCATE TABLE policyi

		TRUNCATE TABLE purch_inv_split_flowi
		TRUNCATE TABLE purch_inv_tax_varni

		TRUNCATE TABLE purchase_invoice_depositi
		TRUNCATE TABLE purchase_invoice_row_cfi
		TRUNCATE TABLE purchase_invoice_rowi
		TRUNCATE TABLE purchase_invoice_taxi
		TRUNCATE TABLE purchase_invoicei
		TRUNCATE TABLE purchase_order_depositi

		TRUNCATE TABLE tax_flowi

	END
	ELSE
	BEGIN
		PRINT 'Contents of Image Tables remain untouched.'
	END


	IF (@logDaysToKeep <> -1)
	BEGIN

		-- -----------------------------------------

		INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, task_id, stamp)
		VALUES ('Script Run', dbo.axsp_get_datetime(), 'Deleting contents of core tables older than ' + CONVERT(varchar(20), @logDaysToKeep) + ' days', 0, 'Script' , 3 , ' ' , 0, 0);

		-- -----------------------------------------

		-- --------------------------------------------------------------------
		-- --------------------------------------------------------------------
		--               STAGE 2 - CLEAR OLDER DATA FROM TABLES
		-- --------------------------------------------------------------------
		-- --------------------------------------------------------------------
				
		-- Clear all but the last X days of data from tables

		-- How many records should be deleted in a single SQL batch
		DECLARE @batchDeleteSize AS INT = 50000;

		-- How many months of history should be retained
		DECLARE @pastDate as DATETIME = DATEADD(day, -@logDaysToKeep, GETDATE());

		DECLARE @batchesRun AS INT = 0;
		DECLARE @toDeleteCount AS INT = 0;

		PRINT CONVERT(varchar(50), GETDATE()) + ' - Deleting contents of core tables older than ' + CONVERT(varchar(20), @logDaysToKeep) + ' days (' + CONVERT(varchar(50), @pastDate) + ')'

		-- =======================================
		-- Delete data from tables for EVENT_LOG
		-- (No dependencies)

		-- event_log
		-- DISABLE INDEX
		ALTER INDEX ix_event_log_task_id ON event_log DISABLE

		SET @batchesRun = 0;
		SELECT 1
		WHILE (@@ROWCOUNT > 0)
		BEGIN
			SET @batchesRun += 1
			PRINT 'event_log - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize)
			FROM event_log
			WHERE input_dt < @pastDate
		END

		-- RE-ENABLE INDEX
		ALTER INDEX ix_event_log_task_id ON event_log REBUILD


		-- =======================================
		-- Delete data from tables for GRID_QUEUE
		-- (No dependencies)

		-- DISABLE INDEX
		ALTER INDEX ix_grid_queue ON grid_queue DISABLE

		-- grid_queue
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'grid_queue - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize)
			FROM grid_queue 
			WHERE input_dt < @pastDate
		END

		-- RE-ENABLE INDEX
		ALTER INDEX ix_grid_queue ON grid_queue REBUILD


		-- =====================================================
		-- Delete data from tables for AII_EVENT_QUEUE_RESPONSE
		-- (No dependencies)

		-- aii_event_queue_response
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'aii_event_queue_response - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) aeqr 
			FROM aii_event_queue_response aeqr
				INNER JOIN aii_event_queue_history aeqh ON aeqr.aii_event_queue_history_id = aeqh.aii_event_queue_history_id
			WHERE 
				aeqh.end_dt < @pastDate
		END


		-- ===============================================
		-- Delete data from tables for CONTRACT_WRITE_OFF
		-- (No dependencies)

		-- DISABLE INDEXES
		ALTER INDEX ix_con_write_off_con_id ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_1 ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_currency ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_pcfh ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_user_id ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_wbr ON contract_write_off DISABLE
		ALTER INDEX ix_contract_write_off_wor ON contract_write_off DISABLE

		-- contract_write_off
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'contract_write_off - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) cwo
			FROM contract_write_off cwo
			WHERE 
				input_dt < @pastDate and
				calc_dt < @pastDate		
		END

		-- RE-ENABLE INDEX - others will be renabled at end of script
		ALTER INDEX ix_contract_write_off_1 ON contract_write_off REBUILD

		-- ========================================
		-- Delete data from tables for EVENT_QUEUE
		-- (No dependencies)

		-- DISABLE INDEX
		ALTER INDEX ix_event_queue1 ON event_queue DISABLE
		ALTER INDEX ix_event_queue2 ON event_queue DISABLE
		ALTER INDEX ix_event_queue3 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_4 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_5 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_6 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_7 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_8 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_9 ON event_queue DISABLE
		ALTER INDEX ix_event_queue_10 ON event_queue DISABLE

		ALTER INDEX ix_event_queue_et_contract ON event_queue DISABLE
		ALTER INDEX ix_event_queue_et_party ON event_queue DISABLE
		ALTER INDEX ix_event_queue_inertia_id ON event_queue DISABLE
		ALTER INDEX ix_event_queue_opportunity ON event_queue DISABLE
		ALTER INDEX ix_event_queue_purch_inv_id ON event_queue DISABLE
		ALTER INDEX ix_event_queue_user_id ON event_queue DISABLE

		-- event_queue
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'event_queue - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize)
			FROM event_queue
			WHERE 
				input_dt < @pastDate
		END

		ALTER INDEX ix_event_queue1 ON event_queue REBUILD
		ALTER INDEX ix_event_queue2 ON event_queue REBUILD
		ALTER INDEX ix_event_queue3 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_4 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_5 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_6 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_7 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_8 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_9 ON event_queue REBUILD
		ALTER INDEX ix_event_queue_10 ON event_queue REBUILD

		ALTER INDEX ix_event_queue_et_contract ON event_queue REBUILD
		ALTER INDEX ix_event_queue_et_party ON event_queue REBUILD
		ALTER INDEX ix_event_queue_inertia_id ON event_queue REBUILD
		ALTER INDEX ix_event_queue_opportunity ON event_queue REBUILD
		ALTER INDEX ix_event_queue_purch_inv_id ON event_queue REBUILD
		ALTER INDEX ix_event_queue_user_id ON event_queue REBUILD



		-- ===============================================
		-- Delete data from tables for WORKFLOW_TASK_TODO
		-- (No dependencies)

		ALTER INDEX ix_workflow_task_todo_0 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_1 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_2 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_3 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_4 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_5 ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_6 ON workflow_task_todo DISABLE

		ALTER INDEX ix_workflow_task_todo_doc_cat ON workflow_task_todo DISABLE
		ALTER INDEX ix_workflow_task_todo_wf_stat ON workflow_task_todo DISABLE

		-- workflow_task_todo
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'workflow_task_todo - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize)
			FROM workflow_task_todo
			WHERE
				completion_date < @pastDate
		END

		ALTER INDEX ix_workflow_task_todo_0 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_1 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_2 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_3 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_4 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_5 ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_6 ON workflow_task_todo REBUILD

		ALTER INDEX ix_workflow_task_todo_doc_cat ON workflow_task_todo REBUILD
		ALTER INDEX ix_workflow_task_todo_wf_stat ON workflow_task_todo REBUILD


		-- ==================================
		-- Delete data from tables for NOTE
		-- Depencies needing edits
		--  - contract_write_off
		--  - note_attachment

		-- contract_write_off
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'contract_write_off (note reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) cwo
			FROM contract_write_off cwo
				INNER JOIN note n ON cwo.note_id = n.note_id
			WHERE 
				n.activity_dt < @pastDate and
				n.last_updated_dt < @pastDate
		END


		-- note_attachment
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'note_attachment (note reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) na
			FROM note_attachment na
				INNER JOIN note n ON na.note_id = n.note_id
			WHERE 
				n.activity_dt < @pastDate and
				n.last_updated_dt < @pastDate
		END


		ALTER INDEX ix_note ON note DISABLE
		ALTER INDEX ix_note1 ON note DISABLE
		ALTER INDEX ix_note2 ON note DISABLE
		ALTER INDEX ix_note3 ON note DISABLE
		ALTER INDEX ix_note4 ON note DISABLE
		ALTER INDEX ix_note5 ON note DISABLE
		ALTER INDEX ix_note7 ON note DISABLE
		ALTER INDEX ix_note8 ON note DISABLE
		ALTER INDEX ix_note9 ON note DISABLE
		ALTER INDEX ix_note10 ON note DISABLE
		ALTER INDEX ix_note11 ON note DISABLE
		ALTER INDEX ix_note12 ON note DISABLE
		ALTER INDEX ix_note13 ON note DISABLE

		ALTER INDEX ix_note_bank_stmt_id ON note DISABLE
		ALTER INDEX ix_note_currency_id ON note DISABLE
		ALTER INDEX ix_note_custom_action_id ON note DISABLE
		ALTER INDEX ix_note_doc_id ON note DISABLE
		ALTER INDEX ix_note_flow_id ON note DISABLE
		ALTER INDEX ix_note_hidden_link ON note DISABLE
		ALTER INDEX ix_note_inertia_id ON note DISABLE
		ALTER INDEX ix_note_last_updated_by_id ON note DISABLE
		ALTER INDEX ix_note_opportunity_id ON note DISABLE
		ALTER INDEX ix_note_reserve_id ON note DISABLE
		ALTER INDEX ix_note_user_id ON note DISABLE

		-- note
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'note - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) n
			FROM note n
			WHERE 
				n.activity_dt < @pastDate and
				n.last_updated_dt < @pastDate		
		END

		ALTER INDEX ix_note_doc_id ON note REBUILD



		-- ===================================================
		-- Delete data from tables for BANK_INTERFACE_RUN_TEMP
		-- (No dependencies)

		ALTER INDEX ix_bank_interface_run_temp_bir ON bank_interface_run_temp DISABLE

		-- bank_interface_run_temp
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'bank_interface_run_temp - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) birt 
			FROM bank_interface_run_temp birt
				INNER JOIN bank_interface_run bir ON birt.bank_interface_run_id = bir.bank_interface_run_id
			WHERE 
				bir.run_dt < @pastDate
		END

		ALTER INDEX ix_bank_interface_run_temp_bir ON bank_interface_run_temp REBUILD

		-- ===========================================
		-- Delete data from tables for CALCULATION_LOG
		-- (No dependencies)

		ALTER INDEX ix_calc_log_cnt_cust_flow ON calculation_log DISABLE
		ALTER INDEX ix_calc_log_term_quote ON calculation_log DISABLE
		ALTER INDEX ix_calculation_log_cfh ON calculation_log DISABLE
		ALTER INDEX ix_calculation_log_contract_id ON calculation_log DISABLE
		ALTER INDEX ix_calculation_log_flow_id ON calculation_log DISABLE

		-- calculation_log
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'calculation_log - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) 
			FROM calculation_log
			WHERE calc_end_dt < @pastDate
		END

		ALTER INDEX ix_calc_log_cnt_cust_flow ON calculation_log REBUILD
		ALTER INDEX ix_calc_log_term_quote ON calculation_log REBUILD
		ALTER INDEX ix_calculation_log_cfh ON calculation_log REBUILD
		ALTER INDEX ix_calculation_log_contract_id ON calculation_log REBUILD
		ALTER INDEX ix_calculation_log_flow_id ON calculation_log REBUILD


		-- =========================================
		-- Delete data from tables for ASSET_BALANCE
		-- (No dependencies)

		ALTER INDEX ix_asset_balance_asset_id ON asset_balance DISABLE
		ALTER INDEX ix_asset_balance_contract_id ON asset_balance DISABLE

		-- asset_balance
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'asset_balance - ' + CONVERT(VARCHAR(10), @batchesRun)
			DELETE TOP (@batchDeleteSize) 
			FROM asset_balance
			WHERE 
				input_dt < @pastDate and
				effective_dt < @pastDate
		END

		ALTER INDEX ix_asset_balance_asset_id ON asset_balance REBUILD
		ALTER INDEX ix_asset_balance_contract_id ON asset_balance REBUILD


		-- =========================================
		-- Delete data from tables for DOC
		-- Depencies needing edits
		-- - doc_list
		-- - doc_link
		-- - note
		--   - contract_write_off
		--   - note_attachment

		ALTER INDEX ix_doc_list_output_party_id ON doc_list DISABLE

		-- doc_list
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'doc_list (doc reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) dl
			FROM doc_list dl
				INNER JOIN doc d ON dl.doc_id = d.doc_id
			WHERE 
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
		END

		ALTER INDEX ix_doc_list_output_party_id ON doc_list REBUILD


		ALTER INDEX ix_doc_link_asset_hdr ON doc_link DISABLE
		ALTER INDEX ix_doc_link_contract ON doc_link DISABLE
		ALTER INDEX ix_doc_link_credit_line ON doc_link DISABLE
		ALTER INDEX ix_doc_link_doc_metadata ON doc_link DISABLE
		ALTER INDEX ix_doc_link_gl_journal ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_asset_hdr ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_contract ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_credit_line ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_gl_journal ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_opportunity ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_party ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_portfolio ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_program ON doc_link DISABLE
		ALTER INDEX ix_doc_link_meta_reserve ON doc_link DISABLE
		ALTER INDEX ix_doc_link_opportunity ON doc_link DISABLE
		ALTER INDEX ix_doc_link_party ON doc_link DISABLE
		ALTER INDEX ix_doc_link_portfolio ON doc_link DISABLE
		ALTER INDEX ix_doc_link_program ON doc_link DISABLE
		ALTER INDEX ix_doc_link_reserve ON doc_link DISABLE

		-- doc_link
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'doc_link (doc reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) dl
			FROM doc_link dl
				INNER JOIN doc d ON dl.doc_id = d.doc_id
			WHERE
				dl.doc_link_id <> 0 and
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
		END

		ALTER INDEX ix_doc_link_asset_hdr ON doc_link REBUILD
		ALTER INDEX ix_doc_link_contract ON doc_link REBUILD
		ALTER INDEX ix_doc_link_credit_line ON doc_link REBUILD
		ALTER INDEX ix_doc_link_doc_metadata ON doc_link REBUILD
		ALTER INDEX ix_doc_link_gl_journal ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_asset_hdr ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_contract ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_credit_line ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_gl_journal ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_opportunity ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_party ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_portfolio ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_program ON doc_link REBUILD
		ALTER INDEX ix_doc_link_meta_reserve ON doc_link REBUILD
		ALTER INDEX ix_doc_link_opportunity ON doc_link REBUILD
		ALTER INDEX ix_doc_link_party ON doc_link REBUILD
		ALTER INDEX ix_doc_link_portfolio ON doc_link REBUILD
		ALTER INDEX ix_doc_link_program ON doc_link REBUILD
		ALTER INDEX ix_doc_link_reserve ON doc_link REBUILD

		-- ---------------
		-- NOTE

		-- contract_write_off
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'contract_write_off (doc reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) cwo
			FROM contract_write_off cwo
				INNER JOIN note n ON cwo.note_id = n.note_id
				INNER JOIN doc d ON n.doc_id = d.doc_id
			WHERE 
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
		END

		ALTER INDEX ix_con_write_off_con_id ON contract_write_off REBUILD
		ALTER INDEX ix_contract_write_off_currency ON contract_write_off REBUILD
		ALTER INDEX ix_contract_write_off_pcfh ON contract_write_off REBUILD
		ALTER INDEX ix_contract_write_off_user_id ON contract_write_off REBUILD
		ALTER INDEX ix_contract_write_off_wbr ON contract_write_off REBUILD
		ALTER INDEX ix_contract_write_off_wor ON contract_write_off REBUILD


		-- note_attachment
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'note_attachment (doc reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) na
			FROM note_attachment na
				INNER JOIN note n ON na.note_id = n.note_id		
				INNER JOIN doc d ON n.doc_id = d.doc_id
			WHERE 
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
		END


		-- note
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'note (doc reference) - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize) n
			FROM note n
				INNER JOIN doc d ON n.doc_id = d.doc_id
			WHERE 
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
		END

		ALTER INDEX ix_note ON note REBUILD
		ALTER INDEX ix_note1 ON note REBUILD
		ALTER INDEX ix_note2 ON note REBUILD
		ALTER INDEX ix_note3 ON note REBUILD
		ALTER INDEX ix_note4 ON note REBUILD
		ALTER INDEX ix_note5 ON note REBUILD
		ALTER INDEX ix_note7 ON note REBUILD
		ALTER INDEX ix_note8 ON note REBUILD
		ALTER INDEX ix_note9 ON note REBUILD
		ALTER INDEX ix_note10 ON note REBUILD
		ALTER INDEX ix_note11 ON note REBUILD
		ALTER INDEX ix_note12 ON note REBUILD
		ALTER INDEX ix_note13 ON note REBUILD

		ALTER INDEX ix_note_bank_stmt_id ON note REBUILD
		ALTER INDEX ix_note_currency_id ON note REBUILD
		ALTER INDEX ix_note_custom_action_id ON note REBUILD
		ALTER INDEX ix_note_flow_id ON note REBUILD
		ALTER INDEX ix_note_hidden_link ON note REBUILD
		ALTER INDEX ix_note_inertia_id ON note REBUILD
		ALTER INDEX ix_note_last_updated_by_id ON note REBUILD
		ALTER INDEX ix_note_opportunity_id ON note REBUILD
		ALTER INDEX ix_note_reserve_id ON note REBUILD
		ALTER INDEX ix_note_user_id ON note REBUILD

		-- ---------------
		-- DOC

		ALTER INDEX ix_doc_1 ON doc DISABLE
		ALTER INDEX ix_doc_2 ON doc DISABLE
		ALTER INDEX ix_doc_3 ON doc DISABLE
		ALTER INDEX ix_doc_bankbatch ON doc DISABLE
		ALTER INDEX ix_doc_contract_id ON doc DISABLE
		ALTER INDEX ix_doc_created_by_user_id ON doc DISABLE
		ALTER INDEX ix_doc_doc_output_method_id ON doc DISABLE
		ALTER INDEX ix_doc_doc_rule_id ON doc DISABLE
		ALTER INDEX ix_doc_facility_id ON doc DISABLE
		ALTER INDEX ix_doc_invoice_id ON doc DISABLE
		ALTER INDEX ix_doc_opportunity ON doc DISABLE
		ALTER INDEX ix_doc_output_by_user_id ON doc DISABLE
		ALTER INDEX ix_doc_portfolio_id ON doc DISABLE
		ALTER INDEX ix_doc_rejected_by_user_id ON doc DISABLE
		ALTER INDEX ix_doc_released_by_user_id ON doc DISABLE
		ALTER INDEX ix_doc_statement_id ON doc DISABLE
		ALTER INDEX ix_doc_template_fmt_id ON doc DISABLE

		-- doc
		SET @batchesRun = 0;
		SELECT 1
		WHILE @@ROWCOUNT > 0
		BEGIN
			SET @batchesRun += 1
			PRINT 'doc - ' + CONVERT(VARCHAR(10), @batchesRun)

			DELETE TOP (@batchDeleteSize)d
			FROM doc d
			WHERE
				d.doc_id <> 0 and
				d.effect_dt < @pastDate and
				d.created_dt < @pastDate and
				d.released_dt < @pastDate and
				d.output_dt < @pastDate and
				d.rejected_dt < @pastDate
				
		END

		ALTER INDEX ix_doc_1 ON doc REBUILD
		ALTER INDEX ix_doc_2 ON doc REBUILD
		ALTER INDEX ix_doc_3 ON doc REBUILD
		ALTER INDEX ix_doc_bankbatch ON doc REBUILD
		ALTER INDEX ix_doc_contract_id ON doc REBUILD
		ALTER INDEX ix_doc_created_by_user_id ON doc REBUILD
		ALTER INDEX ix_doc_doc_output_method_id ON doc REBUILD
		ALTER INDEX ix_doc_doc_rule_id ON doc REBUILD
		ALTER INDEX ix_doc_facility_id ON doc REBUILD
		ALTER INDEX ix_doc_invoice_id ON doc REBUILD
		ALTER INDEX ix_doc_opportunity ON doc REBUILD
		ALTER INDEX ix_doc_output_by_user_id ON doc REBUILD
		ALTER INDEX ix_doc_portfolio_id ON doc REBUILD
		ALTER INDEX ix_doc_rejected_by_user_id ON doc REBUILD
		ALTER INDEX ix_doc_released_by_user_id ON doc REBUILD
		ALTER INDEX ix_doc_statement_id ON doc REBUILD
		ALTER INDEX ix_doc_template_fmt_id ON doc REBUILD --17

	END
	ELSE
	BEGIN
		PRINT 'Contents of Core Tables remain untouched.'
	END

	exec sp_spaceused
	
	-- --------------------------------------------------------------------
	-- --------------------------------------------------------------------
	--               STAGE 3 - SHRINK DATABASE
	-- --------------------------------------------------------------------
	-- --------------------------------------------------------------------
	
	IF (@shrinkDB = 1)
	BEGIN	
		PRINT 'Beginning Database shrink: ' + CONVERT(varchar(50), GETDATE())
		
		DBCC SHRINKDATABASE (@DBName)	

		PRINT 'Database shrink completed: ' + CONVERT(varchar(50), GETDATE())
	END
	ELSE
	BEGIN
		PRINT 'Database has not been shrunk'
	END

	exec sp_spaceused
	
	-- ===========================================================
	-- ===========================================================
	-- ===========================================================
	
	-- RESET RECOVERY MODE
	IF ( @origRecoveryMode <> 'SIMPLE' )
	BEGIN
		SET @SQL = 'ALTER DATABASE ' + @DBName + ' SET RECOVERY' + @origRecoveryMode
		EXEC(@SQL)
		PRINT 'Recovery Mode reverted to ' + @origRecoveryMode
	END


	PRINT 'Finished executing axsp_db_size_reduction: ' + CONVERT(varchar(50), GETDATE())
	-- -----------------------------------------

	INSERT INTO event_log(event_type, input_dt, description, process_category, src_app_name, src_user_id, src_machine_name, task_id, stamp)
	VALUES ('Script Run', dbo.axsp_get_datetime(), 'Finished executing axsp_db_size_reduction', 0, 'Script' , 3 , ' ' , 0, 0);

	-- -----------------------------------------

END



-- Param 1 - Delete data from image tables: This is a 1 or 0. 1 is yes, 0 is no
-- Param 2 - How many days of data to keep in core tables: Eg 30 will keep 30 days of data. -1 (negative 1) keeps everything
-- Param 3 - Force shrink - this takes up to 4 or 5 hours, but makes the DB size smaller. I'd only use it if you're not able to backup andrestore the DB manually (its faster and does more). It is also a 1 or 0; 1 for yes, 0 for no

-- So I'd run something like this
Exec axsp_db_size_reduction 1, 30, 0
-- and if it still is too big, run exec... 0, -1, 1