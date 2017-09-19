<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--ORACLE SEQUENCES

/*Error

"Ax.Frameworks.SysUtils.AxException: An error has occurred when upgrading the database. Please check the event log for further information. ---> Ax.Frameworks.SysUtils.AxException: An error has occurred when upgrading the database. Please check the event log for further information. ---> Ax.Frameworks.SysUtils.AxDuplicateSQLException: Duplicate entries are not supported here.
Error: ORA-00001: unique constraint (HSBCFR_50280_LYLE.PK_AUDIT_HDR) violated
ORA-06512: at "HSBCFR_50280_LYLE.AXSP_AUDIT_HDR_INS", line 12
ORA-06512: at line 1
   at Ax.Frameworks.BOF.Data.BOFDbHelper.CheckAndLogSqlErrors(SystemException e, String sqlCommandText)
   at Ax.Frameworks.BOF.Data.BOFDbHelper.ExecuteScalarSp(String spName, Object[] prmValues)
   at Ax.Frameworks.BOF.Audit.BOFDbAudit.Save(IBOFDbBase db, VOAuditHdr auditHdr, IEnumerable`1 auditKeys, VOBaseCollection auditFlds)
   at Ax.Frameworks.BOF.Audit.BOFDbAudit.AuditInsert(IBOFDbBase db, BOFDbFldInfos dbFldInfos, VOBase vo, VOAuditImagePrms prms)
   at Ax.Frameworks.BOF.Audit.BOFDbAuditHelper.AuditInsert(IBOFDbBase db, BOFDbFldInfos dbFldInfos, VOBase vo, VOAuditImagePrms prms)
   at Ax.Frameworks.BOF.BOFDbBase.InsertInternal(VOBase vo, Boolean getId, Boolean keyOverride, Boolean validate, Boolean audit, Boolean doCustomFields)
   at Ax.Frameworks.BOF.BOFDbBase.Insert(VOBase vo, Boolean getId, Boolean keyOverride, Boolean validate, Boolean audit)
   at Ax.Frameworks.BOF.BOFDbBase.Insert(VOBase vo, Boolean getId, Boolean keyOverride, Boolean validate)
   at Ax.BusinessRules.Upgrades.Upgrades.VOUpgrades.UpgSeedFinancingStatementWorkflow.Execute()
   at Ax.BusinessRules.Upgrades.BRUpgradeVOv2.ExecuteUpgrade(Int32 stepNumber, IDatabaseUpgrade upgrade)
   at Ax.BusinessRules.Upgrades.BRUpgradeVOv2.Upgrade()
   at Ax.BusinessRules.Upgrades.BRUpgrades.UpgradeDb(UpgradeType upgradeType, UpgradeProgressHandler d, String dbUpgradeFileName)
   at Ax.BusinessRules.Upgrades.BRUpgrades.UpgradeDb(UpgradeProgressHandler d)
   at Ax.BusinessRules.Upgrades.BRUpgrades.UpgradeDb(UpgradeProgressHandler d)
   at Ax.BusinessRules.Upgrades.BRUpgradeSingleton.UpgradeThread(Object state)
   at Ax.BusinessRules.Upgrades.BRUpgradeSingleton.UpgradeDbFeedback(Int32& stepMode, Int32& percent, String& description)
   at Ax.BusinessRules.RemoteServices.RemoteSvcs.UpgradeDbFeedback(Int32& stepMode, Int32& percent, String& description)
   at Ax.BusinessRules.RemoteServices.RemoteSvcs.UpgradeDbFeedback(Int32& stepMode, Int32& percent, String& description)
   at Ax.BusinessFacade.BFUpgrades.UpgradeDb(UpgradeProgressHandler d, String dbUpgradeFileName)
   at Ax.UI.AxiomWin.AxiomMain.CheckDbVersion(Boolean silentUpgrade)
   at Ax.UI.AxiomWin.AxiomMain.Main(String[] args) Ax.Frameworks.SysUtils.AxException: An error has occurred when upgrading the database. Please check the event log for further information. ---> Ax.Frameworks.SysUtils.AxException: An error has occurred when upgrading the database. Please check the event log for further information. ---> Ax.Frameworks.SysUtils.AxDuplicateSQLException: Duplicate entries are not supported here.
Error: ORA-00001: unique constraint (HSBCFR_50280_LYLE.PK_AUDIT_HDR) violated
ORA-06512: at "HSBCFR_50280_LYLE.AXSP_AUDIT_HDR_INS", line 12
ORA-06512: at line 1 */

-- 1. check maximum number
select max(audit_hdr_id) from audit_hdr

-- 2. drop sequence table
drop SEQUENCE s_audit_hdr

-- 3. create again
CREATE SEQUENCE s_audit_hdr
START WITH 7000390 --update to max number returned in 1
INCREMENT BY 1;

-- 4. view
select *
from audit_hdr order by audit_hdr_id desc

select * from all_sequences where sequence_name = 'S_ASSET_HDR'


-- 5. Truncate and Drop tables as last case scenario
truncate table audit_key
truncate table audit_fld
delete audit_hdr

-- 6. Stored Procedure responsible
select * from all_sequences where sequence_name = 'S_ASSET_HDR'