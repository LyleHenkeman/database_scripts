<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- Set date for after posting period
Declare
@after_post_dt date;
SET
@after_post_dt = '01-Mar-2013';

-- ### Case 39025 - flow_id not on flow or flowi tables


--List all gl-entry recs THIS MONTH with missing flow info
select
gl.flow_id, gl.contract_id, gl.post_dt, gl.input_dt, ccy.code, coa.name,coa.code,gl.amount, gl.base_amount, bccy.code base_ccy,
gl.fx_rate, gl.fx_rate_dt,gl.flow_id, lu3.value FlowType, gl.narration, gl.asset_hdr_id, lu1.value Type, lu2.value ReversalStatus,
ldg.name, gl.gl_tag_id, gt.ext_name, gt.account_class
From
gl_entry gl, currency ccy, gl_account coa, lookupset lu1, lookupset lu2, lookupset lu3, ledger ldg, gl_tag gt, currency bccy
Where
gl.currency_id = ccy.currency_id
and gl.gl_account_id = coa.gl_account_id
and gl.gl_entry_type = lu1.lookupset_id
and gl.reversal_status = lu2.lookupset_id
and gl.flow_type = lu3.lookupset_id
and gl.ledger_id = ldg.ledger_id
and gl.gl_tag_id = gt.gl_tag_id
and gl.base_currency_id = bccy.currency_id
-- and gl.contract_id in (list)
and flow_id not in (select flow_id from flow union all select flow_id from flowi)
and gl_interface_id = 0
and post_dt < @after_post_dt
Order
by gl.contract_id, gl.post_dt;