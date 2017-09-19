<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- GL ENTRIES
select gl.contract_id, gl.gl_interface_id, gl.gl_entry_id, gl.post_dt, gl.input_dt, ccy.code, coa.name,coa.code,gl.amount, 
gl.flow_id, gl.narration, gl.asset_hdr_id, lu1.value Type, lu2.value ReversalStatus, 
ldg.name, gl.gl_tag_id, gt.ext_name, gt.account_class
From gl_entry gl, currency ccy, gl_account coa, lookupset lu1, lookupset lu2, ledger ldg, gl_tag gt, currency bccy
Where gl.currency_id = ccy.currency_id
  and gl.gl_account_id = coa.gl_account_id
  and gl.gl_entry_type = lu1.lookupset_id
  and gl.reversal_status = lu2.lookupset_id
  and gl.ledger_id = ldg.ledger_id
  and gl.gl_tag_id = gt.gl_tag_id
  and gl.base_currency_id = bccy.currency_id
  and gl.contract_id in (1)
Order by gl.contract_id, gl.post_dt
