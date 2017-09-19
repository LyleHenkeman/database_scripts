<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--VERTEX (oracle)
--find invoice_no
select invno, transuserarea, TRANSEXTDAMT, statetax, cntytax, citytax
from regprereturnstbl
where invno = ('500265583','500265584','500265588','500265614','500265615','500265625','500265626','500265634','500265637','500265639','500155724')


select *
from TRANSEXTDAMT

--SQL

select *
from flow
where invoice_id = 0 --if blank invoice_id, it can be picked up OSG invoicing, is dt correct


--Find flows belonging to invoice / contract
select i.invoice_no, f.flow_id, tf.* 
from flow f 
left join tax_flow tf on tf.flow_id = f.flow_id 
join invoice i on f.invoice_id = i.invoice_id 
join contract c on f.contract_id = c.contract_id 
--Where c.contract_id = 3 
where i.invoice_no = '500477161'

-- same as above, less data
select i.invoice_no, f.invoice_id, f.* 
from flow f 
join invoice i on f.invoice_id = i.invoice_id 
where contract_id = 500477161


--Check if flows are migrated flows (starting in 1)
select distinct f.flow_id, f.contract_id, f.calc_dt, f.invoice_id, i.invoice_no from flow f inner join invoice i on f.invoice_id = i.invoice_id where flow_id in (
select flow_link_id from flow where invoice_id in (select invoice_id
from invoice
where invoice_no in ('500265583','500265584','500265588','500265614','500265615','500265625','500265626','500265634','500265637','500265639')
))
order by 3