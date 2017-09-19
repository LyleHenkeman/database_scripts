<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

--link write-offs to contracts

select * from party where party_id in(
select p.party_id
from party p join contract c on c.cparty_id = p.party_id
where c.contract_id in (select contract_id as flow_id from flow f where flow_type = 1034 and reversal_status = 4200 group by contract_id, flow_link_id, amount having COUNT(contract_id) > 1)
group by p.party_id)




select p.name, p.party_no, sum(f.amount) as total_amount from flow f join party_account pa on pa.party_account_id = f.party_account_id
join party p on pa.party_id = p.party_id where f.flow_type = 1034 and f.reversal_status = 4200
group by p.party_no, p.name having COUNT(f.contract_id) > 1


select contract_id, flow_link_id, amount as flow_id from flow f where flow_type = 1034 and reversal_status = 4200 group by contract_id, flow_link_id, amount having COUNT(contract_id) > 1


select *
from party
where party_no = 197452



select *
from [dbo].[party_overdue_stats]
where party_id = 97492