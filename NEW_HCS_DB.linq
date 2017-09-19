<Query Kind="SQL">
  <Output>DataGrids</Output>
</Query>

-- Update SMTP to nothing

update doc_output_method set smtp_server = '1.1.1.1'

-- Discble all processes (doc gen and all processes)

update doc_rule set is_active = 0 

update scheduler_process set status = 24304


/****** Object:  Trigger [ts_insert_party_id_exposure]    Script Date: 29/01/2014 2:57:25 p.m. ******/ 
DROP TRIGGER [dbo].[ts_insert_party_id_exposure] 
GO   

USE [DATABASE_NAME] 
GO 


exec [dbo].[axsp_party_depersonalisation] N'230369344' -- saves the depersonalisation for restore



/****** Object:  Trigger [dbo].[ts_insert_party_id_exposure]    Script Date: 29/01/2014 2:14:23 p.m. ******/ 
SET ANSI_NULLS ON 
GO 
SET QUOTED_IDENTIFIER ON 
GO 
Create TRIGGER [dbo].[ts_insert_party_id_exposure]              
on [dbo].[party]                
FOR UPDATE              
AS              
BEGIN           
        BEGIN   
        declare @party_cfv_id int;      
        set @party_cfv_id = 0;  
        select @party_cfv_id = party_cfv_id from party_cfv where party_id = (select party_id from inserted) and party_cfd_id = 74; -- Party CFD Id is te id of the Read Only Passport No custom Field added.    

                if @party_cfv_id = 0 
                insert into dbo.party_cfv ( field_value,party_cfd_id,party_id,seq_no,stamp) 
                select party_id, 74, party_id, 0, 0 from inserted; -- Party CFD Id is the id of the Read Only Passport Number custom Field added. Find using select * from party_cfd   -- Party CFD Id is te id of the Read Only Passport No custom Field added.

        end     
END             
GO
