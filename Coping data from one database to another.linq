<Query Kind="SQL" />

--Coping data from one database to another
--As long at they are on the same SQL instance you can run SQl like below to do this:


INSERT INTO iEnable.dbo.tblHostUser
	(strUsername, strPassword, lCompanyId, lPersonId, bActive)
SELECT DISTINCT
	strPersonID, strPersonID, 4, lPersonID, bActive
FROM ServiceConnect_TycoXfireII_SUP.dbo.tblPerson
As an aside, the above code will copy all the Service Connect users into iEnable, and set there company to 4.