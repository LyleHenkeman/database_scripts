<Query Kind="SQL" />

--Convert to local time
SELECT top 10 dbo.fnConvertToLocalTime(ValidFrom,ValidFromZone) AS localAUSTZSTamp,
	SecondaryId, RouteId, DepotId,
	Created,
	CreatedZone,
	ValidFrom,
	ValidFromZone FROM tblevent WHERE created > Getutcdate() + 2 ORDER BY validfrom DESC