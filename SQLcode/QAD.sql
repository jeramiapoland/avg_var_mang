SELECT	E.DSIndexCode
	,	E.IndexDesc
	,	D.Region
	,	D.DSGeoCode
--	,	E.RegCodeTypeID
--	,	E.BaseDate
	,	E.ISOCurrCode
	,	E.IsLocalCurrency
--	,	I.VALUEDATE
--	,	I.PI_
--	,	I.RI
--	,	C.IndexListIntCode
--	,	C.ConstIntCode
--	,	MONTH(C.Date_) as MONTH_
--	,	YEAR(C.Date_) as YEAR_
--	,	C.Shares
--	,	C.MktVal as MCAP
FROM DS2Region D
	JOIN DS2EquityIndex E
		ON E.Region = D.Region
--	JOIN DS2IndexData I
--		ON I.DSIndexCode = E.DSIndexCode
--	JOIN DS2IndexDataType T
--		ON T.DSIndexCode = E.DSIndexCode
--		AND T.EntityTypeId = '3'
--	JOIN Ds2IndexList L
--		ON L.IndexListCode = E.IndexListCode
--	JOIN Ds2ConstDataMth C
--		ON E.InfoCode = C.InfoCode
WHERE D.Region in ('AU','CA','DE','FR','IN','JP','GB')
	AND E.DSIndexCode in ('299','35252','13095','36888','37788','38502','48553')