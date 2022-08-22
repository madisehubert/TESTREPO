--VIEW train TABLE
SELECT * FROM [dbo].[train]

--VIEW test TABLE
SELECT * FROM [dbo].[test]

--VIEW SalePriceComp TABLE
SELECT * FROM [dbo].[SalePriceComp]

--VIEW  TABLE
SELECT * FROM [dbo].[testdatacomp]

--CREATE VIEW WITH RELEVANT COLUMNS
CREATE VIEW test_rel AS
SELECT Id,   MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt, ExterQual, BsmtQual, BsmtCond, BsmtExposure,
		BsmtFinType1, _1stFlrSF, GrLivArea, KitchenQual, FireplaceQu, GarageFinish, YrSold,SalePrice 
FROM testdatacomp

--VIEW  TABLE
SELECT * FROM test_rel

--CREATE VIEW WITH RELEVANT COLUMNS
CREATE VIEW train_rel AS
SELECT Id,   MSSubClass, LotArea, OverallQual, OverallCond, YearBuilt, ExterQual, BsmtQual, BsmtCond, BsmtExposure,
		BsmtFinType1, _1stFlrSF, GrLivArea, KitchenQual, FireplaceQu, GarageFinish, YrSold,SalePrice 
FROM train

--VIEW  TABLE
SELECT * FROM test_rel

