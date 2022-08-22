install.packages(c("odbc","dplyr"))
install.packages("DBI")
library(DBI)
library(odbc)
library(dplyr)
library(corrplot)
library(ggplot2) 
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "DESKTOP-FD4BIU7",
                 Database = "HomePrediction",
                 Port = 1433) 

dbListTables(con) #Tables in DB
dbListFields(con, "train") #List the column names in train table
dbListFields(con, "test") #List the column names in test table

?collect

#load the train dataset
traindata <- tbl(con, "train") #create table train from the connection (con) to the data source
traindata <- collect(traindata) #pull the data
View(traindata)

#Make a copy of traindata to maintain data integrity
traindata_cp <- data.frame(traindata)
View(traindata_cp)
View(traindata_cp[1169:1460, ])

#load the test dataset
testdata <- tbl(con, "test") #create table test from the connection (con) to the data source
testdata <- collect(testdata)#pull the data
View(testdata)

#Make a copy of testdata to maintain data integrity
testdata_cp <- data.frame(testdata)
View(testdata_cp)

#missing data
missing_row <- traindata_cp[!complete.cases(traindata_cp),]
head(missing_row) #No missing data


cor(traindata_cp) #doesnt work because the traindata has categorical vairables
#collect only numeric columns

View(traindata_cp)

#replace NA with 0 on the LotFrontage column
traindata_cp["LotFrontage"][traindata_cp["LotFrontage"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$LotFrontage <- as.numeric(traindata_cp$LotFrontage)

#Replace all categorical variables with numerical variables in Utilities column
traindata_cp["Utilities"][traindata_cp["Utilities"] == "AllPub"] <- 4
traindata_cp["Utilities"][traindata_cp["Utilities"] == "NoSewr"] <- 3
traindata_cp["Utilities"][traindata_cp["Utilities"] == "NoSeWa"] <- 2
traindata_cp["Utilities"][traindata_cp["Utilities"] == "ELO"] <- 1
#Convert to numeric datatype
traindata_cp$Utilities <- as.numeric(traindata_cp$Utilities)

#Replace all categorical variables with numerical variables in CentralAir column
traindata_cp["CentralAir"][traindata_cp["CentralAir"] == "Y"] <- 1
traindata_cp["CentralAir"][traindata_cp["CentralAir"] == "N"] <- 0
#Convert to numeric datatype
traindata_cp$CentralAir <- as.numeric(traindata_cp$CentralAir)

#Replace all categorical variables with numerical variables in ExterQual column
traindata_cp["ExterQual"][traindata_cp["ExterQual"] == "Ex"] <- 5
traindata_cp["ExterQual"][traindata_cp["ExterQual"] == "Gd"] <- 4
traindata_cp["ExterQual"][traindata_cp["ExterQual"] == "TA"] <- 3
traindata_cp["ExterQual"][traindata_cp["ExterQual"] == "Fa"] <- 2
traindata_cp["ExterQual"][traindata_cp["ExterQual"] == "Po"] <- 1
#Convert to numeric datatype
traindata_cp$ExterQual <- as.numeric(traindata_cp$ExterQual)

#Replace all categorical variables with numerical variables in ExterCond column
traindata_cp["ExterCond"][traindata_cp["ExterCond"] == "Ex"] <- 5
traindata_cp["ExterCond"][traindata_cp["ExterCond"] == "Gd"] <- 4
traindata_cp["ExterCond"][traindata_cp["ExterCond"] == "TA"] <- 3
traindata_cp["ExterCond"][traindata_cp["ExterCond"] == "Fa"] <- 2
traindata_cp["ExterCond"][traindata_cp["ExterCond"] == "Po"] <- 1
#Convert to numeric datatype
traindata_cp$ExterCond <- as.numeric(traindata_cp$ExterCond)

#Replace all categorical variables with numerical variables in BsmtQual column
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "Ex"] <- 5
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "Gd"] <- 4
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "TA"] <- 3
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "Fa"] <- 2
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "Po"] <- 1
traindata_cp["BsmtQual"][traindata_cp["BsmtQual"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$BsmtQual <- as.numeric(traindata_cp$BsmtQual)

#Replace all categorical variables with numerical variables in BsmtCond column
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "Ex"] <- 5
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "Gd"] <- 4
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "TA"] <- 3
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "Fa"] <- 2
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "Po"] <- 1
traindata_cp["BsmtCond"][traindata_cp["BsmtCond"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$BsmtCond <- as.numeric(traindata_cp$BsmtCond)

#Replace all categorical variables with numerical variables in BsmtExposure column
traindata_cp["BsmtExposure"][traindata_cp["BsmtExposure"] == "Gd"] <- 4
traindata_cp["BsmtExposure"][traindata_cp["BsmtExposure"] == "Av"] <- 3
traindata_cp["BsmtExposure"][traindata_cp["BsmtExposure"] == "Mn"] <- 2
traindata_cp["BsmtExposure"][traindata_cp["BsmtExposure"] == "No"] <- 1
traindata_cp["BsmtExposure"][traindata_cp["BsmtExposure"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$BsmtExposure <- as.numeric(traindata_cp$BsmtExposure)

#Replace all categorical variables with numerical variables in BsmtFinType1 column
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "GLQ"] <- 6
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "ALQ"] <- 5
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "BLQ"] <- 4
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "Rec"] <- 3
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "LwQ"] <- 2
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "Unf"] <- 1
traindata_cp["BsmtFinType1"][traindata_cp["BsmtFinType1"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$BsmtFinType1 <- as.numeric(traindata_cp$BsmtFinType1)

#Replace all categorical variables with numerical variables in BsmtFinType2 column
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "GLQ"] <- 6
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "ALQ"] <- 5
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "BLQ"] <- 4
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "Rec"] <- 3
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "LwQ"] <- 2
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "Unf"] <- 1
traindata_cp["BsmtFinType2"][traindata_cp["BsmtFinType2"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$BsmtFinType2 <- as.numeric(traindata_cp$BsmtFinType2)

#Replace all categorical variables with numerical variables in HeatingQC column
traindata_cp["HeatingQC"][traindata_cp["HeatingQC"] == "Ex"] <- 5
traindata_cp["HeatingQC"][traindata_cp["HeatingQC"] == "Gd"] <- 4
traindata_cp["HeatingQC"][traindata_cp["HeatingQC"] == "TA"] <- 3
traindata_cp["HeatingQC"][traindata_cp["HeatingQC"] == "Fa"] <- 2
traindata_cp["HeatingQC"][traindata_cp["HeatingQC"] == "Po"] <- 1
#Convert to numeric datatype
traindata_cp$HeatingQC <- as.numeric(traindata_cp$HeatingQC)

#Replace all categorical variables with numerical variables in FireplaceQu column
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "Ex"] <- 5
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "Gd"] <- 4
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "TA"] <- 3
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "Fa"] <- 2
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "Po"] <- 1
traindata_cp["FireplaceQu"][traindata_cp["FireplaceQu"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$FireplaceQu <- as.numeric(traindata_cp$FireplaceQu)

#Replace all categorical variables with numerical variables in KitchenQual column
traindata_cp["KitchenQual"][traindata_cp["KitchenQual"] == "Ex"] <- 5
traindata_cp["KitchenQual"][traindata_cp["KitchenQual"] == "Gd"] <- 4
traindata_cp["KitchenQual"][traindata_cp["KitchenQual"] == "TA"] <- 3
traindata_cp["KitchenQual"][traindata_cp["KitchenQual"] == "Fa"] <- 2
traindata_cp["KitchenQual"][traindata_cp["KitchenQual"] == "Po"] <- 1
#Convert to numeric datatype
traindata_cp$KitchenQual <- as.numeric(traindata_cp$KitchenQual)

#Replace all categorical variables with numerical variables in GarageFinish column
traindata_cp["GarageFinish"][traindata_cp["GarageFinish"] == "Fin"] <- 3
traindata_cp["GarageFinish"][traindata_cp["GarageFinish"] == "RFn"] <- 2
traindata_cp["GarageFinish"][traindata_cp["GarageFinish"] == "Unf"] <- 1
traindata_cp["GarageFinish"][traindata_cp["GarageFinish"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$GarageFinish <- as.numeric(traindata_cp$GarageFinish)

#Replace all categorical variables with numerical variables in GarageQual column
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "Ex"] <- 5
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "Gd"] <- 4
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "TA"] <- 3
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "Fa"] <- 2
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "Po"] <- 1
traindata_cp["GarageQual"][traindata_cp["GarageQual"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$GarageQual <- as.numeric(traindata_cp$GarageQual)

#Replace all categorical variables with numerical variables in GarageCond column
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "Ex"] <- 5
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "Gd"] <- 4
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "TA"] <- 3
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "Fa"] <- 2
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "Po"] <- 1
traindata_cp["GarageCond"][traindata_cp["GarageCond"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$GarageCond <- as.numeric(traindata_cp$GarageCond)

#Replace all categorical variables with numerical variables in PavedDrive column
traindata_cp["PavedDrive"][traindata_cp["PavedDrive"] == "Y"] <- 2
traindata_cp["PavedDrive"][traindata_cp["PavedDrive"] == "P"] <- 1
traindata_cp["PavedDrive"][traindata_cp["PavedDrive"] == "N"] <- 0
#Convert to numeric datatype
traindata_cp$PavedDrive <- as.numeric(traindata_cp$PavedDrive)

#Replace all categorical variables with numerical variables in GarageCond column
traindata_cp["PoolQC"][traindata_cp["PoolQC"] == "Ex"] <- 4
traindata_cp["PoolQC"][traindata_cp["PoolQC"] == "Gd"] <- 3
traindata_cp["PoolQC"][traindata_cp["PoolQC"] == "TA"] <- 2
traindata_cp["PoolQC"][traindata_cp["PoolQC"] == "Fa"] <- 1
traindata_cp["PoolQC"][traindata_cp["PoolQC"] == "NA"] <- 0
#Convert to numeric datatype
traindata_cp$PoolQC <- as.numeric(traindata_cp$PoolQC)


#extract columns with numerical variables
trainnums = unlist(lapply(traindata_cp, is.numeric), use.names = FALSE) 

#confirm class of each column
sapply(traindata_cp, class)

#Make a correlation plot
corrplot(cor(traindata_cp[trainnums])) 
?corrplot

#Reveal correlation in numerical columns
cor(traindata_cp[trainnums])

#insert numerical columns into a dataframe
traindata_nums <- traindata_cp[trainnums]
View(traindata_nums)

#Reveal correlation in numerical columns
cor(traindata_nums)
corrplot(cor(traindata_nums)) #call mtcars correlation matrix using cor

#Partition data 80:20 ie 1168:292 for training and testing
#partition 80% for training on the train dataset
traindata_nums1168 <- traindata_nums[1:1168, ]
View(traindata_nums1168)

#partition 20% for testing on the train dataset
traindata_nums292 <- traindata_nums[1169:1460, ]
View(traindata_nums292)

#remove saleprice on 20% for prediction after model building 
traindata_nums292_test <- traindata_nums292[1:28]
View(traindata_nums292_test)

#MULTIPLE REGRESSION on the 80% partition dataset with SalePrice as the dependent variable
reg = lm(SalePrice ~ ., data = traindata_nums1168) 
summary(reg)

#To eliminate less relevant columns based on P value apply STEPWISE REGRESSION
step_reg = step(reg)
summary(step_reg)


#Model built from summary(step_reg) and applied to the 20% for prediction after model testing
traindata_nums292_test$SalePrice <- with(traindata_nums292_test, 1.883e+06 - 2.114e+02*(MSSubClass) + 4.076e-01*(LotArea) 
        + 1.431e+04*(OverallQual) + 5.256e+03*(OverallCond) + 1.904e+02*(YearBuilt)
        + 1.361e+04*(ExterQual) + 1.101e+04*(BsmtQual) - 1.309e+04*(BsmtCond)
        + 7.283e+03*(BsmtExposure) + 3.444e+03*(BsmtFinType1) + 6.525e+00*(X_1stFlrSF)
        + 5.274e+01*(GrLivArea) + 1.051e+04*(KitchenQual) + 2.676e+03*(FireplaceQu)
        + 2.646e+03*(GarageFinish) - 1.190e+03*(YrSold))
#view updated traindata_nums292_test
View(traindata_nums292_test)

#combine house IDs with their prices on traindata_nums292 table and name is alltraindata_nums292
alltraindata_nums292 <- cbind(house_id = traindata_cp$Id[1169:1460], traindata_nums292)

#combine house IDs with their prices on traindata_nums292_test table and name is alltraindata_nums292_test
alltraindata_nums292_test <- cbind(house_id = traindata_cp$Id[1169:1460], traindata_nums292_test)

#View table
View(alltraindata_nums292)

#View table
View(alltraindata_nums292_test)

#Create a combine plot of original and predicted sale prices for the 20% test data from train data
ggp <- ggplot(NULL, aes(house_id, SalePrice)) +    # Draw ggplot2 plot based on the two data frames
  geom_point(data = alltraindata_nums292, col = "red") +
  geom_point(data = alltraindata_nums292_test, col = "blue")

#view plot
ggp     # Draw plot


#create table with tested model values and original values
SalePriceComp <- cbind(house_id = alltraindata_nums292$house_id, nums292 = alltraindata_nums292$SalePrice, nums292_test = alltraindata_nums292_test$SalePrice )
View(SalePriceComp)

#SAVE RESULT TO CSV
write.csv(SalePriceComp,"C:\\Users\\H  U  B  E  R  T\\Documents\\Hubert_Oive_Madise_PwC_Bootcamp_Project_1\\SalePriceComp.csv", row.names = FALSE)


#MODEL APPLICATION

#View test data
View(testdata_cp)

#Replace all categorical variables with numerical variables in LotFrontage column
testdata_cp["LotFrontage"][testdata_cp["LotFrontage"] == "NA"] <- 0

#Convert to numeric datatype
testdata_cp$LotFrontage <- as.numeric(testdata_cp$LotFrontage)

#Replace all categorical variables with numerical variables in Utilities column
testdata_cp["Utilities"][testdata_cp["Utilities"] == "AllPub"] <- 4
testdata_cp["Utilities"][testdata_cp["Utilities"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$Utilities <- as.numeric(testdata_cp$Utilities)

#Replace all categorical variables with numerical variables in CentralAir column
testdata_cp["CentralAir"][testdata_cp["CentralAir"] == "Y"] <- 1
testdata_cp["CentralAir"][testdata_cp["CentralAir"] == "N"] <- 0
#Convert to numeric datatype
testdata_cp$CentralAir <- as.numeric(testdata_cp$CentralAir)

#Replace all categorical variables with numerical variables in ExterQual column
testdata_cp["ExterQual"][testdata_cp["ExterQual"] == "Ex"] <- 5
testdata_cp["ExterQual"][testdata_cp["ExterQual"] == "Gd"] <- 4
testdata_cp["ExterQual"][testdata_cp["ExterQual"] == "TA"] <- 3
testdata_cp["ExterQual"][testdata_cp["ExterQual"] == "Fa"] <- 2
testdata_cp["ExterQual"][testdata_cp["ExterQual"] == "Po"] <- 1
#Convert to numeric datatype
testdata_cp$ExterQual <- as.numeric(testdata_cp$ExterQual)

#Replace all categorical variables with numerical variables in ExterCond column
testdata_cp["ExterCond"][testdata_cp["ExterCond"] == "Ex"] <- 5
testdata_cp["ExterCond"][testdata_cp["ExterCond"] == "Gd"] <- 4
testdata_cp["ExterCond"][testdata_cp["ExterCond"] == "TA"] <- 3
testdata_cp["ExterCond"][testdata_cp["ExterCond"] == "Fa"] <- 2
testdata_cp["ExterCond"][testdata_cp["ExterCond"] == "Po"] <- 1
#Convert to numeric datatype
testdata_cp$ExterCond <- as.numeric(testdata_cp$ExterCond)

#Replace all categorical variables with numerical variables in BsmtQual column
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "Ex"] <- 5
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "Gd"] <- 4
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "TA"] <- 3
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "Fa"] <- 2
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "Po"] <- 1
testdata_cp["BsmtQual"][testdata_cp["BsmtQual"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$BsmtQual <- as.numeric(testdata_cp$BsmtQual)

#Replace all categorical variables with numerical variables in BsmtCond column
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "Ex"] <- 5
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "Gd"] <- 4
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "TA"] <- 3
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "Fa"] <- 2
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "Po"] <- 1
testdata_cp["BsmtCond"][testdata_cp["BsmtCond"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$BsmtCond <- as.numeric(testdata_cp$BsmtCond)

#Replace all categorical variables with numerical variables in BsmtExposure column
testdata_cp["BsmtExposure"][testdata_cp["BsmtExposure"] == "Gd"] <- 4
testdata_cp["BsmtExposure"][testdata_cp["BsmtExposure"] == "Av"] <- 3
testdata_cp["BsmtExposure"][testdata_cp["BsmtExposure"] == "Mn"] <- 2
testdata_cp["BsmtExposure"][testdata_cp["BsmtExposure"] == "No"] <- 1
testdata_cp["BsmtExposure"][testdata_cp["BsmtExposure"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$BsmtExposure <- as.numeric(testdata_cp$BsmtExposure)

#Replace all categorical variables with numerical variables in BsmtFinType1 column
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "GLQ"] <- 6
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "ALQ"] <- 5
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "BLQ"] <- 4
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "Rec"] <- 3
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "LwQ"] <- 2
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "Unf"] <- 1
testdata_cp["BsmtFinType1"][testdata_cp["BsmtFinType1"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$BsmtFinType1 <- as.numeric(testdata_cp$BsmtFinType1)

#Replace all categorical variables with numerical variables in BsmtFinType2 column
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "GLQ"] <- 6
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "ALQ"] <- 5
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "BLQ"] <- 4
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "Rec"] <- 3
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "LwQ"] <- 2
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "Unf"] <- 1
testdata_cp["BsmtFinType2"][testdata_cp["BsmtFinType2"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$BsmtFinType2 <- as.numeric(testdata_cp$BsmtFinType2)

#Replace all categorical variables with numerical variables in HeatingQC column
testdata_cp["HeatingQC"][testdata_cp["HeatingQC"] == "Ex"] <- 5
testdata_cp["HeatingQC"][testdata_cp["HeatingQC"] == "Gd"] <- 4
testdata_cp["HeatingQC"][testdata_cp["HeatingQC"] == "TA"] <- 3
testdata_cp["HeatingQC"][testdata_cp["HeatingQC"] == "Fa"] <- 2
testdata_cp["HeatingQC"][testdata_cp["HeatingQC"] == "Po"] <- 1
#Convert to numeric datatype
testdata_cp$HeatingQC <- as.numeric(testdata_cp$HeatingQC)

#Replace all categorical variables with numerical variables in FireplaceQu column
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "Ex"] <- 5
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "Gd"] <- 4
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "TA"] <- 3
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "Fa"] <- 2
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "Po"] <- 1
testdata_cp["FireplaceQu"][testdata_cp["FireplaceQu"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$FireplaceQu <- as.numeric(testdata_cp$FireplaceQu)

#Replace all categorical variables with numerical variables in KitchenQual column
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "Ex"] <- 5
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "Gd"] <- 4
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "TA"] <- 3
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "Fa"] <- 2
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "Po"] <- 1
testdata_cp["KitchenQual"][testdata_cp["KitchenQual"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$KitchenQual <- as.numeric(testdata_cp$KitchenQual)

#Replace all categorical variables with numerical variables in GarageFinish column
testdata_cp["GarageFinish"][testdata_cp["GarageFinish"] == "Fin"] <- 3
testdata_cp["GarageFinish"][testdata_cp["GarageFinish"] == "RFn"] <- 2
testdata_cp["GarageFinish"][testdata_cp["GarageFinish"] == "Unf"] <- 1
testdata_cp["GarageFinish"][testdata_cp["GarageFinish"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$GarageFinish <- as.numeric(testdata_cp$GarageFinish)

#Replace all categorical variables with numerical variables in GarageQual column
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "Ex"] <- 5
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "Gd"] <- 4
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "TA"] <- 3
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "Fa"] <- 2
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "Po"] <- 1
testdata_cp["GarageQual"][testdata_cp["GarageQual"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$GarageQual <- as.numeric(testdata_cp$GarageQual)

#Replace all categorical variables with numerical variables in GarageCond column
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "Ex"] <- 5
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "Gd"] <- 4
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "TA"] <- 3
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "Fa"] <- 2
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "Po"] <- 1
testdata_cp["GarageCond"][testdata_cp["GarageCond"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$GarageCond <- as.numeric(testdata_cp$GarageCond)

#Replace all categorical variables with numerical variables in PavedDrive column
testdata_cp["PavedDrive"][testdata_cp["PavedDrive"] == "Y"] <- 2
testdata_cp["PavedDrive"][testdata_cp["PavedDrive"] == "P"] <- 1
testdata_cp["PavedDrive"][testdata_cp["PavedDrive"] == "N"] <- 0
#Convert to numeric datatype
testdata_cp$PavedDrive <- as.numeric(testdata_cp$PavedDrive)

#Replace all categorical variables with numerical variables in GarageCond column
testdata_cp["PoolQC"][testdata_cp["PoolQC"] == "Ex"] <- 4
testdata_cp["PoolQC"][testdata_cp["PoolQC"] == "Gd"] <- 3
testdata_cp["PoolQC"][testdata_cp["PoolQC"] == "TA"] <- 2
testdata_cp["PoolQC"][testdata_cp["PoolQC"] == "Fa"] <- 1
testdata_cp["PoolQC"][testdata_cp["PoolQC"] == "NA"] <- 0
#Convert to numeric datatype
testdata_cp$PoolQC <- as.numeric(testdata_cp$PoolQC)

#Convert to numeric datatype
testdata_cp$OverallCond <- as.numeric(testdata_cp$OverallCond)

#extract columns with numerical variables
testnums = unlist(lapply(testdata_cp, is.numeric), use.names = FALSE) #extract tables with numeric values into a list

#confirm column class
sapply(testdata_cp, class)
View(testdata_cp)

#correlation in numerical columns of testdata_cp
cor(testdata_cp[testnums])

#insert numerical columns into a dataframe
testdata_nums <- testdata_cp[testnums]
View(testdata_nums)

#Apply model to test data set
testdata_nums$SalePrice <- with(testdata_nums, 1.883e+06 - 2.114e+02*(MSSubClass) + 4.076e-01*(LotArea) 
                                         + 1.431e+04*(OverallQual) + 5.256e+03*(OverallCond) + 1.904e+02*(YearBuilt)
                                         + 1.361e+04*(ExterQual) + 1.101e+04*(BsmtQual) - 1.309e+04*(BsmtCond)
                                         + 7.283e+03*(BsmtExposure) + 3.444e+03*(BsmtFinType1) + 6.525e+00*(X_1stFlrSF)
                                         + 5.274e+01*(GrLivArea) + 1.051e+04*(KitchenQual) + 2.676e+03*(FireplaceQu)
                                         + 2.646e+03*(GarageFinish) - 1.190e+03*(YrSold))
#View updated testdata_nums table
View(testdata_nums)
sapply(testdata_nums, class)
#Update testdata table with saleprice
testdatacomp <- cbind(testdata, SalePrice = testdata_nums$SalePrice)
View(testdatacomp)

#Save result to csv
write.csv(testdatacomp,"C:\\Users\\H  U  B  E  R  T\\Documents\\Hubert_Oive_Madise_PwC_Bootcamp_Project_1\\testdatacomp.csv", row.names = FALSE)

#testing 1 2