#Load dependencies
load.libraries <- c('dplyr','ggplot2','readr','mice','Hmisc','missForest','xgboost',
                    'dummies','randomForest','Metrics','rpart','caret')
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for (libs in install.lib) install.packages(libs,dependencies = TRUE)
sapply(load.libraries,require,character=TRUE)

set.seed(5)  
setwd("C:/Users/Aravind Atreya/Desktop/Kaggle/House Data")

#Delete all the existing variables in the workspace
rm(list=ls())

#Function for mode
getmode <- function (x) {
  names(table(x))[which.max(table(na.omit(x)))]
}

#Function for Year Ranges
YrRange <- function(x){
  if(!is.na(x)){
  if(x<=1900){"Before 1900"
  } else if(x>1900 & x<=1925){
    "1901-1925"
  } else if(x>1925 & x<=1950){
    "1926-1950"
  } else if(x>1950 & x<=1960){
    "1951-1960"
  } else if(x>1960 & x<=1970){
    "1961-1970"
  } else if(x>1970 & x<=1980){
    "1971-1980"
  } else if(x>1981 & x<=1990){
    "1981-1990"
  } else if(x>1990 & x<=2000){
    "1991-2000"
  } else if(x>2000 & x<=2005){
    "2001-2005"
  } else {
    "2006-2010"
  }
  } else NA
}

#Function for converting factors to ordinal Variables with levels Ex,Gd,TA,Fa,Po
convert_integer <- function (x){
  levels(x) <- c(levels(x),0,1,2,3,4,5)
  x[which(x=="Ex")] <- 5
  x[which(x=="Gd")] <- 4
  x[which(x=="TA")] <- 3
  x[which(x=="Fa")] <- 2
  x[which(x=="Po")] <- 1
  x[which(x=="No Bsmt" | x=="No Garage")] <- 0
  y <- as.integer(as.character(x))
  return(y)
}


#Load the files into data frames
train <- read.csv("train.csv")
test <- read.csv("test.csv")
SalePrice <- train$SalePrice
total_df <- rbind(select(train,-SalePrice),test)
total_df$Id <- NULL
test_rownum <- dim(train)[1]

#Let's see how many continous and factor variables we've got
types <- sapply(total_df,class)
table(types)
int_fields <- names(total_df)[types=="integer"]
factor_fields <- names(total_df)[(types!="integer")]

#Missing Values
#Let's take a look at the columns with missing values and the number of missing values.

colSums(sapply(total_df[,int_fields],is.na))[colSums(sapply(total_df[,int_fields],is.na))>0]
colSums(sapply(total_df[,factor_fields],is.na))[colSums(sapply(total_df[,factor_fields],is.na))>0]

#These int fields can be replaced with the mean and mode as very few are missing in number wherever applicable
#In our case missing values in most cases actually mean something, like there's no garage or something like that
garage_missing <- which(is.na(total_df$GarageQual))
bsmt_missing <- which(is.na(total_df$BsmtQual))
which(is.na(total_df$BsmtFinSF1)) %in% bsmt_missing #TRUE
total_df$BsmtFinSF1[is.na(total_df$BsmtFinSF1)] <- 0
which(is.na(total_df$BsmtFinSF2)) %in% bsmt_missing #TRUE
total_df$BsmtFinSF2[is.na(total_df$BsmtFinSF2)] <- 0
which(is.na(total_df$BsmtUnfSF)) %in% bsmt_missing #TRUE
total_df$BsmtUnfSF[is.na(total_df$BsmtUnfSF)] <- 0
which(is.na(total_df$GarageCars)) %in% garage_missing #TRUE
total_df$GarageArea[is.na(total_df$GarageArea)] <- 0
which(is.na(total_df$GarageArea)) %in% garage_missing #TRUE
total_df$GarageCars[is.na(total_df$GarageCars)] <- 0
which(is.na(total_df$BsmtFullBath)) %in% bsmt_missing #TRUE TRUE
total_df$BsmtFullBath[is.na(total_df$BsmtFullBath)] <- 0
which(is.na(total_df$BsmtHalfBath)) %in% bsmt_missing #TRUE TRUE
total_df$BsmtHalfBath[is.na(total_df$BsmtHalfBath)]<-0
which(is.na(total_df$TotalBsmtSF)) %in% bsmt_missing #TRUE
total_df$TotalBsmtSF[is.na(total_df$TotalBsmtSF)] <- 0
total_df$MasVnrArea[is.na(total_df$MasVnrArea)] <- mean(total_df$MasVnrArea,na.rm = TRUE)

#These factor variables can be replaced with their mode
#MSZoning,Utilities,#Exterior1st,#Exterior2nd,Electrical,KitchenQual,Functional,SaleType
total_df$MSZoning[is.na(total_df$MSZoning)] <- as.factor(getmode(total_df$MSZoning))
total_df$Utilities[is.na(total_df$Utilities)] <- as.factor(getmode(total_df$Utilities))
total_df$Exterior1st[is.na(total_df$Exterior1st)] <- as.factor(getmode(total_df$Exterior1st))
total_df$Exterior2nd[is.na(total_df$Exterior2nd)] <- as.factor(getmode(total_df$Exterior2nd))
total_df$MasVnrType[is.na(total_df$MasVnrType)] <- as.factor(getmode(total_df$MasVnrType))
total_df$Electrical[is.na(total_df$Electrical)] <- as.factor(getmode(total_df$Electrical))
total_df$KitchenQual[is.na(total_df$KitchenQual)] <- as.factor(getmode(total_df$KitchenQual))
total_df$Functional[is.na(total_df$Functional)] <- as.factor(getmode(total_df$Functional))
total_df$SaleType[is.na(total_df$SaleType)] <- as.factor(getmode(total_df$SaleType))

#Let's drop Alley, FireplaceQu, PoolQC, Fence, MiscFeature as they have too many rows missing
total_df$Alley <- NULL
total_df$FireplaceQu <- NULL
total_df$PoolQC <-  NULL
total_df$Fence <-  NULL
total_df$MiscFeature <- NULL

#Let's update fileds as we've removed a few columns
types <- sapply(total_df,class)
table(types)
int_fields <- names(total_df)[types=="integer" | types=="numeric" ]
factor_fields <- names(total_df)[(types=="factor")]

#Missing Values

colSums(sapply(total_df[,int_fields],is.na))[colSums(sapply(total_df[,int_fields],is.na))>0]
colSums(sapply(total_df[,factor_fields],is.na))[colSums(sapply(total_df[,factor_fields],is.na))>0]

#Only Lorfrontage needs to be filled, rest all can be left as they are supposed to be NA's
#First let's convert GarageYrBuilt, YearBuilt, YearRemoAdd to a factor of year ranges

total_df$Sold_relative <- total_df$MoSold+12*(total_df$YrSold-2006)

total_df$YRBuilt_Range <- as.factor(sapply(total_df$YearBuilt,YrRange))
total_df$GarageYrBlt_Range <- as.factor(sapply(total_df$GarageYrBlt,YrRange))
total_df$YearRemodAdd_Range <- as.factor(sapply(total_df$YearRemodAdd,YrRange))

#Let's back up these years data into another dataframe and remove them from total_df for now

backup_year_df <- select(total_df,YrSold,MoSold,YearBuilt,GarageYrBlt,YearRemodAdd)
total_df$YrSold <- NULL
total_df$MoSold <- NULL
total_df$YearBuilt <- NULL
total_df$GarageYrBlt <- NULL
total_df$YearRemodAdd <- NULL

#Let's update fileds as we've removed a few columns
types <- sapply(total_df,class)
table(types)
int_fields <- names(total_df)[types=="integer" | types=="numeric" ]
factor_fields <- names(total_df)[(types=="factor")]

#Missing Values

colSums(sapply(total_df[,int_fields],is.na))[colSums(sapply(total_df[,int_fields],is.na))>0]
colSums(sapply(total_df[,factor_fields],is.na))[colSums(sapply(total_df[,factor_fields],is.na))>0]

#Replace the NA values with No Basement and No Garage
levels(total_df$BsmtQual) <- c(levels(total_df$BsmtQual),"No Bsmt")
levels(total_df$BsmtCond) <- c(levels(total_df$BsmtCond),"No Bsmt")
levels(total_df$BsmtExposure) <- c(levels(total_df$BsmtExposure),"No Bsmt")
levels(total_df$BsmtFinType1) <- c(levels(total_df$BsmtFinType1),"No Bsmt")
levels(total_df$BsmtFinType2) <- c(levels(total_df$BsmtFinType2),"No Bsmt")
levels(total_df$GarageType) <- c(levels(total_df$GarageType),"No Garage")
levels(total_df$GarageFinish) <- c(levels(total_df$GarageFinish),"No Garage")
levels(total_df$GarageQual) <- c(levels(total_df$GarageQual),"No Garage")
levels(total_df$GarageCond) <- c(levels(total_df$GarageCond),"No Garage")
levels(total_df$GarageYrBlt_Range) <- c(levels(total_df$GarageYrBlt_Range),"No Garage")
total_df$BsmtQual[is.na(total_df$BsmtQual)] <- "No Bsmt"
total_df$BsmtCond[is.na(total_df$BsmtCond)] <- "No Bsmt"
total_df$BsmtExposure[is.na(total_df$BsmtExposure)] <- "No Bsmt"
total_df$BsmtFinType1[is.na(total_df$BsmtFinType1)] <- "No Bsmt"
total_df$BsmtFinType2[is.na(total_df$BsmtFinType2)] <- "No Bsmt"
total_df$GarageType[is.na(total_df$GarageType)] <- "No Garage"
total_df$GarageFinish[is.na(total_df$GarageFinish)] <- "No Garage"
total_df$GarageQual[is.na(total_df$GarageQual)] <- "No Garage"
total_df$GarageCond[is.na(total_df$GarageCond)] <- "No Garage"
total_df$GarageYrBlt_Range[is.na(total_df$GarageYrBlt_Range)] <- "No Garage"

#Missing Fields
colSums(sapply(total_df[,int_fields],is.na))[colSums(sapply(total_df[,int_fields],is.na))>0]
colSums(sapply(total_df[,factor_fields],is.na))[colSums(sapply(total_df[,factor_fields],is.na))>0]

#So only LotFrontage remains to be imputed. Let's use mice package and impute this.
total_df <- complete(mice(total_df))

#Our dataset is now complete but we may have to do some more feature engineering for a few factor variables.
#Some can be converted to dummies, some can be taken as ordinal variables.

#let's take a look at all the factor variables and figure out the best way to convert them to numeric

ordinal_factor_fields <- c("LotShape","LandSlope","ExterQual","ExterCond","BsmtQual",
                           "BsmtCond","BsmtExposure","BsmtFinType1","BsmtFinType2","KitchenQual",
                           "GarageQual","GarageCond","PavedDrive")
dummy_factor_fields <-setdiff(factor_fields,ordinal_factor_fields)

total_df$LotShape <- as.integer(total_df$LotShape)
total_df$LandSlope <- as.integer(total_df$LandSlope)
total_df$ExterQual <- convert_integer(total_df$ExterQual)
total_df$ExterCond <- convert_integer(total_df$ExterCond)
total_df$BsmtQual <- convert_integer(total_df$BsmtQual)
total_df$BsmtCond <- convert_integer(total_df$BsmtCond)
total_df$KitchenQual <- convert_integer(total_df$KitchenQual)
total_df$GarageQual <- convert_integer(total_df$GarageQual)
total_df$GarageCond <- convert_integer(total_df$GarageCond)

levels(total_df$BsmtExposure) <- c(levels(total_df$BsmtExposure),0,1,2,3,4)
total_df$BsmtExposure[total_df$BsmtExposure=="No Bsmt"] <- 0
total_df$BsmtExposure[total_df$BsmtExposure=="No"] <- 1
total_df$BsmtExposure[total_df$BsmtExposure=="Mn"] <- 2
total_df$BsmtExposure[total_df$BsmtExposure=="Gd"] <- 3
total_df$BsmtExposure[total_df$BsmtExposure=="Av"] <- 4
total_df$BsmtExposure <- as.integer(as.character(total_df$BsmtExposure))

levels(total_df$BsmtFinType1) <- c(levels(total_df$BsmtFinType1),0:6)
total_df$BsmtFinType1[total_df$BsmtFinType1=="No Bsmt"] <- 0
total_df$BsmtFinType1[total_df$BsmtFinType1=="Unf"] <- 1
total_df$BsmtFinType1[total_df$BsmtFinType1=="LwQ"] <- 2
total_df$BsmtFinType1[total_df$BsmtFinType1=="Rec"] <- 3
total_df$BsmtFinType1[total_df$BsmtFinType1=="BLQ"] <- 4
total_df$BsmtFinType1[total_df$BsmtFinType1=="ALQ"] <- 5
total_df$BsmtFinType1[total_df$BsmtFinType1=="GLQ"] <- 6
total_df$BsmtFinType1 <- as.integer(as.character(total_df$BsmtFinType1))

levels(total_df$BsmtFinType2) <- c(levels(total_df$BsmtFinType2),0:6)
total_df$BsmtFinType2[total_df$BsmtFinType2=="No Bsmt"] <- 0
total_df$BsmtFinType2[total_df$BsmtFinType2=="Unf"] <- 1
total_df$BsmtFinType2[total_df$BsmtFinType2=="LwQ"] <- 2
total_df$BsmtFinType2[total_df$BsmtFinType2=="Rec"] <- 3
total_df$BsmtFinType2[total_df$BsmtFinType2=="BLQ"] <- 4
total_df$BsmtFinType2[total_df$BsmtFinType2=="ALQ"] <- 5
total_df$BsmtFinType2[total_df$BsmtFinType2=="GLQ"] <- 6
total_df$BsmtFinType2 <- as.integer(as.character(total_df$BsmtFinType2))

levels(total_df$PavedDrive) <- c(levels(total_df$PavedDrive),0:2)
total_df$PavedDrive[total_df$PavedDrive=="N"] <- 0
total_df$PavedDrive[total_df$PavedDrive=="P"] <- 1
total_df$PavedDrive[total_df$PavedDrive=="Y"] <- 2
total_df$PavedDrive <- as.integer(as.character(total_df$PavedDrive))

#Creating dummy variables,# No need to remove last dummy variable as PCA automatically removes correlated variables

dummy_factor_df <- total_df[,dummy_factor_fields]

dummy_fields <- dummy.data.frame(dummy_factor_df,sep="_",all=FALSE)
backup_total_df <- total_df
total_df[,dummy_factor_fields] <- NULL
total_df <- cbind(total_df,dummy_fields)


train_new <- total_df[1:test_rownum,]
test_new <- total_df[(test_rownum+1:dim(total_df)[1]),]
train_new$LogSalePrice <- log(SalePrice)


fitcontrol <- trainControl(method = "repeatedcv", number=10, repeats = 3)
gbm_fit <- train(LogSalePrice~.,method="gbm",trControl=fitcontrol,metric="RMSE",data=train_new)

Test_LogSale <- predict(gbm_fit,test_new)
Test_SalePrice <- exp(Test_LogSale)

Results <- data.frame(Id=c(1461:2919),SalePrice=Test_SalePrice)
write.csv(Results,"CaretResults.csv",row.names = FALSE)
