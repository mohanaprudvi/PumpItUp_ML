# Getting the data directly from the cloud
Training_Data <- read.csv("https://s3.amazonaws.com/drivendata/data/7/public/4910797b-ee55-40a7-8668-10efd5c1b960.csv")
Training_Labels <- read.csv("https://s3.amazonaws.com/drivendata/data/7/public/0bf8bc6e-30d0-4c50-956a-603fc693d966.csv")
Test_Set <- read.csv("https://s3.amazonaws.com/drivendata/data/7/public/702ddfc5-68cd-4d1d-a0de-f5f566f76d91.csv")

#PreProcessing the data
Testing_Data <- Test_Set
Training_Data_Complete <- Training_Data

Training_Data_Complete$recorded_by = NULL
Testing_Data$recorded_by = NULL

Training_Data_Complete$quantity_group = NULL
Testing_Data$quantity_group = NULL

Training_Data_Complete$extraction_type_group = NULL
Testing_Data$extraction_type_group = NULL

Training_Data_Complete$source_type = NULL
Testing_Data$source_type = NULL

Training_Data_Complete$payment_type = NULL
Testing_Data$payment_type = NULL

#Training_Data_Complete$water_quality = NULL
Training_Data_Complete$quality_group = NULL
Testing_Data$quality_group = NULL

Training_Data_Complete$waterpoint_type_group = NULL
Testing_Data$waterpoint_type_group = NULL

Training_Data_Complete$scheme_name = NULL
Testing_Data$scheme_name = NULL

Training_Data_Complete$num_private = NULL
Testing_Data$num_private = NULL

#Training_Data_Complete$public_meeting = NULL
Training_Data_Complete$wpt_name = NULL
Testing_Data$wpt_name = NULL

Training_Data_Complete$extraction_type = NULL
Testing_Data$extraction_type = NULL

Training_Data_Complete$lga = NULL
Testing_Data$lga = NULL

Training_Data_Complete$region_code = NULL
Testing_Data$region_code = NULL

Training_Data_Complete$management_group = NULL
Testing_Data$management_group = NULL

#Training_Data_Complete$source_class = NULL
Training_Data_Complete$scheme_management = NULL
Testing_Data$scheme_management = NULL

Training_Data_Complete$subvillage = NULL
Testing_Data$subvillage =  NULL

Training_Data_Complete$district_code = NULL
Testing_Data$district_code = NULL

Training_Data_Complete$installer = NULL
Testing_Data$installer = NULL

Training_Data_Complete$ward = NULL
Testing_Data$ward = NULL

#Training_Data_Complete$payment = NULL

t <- format(as.Date(Training_Data_Complete$date_recorded, '%Y-%m-%d'),'%Y')
tTest <- format(as.Date(Testing_Data$date_recorded, '%Y-%m-%d'),'%Y')

t <- as.numeric(t)
tTest <- as.numeric(tTest)

Training_Data_Complete$OperationAge <- t - Training_Data_Complete$construction_year
Testing_Data$OperationAge <- tTest - Testing_Data$construction_year

Training_Data_Complete$OperationAge[Training_Data_Complete$OperationAge > 2000] <- 0
Testing_Data$OperationAge[Testing_Data$OperationAge > 2000] <- 0

Training_Data_Complete$construction_year = NULL
Testing_Data$construction_year = NULL

Training_Data_Complete$date_recorded = NULL
Testing_Data$date_recorded = NULL

#taking first 3 letters of funder column
trim<-as.data.frame(Training_Data_Complete$funder)
trimTest <- as.data.frame(Testing_Data$funder)

trim<-as.data.frame(apply(trim,2,function(x)gsub('\\s+', '',x)))
trimTest <- as.data.frame(apply(trimTest,2,function(x)gsub('\\s+','',x)))

trim[trim %in% c("0", "_", "-"," ","")] <- "other"
trimTest[trimTest %in% c("0", "_", "-"," ","")] <- "other"

Training_Data_Complete$newfunder<-trim[,1]
Testing_Data$newfunder <- trimTest[,1]

Training_Data_Complete$newfunder <- substr(tolower(Training_Data_Complete$funder),1,3)
Testing_Data$newfunder <- substr(tolower(Testing_Data$funder),1,3)

Training_Data_Complete$funder<-Training_Data_Complete$newfunder
Testing_Data$funder <- Testing_Data$newfunder

Training_Data_Complete$newfunder<-NULL
Testing_Data$newfunder <- NULL

Training_Data_Complete$funder[Training_Data_Complete$funder %in% c("0", "_", "-"," ","")] <- "other"
Testing_Data$funder[Testing_Data$funder %in% c("0", "_", "-"," ","")] <- "other"

Training_Data_Complete$funder <- as.factor(Training_Data_Complete$funder)
Testing_Data$funder <- as.factor(Testing_Data$funder)

limitLevels = 10

#Reducing the number of levels
######
funders_levels <- names(summary(Training_Data_Complete$funder)[1:limitLevels])
funder <- factor(Training_Data_Complete$funder, levels=c(funders_levels, "Other"))
funder[is.na(funder)] <- "Other"
Training_Data_Complete$funder <- funder

funder <- factor(Testing_Data$funder, levels=c(funders_levels, "Other"))
funder[is.na(funder)] <- "Other"
Testing_Data$funder <- funder

gps_levels <- summary(Training_Data_Complete$gps_height)[1:limitLevels]
gps_height <- factor(Training_Data_Complete$gps_height, levels=c(gps_levels, 1))
gps_height[is.na(gps_height)] <- 1
Training_Data_Complete$gps_height <- gps_height

gps_height <- factor(Testing_Data$gps_height, levels=c(gps_levels, 1))
gps_height[is.na(gps_height)] <- 1
Testing_Data$gps_height <- gps_height


Training_Data_Complete$permit <-  c("True", "False", "")
Testing_Data$permit <- c("True", "False", "")

Training_Data_Complete$permit[Training_Data_Complete$permit %in% c(" ","")] <- "other"
Testing_Data$permit[Testing_Data$permit %in% c(" ","")] <- "other"

Training_Data_Complete$permit <- as.factor(Training_Data_Complete$permit)
Testing_Data$permit <- as.factor(Testing_Data$permit)

Training_Data_Complete <- sapply(Training_Data_Complete,as.numeric)
Testing_Data <- sapply(Testing_Data, as.numeric)

Training_Data_Complete <- as.data.frame(Training_Data_Complete)
Testing_Data <- as.data.frame(Testing_Data)
Training_Data_Complete <- merge(Training_Data_Complete, Training_Labels)
