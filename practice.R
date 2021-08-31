library(tidyverse)
library(readxl)
library(dplyr)
setwd("C:/Users/avdho/Desktop/SEM/DATA MINING/Project/data_to_work_with2")
table2 <- read.csv("H-1B_Disclosure_RAW_Data_FY16.csv", na.strings="", stringsAsFactors=FALSE)
table3 <- read.csv("H-1B_Disclosure_RAW_Data_FY17.csv", na.strings="", stringsAsFactors=FALSE)
table4 <- read.csv("H-1B_Disclosure_RAW_Data_FY18.csv", na.strings="", stringsAsFactors=FALSE)

summary(table2)
summary(table3)
summary(table4)

getwd()

#names(table1)[35] <- 'H.1B_DEPENDENT'
#names(table1)[25] <- 'TOTAL_WORKERS'
#names(table1)[36] <- 'WILLFUL_VIOLATOR'
names(table2)[24] <- 'TOTAL_WORKERS'
#names(table1)[9]  <- 'EMPLOYER_ADDRESS'
names(table2)[29] <- 'PW_SOURCE_YEAR'
names(table2)[30] <- 'PW_SOURCE_OTHER'
names(table2)[28] <- 'PW_SOURCE'
names(table2)[23] <- 'NAICS_CODE'
names(table2)[34] <- 'H1B_DEPENDENT'
#colnames(table1)
#table2 <- select(table2,-c("WAGE_RATE_OF_PAY_FROM","WAGE_RATE_OF_PAY_TO","ORIGINAL_CERT_DATE"))



new <- table2[ ,!(colnames(table2) %in% c('CASE_NUMBER','CASE_STATUS','CASE_SUBMITTED','DECISION_DATE','VISA_CLASS','EMPLOYMENT_START_DATE','EMPLOYMENT_END_DATE','EMPLOYER_NAME','EMPLOYER_ADDRESS1','EMPLOYER_ADDRESS2','EMPLOYER_CITY','EMPLOYER_STATE','EMPLOYER_POSTAL_CODE','EMPLOYER_COUNTRY','EMPLOYER_PROVINCE','EMPLOYER_PHONE','EMPLOYER_PHONE_EXT','AGENT_ATTORNEY_NAME','AGENT_ATTORNEY_CITY','AGENT_ATTORNEY_STATE','JOB_TITLE','SOC_CODE','SOC_NAME','NAIC_CODE','TOTAL WORKERS','FULL_TIME_POSITION','PREVAILING_WAGE','PW_UNIT_OF_PAY','PW_WAGE_LEVEL','PW_WAGE_SOURCE','PW_WAGE_SOURCE_YEAR','PW_WAGE_SOURCE_OTHER','WAGE_RATE_OF_PAY','WAGE_UNIT_OF_PAY','H-1B_DEPENDENT','WILLFUL VIOLATOR','WORKSITE_CITY','WORKSITE_COUNTY','WORKSITE_STATE','WORKSITE_POSTAL_CODE'))]
colnames(table1)
#rm(kk)
#new1  <- df[,!(colnames(df) %in% colnames(table2))] ##Table 1 coloumns not present in table 2
#new2 <- table2[,!colnames(table1) %in% colnames(table2)]  ##table 2 coloumn not present in table 1
#colnames(table2)



#df['SOC_CODE']
names(table2)
names(df)

common_cols <- intersect(colnames(table3),colnames(table4))
df <- rbind(table3[,common_cols],table4[, common_cols])

common_cols1 <- intersect(colnames(df),colnames(table2)) 
df_new <- rbind(df[,common_cols1],table2[, common_cols1])

summary(df_new)





x <- is.na(df_new$CASE_SUBMITTED)
summary(x)
x_new <- is.na(na.omit(df_new$CASE_SUBMITTED))
summary(x_new)

summary(is.na(df_new$AGENT_ATTORNEY_STATE))


count(df_new[!complete.cases(df_new$AGENT_ATTORNEY_STATE),])

summary(df_new$VISA_CLASS)

#################################################################################


df_rmcol <- data.frame(df_new, df_new$VISA_CLASS == "H-1B")
summary(df_rmcol)

data.frame(colnames(df_rmcol)) #to get index of columns

######## Data with all un-necessary variables removed #########################
mydata <- df_rmcol[-c(9,12:19,21,23,39)]
data.frame(colnames(mydata))
summary(mydata)
# from the summary() we understand that, {ORIGINAL_CERT_DATE, PW_SOURCE_YEAR, 
# EMPLOYMENT_END_DATE, EMPLOYMENT_START_DATE, PREVAILING_WAGE, WAGE_RATE_OF_PAY_TO,
# CASE_SUBMITTED} have NAs (in decending order)
count(mydata)

# we only need the data for H1B type of visa class, therefore need to remove
# other types form column VISA_CLASS
mydata1 <- mydata[mydata$VISA_CLASS == "H-1B",] #to create the dataset with only H1B visa type
count(mydata1)
summary(mydata1)

# now we need to remove or replace NAs in the columns identified
# time to decide on other variables with NA values
# 1] ORIGINAL_CERT_DATE : [This situation arises when either company fires the employee
#                           or employee leaves the job], our aim is to predict whether
#                           a person gets certified or not, therefore it seems that we could 
#                           eliminate this variable]
data.frame(colnames(mydata1))
mydata2 <- mydata1[-c(5,28)]
summary(mydata2)
data.frame(colnames(mydata2))

names(mydata2)[27] <- paste("VISA_STATUS") #to change the column name to "VISA_STATUS"

mydata2 <- mydata2[-c(27)]
data.frame(colnames(mydata2))
summary(mydata2)
#above is the new dataset with all H1B VISA_Type
str(mydata2)

table(is.na(mydata2$FULL_TIME_POSITION))
##################### EXTRA #################################################################
library(skimr)
skim(mydata2)
#############################################################################################


unique(mydata2$FULL_TIME_POSITION)

table(is.na(mydata2$CASE_NUMBER))
table(is.na(mydata2$CASE_STATUS))
table(is.na(mydata2$CASE_SUBMITTED))
table(is.na(mydata2$DECISION_DATE))
table(is.na(mydata2$EMPLOYMENT_START_DATE))
table(is.na(mydata2$EMPLOYMENT_END_DATE))
table(is.na(mydata2$EMPLOYER_NAME))
table(is.na(mydata2$EMPLOYER_CITY))
table(is.na(mydata2$EMPLOYER_STATE))
table(is.na(mydata2$JOB_TITLE))
table(is.na(mydata2$SOC_NAME))
table(is.na(mydata2$TOTAL_WORKERS))


table(is.na(mydata2))

table(mydata2)

