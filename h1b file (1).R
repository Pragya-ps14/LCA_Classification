library(tidyverse)
library(stringr)
library(lubridate)
setwd("C:/Users/user/Documents/Data Mining/Data Mining Project/Project")

table1<-read.csv("H-1B_Disclosure_RAW_Data_FY15.csv",stringsAsFactors = FALSE)
table2<-read.csv("H-1B_Disclosure_RAW_Data_FY16.csv",stringsAsFactors = FALSE)
table3<-read.csv("H-1B_Disclosure_RAW_Data_FY17.csv",stringsAsFactors = FALSE)
table4<-read.csv("H-1B_Disclosure_RAW_Data_FY18.csv",stringsAsFactors = FALSE)
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

#new1  <- df[,!(colnames(df) %in% colnames(table2))] ##Table 1 coloumns not present in table 2
#new2 <- table2[,!colnames(table1) %in% colnames(table2)]  ##table 2 coloumn not present in table 1
#colnames(table2)


common_cols <- intersect(colnames(table3),colnames(table4))
df <- rbind(table3[,common_cols],table4[, common_cols])

common_cols1 <- intersect(colnames(df),colnames(table2)) 
df_new <- rbind(df[,common_cols1],table2[, common_cols1])



colnames(table1)
colnames(df_new)
df_new <- df_new %>% filter( VISA_CLASS =="H-1B")
df_new <- df_new[,c(-1,-5,-9,-12,-14,-15,-16,-17,-39)]
table1 <- table1[,c(-1,-9,-10,-13,-15,-16,-17,-18,-40)]
table1 <- table1 %>% filter( VISA_CLASS =="H-1B")
table1 <- table1[,c(-4)]
names(table1)[26]<-"H1B_DEPENDENT"
names(table1)[27]<-"WILLFUL_VIOLATOR"
names(table1)[15]<-"NAICS_CODE"
names(table1)[16] <- 'TOTAL_WORKERS'
names(table1)[22] <- 'PW_SOURCE_YEAR'
names(table1)[23] <- 'PW_SOURCE_OTHER'
names(table1)[21] <- 'PW_SOURCE'
unique(df_new$EMPLOYER_COUNTRY)    ##talk about employer country
table1 <- cbind(as.data.frame(str_split_fixed(table1$WAGE_RATE_OF_PAY,"-",2)),table1)
colnames(table1)
names(table1)[1] <- "WAGE_RATE_OF_PAY_FROM"
names(table1)[2] <- "WAGE_RATE_OF_PAY_TO"

str(table1$WAGE_RATE_OF_PAY)

intersect_new <- intersect(colnames(df_new),colnames(table1))   #Join table1 with df_new--number of common coloumns
final <- rbind(df_new[,intersect_new],table1[, intersect_new])





#######################Runtillhere###########################


summary(final)

#no null values in case number
#employer_province--missing 4171 values
#employer_postal_code-missing 2 values
#employerphone extension missing 39 values
#full_time_position--missing 647852 valyes---->Missing in coloumn-----The value can be imputed using the coloumn value
#NAICS_code---->Missing 11
#PW_Source-->106 missing values
#PW_Source_Year---->92 values
#Worksite_city--->8 Values
#Worksite_State---->1 value



for (a in 1:30){
  if(final[,a] == "")
    final[,a]<- NA
}
colnames(final)
#ROUGH CODE
final$AGENT_ATTORNEY_STATE[final$AGENT_ATTORNEY_STATE==""]<- NA
final$AGENT_ATTORNEY_STATE[final$AGENT_ATTORNEY_STATE==""]<- NA
final$CASE_STATUS[final$CASE_STATUS==""]<- NA
final$CASE_SUBMITTED[final$CASE_SUBMITTED==""]<- NA
final$DECISION_DATE[final$DECISION_DATE==""]<- NA
final$EMPLOYMENT_START_DATE[final$EMPLOYMENT_START_DATE==""]<- NA
final$EMPLOYMENT_END_DATE[final$EMPLOYMENT_END_DATE==""]<- NA



str(final$AGENT_ATTORNEY_CITY)

for(i in 1:nrow(final)){
  print(sum(is.na(final[i])))
  print(names(final)[i])
}
final$CASE_SUBMITTED
final <- final %>% drop_na(-FULL_TIME_POSITION)
unique(final$CASE_STATUS)
kk<-final %>% filter(CASE_STATUS == "CERTIFIED"|CASE_STATUS =="DENIED")

#CONVERSION OF DATES TO NUMERIC VALUES
kk$CASE_SUBMITTED <- as.POSIXct(kk$CASE_SUBMITTED,format = "%m/%d/%Y")
kk$DECISION_DATE <- as.POSIXct(kk$DECISION_DATE,format = "%m/%d/%Y")

kk$EMPLOYMENT_START_DATE <- as.POSIXct(kk$EMPLOYMENT_START_DATE,format = "%m/%d/%Y")
kk$EMPLOYMENT_END_DATE <- as.POSIXct(kk$EMPLOYMENT_END_DATE,format = "%m/%d/%Y")

#converting to numeric data type
kk$WAGE_RATE_OF_PAY_FROM<-as.numeric(as.factor(kk$WAGE_RATE_OF_PAY_FROM))
kk$WAGE_RATE_OF_PAY_TO<-as.numeric(as.factor(kk$WAGE_RATE_OF_PAY_TO))
kk$PREVAILING_WAGE<-as.numeric(as.factor(kk$WAGE_RATE_OF_PAY_TO))
kk$TOTAL_WORKERS<-as.numeric(as.factor(kk$TOTAL_WORKERS))



#CREATING DUMMY VARIABLE:

sum(is.na(kk$WAGE_RATE_OF_PAY_FROM))
sum(is.na(kk$WAGE_RATE_OF_PAY_to))
colnames(df_new)
colnames(table2)
sum(is.na(df_new[25]))
table2[25]

