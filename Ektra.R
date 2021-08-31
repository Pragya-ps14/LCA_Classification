#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111#
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
setwd("C:/Users/avdho/Desktop/SEM/DATA MINING/Project/data_to_work_with2")
table1 <- read.csv("H-1B_Disclosure_RAW_Data_FY15.csv",stringsAsFactors = FALSE)
table2 <- read.csv("H-1B_Disclosure_RAW_Data_FY16.csv",stringsAsFactors = FALSE)
table3 <- read.csv("H-1B_Disclosure_RAW_Data_FY17.csv",stringsAsFactors = FALSE)

table4 <- read.csv("H-1B_Disclosure_RAW_Data_FY18.csv",stringsAsFactors = FALSE)
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
colnames(df_new)

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
table1<-cbind(as.data.frame(str_split_fixed(table1$WAGE_RATE_OF_PAY,"-",2)),table1)
colnames(table1)
names(table1)[1]<-"WAGE_RATE_OF_PAY_FROM"
names(table1)[2]<-"WAGE_RATE_OF_PAY_TO"

str(table1$WAGE_RATE_OF_PAY)

intersect_new <- intersect(colnames(df_new),colnames(table1))   #Join table1 with df_new--number of common coloumns
final <- rbind(df_new[,intersect_new],table1[, intersect_new])
##############################################################################################
NROW(final$CASE_STATUS)

data.frame(colnames(final))
sapply(final, function(x) sum(is.na(x)))



# to replace missing values in columns by NA
#1) 
sum(final$CASE_SUBMITTED %in% "")
final$CASE_SUBMITTED <- replace(final$CASE_SUBMITTED, final$CASE_SUBMITTED == "", NA)
table(is.na(final$CASE_SUBMITTED))

#2)
sum(final$EMPLOYMENT_START_DATE %in% "")
final$EMPLOYMENT_START_DATE <- replace(final$EMPLOYMENT_START_DATE, final$EMPLOYMENT_START_DATE == "", NA)
table(is.na(final$EMPLOYMENT_START_DATE))

#3) 
sum(final$EMPLOYMENT_END_DATE %in% "")
final$EMPLOYMENT_END_DATE <- replace(final$EMPLOYMENT_END_DATE, final$EMPLOYMENT_END_DATE == "", NA)
table(is.na(final$EMPLOYMENT_END_DATE))

#4) 
sum(final$EMPLOYER_NAME %in% "")
final$EMPLOYER_NAME <- replace(final$EMPLOYER_NAME, final$EMPLOYER_NAME =="", NA)
table(is.na(final$EMPLOYER_NAME))

#5) 
sum(final$EMPLOYER_CITY %in% "")
final$EMPLOYER_CITY <- replace(final$EMPLOYER_CITY, final$EMPLOYER_CITY =="", NA)
table(is.na(final$EMPLOYER_CITY))

#6)
sum(final$EMPLOYER_STATE %in% "")
final$EMPLOYER_STATE <- replace(final$EMPLOYER_STATE, final$EMPLOYER_STATE == "", NA)
table(is.na(final$EMPLOYER_STATE))

#7) 
sum(final$EMPLOYER_COUNTRY %in% "")
final$EMPLOYER_COUNTRY <- replace(final$EMPLOYER_COUNTRY, final$EMPLOYER_COUNTRY =="", NA)
table(is.na(final$EMPLOYER_COUNTRY))

#8) 
unique(final$AGENT_ATTORNEY_CITY) # missing values -> ""
sum(final$AGENT_ATTORNEY_CITY %in% "")
final$AGENT_ATTORNEY_CITY <- replace(final$AGENT_ATTORNEY_CITY, final$AGENT_ATTORNEY_CITY == "", NA)
table(is.na(final$AGENT_ATTORNEY_CITY))

#9) 
sum(final$AGENT_ATTORNEY_STATE %in% "")
final$AGENT_ATTORNEY_STATE <- replace(final$AGENT_ATTORNEY_STATE, final$AGENT_ATTORNEY_STATE == "", NA)
table(is.na(final$AGENT_ATTORNEY_STATE))

#10)
sum(final$JOB_TITLE %in% "")
final$JOB_TITLE <- replace(final$JOB_TITLE, final$JOB_TITLE == "", NA)
table(is.na(final$JOB_TITLE))

#11)
sum(final$SOC_CODE %in% "")
final$SOC_CODE <- replace(final$SOC_CODE, final$SOC_CODE == "", NA)
table(is.na(final$SOC_CODE))

#12) 
sum(final$SOC_NAME %in% "")
final$SOC_NAME <- replace(final$SOC_NAME, final$SOC_NAME == "", NA)
table(is.na(final$SOC_NAME))

#13) 
sum(final$NAICS_CODE %in% "")
final$NAICS_CODE <- replace(final$NAICS_CODE, final$NAICS_CODE =="", NA)
table(is.na(final$NAICS_CODE))

#14)
sum(final$FULL_TIME_POSITION %in% "")
final$FULL_TIME_POSITION <- replace(final$FULL_TIME_POSITION, final$FULL_TIME_POSITION == "", NA)
table(is.na(final$FULL_TIME_POSITION))

#15)
sum(final$PREVAILING_WAGE %in% "")
final$PREVAILING_WAGE <- replace(final$PREVAILING_WAGE, final$PREVAILING_WAGE == "", NA)
table(is.na(final$PREVAILING_WAGE))

#16) 
sum(final$PW_UNIT_OF_PAY %in% "")
final$PW_UNIT_OF_PAY <- replace(final$PW_UNIT_OF_PAY, final$PW_UNIT_OF_PAY == "", NA)
table(is.na(final$PW_UNIT_OF_PAY))

#18)
sum(final$PW_SOURCE %in% "")
final$PW_SOURCE <- replace(final$PW_SOURCE, final$PW_SOURCE == "", NA)
table(is.na(final$PW_SOURCE))

#19) 
sum(final$PW_SOURCE_YEAR %in% "")
final$PW_SOURCE_YEAR <- replace(final$PW_SOURCE_YEAR, final$PW_SOURCE_YEAR == "", NA)
table(is.na(final$PW_SOURCE_YEAR))

#20) 
sum(final$PW_SOURCE_OTHER %in% "")
final$PW_SOURCE_OTHER <- replace(final$PW_SOURCE_OTHER, final$PW_SOURCE_OTHER == "", NA)
table(is.na(final$PW_SOURCE_OTHER))

#21) 
sum(final$WAGE_RATE_OF_PAY_FROM %in% "")
final$WAGE_RATE_OF_PAY_FROM <- replace(final$WAGE_RATE_OF_PAY_FROM, final$WAGE_RATE_OF_PAY_FROM == "", NA) 
table(is.na(final$WAGE_RATE_OF_PAY_FROM))

#22)
sum(final$WAGE_RATE_OF_PAY_TO %in% "")
final$WAGE_RATE_OF_PAY_TO <- replace(final$WAGE_RATE_OF_PAY_TO, final$WAGE_RATE_OF_PAY_TO == "", NA)
table(is.na(final$WAGE_RATE_OF_PAY_TO))

#23)
sum(final$WAGE_UNIT_OF_PAY %in% "")
final$WAGE_UNIT_OF_PAY <- replace(final$WAGE_UNIT_OF_PAY, final$WAGE_UNIT_OF_PAY == "", NA)
table(is.na(final$WAGE_UNIT_OF_PAY))

#24)
sum(final$H1B_DEPENDENT %in% "")
final$H1B_DEPENDENT <- replace(final$H1B_DEPENDENT, final$H1B_DEPENDENT == "", NA)
table(is.na(final$H1B_DEPENDENT))

#25) 
sum(final$WILLFUL_VIOLATOR %in% "")
final$WILLFUL_VIOLATOR <- replace(final$WILLFUL_VIOLATOR, final$WILLFUL_VIOLATOR =="", NA)
table(is.na(final$WILLFUL_VIOLATOR))

#26) 
sum(final$WORKSITE_CITY %in% "")
final$WORKSITE_CITY <- replace(final$WORKSITE_CITY, final$WORKSITE_CITY == "", NA)
table(is.na(final$WORKSITE_CITY))

#27) 
sum(final$WORKSITE_COUNTY %in% "")
final$WORKSITE_COUNTY <- replace(final$WORKSITE_COUNTY, final$WORKSITE_COUNTY == "", NA)
table(is.na(final$WORKSITE_COUNTY))

#28) 
sum(final$WORKSITE_STATE %in% "")
final$WORKSITE_STATE <- replace(final$WORKSITE_STATE, final$WORKSITE_STATE == "", NA)
table(is.na(final$WORKSITE_STATE))
######################################################################

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
kk$PW_SOURCE_YEAR<-as.numeric(as.factor(kk$PW_SOURCE_YEAR))


#CREATING DUMMY VARIABLE:

sum(is.na(kk$WAGE_RATE_OF_PAY_FROM))
sum(is.na(kk$WAGE_RATE_OF_PAY_to))
colnames(df_new)
colnames(table2)
sum(is.na(df_new[25]))
table2[25]

#2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222#
str(kk)
sapply(kk, function(x) sum(is.na(x))) #gives NAs
summary(kk)

table(is.na(kk$FULL_TIME_POSITION)) #348136 NAs
unique(kk$FULL_TIME_POSITION)
summary(kk$FULL_TIME_POSITION)

kk %>%
  group_by(FULL_TIME_POSITION) %>%
  summarise(count = n(), percentage = 100*count/dim(kk)[1])
#there are total 31.3% NA values in FULL_TIME_POSITION
# lets see what happens when we remove all the rows with NAs

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

kk <- kk[!is.na(kk$FULL_TIME_POSITION),] #dataset without any NAs (removed all the NAs)
NROW(kk$CASE_STATUS)

summary(kk$FULL_TIME_POSITION)
summary(kk)
sapply(kk, function(x) sum(is.na(x)))
data.frame(colnames(kk))
# seems like we have a hell lot of rows to operate with...(762928 rows)

unique(kknew$NewSOCNAME)



















#need to create a table YEAR for visualization 
kk1 <- kk
kk1$YEAR <- substring(kk1$DECISION_DATE, 1, 4)
unique(kk1$YEAR)

# Generic ggplot graphics configuration
get_theme <- function() {
  return(theme(axis.title = element_text(size = rel(1.5)),
               legend.position = "bottom",
               legend.text = element_text(size = rel(1.5)),
               legend.title = element_text(size=rel(1.5)),
               axis.text = element_text(size=rel(1.5))))
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#333333333333333333333333333333333333333333333333333333333333333333333333333333333333333#
kk %>%
  group_by(PREVAILING_WAGE) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
kk %>%
  group_by(WAGE_RATE_OF_PAY_TO) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


min(kk$PREVAILING_WAGE)

kk %>%
  group_by(SOC_NAME) %>%
  summarise(count = n(), percentage = 100*count/dim(kk)[1]) %>%
  arrange(desc(count))

unique(kk$EMPLOYER_COUNTRY) #need to remove this column [9]
kk <- kk[,-c(9)]
data.frame(colnames(kk))
sapply(kk, function(x) sum(is.na(x)))
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!! TO CONVERT HOURLY WAGE INTO YEARLY WAGE !!!!!!!!!!!!!!!!!!!!!!!!!#
summary(kk)
unit <- kk %>% filter(PW_UNIT_OF_PAY == "Hour")
NROW(unit)

unit$PREVAILING_WAGE <- unit$PREVAILING_WAGE * 2080 #(52*40 = 2080)
max(unit$PREVAILING_WAGE)
min(unit$PREVAILING_WAGE)

NROW(kk$PREVAILING_WAGE)

kk2 <- kk %>% filter(!PW_UNIT_OF_PAY == "Hour")
NROW(kk2)


kk <- rbind(unit, kk2)
NROW(kk$PREVAILING_WAGE)
max(kk$PREVAILING_WAGE)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# !!!!!!!!!!!!!!!!!!!!!!!!!! DON'T USE BELOW CODE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
summary(kk)
unit <- kk %>% filter(PW_UNIT_OF_PAY == "Hour") %>%
  select(PREVAILING_WAGE <= 50)
nrow(unit)

unit$PREVAILING_WAGE <- unit$PREVAILING_WAGE * 8760
max(unit$PREVAILING_WAGE)
min(unit$PREVAILING_WAGE)



#columns with prevailing wage less than $50 and then converting it into yearly wage
unit1 <- kk %>%
  filter(PREVAILING_WAGE <= 50) 
NROW(unit1)  
unit1$PREVAILING_WAGE <- unit1$PREVAILING_WAGE * 8760
max(unit1$PREVAILING_WAGE)

#removing rows with prevailing wage less than $50 from the dataset
a <- with(kk, which(kk$PREVAILING_WAGE <= 50, arr.ind=TRUE))
NROW(a)
kk2 <- kk[-a,]
max(kk2$PREVAILING_WAGE)

#now, adding both columns 
new_wage <- rbind(kk2, unit1)
NROW(new_wage)
new_wage <- new_wage %>% 
  mutate(PW_UNIT_OF_PAY = ifelse(str_detect(PW_UNIT_OF_PAY, "Hour"), "Year", PW_UNIT_OF_PAY))

max(new_wage$PREVAILING_WAGE)
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


#44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444#
kk$SOC_NAME <- tolower(kk$SOC_NAME)

kknew<-kk %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"computer"),'Computers Related',SOC_NAME))
bk <- kknew %>% filter(NewSOCNAME == "Computers Related")

#309804

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"programmer"),'Computers Related',NewSOCNAME))
bk <- kknew %>% filter(NewSOCNAME == "Computers Related")

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"software"),'Computers Related',NewSOCNAME))
bk <- kknew %>% filter(NewSOCNAME == "Computers Related")
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"software developer"),'Computers Related',NewSOCNAME))
bk <- kknew %>% filter(NewSOCNAME == "Computers Related")
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"//.net"),'Computers Related',NewSOCNAME))
bk <- kknew %>% filter(NewSOCNAME == "Computers Related")


#Data Related
############
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"database"),'Data Related',NewSOCNAME))
ab<-kknew%>%filter(NewSOCNAME=='Data Related'|NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"developer"),'Data Related',NewSOCNAME))
ab<-kknew%>%filter(NewSOCNAME=='Data Related'|NewSOCNAME=='Data Related')
############
unique(kknew$NewSOCNAME)

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"^data"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"data analyst"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"business intelligence"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"business inelligence"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"business analysts"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"business analyst"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"professional data warehouse"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME," data"),'Data Related',NewSOCNAME))
ab<-kknew %>% filter(NewSOCNAME=='Data Related')

#Education
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"^education"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"professor$"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"teachers"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"teacher"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"tutor"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"professor"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"professor"),'Education Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Education Related')



#Marketing and finance related
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"financial"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"fiancial"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"market"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"marketing"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"sales agents"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew<- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"loan"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')
kknew<- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"accountant"),'Marketing and Finance Related',NewSOCNAME))
fin<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Marketing and Finance Related')

#Mnagement
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"manager"),'Management Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Management Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"management"),'Management Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Management Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"mangement"),'Management Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Management Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"managment"),'Management Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Management Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"manger"),'Management Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Management Related')
#MEDICAL
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"^healthcare"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"medicine"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"dentist"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"doctor"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"surgeon"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"medical"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"therapist"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"health"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"radiologist"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"speech and language"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"biological"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"psychiatrists"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"psychiatrist"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"physician"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"nurse"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"nurses"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"hospitalists"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"childcare"),'Medical and Healthcare Related',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Medical and Healthcare Related')
#Agriculture

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"farmworker"),'Agriculture and Related',NewSOCNAME))
ab <- kknew %>% select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Agriculture and Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"^Agriculture"),'Agriculture and Related',NewSOCNAME))
ab <- kknew %>% select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Agriculture and Related')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"agriculture"),'Agriculture and Related',NewSOCNAME))
ab<-kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME=='Agriculture and Related')


#Law

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"lawyer"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"police"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"attorney"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"paralegal"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')

unique(kknew$NewSOCNAME)

#Engineering 
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"systems analysts"),'Engineering and Science ',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"graphic designer"),'Engineering and Science ',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"geneticists"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"information security"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"network administrators"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"network administrator"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"enigneers"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"engineering"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"physicist"),'Engineering and Science ',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"statistic"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"sociologist"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"scientist"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"engineers"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"engineer"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"scientists"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"social science"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"bioinformatic"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
unique(kknew$NewSOCNAME)
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"engineers"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"mathematician"),'Engineering and Science',NewSOCNAME))
ab<-kknew%>%select(NewSOCNAME,SOC_NAME)%>%filter(NewSOCNAME=='Engineering and Science')
#Operations and Logistics


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"logistic"),'Operations and Logistics',NewSOCNAME))

ab<-kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME == 'Operations and Logistics')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"operation research"),'Operations and Logistics',NewSOCNAME))

ab <- kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME =='Operations and Logistics')
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"operations"),'Operations and Logistics',NewSOCNAME))
ab <- kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME =='Operations and Logistics')

#Artists

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"musician"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"poets"),'Artist Related',NewSOCNAME))
ab<-kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME == 'Artist Related')

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"dancers"),'Artist Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"artists"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"choreographer"),'Artist Related',NewSOCNAME))
ab<-kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME == 'Artist Related')



#technicians and workers
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"technician"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"workers"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'electrician'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'electricians'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'electrician'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'painters'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'die makers'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,'repairer'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'millwrights'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'bartender'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'cooks'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'tour guide'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'tailor'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'welders'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'machinist'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME, 'machine operators'),'technicians and workers',NewSOCNAME))
ab<-kknew %>% select(NewSOCNAME,SOC_NAME) %>% filter(NewSOCNAME =='technicians and workers')
unique(kknew$NewSOCNAME)

summary(df_new)
yello <- 1926862-647852
unique(df_new$JOB_TITLE)
unique(df_new$SOC_CODE)
k <- df_new %>% filter(df_new$CASE_STATUS == "CERTIFIED")
unique(df_new$NAICS_CODE)
unique()



#555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555#
# DATA ANALYSIS #
# at this stage out data is ready.....Almost!!

# We need to split the data into train(60%) and test(40%) datasets
set.seed(123)
n <- nrow(kk1)
shuffled_df <- kk1[sample(n), ]
train_indices <- 1:round(0.6 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.6 * n) + 1):n
test <- shuffled_df[test_indices, ]

NROW(kk1$CASE_STATUS)
NROW(train$CASE_STATUS)
NROW(test$CASE_STATUS)

# Firstly we can plot verticle bar charts to find out the proportion of outcome variable in out data
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(phenofit)
sapply(train, function(x) sum(is.na(x)))
colnames(train)

train %>% ggplot(aes(x = CASE_STATUS)) +
  geom_bar()

#As we can see here,our data is highly imbalanced like hell, so we have to balance it first


#applications per state
train %>% ggplot(aes(y = EMPLOYER_STATE)) +
  geom_bar()

#prevailing wage per year
train %>% ggplot(aes(x = YEAR, y = PREVAILING_WAGE)) +
  geom_boxplot()

#applications per year
train %>% ggplot(aes(x = YEAR)) +
  geom_bar() 

train %>% ggplot(aes(x = FULL_TIME_POSITION)) +
  geom_bar() 



unique(kk1$EMPLOYER_STATE)
unique(kk1$JOB_TITLE)
unique(kk1$SOC_NAME)


#top 5 states by visa status
#full time position per state
#job title vs. visa status
