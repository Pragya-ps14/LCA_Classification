
#!!!!!!!!!!!!!!!!!!!!! TO CONVERT HOURLY WAGE INTO YEARLY WAGE !!!!!!!!!!!!!!!!!!!!!!!!!#
################################### USE kk1 (NAs removed) ###############################
summary(kk1)
unit <- kk1 %>% filter(PW_UNIT_OF_PAY == "Hour")
NROW(unit)

unit$PREVAILING_WAGE <- unit$PREVAILING_WAGE * 2080 #(52*40 = 2080)
max(unit$PREVAILING_WAGE)
min(unit$PREVAILING_WAGE)

NROW(kk1$PREVAILING_WAGE)

kk2 <- kk1 %>% filter(!PW_UNIT_OF_PAY == "Hour")
NROW(kk2)


kk1 <- rbind(unit, kk2)
NROW(kk1$PREVAILING_WAGE)
max(kk1$PREVAILING_WAGE)
min(kk1$PREVAILING_WAGE)





#################### CREATING DUMMY VARIABLES ######################################
library(dummies)
#1) CASE_STATUS -> Certified/Denied
as.data.frame(model.matrix(~0 + CASE_STATUS, data = kk1))

data.frame(colnames(kk1))

unique(kk1$CASE_STATUS)

kk1$STATUS <- as.numeric(ifelse(kk1$CASE_STATUS == "DENIED", 0, 1))
unique(kk1$STATUS)

#2) PROCESSING_TIME = DECISION_DATE - CASE_SUBMITTED
kk1 %>%
  group_by(CASE_SUBMITTED) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

str(kk1)
kk1$process_time <- as.Date(as.character(kk1$DECISION_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kk1$CASE_SUBMITTED), format = "%Y-%m-%d")
summary(kk1)
unique(kk1$process_time)                               


#3) Employment_time = EMPLOYMENT_END_DATE - EMPLOYMENT_START_DATE
str(kk1)
kk1$Employment_Time <- as.Date(as.character(kk1$EMPLOYMENT_END_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kk1$EMPLOYMENT_START_DATE), format = "%Y-%m-%d")
summary(kk1)
max(kk1$Employment_Time)

#4) FULL_TIME_POSITION
unique(kk1$FULL_TIME_POSITION)

kk1$POSITION <- as.numeric(ifelse(kk1$FULL_TIME_POSITION == "N", 0, 1))
unique(kk1$POSITION)

data.frame(colnames(kk1))

#5) H1B_DEPENDENT
unique(kk1$H1B_DEPENDENT)

kk1$H1B_Dependent <- as.numeric(ifelse(kk1$H1B_DEPENDENT == "N", 0, 1))
unique(kk1$H1B_Dependent)

#6) WILLFUL_VIOLATOR
unique(kk1$WILLFUL_VIOLATOR)

kk1$Willful_Violator <- as.numeric(ifelse(kk1$WILLFUL_VIOLATOR == "N", 0, 1))
unique(kk1$Willful_Violator)

data.frame(colnames(kk1))

k <- kk1[-c(1,2,3,4,5,9,10,12,14,16,20,21,22,23,24,25,26)]
data.frame(colnames(k))


#################### CREATING DUMMY VARIABLES ######################################

data.frame(colnames(kk2))
library(caret)

l <- kk2[c(1,16,19,25,26)]
m <- kk2[-c(1,16,19,25,26)]
#
dmy <- dummyVars("~.", data = l)
trsf <- data.frame(predict(dmy, newdata = l))
trsf
#

data.frame(colnames(m))
kk3 <- cbind(trsf, m)
data.frame(colnames(kk3))

#2) PROCESSING_TIME = DECISION_DATE - CASE_SUBMITTED
kk3 %>%
  group_by(CASE_SUBMITTED) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

str(kk3)
kk3$process_time <- as.Date(as.character(kk3$DECISION_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kk3$CASE_SUBMITTED), format = "%Y-%m-%d")

unique(kk3$process_time)                               


#3) Employment_time = EMPLOYMENT_END_DATE - EMPLOYMENT_START_DATE

kk3$Employment_Time <- as.Date(as.character(kk3$EMPLOYMENT_END_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kk3$EMPLOYMENT_START_DATE), format = "%Y-%m-%d")

max(kk3$Employment_Time)

data.frame(colnames(kk3))
kk3 <- kk3[-c()]

#SCREE PLOT
library(psych)
str(k)
k_new <- k[,c(6, 7, 16, 17, 18)]
new_k <- fa.parallel(k_new, fa = 'pc', n.iter=100, show.legend = FALSE, main = "Scree plot")

unique(kk1$PW_SOURCE)













