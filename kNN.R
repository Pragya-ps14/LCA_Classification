h1b <- read.csv("C:/Users/avdho/Desktop/SEM/DATA MINING/Project/data_to_work_with2/h1b.csv", na.strings="", stringsAsFactors=FALSE)
h1b <- h1b[-c(1)]

data.frame(colnames(h1b))
library(dplyr)
library(ggplot2)
library(cowplot)
# to check how many values each observation has for CERTIFIED / DENIED condition 
xtabs(~CASE_STATUS + FULL_TIME_POSITION, data = h1b)
xtabs(~CASE_STATUS + TOTAL_WORKERS, data = h1b)
xtabs(~CASE_STATUS + PW_SOURCE, data = h1b)
xtabs(~CASE_STATUS + H1B_DEPENDENT, data = h1b)
xtabs(~CASE_STATUS + WILLFUL_VIOLATOR, data = h1b)
xtabs(~CASE_STATUS + NewSOCNAME, data = h1b)
xtabs(~CASE_STATUS + SUBMITTED_MONTH, data = h1b)

# now lets make test and train sets
h1b %>%
  group_by(CASE_STATUS) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
data.frame(colnames(h1b))


# there are only 9643 DENIED cases in the set. Dataset is highly imbalanced 
# we need to balance the dataset while we split it 
set.seed(123)
# now we need to conver the variables into numerical 
library(caret)
l <- h1b[c(1,3,5,6,7,8)]
m <- h1b[-c(1,3,5,6,7,8)]

dmy <- dummyVars("~.", data = l, fullRank = T)
trsf <- data.frame(predict(dmy, newdata = l))
trsf

data.frame(colnames(m))
logist_reg <- cbind(trsf, m)
data.frame(colnames(logist_reg))


#rm(dmy, h1b, l, m, trsf) # removing all datasets which i will not use here

logist_reg$CASE_STATUSDENIED <- as.factor(logist_reg$CASE_STATUSDENIED)
logist_reg$PREVAILING_WAGE <- as.factor(logist_reg$PREVAILING_WAGE)
logist_reg$PREVAILING_WAGE <- as.numeric(logist_reg$PREVAILING_WAGE)
logist <- logist_reg %>%
  group_by(PREVAILING_WAGE) %>%
  filter(PREVAILING_WAGE >= 10000)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

logist_reg_n <- as.data.frame(lapply(logist_reg[,2:33], normalize))

head(logist_reg)

###############################################################################
###
library(DMwR)
library(caret)
#str(logist)
#prop.table(table(logist$CASE_STATUSDENIED))

#splitIndex <- createDataPartition(logist$CASE_STATUSDENIED, p=0.6, 
                                  list=FALSE, 
                                  times = 1)
#train <- logist[splitIndex,]
#test <- logist[-splitIndex,]

#NROW(logist)
#NROW(train)
#NROW(test)

set.seed(123)
dat.d <- sample(1:nrow(logist_reg_n),size=nrow(logist_reg_n)*0.7,replace = FALSE) #random selection of 70% data.

train <- logist_reg[dat.d,] # 70% training data
test <- logist_reg[-dat.d,] # remaining 30% test data
NROW(train)

# Under-sampling training dataset
library(ROSE)
log.train <- ovun.sample(CASE_STATUSDENIED~., data = train, method = "under")$data

table(log.train$CASE_STATUSDENIED)
NROW(log.train)

summary(log.train)

NROW(log.train)
NROW(train_labels)
NROW(test)

train_labels <- logist_reg[dat.d,1]
test_labels <-logist_reg[-dat.d,1]


library(class)
knn.95 <- knn(train=train, test=test, cl=train_labels, k=95)
knn.96 <- knn(train=log.train, test=test, cl=train_labels, k=96)


ACC.95 <- 100 * sum(test[,1] == knn.95)/NROW(test[,1])
ACC.96 <- 100 * sum(test[,1] == knn.96)/NROW(test[,1])

