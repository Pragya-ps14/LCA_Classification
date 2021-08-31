h1b <- read.csv("C:/Users/avdho/Desktop/SEM/DATA MINING/Project/data_to_work_with2/h1b.csv", na.strings="", stringsAsFactors=FALSE)
h1b <- h1b[-c(1)]
#LOGISTIC REGRESSION :
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
# Creating Dummy variables  
library(caret)
l <- h1b[c(3,5,6,7,8)]
m <- h1b[-c(3,5,6,7,8)]

dmy <- dummyVars("~.", data = l, fullRank = T)
trsf <- data.frame(predict(dmy, newdata = l))
trsf

data.frame(colnames(m))
logist_reg <- cbind(trsf, m) #combining newly created dummy variable and other variables 
data.frame(colnames(logist_reg))

logist_reg$CASE_STATUS <- ifelse(logist_reg$CASE_STATUS == "CERTIFIED", 1, 0)

# converting variables into their appropriate data types 
logist_reg$CASE_STATUSDENIED <- as.factor(logist_reg$CASE_STATUSDENIED)
logist_reg$CASE_STATUSDENIED <- as.numeric(logist_reg$CASE_STATUSDENIED)
logist_reg$PREVAILING_WAGE <- as.factor(logist_reg$PREVAILING_WAGE)
logist_reg$PREVAILING_WAGE <- as.numeric(logist_reg$PREVAILING_WAGE)

# filtering values in data set 
logist <- logist_reg %>%
  group_by(PREVAILING_WAGE) %>%
  filter(PREVAILING_WAGE >= 10000)
str(logist)

# normalizing dataset 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }
logist <- as.data.frame(lapply(logist_reg, normalize))

###
library(DMwR)
library(caret)
str(logist)
prop.table(table(logist$CASE_STATUS)) 

# Splitting the dataset 
set.seed(123)
dat.d <- sample(1:nrow(logist),size=nrow(logist)*0.6,replace = FALSE) #random selection of 70% data.

train <- logist[dat.d,] # 60% training data
test <- logist[-dat.d,] # remaining 40% test data
NROW(train)

NROW(logist)
NROW(train)
NROW(test)

# Under-sampling training dataset
library(ROSE)
log.train <- ovun.sample(CASE_STATUS~., data = train, method = "under")$data
table(log.train$CASE_STATUS)
NROW(log.train)
str(log.train)
log.train$CASE_STATUS <- as.factor(log.train$CASE_STATUS)
# Feature Scaling 
log.train[-29] <- scale(log.train[-29])
test[-29] <- scale(test[-29])

# Fitting SVM to training data-set
library(e1071)

classifier <- svm(formula = CASE_STATUS~., 
                  data = log.train, 
                  type = "C-classification",
                  kernel = "linear")

classifier
print(classifier)


library(dplyr)

# Predicting test set results 
y_pred <- predict(classifier, newdata = test[-29])
y_pred
# Making the confusion matrix
cm <- table(test[,29], y_pred)
cm



# Need to plot the results
print(y_pred)
NROW(y_pred)
NROW(test)
plot(classifier, train)
data.frame(colnames(h1b))
NROW(h1b)










