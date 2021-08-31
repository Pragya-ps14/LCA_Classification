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
prop.table(table(logist$CASE_STATUSDENIED)) 

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

#applying logistic regression model
lm <- glm(CASE_STATUS~., data = log.train, family = "binomial")
summary(lm)
lm
library(bestglm)
library(leaps)
library(VGAM)
library(stats4)
library(splines)
library(rpart)
library(randomForest)
library(regclass)
confusion_matrix(lm)

data.frame(colnames(log.train))
# Applying model to the test data-set
test_result <- predict(lm,newdata=subset(test,select=c(1:28, 30:33)),type='response')
test_result
test_result <- ifelse(test_result > 0.5,1,0)

#let's calculate misclassification error
misClasificError <- mean(test_result != test$CASE_STATUS)
print(paste('Accuracy',1-misClasificError))


#calculate R^2 value
ll.null <- lm$null.deviance/-2
ll.proposed <- lm$deviance/-2
(ll.null - ll.proposed)/ll.null

#calculate p-value for that R^2 value using a Chi-square distribution 
1 - pchisq(2*(ll.proposed - ll.null), df=(length(lm$coefficients)-1))
# here the p-value is tiny (i.e., 0), so we can say that R^2 value was not out of dumb luck!

#plot the graph showing predicted probabilities that each application with it's actual status
# creating a data.frame() that contains probability of getting denied along with actual status
predicted.data <- data.frame(probability.of.status=lm$fitted.values,
                             Status = log.train$CASE_STATUSDENIED)

#sorting the data frame from low probabilities to high probabilities 
predicted.data <- predicted.data[order(predicted.data$probability.of.status,
                                       decreasing = FALSE),]
#adding a new column containing rank of each sample from low probability to high probability
predicted.data$rank <- 1:nrow(predicted.data)

library(ggplot2)
library(cowplot)
ggplot(data=predicted.data, aes(x=rank, y=probability.of.status)) +
  geom_point(aes(color=Status), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of Case Status")

# IN ABOVE MODEL : DENIED = 1 & ACCEPTED = 0



