data.frame(colnames(h1b))

table(h1b$CASE_STATUS)


library(caret)
#FULL_TIME, PW_SOURCE, H1B_DEPENDENT, WWLLFUL_VIOLATOR, [NewSOCNAMES]
experiment <- h1b[-c(2)]
data.frame(colnames(experiment))

e <- experiment[c(2,4,6,7,8,11)]
f <- experiment[-c(2,4,6,7,8,11)]
#
dmy <- dummyVars("~.", data = e, fullRank = T)
trsf <- data.frame(predict(dmy, newdata = e))
trsf
#

data.frame(colnames(f))
kk2 <- cbind(trsf, f)
data.frame(colnames(experiment))

kk3 <- kk3[-c(33,34,37)]



#RANDOM FOREST
install.packages("randomForest")
library(randomForest)

#splitting dataset
set.seed(123)
n <- nrow(experiment)
shuffled_df <- experiment[sample(n), ]
train_indices <- 1:round(0.6 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.6 * n) + 1):n
test <- shuffled_df[test_indices, ]

NROW(experiment$CASE_STATUS)
NROW(train$CASE_STATUS)
NROW(test$CASE_STATUS)

#random forest model
data.frame(colnames(train))
rftrain <- randomForest(CASE_STATUS~JOB_TITLE + TOTAL_WORKERS + FULL_TIME_POSITION + PREVAILING_WAGE + PW_SOURCE +
                          H1B_DEPENDENT + WILLFUL_VIOLATOR + WORKSITE_STATE + SUBMITTED_MONTH + NewSOCNAME + YEAR + Employment_Time, data = train)

sapply(train, function(x) sum(is.na(x)))


data.frame(colnames(experiment))


h1b$Apps_per_emp <- h1b %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))





data.frame(colnames(kk))
data.frame(colnames(h1b))
