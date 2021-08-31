h1b <- read.csv("C:/Users/avdho/Desktop/SEM/DATA MINING/Project/data_to_work_with2/h1b.csv", na.strings="", stringsAsFactors=FALSE)
h1b <- h1b[-c(1)]

summary(h1b)
library(dplyr)
str(h1b)
h1b$PREVAILING_WAGE <- as.factor(h1b$PREVAILING_WAGE)
h1b$PREVAILING_WAGE <- as.numeric(h1b$PREVAILING_WAGE)
max(h1b$PREVAILING_WAGE)
min(h1b$PREVAILING_WAGE)
h1b <- h1b %>%
  group_by(PREVAILING_WAGE) %>%
  filter(PREVAILING_WAGE >= 10000)

NROW(h1b$CASE_STATUS)

library(ggplot2)
library(cowplot)
library(randomForest)
head(h1b)

h1b$STATUS <- as.numeric(ifelse(h1b$CASE_STATUS == "DENIED", 0, 1))
h1b$FULL_TIME <- as.numeric(ifelse(h1b$FULL_TIME_POSITION == "N", 0, 1))
h1b$H1B_dependent <- as.numeric(ifelse(h1b$H1B_DEPENDENT == "N", 0, 1))
h1b$Violator <- as.numeric(ifelse(h1b$WILLFUL_VIOLATOR == "N", 0, 1))
h1b <- h1b[-c(1,3,6,7)]
data.frame(colnames(h1b))

h1b$FULL_TIME <- as.factor(h1b$FULL_TIME)
h1b$H1B_dependent <- as.factor(h1b$H1B_dependent)
h1b$Violator <- as.factor(h1b$Violator)
h1b$STATUS <- as.factor(h1b$STATUS)



summary(h1b)
# Split the data
library(caret)
set.seed(123)
splitIndex <- createDataPartition(h1b$STATUS, p=0.6, 
                                  list=FALSE, 
                                  times = 1)
train <- h1b[splitIndex,]
test <- h1b[-splitIndex,]

# Under-sampling training dataset
library(ROSE)
train_u <- ovun.sample(STATUS~., data = train, method = "under")$data
table(train_u$STATUS)
NROW(train_u)

str(train_u)
train_u$PREVAILING_WAGE <- as.factor(train_u$PREVAILING_WAGE)
train_u$PREVAILING_WAGE <- as.numeric(train_u$PREVAILING_WAGE)
train_u$PW_SOURCE <- as.factor(train_u$PW_SOURCE)
train_u$NewSOCNAME <- as.factor(train_u$NewSOCNAME)
sapply(train_u, function(x) sum(is.na(x)))


model_rm1 <- randomForest(STATUS ~., data=train_u, proximity=TRUE)
model_rm1


# We can plot the error rate to see if 500 trees are enough 
model_rm1$err.rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model_rm1$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(model_rm1$err.rate)),
  Error=c(model_rm1$err.rate[,"OOB"], 
          model_rm1$err.rate[,"1"], 
          model_rm1$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# 1000 trees
model_rm2 <- randomForest(STATUS ~., data=train_u, ntree = 1000, proximity=TRUE)
model_rm2

model_rm2$err.rate
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model_rm2$err.rate), times=3),
  Type=rep(c("OOB", "1", "0"), each=nrow(model_rm2$err.rate)),
  Error=c(model_rm2$err.rate[,"OOB"], 
          model_rm2$err.rate[,"1"], 
          model_rm2$err.rate[,"0"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type))

# We can see that error rate remains constant after 150 trees
# OOB error also remains same in both models

summary(h1b)

# changing split
oob.values <- vector(length=9)
for(i in 1:9) {
  model_rm2 <- randomForest(STATUS ~., data=train_u, mtry=i, ntree=1000)
  oob.values[i] <- model_rm2$err.rate[nrow(model_rm2$err.rate),1]
}
oob.values
#find minimum error
min(oob.values)
#find the optimal value of for mtry
which(oob.values == min(oob.values))

opt_model <- randomForest(STATUS~., 
                          data=train_u,
                          ntree=1000, 
                          proximity=TRUE, 
                          mtry=which(oob.values == min(oob.values)))
opt_model



## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-opt_model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       Status=train_u$STATUS)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=Status)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# ggsave(file="random_forest_mds_plot.pdf")


