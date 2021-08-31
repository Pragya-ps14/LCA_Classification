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







h1b %>% ggplot(aes(x = CASE_STATUS)) +
  geom_bar(col = "black", fill = "lightblue")
#As we can see here,our data is highly imbalanced like hell, so we have to balance it first

#applications per state
h1b %>% ggplot(aes(y = EMPLOYER_STATE)) +
  geom_bar(col = "blue")

#applications per year
h1b %>% ggplot(aes(x = YEAR)) +
  geom_bar() 

h1b %>% ggplot(aes(x = FULL_TIME_POSITION)) +
  geom_bar(col = "black", fill = "green") 


h1b %>% ggplot(aes(y = WORKSITE_STATE, fill = WORKSITE_STATE)) +
  geom_bar()


unique(kk1$EMPLOYER_STATE)
unique(kk1$JOB_TITLE)
unique(kk1$SOC_NAME)


#top 5 states by visa status
#full time position per state
#job title vs. visa status


# PIE CHART

mytable <- table(o$WORKSITE_STATE)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart")

#
library(GGally)
data.frame(colnames(kknew))




kknew %>%
  group_by(EMPLOYER_NAME) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


mytable <- table(kknew$EMPLOYER_NAME)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls,
    main="Pie Chart")






