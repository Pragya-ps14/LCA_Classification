

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

unique(kk$EMPLOYER_COUNTRY)
sapply(kk, function(x) sum(is.na(x)))


# 280637, 


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




