str(train_under)
train_under$CASE_STATUSDENIED <- as.factor(train_under$CASE_STATUSDENIED)
train_under$FULL_TIME_POSITIONY <- as.factor(train_under$FULL_TIME_POSITIONY)
train_under$PW_SOURCEDBA <- as.factor(train_under$PW_SOURCEDBA)
train_under$PW_SOURCEOES <- as.factor(train_under$PW_SOURCEOES)
train_under$PW_SOURCEOther <- as.factor(train_under$PW_SOURCEOther)
train_under$PW_SOURCESCA <- as.factor(train_under$PW_SOURCESCA)
train_under$H1B_DEPENDENTY <- as.factor(train_under$H1B_DEPENDENTY)
train_under$WILLFUL_VIOLATORY <- as.factor(train_under$WILLFUL_VIOLATORY)
train_under$NewSOCNAMEArts..Design.Entertainment.Sports..and.Media.Occupations <- as.factor(train_under$NewSOCNAMEArts..Design.Entertainment.Sports..and.Media.Occupations)
train_under$NewSOCNAMEBuilding.and.Grounds.Cleaning.Maintenance.Occupations <- as.factor(train_under$NewSOCNAMEBuilding.and.Grounds.Cleaning.Maintenance.Occupations)
train_under$NewSOCNAMEBusiness.and.Financial.Operations.Occupations <- as.factor(train_under$NewSOCNAMEBusiness.and.Financial.Operations.Occupations)
train_under$NewSOCNAMECommunity.and.Social.Service.Occupations <- as.factor(train_under$NewSOCNAMECommunity.and.Social.Service.Occupations)
train_under$NewSOCNAMEComputer.and.Mathematical.Operations.Occupations <- as.factor(train_under$NewSOCNAMEComputer.and.Mathematical.Operations.Occupations)
train_under$NewSOCNAMEConstruction.and.Extraction.Occupations <- as.factor(train_under$NewSOCNAMEConstruction.and.Extraction.Occupations)
train_under$NewSOCNAMEEducational.Instrction.and.Library.Occupations <- as.factor(train_under$NewSOCNAMEEducational.Instrction.and.Library.Occupations)
train_under$NewSOCNAMEFarming.Fishing.and.Forestry.Occupations <- as.factor(train_under$NewSOCNAMEFarming.Fishing.and.Forestry.Occupations)
train_under$NewSOCNAMEFood.Preparation.and.Serving.Related.Occupations <- as.factor(train_under$NewSOCNAMEFood.Preparation.and.Serving.Related.Occupations)
train_under$NewSOCNAMEHealthcare.Practitioners.and.Technical.Occupations <- as.factor(train_under$NewSOCNAMEHealthcare.Practitioners.and.Technical.Occupations)
train_under$NewSOCNAMEHealthcare.Support.Occupations <- as.factor(train_under$NewSOCNAMEHealthcare.Support.Occupations)
train_under$NewSOCNAMEInstallation..Repair.and.Maintenance.Occupations <- as.factor(train_under$NewSOCNAMEInstallation..Repair.and.Maintenance.Occupations)
train_under$NewSOCNAMELegal.Occupations <- as.factor(train_under$NewSOCNAMELegal.Occupations)
train_under$NewSOCNAMELife.Physical.and.Social.Science.Occupations <- as.factor(train_under$NewSOCNAMELife.Physical.and.Social.Science.Occupations)
train_under$NewSOCNAMEManagement.Occupations <- as.factor(train_under$NewSOCNAMEManagement.Occupations)
train_under$NewSOCNAMEOffice.and.Administrative.Support.Occupations <- as.factor(train_under$NewSOCNAMEOffice.and.Administrative.Support.Occupations)
train_under$NewSOCNAMEPersoanl.Care.and.Service.Occupations <- as.factor(train_under$NewSOCNAMEPersoanl.Care.and.Service.Occupations)
train_under$NewSOCNAMEProduction.Occupations <- as.factor(train_under$NewSOCNAMEProduction.Occupations)
train_under$NewSOCNAMEProtective.Service.Occupations <- as.factor(train_under$NewSOCNAMEProtective.Service.Occupations)
train_under$NewSOCNAMESales.and.Related.Occupations <- as.factor(train_under$NewSOCNAMESales.and.Related.Occupations)
train_under$NewSOCNAMETransportation.and.Material.Moving.Occupations <- as.factor(train_under$NewSOCNAMETransportation.and.Material.Moving.Occupations)
train_under$SUBMITTED_MONTH <- as.factor(train_under$SUBMITTED_MONTH)
train_under$TOTAL_WORKERS <- as.factor(train_under$TOTAL_WORKERS)
train_under$Employment_Time <- as.factor(train_under$Employment_Time)
train_under$PREVAILING_WAGE <- as.factor(train_under$PREVAILING_WAGE)
str(train_under)
#
logistic <- glm(CASE_STATUSDENIED ~ FULL_TIME_POSITIONY, data = train_under, family = "binomial")
summary(logistic)

#
str(train_under)
data.frame(colnames(train_under))

train <- train_under[-c(14,27)]
str(train_under)
train_under$PREVAILING_WAGE <- as.numeric(train_under$PREVAILING_WAGE)

logistic1 <- glm(CASE_STATUSDENIED~PREVAILING_WAGE, data = train, family = "binomial") #not running
summary(logistic1)

prop.table(table(train_under$CASE_STATUSDENIED))



