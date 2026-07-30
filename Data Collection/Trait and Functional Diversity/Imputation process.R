# The following script describes how to impute the missing trait values
# note combined trait set is referred to as 
# Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PDS

#STEP 1: INSTALL AND LOAD 

install.packages("mice")
library(mice)

#STEP 2: CHECK AND CHANGE THE DISPERSAL MODE TO FACTOR 

VAST_IMPUTATE<- Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PDS
VAST_IMPUTATE <- str(VAST_IMPUTATE)

VAST_IMPUTATE$Dispersal <- as.factor(VAST_IMPUTATE$Dispersal)

#STEP 3: CHECK THAT ALL THE NUMERIC TRAITS HAVE BEEN READ IN CORRECTLY
VAST_IMPUTATE$"INSET TRAIT NAME" <- as.numeric(VAST_IMPUTATE$"INSET TRAIT NAME")

#STEP 3: PERFORM THE IMPUTATION PROCESS USING MICE 
# Impute missing values using MICE
imp2 <- mice(VAST_IMPUTATE, method = "rf", m = 10) #NOTE - m can be changes depending on the missing data

#STEP 4: GENERATE THE COMPLETE DATASET
complete_data <- complete(imp2)

write.csv(complete_data, "TRAIT_data.csv")

