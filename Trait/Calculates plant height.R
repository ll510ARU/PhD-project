# The following script describes how sla was generated from TRY, BIEN, BROT 
# note combine sla together use dataset with 
#all the resolved species names and crops removed (Vascular_plants_list)

#STEP 1: INSTALL AND LOAD 

install.packages(readr)    
install.packages(tidyverse)    
install.packages(taxize)   
install.packages(bdc)   
install.packages("TR8")


library(readr)    
library(tidyverse)    
library(taxize)   
library(bdc)
library(TR8)


#STEP 2: IMPORT THE TRY DATA

data <- read_delim("FILE NAME.txt", delim = "\t") 

#Isolate the study species 
names <- Vascular_plants_list$species

#STEP 3: SELECT THE PLANT HIEGHT (PH) TRAIT AND EXACT THE SPECIES NAMES
PH <- subset(data, TraitName == "Plant height")
PH_unique <- unique(PH$AccSpeciesName)
PH_unique <- as.data.frame(PH_unique)
PH_EX<-PH_unique %>% filter(!PH_unique %in% names)


#STEP 4: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT
# Separate the names into subsets for 1000 rows, this is to speed up the process
#And to check that each subset.

subset1_row <- 1:1000
subset2_row <- 1001:2000
subset3_row <- 2001:2477



PH_EX1 <-PH_EX$PH_unique[subset1_row]
PH_EX1 <- as.data.frame(PH_EX1)
PH_EX2 <-PH_EX$PH_unique[subset2_row]
PH_EX2 <- as.data.frame(PH_EX2)
PH_EX3 <-PH_EX$PH_unique[subset3_row]
PH_EX3 <- as.data.frame(PH_EX3)

names(PH_EX1) <- "species"
names(PH_EX2) <- "species"
names(PH_EX3) <- "species"



# Resolve the species using GBIF Backbone Taxonomy
resolve_PH_EX1 <-PH_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX2 <-PH_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX3 <-PH_EX3$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_PH_EX <- rbind(resolve_PH_EX1, resolve_PH_EX2, resolve_PH_EX3)


#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
colnames(resolve_PH_EX)[colnames(resolve_PH_EX) == "user_supplied_name"]<- "Species names"
names(PH_EX) <- "Species names"
results_df<- PH_EX %>% left_join(resolve_PH_EX, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
results_df$`Accepted names`<- NA
filPH4_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_filPH4_result_df <- filPH4_result_df %>% distinct(`Species names`, .keep_all = TRUE)

write.csv(dis_filPH4_result_df, "resolved_namesPH.csv")

# Note - unresolved names were addressed mannually

#STEP 5:FILTER TRY DATA FOR THE STUDY SPECIES USING Vascular_plants_list
#Select resolved plant hieght names and keeps only those whose 
#accepted names appear in the vascular plant list.
#selects for TRY SLA species that were already in 
#the vascular plant list without needing resolution.
PH_EX_in<- resolved_namesPH %>% filter(`Accepted names` %in% names)
PH_in <- PH_unique %>% filter(PH_unique %in% names)

#Extract and standardise labels
PH_EX_in_AN <- as.data.frame(PH_EX_in$`Accepted names`)
PH_EX_in_AN <-PH_EX_in_AN %>%
  rename(Accepted_names = `PH_EX_in$\`Accepted names\``)

PH_in_AN <- as.data.frame(PH_in$PH_unique)
PH_in_AN <-PH_in_AN %>%
  rename(Accepted_names = `PH_in$PH_unique`)

#Combine all accepted names
Accepted_names<-rbind(PH_EX_in_AN, PH_in_AN)

# Extract and standardise
PH_EX_in_ID <- as.data.frame(PH_EX_in$`Species names`)
PH_EX_in_ID <- PH_EX_in_ID %>%
  rename(AccSpeciesName = `PH_EX_in$\`Species names\``)

PH_in_ID <- as.data.frame(PH_in$PH_unique)
PH_in_ID <- PH_in_ID %>%
  rename(AccSpeciesName = `PH_in$PH_unique`)

# Combine all accepted names
AccSpeciesName <- rbind(PH_EX_in_ID, PH_in_ID)

# pairs the resolved names to unresolved names
PH_Accepted_names_TRY <- cbind(AccSpeciesName, Accepted_names)

# Attach accepted names back to the plant hieght dataset
resolved_PH<- PH %>% left_join(PH_Accepted_names_TRY, by = "AccSpeciesName")

#Remove unresolved rows
resolved_PH <- resolved_PH[complete.cases(resolved_PH$Accepted_names), ]

write.csv(resolved_PH, "resolved_PH_TRY.csv")

# calculate the mean plant hieght
resolved_PH_mean <- resolved_PH_TRY %>%
  group_by(Accepted_names) %>%
  summarize(PH = mean(StdValue, na.rm = TRUE))

# standardise label
colnames(resolved_PH_mean)[colnames(resolved_PH_mean) == "Accepted_names"]<- "species"

# Inserts results into Vascular_plants_list
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PH <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM, resolved_PH_mean, by = "species", all.x = TRUE)


### BIEN ### ----

#  STEP 6: EXTRACT THE SLA BIEN TO FILL IN THE GAPS 

trait <- c("whole plant height")
BIEN_PH<-BIEN_trait_trait(
  trait,
  all.taxonomy = TRUE,
  political.boundaries = FALSE,
  source.citation = FALSE,
)

PH <- BIEN_PH
PH_unique <- unique(PH$scrubbed_species_binomial)
PH_unique <- as.data.frame(PH_unique)
PH_EX<-PH_unique %>% filter(!PH_unique %in% names)

# STEP 8: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT

# Separate the names into subsets for 1000 rows, this is to speed up the process
#And to check that each subset.

subset1_row <- 1:1000
subset2_row <- 1001:2000
subset3_row <- 2001:3000
subset4_row <- 3001:4000
subset5_row <- 4001:5000

subset6_row <- 5001:6000
subset7_row <- 6001:6152


PH_EX1 <-PH_EX$PH_unique[subset1_row]
PH_EX1 <- as.data.frame(PH_EX1)
PH_EX2 <-PH_EX$PH_unique[subset2_row]
PH_EX2 <- as.data.frame(PH_EX2)
PH_EX3 <-PH_EX$PH_unique[subset3_row]
PH_EX3 <- as.data.frame(PH_EX3)
PH_EX4 <-PH_EX$PH_unique[subset4_row]
PH_EX4 <- as.data.frame(PH_EX4)
PH_EX5 <-PH_EX$PH_unique[subset5_row]
PH_EX5 <- as.data.frame(PH_EX5)


PH_EX6 <-PH_EX$PH_unique[subset6_row]
PH_EX6 <- as.data.frame(PH_EX6)
PH_EX7 <-PH_EX$PH_unique[subset7_row]
PH_EX7 <- as.data.frame(PH_EX7)




names(PH_EX1) <- "species"
names(PH_EX2) <- "species"
names(PH_EX3) <- "species"
names(PH_EX4) <- "species"
names(PH_EX5) <- "species"

names(PH_EX6) <- "species"
names(PH_EX7) <- "species"


# Resolve the species using GBIF Backbone Taxonomy
resolve_PH_EX1 <-PH_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

resolve_PH_EX2 <-PH_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX3 <-PH_EX3$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX4 <-PH_EX4$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX5 <-PH_EX5$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)


resolve_PH_EX6 <-PH_EX6$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_PH_EX7 <-PH_EX7$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_PH_EX <- rbind(resolve_PH_EX1, resolve_PH_EX2, resolve_PH_EX3, resolve_PH_EX4, resolve_PH_EX5,
                       resolve_PH_EX6, resolve_PH_EX7)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
colnames(resolve_PH_EX)[colnames(resolve_PH_EX) == "user_supplied_name"]<- "Species names"
names(PH_EX) <- "Species names"
results_df<- PH_EX %>% left_join(resolve_PH_EX, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
results_df$`Accepted names`<- NA
filPH4_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_filPH4_result_df <- filPH4_result_df %>% distinct(`Species names`, .keep_all = TRUE)

#mannually resolve the unresolved names
write.csv(dis_filPH4_result_df, "resolved_namesPH_BIEN.csv")


#STEP 9:FILTER LEDA DATA FOR STUDY SPECIES USING Vascular_plants_list
PH_EX_in<- resolved_namesPH_BIEN %>% filter(`Accepted names` %in% names)
PH_in <- PH_unique %>% filter(PH_unique %in% names)
PH_EX_in_AN <- as.data.frame(PH_EX_in$`Accepted names`)
PH_EX_in_AN <-PH_EX_in_AN %>%
  rename(Accepted_names = `PH_EX_in$\`Accepted names\``)

PH_in_AN <- as.data.frame(PH_in$PH_unique)
PH_in_AN <-PH_in_AN %>%
  rename(Accepted_names = `PH_in$PH_unique`)


Accepted_names<-rbind(PH_EX_in_AN, PH_in_AN)

PH_EX_in_ID <- as.data.frame(PH_EX_in$`Species names`)
PH_EX_in_ID <- PH_EX_in_ID %>%
  rename(scrubbed_species_binomial = `PH_EX_in$\`Species names\``)

PH_in_ID <- as.data.frame(PH_in$PH_unique)
PH_in_ID <- PH_in_ID %>%
  rename(scrubbed_species_binomial = `PH_in$PH_unique`)

#Combine all accepted names
scrubbed_species_binomial <- rbind(PH_EX_in_ID, PH_in_ID)


PH_Accepted_names_  <- cbind(scrubbed_species_binomial, Accepted_names)

resolved_PH<- PH %>% left_join(PH_Accepted_names_ , by = "scrubbed_species_binomial")

resolved_PH <- resolved_PH[complete.cases(resolved_PH$Accepted_names), ]

write.csv(resolved_PH, "resolved_PH_BIEN.csv")

# calculate the mean plant hieght 

resolved_PH_mean <- resolved_PH_BIEN %>%
  group_by(Accepted_names) %>%
  summarize(PH_BIEN = mean(trait_value, na.rm = TRUE))


colnames(resolved_PH_mean)[colnames(resolved_PH_mean) == "Accepted_names"]<- "species"

# Inserts results into Vascular_plants_list
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PH_BIEN <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PH, resolved_PH_mean, by = "species", all.x = TRUE)


#### TR8 #### ----
# Note - names could not be resolved using tr8 as was only used for gap filling

#STEP 10:EXTRACT THE TR8 plant hieght TO FILL IN THE GAPS 
BROT_Plant_hieght <-tr8(species_list=c(names), download_list=c("Height"), allow_persistent=TRUE)
TR8_HIEGHT<-BROT_Plant_hieght@results

#save the results and inspect
write.csv(TR8_HIEGHT, "TR8_HIEGHT.csv")

#STEP 10: STANDARDISE LABEL AND ADD THE RESULT TO Vascular_plants_list_Traits
colnames(TR8_HIEGHT)[colnames(TR8_HIEGHT) == "...1"] <- "species"
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PH_BIEN_TR8 <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PH_BIEN, TR8_HIEGHT, by = "species", all.x = TRUE)

