#This following script describes how sla was generated from TRY, LEDA AND BIEN
# note combine sla together use dataset with 
#all the resolved species names and crops removed (Vascular_plants_list)

#STEP 1: INSTALL AND LOAD 

install.packages(readr)    
install.packages(tidyverse)    
install.packages(taxize)   

library(readr)    
library(tidyverse)    
library(taxize)   

#STEP 2: IMPORT THE TRY DATA

data <- read_delim("FILE NAME.txt", delim = "\t") 

#STEP 3: SELECT THE SLA TRAIT AND EXACT THE SPECIES NAMES

SLA <- subset(data, TraitName == "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included")
SLA_unique <- unique(SLA$AccSpeciesName)
SLA_unique <- as.data.frame(SLA_unique)
SLA_EX<-SLA_unique %>% filter(!SLA_unique %in% names)


#STEP 4: RESOLVE THE SPECIES NAMES SO THAT WILL CORRESPONSE TO Vascular_plants_list FORMAT

#Seporate the names into subeset for 1000 row, this is to speed up the proccess
#And to check that each subset.
subset1_row <- 1:1000
subset2_row <- 1001:2000
subset3_row <- 2001:3000

subset4_row <- 3001:4000
subset5_row <- 4001:5000

subset6_row <- 5001:6000
subset7_row <- 6001:6975


SLA_EX1 <-SLA_EX$SLA_unique[subset1_row]
SLA_EX1 <- as.data.frame(SLA_EX1)
SLA_EX2 <-SLA_EX$SLA_unique[subset2_row]
SLA_EX2 <- as.data.frame(SLA_EX2)
SLA_EX3 <-SLA_EX$SLA_unique[subset3_row]
SLA_EX3 <- as.data.frame(SLA_EX3)
SLA_EX4 <-SLA_EX$SLA_unique[subset4_row]
SLA_EX4 <- as.data.frame(SLA_EX4)
SLA_EX5 <-SLA_EX$SLA_unique[subset5_row]
SLA_EX5 <- as.data.frame(SLA_EX5)


SLA_EX6 <-SLA_EX$SLA_unique[subset6_row]
SLA_EX6 <- as.data.frame(SLA_EX6)
SLA_EX7 <-SLA_EX$SLA_unique[subset7_row]
SLA_EX7 <- as.data.frame(SLA_EX7)


names(SLA_EX1) <- "species"
names(SLA_EX2) <- "species"
names(SLA_EX3) <- "species"
names(SLA_EX4) <- "species"
names(SLA_EX5) <- "species"

names(SLA_EX6) <- "species"
names(SLA_EX7) <- "species"


# Resolve the species using GBIF Backbone Taxonomy
resolve_SLA_EX1 <-SLA_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

resolve_SLA_EX2 <-SLA_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX3 <-SLA_EX3$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX4 <-SLA_EX4$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX5 <-SLA_EX5$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)


resolve_SLA_EX6 <-SLA_EX6$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX7 <-SLA_EX7$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_SLA_EX <- rbind(resolve_SLA_EX1, resolve_SLA_EX2, resolve_SLA_EX3, resolve_SLA_EX4, 
                        resolve_SLA_EX5, resolve_SLA_EX6, resolve_SLA_EX7)


#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
colnames(resolve_SLA_EX)[colnames(resolve_SLA_EX) == "user_supplied_name"]<- "Species names"
names(SLA_EX) <- "Species names"
results_df<- SLA_EX %>% left_join(resolve_SLA_EX, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
results_df$`Accepted names`<- NA
filSLA4_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_filSLA4_result_df <- filSLA4_result_df %>% distinct(`Species names`, .keep_all = TRUE)

write.csv(dis_filSLA4_result_df, "resolved_namesSLA.csv")


# check for the unresolved names

sci_name <- c(UN_resolvedSLA$UN)

names_harmonizationSLA <-
  bdc_query_names_taxadb(
    sci_name,
    replace_synonyms = TRUE,
    suggest_names = TRUE,
    suggestion_distance = 0.9,
    db = "ncbi",
    parallel = TRUE,
    ncores = 2,
    export_accepted = FALSE
  )

#MANNUAL ADD THE UNRESOLVED NAMES INTO resolved_namesSLA
write.csv(names_harmonization, "names_harmonizationSLA.csv")


#STEP 5:FILTER TRY DATA FOR STUDY SPECIES USING Vascular_plants_list

#Isolate the study species 
names <- Vascular_plants_list$species

#Select resolved SLA names and keeps only those whose 
#accepted names appear in the vascular plant list.
#selects for TRY SLA species that were already in 
#the vascular plant list without needing resolution.
SLA_EX_in<- resolved_namesSLA %>% filter(`Accepted names` %in% names)
SLA_in <- SLA_unique %>% filter(SLA_unique %in% names)


#Extract and standardize labels
SLA_EX_in_AN <- as.data.frame(SLA_EX_in$`Accepted names`)
SLA_EX_in_AN <-SLA_EX_in_AN %>%
  rename(Accepted_names = `SLA_EX_in$\`Accepted names\``)

SLA_in_AN <- as.data.frame(SLA_in$SLA_unique)
SLA_in_AN <-SLA_in_AN %>%
  rename(Accepted_names = `SLA_in$SLA_unique`)

#Combine all accepted names
Accepted_names<-rbind(SLA_EX_in_AN, SLA_in_AN)


# Extract and standardize
SLA_EX_in_ID <- as.data.frame(SLA_EX_in$`Species names`)
SLA_EX_in_ID <- SLA_EX_in_ID %>%
  rename(AccSpeciesName = `SLA_EX_in$\`Species names\``)

SLA_in_ID <- as.data.frame(SLA_in$SLA_unique)
SLA_in_ID <- SLA_in_ID %>%
  rename(AccSpeciesName = `SLA_in$SLA_unique`)

# Combine all accepted names
AccSpeciesName <- rbind(SLA_EX_in_ID, SLA_in_ID)


# pairs the resolved names to unresolved names
SLA_Accepted_names_TRY <- cbind(AccSpeciesName, Accepted_names)

# Attach accepted names back to the SLA dataset
resolved_SLA<- SLA %>% left_join(SLA_Accepted_names_TRY, by = "AccSpeciesName")

#Remove unresolved rows
resolved_SLA <- resolved_SLA[complete.cases(resolved_SLA$Accepted_names), ]

write.csv(resolved_SLA, "resolved_SLA_TRY.csv")

# calculate the mean SLA
resolved_SLA_mean <- resolved_SLA_TRY %>%
  group_by(Accepted_names) %>%
  summarize(SLA = mean(StdValue, na.rm = TRUE))

# standardize label
colnames(resolved_SLA_mean)[colnames(resolved_SLA_mean) == "Accepted_names"]<- "species"

# Inserts results into Vascular_plants_list
Vascular_plants_list_SLA <- merge(Vascular_plants_list_SLA, resolved_SLA_mean, by = "species", all.x = TRUE)


### ------ BIEN -------- ###

#  STEP 6: EXTACT THE SLA BIEN TO FILL IN THE GAPS 
  
  BIEN_TL<-BIEN_trait_list()
  
trait <- c("leaf area per leaf dry mass")
BIEN_TT<-BIEN_trait_trait(
  trait,
  all.taxonomy = TRUE,
  political.boundaries = FALSE,
  source.citation = FALSE,
)


# STEP 7: SELECT THE SLA TRAIT AND EXACT THE SPECIES NAMES

SLA_unique <- unique(SLA_df$verbatim_scientific_name)
SLA_unique <- as.data.frame(SLA_unique)
SLA_EX<- SLA_unique %>% filter(!SLA_unique %in% names)

# STEP 8: RESOLVE THE SPECIES NAMES SO THAT WILL CORRESPONSE TO Vascular_plants_list FORMAT

#Seporate the names into subeset for 1000 row, this is to speed up the proccess
#And to check that each subset.


subset1_row <- 0:1000
subset2_row <- 1001:2000
subset3_row <- 2001:3000
subset4_row <- 3001:4000
subset5_row <- 4001:4654


SLA_EX1 <- SLA_EX$SLA_unique[subset1_row]
SLA_EX1 <- as.data.frame(SLA_EX1)
SLA_EX2 <- SLA_EX$SLA_unique[subset2_row]
SLA_EX2 <- as.data.frame(SLA_EX2)
SLA_EX3 <- SLA_EX$SLA_unique[subset3_row]
SLA_EX3 <- as.data.frame(SLA_EX3)
SLA_EX4 <- SLA_EX$SLA_unique[subset4_row]
SLA_EX4 <- as.data.frame(SLA_EX4)
SLA_EX5 <- SLA_EX$SLA_unique[subset5_row]
SLA_EX5 <- as.data.frame(SLA_EX5)


names(SLA_EX1) <- "species"
names(SLA_EX2) <- "species"
names(SLA_EX3) <- "species"
names(SLA_EX4) <- "species"
names(SLA_EX5) <- "species"

# Resolve the species using GBIF Backbone Taxonomy

resolve_SLA_EX1 <- SLA_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX2 <- SLA_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX3 <- SLA_EX3$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX4 <- SLA_EX4$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SLA_EX5 <- SLA_EX5$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy

resolve_SLA_EX <- rbind(resolve_SLA_EX1, resolve_SLA_EX2,
                        resolve_SLA_EX3, resolve_SLA_EX4, resolve_SLA_EX5)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
SLA_EX_in<- resolve_SLA_EX %>% filter(matched_name2 %in% names)
SLA_in <-  SLA_unique %>% filter(SLA_unique %in% names)
SLA_EX_in_AN <- as.data.frame(SLA_EX_in$matched_name2)
SLA_EX_in_AN <- SLA_EX_in_AN %>%
  rename(Accepted_names = `SLA_EX_in$matched_name2`)

#Combine all accepted names
SLA_in_AN <- as.data.frame(SLA_in$SLA_unique)
SLA_in_AN <- SLA_in_AN %>%
  rename(Accepted_names = `SLA_in$SLA_unique`)

#Combine all accepted names
Accepted_names<-rbind(SLA_EX_in_AN, SLA_in_AN)

# Extract and standardize

SLA_EX_in_ID <- as.data.frame(SLA_EX_in$user_supplied_name)
SLA_EX_in_ID <- SLA_EX_in_ID %>%
  rename(verbatim_scientific_name = `SLA_EX_in$user_supplied_name`)

SLA_in_ID <- as.data.frame(SLA_in$SLA_unique)
SLA_in_ID <- SLA_in_ID %>%
  rename(verbatim_scientific_name = `SLA_in$SLA_unique`)

# Combine all accepted names
verbatim_scientific_name<-rbind(SLA_EX_in_ID, SLA_in_ID)


# pairs the resolved names to unresolved names
SLA_Accepted_names_BIEN <- cbind(verbatim_scientific_name, Accepted_names)

# Attach accepted names back to the SLA dataset
resolved_SLA<- SLA_df %>% left_join(SLA_Accepted_names_BIEN, by = "verbatim_scientific_name")

resolved_SLA <- resolved_SLA[complete.cases(resolved_SLA$Accepted_names), ]
#m2 kg-1 to mm2 mg-1 are the same units going change them for SLA to mm2 mg-1
write.csv(resolved_SLA, "resolved_SLA_BIEN1.csv")

#STEP 9: CALCULATE THE MEAN SLA FROM BIEN
# calculate the mean SLA
resolved_SLA_mean <- resolved_SLA_BIEN %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(trait_value, na.rm = TRUE))

#STEP 10: STANDARDISE LABEL AND ADD THE RESULT TO TRY SLA

resolved_SLA_mean <- resolved_SLA_mean %>%
  rename(species = Accepted_names)

Vascular_plants_list_SLA <- merge(Vascular_plants_list_SLA, resolved_SLA_mean, by = "species", all.x = TRUE)

Vascular_plants_list_SLA <- Vascular_plants_list_SLA %>% mutate(SLA = coalesce(SLA, mean_value))

# STEP 11: CHECK FOR THE GAPS IN THE DATASET REMAINS 
SLAB_na_count <- colSums(is.na(Vascular_plants_list_SLA))
SLAA_na_count <- colSums(is.na(Vascular_plants_list_SLA))

SLAB_na_count 
SLAA_na_count


#SLA_LEDA

#STEP 12: SELECT THE SLA TRAIT AND EXACT THE SPECIES NAMES
SLA_df <- SLA_LEDA
SLA_unique <- unique(SLA_df$`SBS name`)
SLA_unique <- as.data.frame(SLA_unique)
SLA_EX<- SLA_unique %>% filter(!SLA_unique %in% names)
names(SLA_EX) <- "species"

#STEP 13: RESOLVE THE SPECIES NAMES SO THAT WILL CORRESPONSE TO Vascular_plants_list FORMAT
resolve_SLA_EX <- SLA_EX$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.

SLA_EX_in<- resolve_SLA_EX %>% filter(matched_name2 %in% names)
SLA_in <-  SLA_unique %>% filter(SLA_unique %in% names)
SLA_EX_in_AN <- as.data.frame(SLA_EX_in$matched_name2)
SLA_EX_in_AN <- SLA_EX_in_AN %>%
  rename(Accepted_names = `SLA_EX_in$matched_name2`)


#STEP 14:FILTER LEDA DATA FOR STUDY SPECIES USING Vascular_plants_list
SLA_in_AN <- as.data.frame(SLA_in$SLA_unique)
SLA_in_AN <- SLA_in_AN %>%
  rename(Accepted_names = `SLA_in$SLA_unique`)

Accepted_names<-rbind(SLA_EX_in_AN, SLA_in_AN)

SLA_EX_in_ID <- as.data.frame(SLA_EX_in$user_supplied_name)
SLA_EX_in_ID <- SLA_EX_in_ID %>%
  rename(`SBS name` = `SLA_EX_in$user_supplied_name`)

SLA_in_ID <- as.data.frame(SLA_in$SLA_unique)
SLA_in_ID <- SLA_in_ID %>%
  rename(`SBS name` = `SLA_in$SLA_unique`)

#Combine all accepted names
`SBS name`<-rbind(SLA_EX_in_ID, SLA_in_ID)

SLA_Accepted_names_LEDA <- cbind(`SBS name`, Accepted_names)

resolved_SLA<- SLA_df %>% left_join(SLA_Accepted_names_LEDA, by = "SBS name")
resolved_SLA <- resolved_SLA[complete.cases(resolved_SLA$Accepted_names), ]

# calculate the mean SLA
resolved_SLA_mean <- resolved_SLA %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(`single value [mm^2/mg]`, na.rm = TRUE))

# standardize label
resolved_SLA_mean <- resolved_SLA_mean %>%
  rename(species = Accepted_names)
VasTraitWP_ALDMC_LEDA <- select(VasTraitWP_ALDMC_LEDA, -mean_value)


# Inserts results into Vascular_plants_list
Vascular_plants_list_SLA <- merge(Vascular_plants_list_SLA, resolved_SLA_mean, by = "species", all.x = TRUE)


