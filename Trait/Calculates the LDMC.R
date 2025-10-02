#This following script describes how LDMC was generated from TRY, LEDA AND BIEN
# note combine  LDMC together use dataset with 
#all the resolved species names and crops removed (Vascular_plants_list)

#STEP 1: INSTALL AND LOAD 

install.packages(readr)    
install.packages(tidyverse)    
install.packages(taxize)   

library(readr)    
library(tidyverse)    
library(taxize)   


#Isolate the study species 
names <- Vascular_plants_list$species

#STEP 2: IMPORT THE TRY DATA
data <- read_delim("FILE NAME.txt", delim = "\t") 

#STEP 3: SELECT THE SLA TRAIT AND EXACT THE SPECIES NAMES
LDMC <- subset(data, TraitName == "Leaf dry mass per leaf fresh mass (leaf dry matter content, LDMC)")
LDMC_unique <- unique(LDMC$AccSpeciesName)
LDMC_unique <- as.data.frame(LDMC_unique)
LDMC_EX<-LDMC_unique %>% filter(!LDMC_unique %in% names)


#STEP 4: RESOLVE THE SPECIES NAMES SO THAT WILL CORRESPONSE TO Vascular_plants_list FORMAT

#Separate the names into subset for 1000 row, this is to speed up the process
#And to check that each subset.
subset1_row <- 1:1000
subset2_row <- 1001:2000
subset3_row <- 2001:3000
subset4_row <- 3001:4000
subset5_row <- 4001:5000

subset6_row <- 5001:6000
subset7_row <- 6001:7000
subset8_row <- 7001:8000
subset9_row <- 8001:8071


LDMC_EX1 <-LDMC_EX$LDMC_unique[subset1_row]
LDMC_EX1 <- as.data.frame(LDMC_EX1)
LDMC_EX2 <-LDMC_EX$LDMC_unique[subset2_row]
LDMC_EX2 <- as.data.frame(LDMC_EX2)
LDMC_EX3 <-LDMC_EX$LDMC_unique[subset3_row]
LDMC_EX3 <- as.data.frame(LDMC_EX3)
LDMC_EX4 <-LDMC_EX$LDMC_unique[subset4_row]
LDMC_EX4 <- as.data.frame(LDMC_EX4)
LDMC_EX5 <-LDMC_EX$LDMC_unique[subset5_row]
LDMC_EX5 <- as.data.frame(LDMC_EX5)


LDMC_EX6 <-LDMC_EX$LDMC_unique[subset6_row]
LDMC_EX6 <- as.data.frame(LDMC_EX6)
LDMC_EX7 <-LDMC_EX$LDMC_unique[subset7_row]
LDMC_EX7 <- as.data.frame(LDMC_EX7)
LDMC_EX8 <-LDMC_EX$LDMC_unique[subset8_row]
LDMC_EX8 <- as.data.frame(LDMC_EX8)
LDMC_EX9 <-LDMC_EX$LDMC_unique[subset9_row]
LDMC_EX9 <- as.data.frame(LDMC_EX9)



names(LDMC_EX1) <- "species"
names(LDMC_EX2) <- "species"
names(LDMC_EX3) <- "species"
names(LDMC_EX4) <- "species"
names(LDMC_EX5) <- "species"

names(LDMC_EX6) <- "species"
names(LDMC_EX7) <- "species"
names(LDMC_EX8) <- "species"
names(LDMC_EX9) <- "species"

# Resolve the species using GBIF Backbone Taxonomy

resolve_LDMC_EX1 <-LDMC_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

resolve_LDMC_EX2 <-LDMC_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX3 <-LDMC_EX3$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX4 <-LDMC_EX4$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX5 <-LDMC_EX5$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)


resolve_LDMC_EX6 <-LDMC_EX6$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX7 <-LDMC_EX7$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX8 <-LDMC_EX8$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX9 <-LDMC_EX9$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_LDMC_EX <- rbind(resolve_LDMC_EX1, resolve_LDMC_EX2, resolve_LDMC_EX3, resolve_LDMC_EX4, 
                         resolve_LDMC_EX5, resolve_LDMC_EX6, resolve_LDMC_EX7, resolve_LDMC_EX8, resolve_LDMC_EX9)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
colnames(resolve_LDMC_EX)[colnames(resolve_LDMC_EX) == "user_supplied_name"]<- "Species names"
names(LDMC_EX) <- "Species names"
results_df<- LDMC_EX %>% left_join(resolve_LDMC_EX, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
results_df$`Accepted names`<- NA
filLDMC4_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_filLDMC4_result_df <- filLDMC4_result_df %>% distinct(`Species names`, .keep_all = TRUE)

write.csv(dis_filLDMC4_result_df, "resolved_namesLDMC.csv")

# check for the unresolved names in gbif and ncbi
sci_name <- c(UN_resolved_LDMC14032024$`Species UN`)

names_harmonization <-
  bdc_query_names_taxadb(
    sci_name,
    replace_synonyms = TRUE,
    suggest_names = TRUE,
    suggestion_distance = 0.9,
    db = "gbif",
    parallel = TRUE,
    ncores = 2,
    export_accepted = FALSE
  )

#MANNUAL ADD THE UNRESOLVED NAMES INTO resolved_namesSLA
write.csv(names_harmonization, "names_harmonizationLDMC.csv")

#STEP 5:FILTER TRY DATA FOR THE STUDY SPECIES USING Vascular_plants_list

#Select resolved SLA names and keeps only those whose 
#accepted names appear in the vascular plant list.
#selects for TRY SLA species that were already in 
#the vascular plant list without needing resolution.
LDMC_EX_in<- resolved_namesLDMC %>% filter(`Accepted names` %in% names)
LDMC_in <- LDMC_unique %>% filter(LDMC_unique %in% names)

#Extract and standardise labels
LDMC_EX_in_AN <- as.data.frame(LDMC_EX_in$`Accepted names`)
LDMC_EX_in_AN <-LDMC_EX_in_AN %>%
  rename(Accepted_names = `LDMC_EX_in$\`Accepted names\``)

LDMC_in_AN <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_AN <-LDMC_in_AN %>%
  rename(Accepted_names = `LDMC_in$LDMC_unique`)

#Combine all accepted names
Accepted_names<-rbind(LDMC_EX_in_AN, LDMC_in_AN)


# Extract and standardise
LDMC_EX_in_ID <- as.data.frame(LDMC_EX_in$`Species names`)
LDMC_EX_in_ID <- LDMC_EX_in_ID %>%
  rename(AccSpeciesName = `LDMC_EX_in$\`Species names\``)

LDMC_in_ID <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_ID <- LDMC_in_ID %>%
  rename(AccSpeciesName = `LDMC_in$LDMC_unique`)

# Combine all accepted names
AccSpeciesName <- rbind(LDMC_EX_in_ID, LDMC_in_ID)

# pairs the resolved names to unresolved names
LDMC_Accepted_names_TRY <- cbind(AccSpeciesName, Accepted_names)

# Attach accepted names back to the SLA dataset
resolved_LDMC<- LDMC %>% left_join(LDMC_Accepted_names_TRY, by = "AccSpeciesName")

#Remove unresolved rows
resolved_LDMC <- resolved_LDMC[complete.cases(resolved_LDMC$Accepted_names), ]

write.csv(resolved_LDMC, "resolved_LDMC_TRY.csv")

# calculate the mean LDMC
resolved_LDMC_mean <- resolved_LDMC_TRY %>%
  group_by(Accepted_names) %>%
  summarize(LDMC = mean(StdValue, na.rm = TRUE))

# standardise label
colnames(resolved_LDMC_mean)[colnames(resolved_LDMC_mean) == "Accepted_names"]<- "species"

# Inserts results into Vascular_plants_list
Vascular_plants_list_LDMC <- merge(Vascular_plants_list, resolved_LDMC_mean, by = "species", all.x = TRUE)

### BIEN ### -----
#  STEP 6: EXTRACT THE SLA BIEN TO FILL IN THE GAPS 

#Identify the name for LDMC
BIEN_TL<-BIEN_trait_list()

trait <- c("leaf dry mass per leaf fresh mass")
BIEN_TT<-BIEN_trait_trait(
  trait,
  all.taxonomy = TRUE,
  political.boundaries = FALSE,
  source.citation = FALSE,
)

# STEP 7: SELECT THE LDMC TRAIT AND EXTRACT THE SPECIES NAMES
#leaf dry mass per leaf fresh mass-LDMC from BIEN

LDMC_unique <- unique(LDMC_df$verbatim_scientific_name)
LDMC_unique <- as.data.frame(LDMC_unique)
LDMC_EX<- LDMC_unique %>% filter(!LDMC_unique %in% names)

# STEP 8: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT

# Separate the names into subsets for 1000 rows, this is to speed up the process
#And to check that each subset.

subset1_row <- 0:1000
subset2_row <- 1001:1404


LDMC_EX1 <- LDMC_EX$LDMC_unique[subset1_row]
LDMC_EX1 <- as.data.frame(LDMC_EX1)
LDMC_EX2 <- LDMC_EX$LDMC_unique[subset2_row]
LDMC_EX2 <- as.data.frame(LDMC_EX2)


names(LDMC_EX1) <- "species"
names(LDMC_EX2) <- "species"

# Resolve the species using GBIF Backbone Taxonomy
resolve_LDMC_EX1 <- LDMC_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_LDMC_EX2 <- LDMC_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_LDMC_EX <- rbind(resolve_LDMC_EX1, resolve_LDMC_EX2)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
LDMC_EX_in<- resolve_LDMC_EX %>% filter(matched_name2 %in% names)
LDMC_in <-  LDMC_unique %>% filter(LDMC_unique %in% names)
LDMC_EX_in_AN <- as.data.frame(LDMC_EX_in$matched_name2)
LDMC_EX_in_AN <- LDMC_EX_in_AN %>%
  rename(Accepted_names = `LDMC_EX_in$matched_name2`)

#Combine all accepted names
LDMC_in_AN <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_AN <- LDMC_in_AN %>%
  rename(Accepted_names = `LDMC_in$LDMC_unique`)

#Combine all accepted names
Accepted_names<-rbind(LDMC_EX_in_AN, LDMC_in_AN)

# Extract and standardise
LDMC_EX_in_ID <- as.data.frame(LDMC_EX_in$user_supplied_name)
LDMC_EX_in_ID <- LDMC_EX_in_ID %>%
  rename(verbatim_scientific_name = `LDMC_EX_in$user_supplied_name`)

LDMC_in_ID <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_ID <- LDMC_in_ID %>%
  rename(verbatim_scientific_name = `LDMC_in$LDMC_unique`)

# Combine all accepted names
verbatim_scientific_name<-rbind(LDMC_EX_in_ID, LDMC_in_ID)

# pairs the resolved names to unresolved names
LDMC_Accepted_names_BIEN <- cbind(verbatim_scientific_name, Accepted_names)

# Attach accepted names back to the SLA dataset
resolved_LDMC<- LDMC_df %>% left_join(LDMC_Accepted_names_BIEN, by = "verbatim_scientific_name")

resolved_LDMC <- resolved_LDMC[complete.cases(resolved_LDMC$Accepted_names), ]
write.csv(resolved_LDMC, "resolved_LDMC.csv")


#NOTE THAT THE UNITS NEED TO BE CHANGED 
resolved_LDMC$trait_value <- as.numeric(resolved_LDMC$trait_value)
resolved_LDMC$trait_value <- (resolved_LDMC$trait_value/1000)
resolved_LDMC$unit <- "g g^-1"

#STEP 9: CALCULATE THE MEAN SLA FROM BIEN

resolved_LDMC_mean <- resolved_LDMC %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(trait_value, na.rm = TRUE))

#STEP 10: STANDARDISE LABELS

resolved_LDMC_mean <- resolved_LDMC_mean %>%
  rename(species = Accepted_names)

# STEP 11: CHECK FOR THE GAPS IN THE DATASET REMAINS  AND 
#ADD THE RESULT TO Vascular_plants_list

Vascular_plants_list_Traits_ASLA <- select(Vascular_plants_list_SLA, -mean_value)
Vascular_plants_corrected_list1Traits_ALDMC_SLA <- merge(Vascular_plants_list_ASLA, resolved_LDMC_mean, by = "species", all.x = TRUE)

Vascular_plants_corrected_list_Traits_ALDMC_SLA <- Vascular_plants_corrected_list1Traits_ALDMC_SLA %>% mutate(LDMC = coalesce(LDMC, mean_value))
LDMCA_na_count <- colSums(is.na(Vascular_plants_corrected_list1Traits_ALDMC_SLA))


LDMCB_na_count
LDMCA_na_count

# STEP 12:SELECT THE LDMC TRAIT AND EXACT THE SPECIES NAMES FROM LEDA

LDMC_unique <- unique(LDMC_LEDA$`SBS name`)
LDMC_unique <- as.data.frame(LDMC_unique)
LDMC_EX<- LDMC_unique %>% filter(!LDMC_unique %in% names)

names(LDMC_EX) <- "species"

#STEP 13: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT
resolve_LDMC_EX1 <- LDMC_EX$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.

LDMC_EX_in<- resolve_LDMC_EX1 %>% filter(matched_name2 %in% names)
LDMC_in <-  LDMC_unique %>% filter(LDMC_unique %in% names)
LDMC_EX_in_AN <- as.data.frame(LDMC_EX_in$matched_name2)
LDMC_EX_in_AN <- LDMC_EX_in_AN %>%
  rename(Accepted_names = `LDMC_EX_in$matched_name2`)

#STEP 14:FILTER LEDA DATA FOR STUDY SPECIES USING Vascular_plants_list
LDMC_in_AN <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_AN <- LDMC_in_AN %>%
  rename(Accepted_names = `LDMC_in$LDMC_unique`)

#Combine all accepted names
Accepted_names<-rbind(LDMC_EX_in_AN, LDMC_in_AN)

LDMC_EX_in_ID <- as.data.frame(LDMC_EX_in$user_supplied_name)
LDMC_EX_in_ID <- LDMC_EX_in_ID %>%
  rename(`SBS name` = `LDMC_EX_in$user_supplied_name`)

LDMC_in_ID <- as.data.frame(LDMC_in$LDMC_unique)
LDMC_in_ID <- LDMC_in_ID %>%
  rename(`SBS name` = `LDMC_in$LDMC_unique`)

`SBS name`<-rbind(LDMC_EX_in_ID, LDMC_in_ID)

LDMC_Accepted_names_LEDA <- cbind(`SBS name`, Accepted_names)

resolved_LDMC<- LDMC_LEDA %>% left_join(LDMC_Accepted_names_LEDA, by = "SBS name")

resolved_LDMC <- resolved_LDMC[complete.cases(resolved_LDMC$Accepted_names), ]


# NOTE THAT THE UNITS NEED TO BE CHANGE TO MATCH THE REST OF THE DATASET
resolved_LDMC$`single value [mg/g]` <- (resolved_LDMC$`single value [mg/g]`/1000)
resolved_LDMC$unit <- "g g^-1"
names(resolved_LDMC)[names(resolved_LDMC) == "single value [mg/g]"] <- "single value [g g^-1]"

# Calculate the mean LDMC
resolved_LDMC_mean <- resolved_LDMC %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(`single value [g g^-1]`, na.rm = TRUE))

# standardise label
resolved_LDMC_mean <- resolved_LDMC_mean %>%
  rename(species = Accepted_names)

# Inserts results into Vascular_plants_list
Vascular_plants_list_Traits_ALDMC_SLA <- select(Vascular_plants_list_Traits_ALDMC_SLA, -mean_value)

Vascular_plants_list_Traits_ALDMC_SLA_LEDA <- merge(Vascular_plants_list_Traits_ALDMC_SLA
                                                              , resolved_LDMC_mean, by = "species", all.x = TRUE)

Vascular_plants_list_Traits_ALDMC_SLA_LEDA<- Vascular_plants_list_Traits_ALDMC_SLA_LEDA %>% mutate(LDMC = coalesce(LDMC, mean_value))

write.csv(Vascular_plants_list_Traits_ALDMC_SLA_LEDA, "Vascular_plants_list_Traits_LDMC_SLA.csv")
