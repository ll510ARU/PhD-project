#This following script describes how sla was generated from LEDA AND BIEN
# note combine seed mass together use dataset with 
#all the resolved species names and crops removed (Vascular_plants_list)


#STEP 1: INSTALL AND LOAD 

install.packages(readr)    
install.packages(tidyverse)    
install.packages(taxize)   
install.packages(bdc)   

library(readr)    
library(tidyverse)    
library(taxize)   
library(bdc)


#STEP 2: IMPORT THE BIEN DATA

SM_df1<-BIEN_trait_trait(
  trait,
  all.taxonomy = TRUE,
  political.boundaries = FALSE,
  source.citation = FALSE,
)


#STEP 3: SELECT THE SEED MASS TRAIT AND EXACT THE SPECIES NAMES

#Isolate the study species 
names <- Vascular_plants_list$species

SM_unique <- unique(SM_df1$verbatim_scientific_name)
SM_unique <- as.data.frame(SM_unique)
SM_EX<- SM_unique %>% filter(!SM_unique %in% names)



#STEP 4: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT

# Separate the names into subsets for 1000 rows, this is to speed up the process
#And to check that each subset.
subset1_row <- 0:1000
subset2_row <- 1001:2000
subset3_row <-2001:3000
subset4_row <- 3001:4000
subset5_row <- 4001:5000


subset6_row <- 5001:6000
subset7_row <- 6001:7000
subset8_row <-7001:8000
subset9_row <- 8001:9000
subset10_row <-9001:10000

subset11_row <- 10001:11109

SM_EX1 <- SM_EX$SM_unique[subset1_row]
SM_EX1 <- as.data.frame(SM_EX1)
SM_EX2 <- SM_EX$SM_unique[subset2_row]
SM_EX2 <- as.data.frame(SM_EX2)
SM_EX2 <- as.data.frame(SM_EX2)
SM_EX3 <- SM_EX$SM_unique[subset3_row]
SM_EX3 <- as.data.frame(SM_EX3)
SM_EX4 <- SM_EX$SM_unique[subset4_row]
SM_EX4 <- as.data.frame(SM_EX4)
SM_EX5 <- SM_EX$SM_unique[subset5_row]
SM_EX5 <- as.data.frame(SM_EX5)


SM_EX6 <- SM_EX$SM_unique[subset6_row]
SM_EX6 <- as.data.frame(SM_EX6)
SM_EX7 <- SM_EX$SM_unique[subset7_row]
SM_EX7 <- as.data.frame(SM_EX7)
SM_EX8 <- SM_EX$SM_unique[subset8_row]
SM_EX8 <- as.data.frame(SM_EX8)
SM_EX9 <- SM_EX$SM_unique[subset9_row]
SM_EX9 <- as.data.frame(SM_EX9)
SM_EX10 <- SM_EX$SM_unique[subset10_row]
SM_EX10 <- as.data.frame(SM_EX10)

SM_EX11 <- SM_EX$SM_unique[subset11_row]
SM_EX11 <- as.data.frame(SM_EX11)

names(SM_EX1) <- "species"
names(SM_EX2) <- "species"
names(SM_EX3) <- "species"
names(SM_EX4) <- "species"
names(SM_EX5) <- "species"
names(SM_EX6) <- "species"
names(SM_EX7) <- "species"
names(SM_EX8) <- "species"
names(SM_EX9) <- "species"
names(SM_EX10) <- "species"
names(SM_EX11) <- "species"

# Resolve the species using GBIF Backbone Taxonomy
resolve_SM_EX1 <- SM_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX2 <- SM_EX2$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX3 <-  SM_EX3$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX4 <-  SM_EX4$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX5 <-  SM_EX5$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX6 <-  SM_EX6$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX7 <-  SM_EX7$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX8 <-  SM_EX8$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX9 <-  SM_EX9$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX10 <-  SM_EX10$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
resolve_SM_EX11 <-  SM_EX11$species %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#combine the results from the GBIF Backbone Taxonomy
resolve_SM_EX <- rbind(resolve_SM_EX1, resolve_SM_EX2, resolve_SM_EX3, resolve_SM_EX4, resolve_SM_EX5, resolve_SM_EX6, 
                       resolve_SM_EX7, resolve_SM_EX8, resolve_SM_EX9, resolve_SM_EX10, resolve_SM_EX11)


# check for the unresolved names - no addition names were idenitfied with bdc


#STEP 5:FILTER TRY DATA FOR THE STUDY SPECIES USING Vascular_plants_list

SM_EX_in<- resolve_SM_EX %>% filter(matched_name2 %in% names)
SM_in <-  SM_unique %>% filter(SM_unique %in% names)
SM_EX_in_AN <- as.data.frame(SM_EX_in$matched_name2)
SM_EX_in_AN <- SM_EX_in_AN %>%
  rename(Accepted_names = `SM_EX_in$matched_name2`)

SM_in_AN <- as.data.frame(SM_in$SM_unique)
SM_in_AN <- SM_in_AN %>%
  rename(Accepted_names = `SM_in$SM_unique`)


#Combine all accepted names
Accepted_names<-rbind(SM_EX_in_AN, SM_in_AN)

SM_EX_in_ID <- as.data.frame(SM_EX_in$user_supplied_name)
SM_EX_in_ID <- SM_EX_in_ID %>%
  rename(verbatim_scientific_name = `SM_EX_in$user_supplied_name`)

# Extract and standardise
SM_in_ID <- as.data.frame(SM_in$SM_unique)
SM_in_ID <- SM_in_ID %>%
  rename(verbatim_scientific_name = `SM_in$SM_unique`)

# Combine the orginal names as ids
verbatim_scientific_name<-rbind(SM_EX_in_ID, SM_in_ID)


# Pairs the resolved names to unresolved names
SM_Accepted_names_BIEN <- cbind(verbatim_scientific_name, Accepted_names)

# Attach accepted names back to the BIEN dataset
resolved_SM<- SM_df1 %>% left_join(SM_Accepted_names_BIEN, by = "verbatim_scientific_name")

#Remove unresolved rows
resolved_SM <- resolved_SM[complete.cases(resolved_SM$Accepted_names), ]

#NOTE - no Changed to units required
write.csv(resolved_SM, "resolved_SM.csv")

# calculate the mean seed mass
resolved_SM <- replace(resolved_SM, resolved_SM == ".", NA)
resolved_SM$trait_value <- as.numeric(resolved_SM$trait_value)
resolved_SM_mean <- resolved_SM %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(trait_value, na.rm = TRUE))

# standardise label
resolved_SM_mean <- resolved_SM_mean %>%
  rename(species = Accepted_names)

# Inserts results into Vascular_plants_list
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM <- merge(Vascular_plants_corrected_list1Traits_ALDMC_SLA, resolved_SM_mean, by = "species", all.x = TRUE)
Vascular_plants_list_Traits_ALDMC_SLA_LEDA  <- Vascular_plants_list_Traits_ALDMC_SLA_LEDA  %>% mutate(seed_mass = coalesce(seed_mass, mean_value))
# STEP 6: CHECK FOR THE GAPS IN THE DATASET REMAINS 

SMA_na_count <- colSums(is.na(Vascular_plants_list_Traits_ALDMC_SLA_LEDA))
SMB_na_count <- colSums(is.na(Vascular_plants_list_Traits_ALDMC_SLA_LEDA))

SMB_na_count
SMA_na_count


#### Seed mass LEDA #### ----

#STEP 7: SELECT THE SEED MASS TRAIT AND EXACT THE SPECIES NAMES

SM_df <- seed_mass_LEDA
SM_unique <- unique(SM_df$`SBS name`)
SM_unique <- as.data.frame(SM_unique)
SM_EX<- SM_unique %>% filter(!SM_unique %in% names)

names(SM_EX) <- "species"

#STEP 8: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT
resolve_SM_EX <- SM_EX$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
SM_EX_in<- resolve_SM_EX %>% filter(matched_name2 %in% names)
SM_in <-  SM_unique %>% filter(SM_unique %in% names)
SM_EX_in_AN <- as.data.frame(SM_EX_in$matched_name2)
SM_EX_in_AN <- SM_EX_in_AN %>%
  rename(Accepted_names = `SM_EX_in$matched_name2`)


#STEP 9:FILTER LEDA DATA FOR STUDY SPECIES USING Vascular_plants_list
SM_in_AN <- as.data.frame(SM_in$SM_unique)
SM_in_AN <- SM_in_AN %>%
  rename(Accepted_names = `SM_in$SM_unique`)


Accepted_names<-rbind(SM_EX_in_AN, SM_in_AN)

SM_EX_in_ID <- as.data.frame(SM_EX_in$user_supplied_name)
SM_EX_in_ID <- SM_EX_in_ID %>%
  rename(`SBS name` = `SM_EX_in$user_supplied_name`)

SM_in_ID <- as.data.frame(SM_in$SM_unique)
SM_in_ID <- SM_in_ID %>%
  rename(`SBS name` = `SM_in$SM_unique`)

#Combine all accepted names
`SBS name`<-rbind(SM_EX_in_ID, SM_in_ID)

SM_Accepted_names_LEDA <- cbind(`SBS name`, Accepted_names)

resolved_SM<- SM_df %>% left_join(SM_Accepted_names_LEDA, by = "SBS name")

resolved_SM <- resolved_SM[complete.cases(resolved_SM$Accepted_names), ]

#NOTE - no Changes to units requied
write.csv(resolved_SM, "R1esolved_SM.csv")

# calculate the mean seed mass
resolved_SM_mean <- R1esolved_SM %>%
  group_by(Accepted_names) %>%
  summarize(mean_value = mean(`single value [mg]`, na.rm = TRUE))


resolved_SM_mean <- resolved_SM_mean %>%
  rename(species = Accepted_names)

# Inserts results into Vascular_plants_list
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM <- select(Vascular_plants_list_Traits_ALDMC_SLA_LEDA, -mean_value)
Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA, resolved_SM_mean, by = "species", all.x = TRUE)

Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM  <- Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM  %>% mutate(seed_mass = coalesce(seed_mass, mean_value))

#OPTIONAL - count the remaining gaps in the datasets
SMA_na_count <- colSums(is.na(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM))
SMB_na_count <- colSums(is.na(Vascular_plants_list_Traits_ALDMC_SLA_LEDA))

SMB_na_count
SMA_na_count
