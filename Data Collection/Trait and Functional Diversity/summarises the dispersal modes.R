# The following script describes how sla was generated from BIEN and TR8
# note: combine sla together, use the  dataset with 
#all the resolved species names and crops removed (Vascular_plants_list)

#STEP 1: INSTALL AND LOAD 

install.packages("readr")    
install.packages("tidyverse")    
install.packages("taxize")   
install.packages("bdc")   
install.packages("TR8")


library(readr)    
library(tidyverse)    
library(taxize)   
library(bdc)
library(TR8)

#STEP 2: IMPORT THE BIEN DATA

trait <- c("whole plant dispersal syndrome")
#whole plant dispersal syndrome from BIEN

BIEN_PH<-BIEN_trait_trait(
  trait,
  all.taxonomy = TRUE,
  political.boundaries = FALSE,
  source.citation = FALSE,
)

#Isolate the study species 
names <- Vascular_plants_list$species

PDS_unique <- unique(PDS_df$verbatim_scientific_name)
PDS_unique <- as.data.frame(PDS_unique)
PDS_EX<- PDS_unique %>% filter(!PDS_unique %in% names)

# STEP 3: RESOLVE THE SPECIES NAMES SO THAT THEY WILL CORRESPOND TO Vascular_plants_list FORMAT

subset1_row <- 0:1261

PDS_EX1 <- PDS_EX$PDS_unique[subset1_row]
PDS_EX1 <- as.data.frame(PDS_EX1)

names(PDS_EX1) <- "species"


# Resolve the species using GBIF Backbone Taxonomy
resolve_PDS_EX <- PDS_EX1$species  %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)

#mannually resolve the unresolved names

#STEP 4:FILTER BIEN DATA FOR STUDY SPECIES USING Vascular_plants_list

#Copies the column matched_name2 (resolved names) into a new column FillAccepted_names.
PDS_EX_in<- resolve_PDS_EX %>% filter(matched_name2 %in% names)
PDS_in <-  PDS_unique %>% filter(PDS_unique %in% names)
PDS_EX_in_AN <- as.data.frame(PDS_EX_in$matched_name2)
PDS_EX_in_AN <- PDS_EX_in_AN %>%
  rename(Accepted_names = `PDS_EX_in$matched_name2`)

PDS_in_AN <- as.data.frame(PDS_in$PDS_unique)
PDS_in_AN <- PDS_in_AN %>%
  rename(Accepted_names = `PDS_in$PDS_unique`)

#Combine all accepted names
Accepted_names<-rbind(PDS_EX_in_AN, PDS_in_AN)




PDS_EX_in_ID <- as.data.frame(PDS_EX_in$user_supplied_name)
PDS_EX_in_ID <- PDS_EX_in_ID %>%
  rename(verbatim_scientific_name = `PDS_EX_in$user_supplied_name`)

PDS_in_ID <- as.data.frame(PDS_in$PDS_unique)
PDS_in_ID <- PDS_in_ID %>%
  rename(verbatim_scientific_name = `PDS_in$PDS_unique`)

verbatim_scientific_name<-rbind(PDS_EX_in_ID, PDS_in_ID)

# pairs the resolved names to unresolved names

PDS_Accepted_names_BIEN <- cbind(verbatim_scientific_name, Accepted_names)

# Attach accepted names back to the disperal mode dataset
resolved_PDS<- PDS_df %>% left_join(PDS_Accepted_names_BIEN, by = "verbatim_scientific_name")

#Remove unresolved rows
resolved_PDS <- resolved_PDS[complete.cases(resolved_PDS$Accepted_names), ]


# summerised the disperal modes
resolved_PDS_mean <- resolved_PDS %>%
  group_by(Accepted_names) %>%
  summarize(disperal_method = paste(trait_value, collapse = " / "))

# Inserts results into Vascular_plants_list
resolved_PDS_mean <- resolved_PDS_mean %>%
  rename(species = Accepted_names)

Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PDS <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM, resolved_PDS_mean, by = "species", all.x = TRUE)


### TR8 ### ----

# Note - names could not be resolved using tr8 as was only used for gap filling

#STEP 10:EXTRACT THE TR8 plant hieght TO FILL IN THE GAPS 
My_traits<-tr8(species_list=c(names), download_list=c("DispMode"), allow_persistent=TRUE)
TR8_DIS<-My_traits@results

#save the results and inspect
write.csv(TR8_DIS, "TR8_DIS.csv")

#STEP 11: STANDARDISE LABEL AND ADD THE RESULT TO Vascular_plants_list_Traits
colnames(TR8_DIS)[colnames(TR8_DIS) == "...1"] <- "species"

Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PDS <- merge(Vascular_plants_list_Traits_ALDMC_SLA_LEDA_SM_PDS, TR8_DIS, by = "species", all.x = TRUE)

