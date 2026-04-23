#The following script explains how to remove the crop species using .... after resolving the names
# Note: Final_species_list is the file that has the resolved species names generated from the R script named species occurrence.


#STEP 1: Create a new column with the combined genus and species of the crop species.

Crop_speciesGBR$SpeciesName <- paste(Crop_speciesGBR$GENUS, Crop_speciesGBR$SPECIES, sep = " ")

#STEP 2: Keep only one copy of the species names and save the list for reference.
data_unique <- unique(Crop_speciesGBR)
write.csv(data_unique, "crop2_unique.csv")

#STEP 3: Separate the names into subsets for 1000 rows, this is to speed up the process
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

subset11_row <- 10001:11000
subset12_row <- 11001:12000
subset13_row <- 12001:13000
subset14_row <- 13001:14000
subset15_row <- 14001:15000

subset16_row <- 15001:16000
subset17_row <- 16001:17000
subset18_row <- 17001:18000
subset19_row <- 18001:19000
subset20_row <- 19001:19674


CropST1 <- crop2_unique$SpeciesName[subset1_row]
CropST1 <- as.data.frame(CropST1)
CropST2 <- crop2_unique$SpeciesName[subset2_row]
CropST2 <- as.data.frame(CropST2)
CropST3 <- crop2_unique$SpeciesName[subset3_row]
CropST3 <- as.data.frame(CropST3)
CropST4 <- crop2_unique$SpeciesName[subset4_row]
CropST4 <- as.data.frame(CropST4)
CropST5 <- crop2_unique$SpeciesName[subset5_row]
CropST5 <- as.data.frame(CropST5)


CropST6 <- crop2_unique$SpeciesName[subset6_row]
CropST6 <- as.data.frame(CropST6)
CropST7 <- crop2_unique$SpeciesName[subset7_row]
CropST7 <- as.data.frame(CropST7)
CropST8 <- crop2_unique$SpeciesName[subset8_row]
CropST8 <- as.data.frame(CropST8)
CropST9 <- crop2_unique$SpeciesName[subset9_row]
CropST9 <- as.data.frame(CropST9)
CropST10 <- crop2_unique$SpeciesName[subset10_row]
CropST10 <- as.data.frame(CropST10)

CropST11 <- crop2_unique$SpeciesName[subset11_row]
CropST11 <- as.data.frame(CropST11)
CropST12 <- crop2_unique$SpeciesName[subset12_row]
CropST12 <- as.data.frame(CropST12)
CropST13 <- crop2_unique$SpeciesName[subset13_row]
CropST13 <- as.data.frame(CropST13)
CropST14 <- crop2_unique$SpeciesName[subset14_row]
CropST14 <- as.data.frame(CropST14)
CropST15 <- crop2_unique$SpeciesName[subset15_row]
CropST15 <- as.data.frame(CropST15)


CropST16 <- crop2_unique$SpeciesName[subset16_row]
CropST16 <- as.data.frame(CropST16)
CropST17 <- crop2_unique$SpeciesName[subset17_row]
CropST17 <- as.data.frame(CropST17)
CropST18 <- crop2_unique$SpeciesName[subset18_row]
CropST18 <- as.data.frame(CropST18)
CropST19 <- crop2_unique$SpeciesName[subset19_row]
CropST19 <- as.data.frame(CropST19)
CropST20 <- crop2_unique$SpeciesName[subset20_row]
CropST20 <- as.data.frame(CropST20)

# Resolve the species using GBIF Backbone Taxonomy
result.long <-  CropST1$CropST1 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)

results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp1_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df <- fillp1_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST2$CropST2 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df2 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST3$CropST3 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df3 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST4$CropST4 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df4 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST5$CropST5 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df5 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST6$CropST6 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df6 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST7$CropST7 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df7 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST8$CropST8 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df8 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST9$CropST9 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df9 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST10$CropST10 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df10 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST11$CropST11 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df11 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST12$CropST12 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df12 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST13$CropST13 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df13 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST14$CropST14 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df14 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST15$CropST15 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df15 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST16$CropST16 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df16 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST17$CropST17 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df17 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST18$CropST18 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df18 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

result.long <-  CropST19$CropST19 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df19 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)


result.long <-  CropST20$CropST20 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
result.long <- result.long %>%
  rename(SpeciesName = user_supplied_name)
results_df<- crop2_unique %>% left_join(result.long, by = "SpeciesName")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df20 <- fillp_result_df %>% distinct(SpeciesName, .keep_all = TRUE)

# Save files for reference and check resolusions
FCropST1 <- dis_fillp1_result_df$`Accepted names`[subset1_row]
FCropST1 <- as.data.frame(FCropST1)
FCropST2 <- dis_fillp1_result_df2$`Accepted names`[subset2_row]
FCropST2 <- as.data.frame(FCropST2)
FCropST3 <- dis_fillp1_result_df3$`Accepted names`[subset3_row]
FCropST3 <- as.data.frame(FCropST3)
FCropST4 <- dis_fillp1_result_df4$`Accepted names`[subset4_row]
FCropST4 <- as.data.frame(FCropST4)
FCropST5 <- dis_fillp1_result_df5$`Accepted names`[subset5_row]
FCropST5 <- as.data.frame(FCropST5)
FCropST6 <- dis_fillp1_result_df6$`Accepted names`[subset6_row]
FCropST6 <- as.data.frame(FCropST6)
FCropST7 <- dis_fillp1_result_df7$`Accepted names`[subset7_row]
FCropST7 <- as.data.frame(FCropST7)
FCropST8 <- dis_fillp1_result_df8$`Accepted names`[subset8_row]
FCropST8 <- as.data.frame(FCropST8)
FCropST9 <- dis_fillp1_result_df9$`Accepted names`[subset9_row]
FCropST9 <- as.data.frame(FCropST9)
FCropST10 <- dis_fillp1_result_df10$`Accepted names`[subset10_row]
FCropST10 <- as.data.frame(FCropST10)
FCropST11 <- dis_fillp1_result_df11$`Accepted names`[subset11_row]
FCropST11 <- as.data.frame(FCropST11)
FCropST12 <- dis_fillp1_result_df12$`Accepted names`[subset12_row]
FCropST12 <- as.data.frame(FCropST12)
FCropST13 <- dis_fillp1_result_df13$`Accepted names`[subset13_row]
FCropST13 <- as.data.frame(FCropST13)
FCropST14 <- dis_fillp1_result_df14$`Accepted names`[subset14_row]
FCropST14 <- as.data.frame(FCropST14)
FCropST15 <- dis_fillp1_result_df15$`Accepted names`[subset15_row]
FCropST15 <- as.data.frame(FCropST15)
FCropST16 <- dis_fillp1_result_df16$`Accepted names`[subset16_row]
FCropST16 <- as.data.frame(FCropST16)
FCropST17 <- dis_fillp1_result_df17$`Accepted names`[subset17_row]
FCropST17 <- as.data.frame(FCropST17)
FCropST18 <- dis_fillp1_result_df18$`Accepted names`[subset18_row]
FCropST18 <- as.data.frame(FCropST18)
FCropST19 <- dis_fillp1_result_df19$`Accepted names`[subset19_row]
FCropST19 <- as.data.frame(FCropST19)
FCropST20 <- dis_fillp1_result_df20$`Accepted names`[subset20_row]
FCropST20 <- as.data.frame(FCropST20)

# Combine the resolved crop species names
names(FCropST1) <- "Accepted names"
names(FCropST2) <- "Accepted names"
names(FCropST3) <- "Accepted names"
names(FCropST4) <- "Accepted names"
names(FCropST5) <- "Accepted names"
names(FCropST6) <- "Accepted names"
names(FCropST7) <- "Accepted names"
names(FCropST8) <- "Accepted names"
names(FCropST9) <- "Accepted names"
names(FCropST10) <- "Accepted names"
names(FCropST11) <- "Accepted names"
names(FCropST12) <- "Accepted names"
names(FCropST13) <- "Accepted names"
names(FCropST14) <- "Accepted names"
names(FCropST15) <- "Accepted names"
names(FCropST16) <- "Accepted names"
names(FCropST17) <- "Accepted names"
names(FCropST18) <- "Accepted names"
names(FCropST19) <- "Accepted names"
names(FCropST20) <- "Accepted names"
joined_df <- rbind(FCropST1, FCropST2, FCropST3, FCropST4, FCropST5, FCropST6, 
                   FCropST7, FCropST8, FCropST9, FCropST10, FCropST11, FCropST12,
                   FCropST13, FCropST14, FCropST15, FCropST16, FCropST17, FCropST18, FCropST19, FCropST20)

#STEP 4: join to the orginal names
new_values <- c(joined_df$`Accepted names`)
crop2_unique <- crop2_unique %>%
  mutate(`Accepted names` = new_values)

write.csv(crop2_unique, "crop2_unique_accepted.csv")

#STEP 5: Check the unresolved crop species names
SP1<- c("WPGS Amphiploid", "Eremopyron bounapartis")
names_har1<- bdc_query_names_taxadb(
  SP1,
  replace_synonyms = TRUE,
  suggest_names = TRUE,
  suggestion_distance = 0.9,
  db = "ncbi",
  parallel = TRUE,
  ncores = 2,
  export_accepted = FALSE)

# MANNUAL - Within excel addin the UN_name_check to main species and fill in the 
#main gaps with World Checklist of Vascular Plants 

#STEP 6: reove the crop species from the study species list 
names_to_remove <- c(crop2_unique$`Accepted names`)
Vascular_plants_corrected_list$species
# Remove rows with names in the list
Vascular_plants_list <- Vascular_plants_list %>%
  filter(!species %in% names_to_remove)

write.csv(Vascular_plants_list, "Vascular_plants_corrected_list_crops_removed.csv")

# Note that before the fourth corner the crop species were remove from the species occurrence and traits.

