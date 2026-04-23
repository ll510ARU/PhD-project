#The following script calculates river length, canal length and the hydrological features within the watercourse
#The orginal files are generated from OS Strategi via Edina Digimap and were pre-proccessed by adding the monad grid referrences in QGIS

#STEP 1: INSTALL AND LOAD THE FOLLOWING PACKAGES
install.packages("tidyverse")
library("tidyverse")

#STEP 2: CREATE AN ID FOR THE RIVER AND CANAL LENGTH WITH THE MONAD LOCATIONS
River_corrected$mon_form <- paste(River_corrected$tile_name, "-", River_corrected$form)

#STEP 3: SELECT COLUMNS RIVER_ID = "mon_form", MONAD NAME = "tile_name", WATERCOURSE_TYPE = "form", WATERCOUSE LENGTH = "WATERCOURSE_leng"
River_length <- River_corrected[, c("mon_form", "tile_name", "form", "WATERCOURSE_leng")]

#STEP 4: SUMERISE THE TOTAL LENGTH OF THE WATERCOUSE WITHIN THE MONAD LOCATION
River_length <- WATERCOURSE_length %>%
  group_by(mon_form) %>%
  summarize(Tot_River_length = sum(River_leng, na.rm = TRUE))

#STEP 5: SPLIT THE ID IN WATERCOURSE AND MONAD LOCATIONS WITH SUMMERISED LENGTH
split <- strsplit(River_length$mon_form, "-")
River_length$tile_name <- sapply(split, `[`, 1)
River_length$form <- sapply(split, `[`, 2)
River_length <- River_length[, c("tile_name", "form", "Tot_River_length")]

#STEP 6: SAVE FILE AND EDIT FORM TO HAVE "inlandRiver" AND "tidalRiver" SUMMERISED TO "river "AND REMOVE "lake"
write.csv(River_length, "River_length.csv")

River_length <- River_length %>%
  mutate(form = case_when(
    form %in% c("inlandRiver", "tidalRiver") ~ "river", 
    form == "lake" ~ NA_character_,                    
    TRUE ~ form                                        
  )) %>%
  filter(!is.na(form))  

#STEP 6: CONVERT THE DATAFRAME TO LONG TO WIDE FORMATING 
Rivers_Length <- spread(River_length, form, Tot_River_length)
write.csv(Rivers_Length, "Rivers_Length_Wide.csv")


### HYDROLOGICAL FEATURES ### ------

#STEP 7: CREATE AN ID FOR THE HYDROLOGICAL FEATURES IN MONAD LOCATIONS 
Hydro_features_nodes$mon_form <- paste(Hydro_features_nodes$tile_name, "-", Hydro_features_nodes$hydro_node)
Hydro_features_nodes <- Hydro_features_nodes[, c("mon_form", "tile_name", "hydro_node")]

#STEP 8: SUMMERISE THE TOTAL COUNT OF HYDROLOGICAL FEATURES WITHIN MONAD LOCATIONS
Hydro_features_nodes_count <- Hydro_features_nodes %>%
  group_by(mon_form) %>%
  summarize(count = n())

#STEP 9: SPLIT THE ID IN HYDROLOGICAL FEATURES AND MONAD LOCATIONS WITH SUMMERISED COUNTS
split <- strsplit(Hydro_features_nodes_count$mon_form, "-")
Hydro_features_nodes_count$tile_name <- sapply(split, `[`, 1)
Hydro_features_nodes_count$form <- sapply(split, `[`, 2)

Hydro_features_nodes_count <- na.omit(Hydro_features_nodes_count)

#STEP 10: CONVERT THE DATAFRAME TO LONG TO WIDE FORMATING 
Hydro_features_nodes_count_wide <- spread(Hydro_features_nodes_count, form, count)
write.csv(Hydro_features_nodes_count_wide, "Hydro_features_nodes_count_wide.csv")







