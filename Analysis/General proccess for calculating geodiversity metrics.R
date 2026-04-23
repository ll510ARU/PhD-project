# The following script describes how to calculate geodiversity metrics for given spatial resolution 
# The individual landscape features recorded as a given spatial resolution are referred to as followed as Landscape_features_complete generated from British Geological Society via Edina Digimap data
# and sourced from Bailey et al., 2017 study see Landscape scripts view how the data was generated.


#STEP 1: Organised the data into the separate geodiversity metrics to calculate Shannon Diversity
# Landforms: Flats, Valley, Peak_ridges, Hollows and pits
# Geology : Bedrock, soil depth, soil texture
# Superfical deposits sources


# Formating landforms

Landforms <- data.frame(Tile_name = Landscape_features_complete$Tile_name,
  Flats = Landscape_features_complete$flats,
              Valleys = Landscape_features_complete$valleys, 
              Peak_ridges =  Landscape_features_complete$Peak_ridges, 
              Hollows = Landscape_features_complete$hollows,
              Pits = Landscape_features_complete$pits)

data_long_Landform <- Landforms %>%
  pivot_longer(
    cols = -Tile_name,                 # Select all columns except tile name
    names_to = c("Landform"))


# Formating geology

Geology <- data.frame(Tile_name = Landscape_features_complete$Tile_name,
                            Bedrock = Landscape_features_complete$Bedrock,
                            Soil_depth = Landscape_features_complete$Soil_depth,
                          Soil_text = Landscape_features_complete$Soil_text)

data_long_Geology <- Geology %>%
  pivot_longer(
    cols = -Tile_name,                 # Select all columns except tile name
    names_to = c("Geo_type"))


# Formating Superfical Deposit sources (SD)

Superfical_deposits <- data.frame(Tile_name = Landscape_features_complete$Tile_name,
                                     Superfical_deposits = Landscape_features_complete$Superfical_deposits)


#STEP 2: Calculate Sharon Diversity Index for 
# Landforms: Flats, Valley, Peak_ridges, Hollows and pits
# Geology : Bedrock, soil depth, soil texture
# Superfical deposits sources

#Landforms
shannon_LANDFORM <- data_long_Landform %>%
  filter(value > 0) %>%                           # Remove rows where area is zero
  group_by(Tile_name) %>%
  mutate(total_area = sum(value),                 # Calculate total area for each tile
         proportion = value / total_area) %>%     # Calculate proportion of each feature
  summarise(
    m = n_distinct(Landform),                     # Number of unique features in the tile
    shannon_diversity = -sum(proportion * log(proportion), na.rm = TRUE), # Shannon diversity index
    shannon_evenness = shannon_diversity / log(m) # Shannon evenness index (same as normalized)
  )


# save Landform Shannon diversity

write.csv(shannon_LANDFORM, "shannon_LANDFORM_write_the_given_resolution.csv")


# Geology

shannon_GEOLOGY <- data_long_Geology %>%
  filter(value > 0) %>%                           # Remove rows where area is zero
  group_by(Tile_name) %>%
  mutate(total_area = sum(value),                 # Calculate total area for each tile
         proportion = value / total_area) %>%     # Calculate proportion of each feature
  summarise(
    m = n_distinct(GEO_Type),                     # Number of unique features in the tile
    shannon_diversity = -sum(proportion * log(proportion), na.rm = TRUE), # Shannon diversity index
    shannon_evenness = shannon_diversity / log(m) # Shannon evenness index 
  )


# save Landform Shannon diversity

write.csv(shannon_GEOLOGY, "shannon_LANDFORM_write_the_given_resolution.csv")


# Superfical deposits sources

shannon_Superfical_deposits <- Superfical_deposits %>%
  filter(AREA > 0) %>%                           # Remove rows where area is zero
  group_by(Tile_name) %>%
  mutate(total_area = sum(AREA),                 # Calculate total area for each tile
         proportion = AREA / total_area) %>%     # Calculate proportion of each feature
  summarise(
    m = n_distinct(Board_group),                     # Number of unique features in the tile
    shannon_diversity = -sum(proportion * log(proportion), na.rm = TRUE), # Shannon diversity index
    shannon_evenness = shannon_diversity / log(m) # Shannon evenness index 
  )

# save Superfical_deposits 

write.csv(shannon_Superfical_deposits, "shannon_Superfical_deposits_write_the_given_resolution.csv")

#STEP 3: Organise the data into the seporate geodiversity metrics 
#to calculate river and canal complexity


hydrological_Features <- data.frame(Tile_name = Landscape_features_complete$Tile_name,
                                  Rivers = Landscape_features_complete$River_length,
                                  Canals = Landscape_features_complete$Canal_length,
                                  HF_RF = Landscape_features_complete$HF_RF)

#STEP 4: Creating a new column and use the Max-Min Normalisation

#Identify the max and min values

summary(hydrological_Features)

# Calculate the normalised values 

hydrological_Features$NORM_RIVER <- 
  (hydrological_Features$Rivers - 0) / 
  #(wite the max value# - #write the min values#) * 10



hydrological_Features$NORM_Canal <- 
  (hydrological_Features$Canals - 0) / 
 # (wite the max value# - #write the min values#) * 10

# STEP 5: Round the normalize score to 2 decimal places 
    #and then define the boundary and associated scores for 
    #river and canal length.

# Rounds the values
hydrological_Features$NORM_RIVER_2dp <- round(hydrological_Features$NORM_RIVER, digits = 2)  
hydrological_Features$NORM_Canal_2dp <- round(hydrological_Features$NORM_Canal, digits = 2)  

# Define the boundary values and the the corresponding scores

breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  
scores <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)  
    
# STEP 6: Assign scores 

hydrological_Features$NORM_Rivers_scores <- scores[findInterval(hydrological_Features$NORM_RIVER, breaks)]
hydrological_Features$NORM_Canal_scores <- scores[findInterval(hydrological_Features$NORM_Canal, breaks)]

# STEP 7: Calculate waterway complexity for rivers and canals

hydrological_Features$RIVER_COM <- (hydrological_Features$HF_RF * hydrological_Features$NORM_River_score)


hydrological_Features$Canal_COM <- (hydrological_Features$HF_RF * hydrological_Features$NORM_Canal_score)


write.csv(hydrological_Features, "hydrological_Features__write_the_given_resolution.csv")

# STEP 8: Load elevation and slope range files
#referred here after as Elev_range and slope_range


#STEP 9: ensure that elevation and slope range is 
# confined to the correct resolution

BASE <- data.frame(Tile_name = Landscape_features_complete$Tile_name)

# ELEVATION
Elev_range <- merge(BASE, Elev_range, by = "Tile_name", all.x = TRUE)

#Slope
slope_range <- merge(BASE, slope_range, by = "Tile_name", all.x = TRUE)

# STEP 10: Combine the individual Geodiversity metrics into one dataset

GEOC_LAN_GEO <-merge(Gshannon_LANDFORM, shannon_GEOLOGY, 
                    by = "Tile_name", all = TRUE)

GEOC_LAN_GEO_SD <-merge(GEOC_LAN_GEO, shannon_Superfical_deposits, 
                     by = "Tile_name", all = TRUE)


GEOC_LAN_GEO_SD_HF <-merge(GEOC_LAN_GEO_SD, hydrological_Features, 
                        by = "Tile_name", all = TRUE)


GEOC_LAN_GEO_SD_HF_EL <-merge(GEOC_LAN_GEO_SD_HF, Elev_range, 
                           by = "Tile_name", all = TRUE)


GEOC_LAN_GEO_SD_HF_EL_SL <-merge(GEOC_LAN_GEO_SD_HF_EL_SL, slope_range, 
                              by = "Tile_name", all = TRUE)

#STEP 11: Save the combined datasets of results

write.csv(GEOC_LAN_GEO_SD_HF_EL_SL, "GEODIVERSITY_Metrics.csv")

#STEP 12: Repeat for other spatial resolutions