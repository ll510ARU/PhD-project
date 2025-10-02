# The following scripts describes how the individual landscape datasets were put together 
# Note before running this code a dataset was created containing only the species occurrence monads (locations) in excel
# This was to make it easier the filter the landscape features location of that correspond to the species occurrences locations
# where approiate after this script the locations that did not have landscape feature recorded were in filled with a zero using excel

#STEP 1: INSTALL AND LOAD PACKAGES
install.packages("tidyverse")
library("tidyverse")

install.packages(tidyr)
library("tidyr")

#STEP 2: JOIN THE CLIMATE VARIABLES 

#Ensure the location column is the same use this 
command colnames(INSERT CLIMATE DATASET)[colnames(INSERT CLIMATE DATASET) == "INSERT THE PREVIOUS CLIMATE DATASET LOCATION NAME"]<- "tile_name"


Landscape_features <- merge(Locations,
                            Mean_T_TEM_Subset,
                            by = "tile_name", 
                            all.x = TRUE)


Landscape_features <- merge(Landscape_features,
                            Seasonalitiy_T_TEM_Subset,
                            by = "tile_name", 
                            all.x = TRUE)

Landscape_features <- merge(Landscape_features,
                            Mean_prep_Subset,
                            by = "tile_name", 
                            all.x = TRUE)

Landscape_features <- merge(Landscape_features,
                            Seasonality_Prec_Subset,
                            by = "tile_name", 
                            all.x = TRUE)

# RENAME THE COLUMNS
colnames(Landscape_features)[colnames(Landscape_features) == "insert previous column name"]<- "mean_Prep_seasonality"
colnames(Landscape_features)[colnames(Landscape_features) == "insert previous column name"]<- "mean_Tem_seasonality"

#STEP 3: JOIN THE LANDFORM

#REMINDER - ASPECT RATIO NEED THIS CONVERSION
Aspect_tem$MEAN_asp <- cos(Aspect_tem$MEAN_asp * pi / 180)

# merge aspect ratio

Landscape_features <- merge(Landscape_features,
                            Aspect_tem,
                            by = "tile_name", 
                            all.x = TRUE)

# merge mean elevation 

Landscape_features <- merge(Landscape_features,
                             Elevation_TEM,
                             by = "tile_name", 
                             all.x = TRUE)

# merge flats, peaks-ridges and slope

Landscape_features <- merge(Landscape_features,
                            Flats_valley_Peaks-Ridges_Slope,
                            by = "tile_name", 
                            all.x = TRUE)


write.csv(Landscape_features, "Summerised_Lanscape_features.csv")


#STEP 4: JOIN THE BEDROCK 

Landscape_features <- merge(Landscape_features,
                            BR_TYPE_S,
                            by = "tile_name", 
                            all.x = TRUE)


# STEP 5: JOIN THE HYDROLOGICAL FEATURES 

#REMINDER - THE HYDROLOGICAL DATA NEED TO BE IN WIDE FORMAT

Rivers_Length <- spread(River_length, form, Tot_River_length)
Hydro_features_nodes_count_wide <- spread(Hydro_features_nodes_count, form, count)


Landscape_features <- merge(Landscape_features,
                            Rivers_Length,
                            by = "tile_name", 
                            all.x = TRUE)


Landscape_features <- merge(Landscape_features,
                            Hydro_features_nodes_count_wide,
                            by = "tile_name", 
                            all.x = TRUE)



#STEP 6: JOIN THE SOIL FEATURES

# Ensure that all tie breakers have been resolved using this script - 
# Calculates percentage coverage of soil texture and depth

Landscape_features <- merge(Landscape_features,
                            Soil_depth_resolved,
                            by = "tile_name", 
                            all.x = TRUE)

Landscape_features <- merge(Landscape_features,
                            Soil_tex_sortedtr,
                            by = "tile_name", 
                            all.x = TRUE)



#STEP 6: JOIN THE SUPERIFICIAL DEPOSITS 


Landscape_features <- merge(Landscape_features,
                            SD_Board_wide,
                            by = "tile_name", 
                            all.x = TRUE)


#STEP 7: SAVE THE LANDSCAPE FEATURES
write.csv(Landscape_features, "Landscape_features_complete.csv")



