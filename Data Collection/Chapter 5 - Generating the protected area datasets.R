## The following script describes how the protected areas were selected 
# after being collected from WDPA, area calculated and converted into a shapefile on QGIS
# The datasets generated are for the British designate protected areas () and the IUCN framework ()

#STEP 1: INSTALL AND LOAD PACKAGES


install.packages("stringr")
library(stringr)

install.packages("sf")
library(sf)

install.packages("dplyr")
library(dplyr)

#STEP 2: READ THE PROTECTED AREA SHAPEFILE INTO RSTUDIO

shapefile <- st_read("AREA_COVERAGE_PA.shp")


str(shapefile)
head(shapefile)

PA_SOURCE <- as.data.frame(shapefile)


#STEP 3: REMOVE THE AREAS WITHOUT PROTECTED AREA COVERAGE

PA_SOURCE <- PA_SOURCE %>% filter(!is.na(WDPA_PID))

PA_SOURCE <- PA_SOURCE %>% filter(!is.na(area))



#STEP 4: GENERATE THE LAND AREA ASSOCIATED WITH THE PROTECTED AREAS

LAND_AREA <- data.frame(tile_name = species_occ$tile_name, LAND_Percentage = species_occ$LAND_Percentage)

PA_SOURCE_DES <- left_join(PA_SOURCE, LAND_AREA, by = "tile_name")

#STEP 5: REMOVE THE SQUARES WITH LESS 50% LAND COVER
PA_SOURCE_DES_50 <- PA_SOURCE_DES %>%
  filter(LAND_Percentage >= 50)

PA_SOURCE_DES_50 <- PA_SOURCE_DES_50 %>% select(-geometry)

#STEP 6: SAVE FILE FOR REFERNCE
write.csv(PA_SOURCE_DES_50, "PA_SOURCE_DES_50.csv")


# STEP 7; REMOVE THE DESIGNATION THAT MARINE PROTECTED AREAS
PA_SOURCE_DES_50_WMAINE <- PA_SOURCE_DES_50 %>%
  filter(!DESIG %in% c(
    "Community Marine Conservation Area",
    "Marine Protected Area (OSPAR)",
    "Nature Conservation Marine Protected Area",
    "Marine Conservation Zone", "Heritage Coast"
    ))


#STEP 8: CALCULATE THE PERCENTAGE COVER FOR THE BRITISH DESIGNATION WITHOUT MARINE 
#CREATE ID FOR PROTECTED AREA AND MONAD
PA_SOURCE_DES_50_WMAINE <- left_join(PA_SOURCE_DES_50_WMAINE, Whole_area, by = "tile_name")
PA_SOURCE_DES_50_WMAINE$Mon_Cat <- paste(PA_SOURCE_DES_50_WMAINE$tile_name, PA_SOURCE_DES_50_WMAINE$DESIG_ENG_ALTERED, sep = "_")

#SUMMERISE THE PROTECTED AREA WITHIN THE MONAD LOCATIONS
df_CAT_Dis <- PA_SOURCE_DES_50_WMAINE %>% group_by(Mon_Cat) %>% summarize(Sum = sum(AREA_COVER))

#SPLIT THE ID IN THE PROTECTED AREA AND MONAD LOCATIONS WITH SUMMERISED AREA
split <- strsplit(df_CAT_Dis$Mon_Cat, "_")
df_CAT_Dis$tile_name <- sapply(split, `[`, 1)
df_CAT_Dis$DES<- sapply(split, `[`, 2)

#THE MONAD AREA INFORMATION
df_CAT_INFO <- PA_SOURCE_DES_50_WMAINE[, c("tile_name", "area")]

#SUMMERISE MONAD LOCATIONS AREA
df_CAT_INFO  <- df_CAT_INFO  %>% group_by(tile_name) %>% summarize(tile_ARA = sum(area))

#COMBINE THE MONADS AND PROTECTED AREA DATASETS 
df_CAT_result_WMAINE <- merge(df_CAT_Dis, df_CAT_INFO, by = "tile_name")


df_CAT_result_WMAINE <- df_CAT_result_WMAINE[, c("tile_name", "Sum", "tile_ARA", "DES")]

names(df_CAT_result_WMAINE)[names(df_CAT_result_WMAINE) == "Sum"] <- "DES_area"
names(df_CAT_result_WMAINE)[names(df_CAT_result_WMAINE) == "tile_ARA"] <- "Title_Area"

#CALCULATE THE PERCENTAGE COVER
df_CAT_result_WMAINE$Title_Area <- df_CAT_result_WMAINE$Title_Area/1000000
df_CAT_result_WMAINE$DES_area <- df_CAT_result_WMAINE$DES_area/1000000
df_CAT_result_WMAINE$Title_Area  <- round(df_CAT_result_WMAINE$Title_Area, digits = 2)
df_CAT_result_WMAINE$DES_area <- round(df_CAT_result_WMAINE$DES_area, digits = 2)
df_CAT_result_WMAINE$DES_Percentage <- (df_CAT_result_WMAINE$DES_area/df_CAT_result_WMAINE$Title_Area)*100
df_CAT_result_WMAINE$DES_Percentage <- round(df_CAT_result_WMAINE$DES_Percentage, digits = 2)


#STEP 9: REMOVE THE OVERLAPPING PROTECTED AREAS
df_CAT_result_WMAINE_EN_NR_WR <- df_CAT_result_WMAINE %>%
  filter(!DES %in% c("Emerald Network", "Nature Reserve", "Wildland reserve", "Heritage Coast", "Special Protection Area (Birds Directive)", "UNESCO-MAB Biosphere Reserve"))


#STEP 10: GENERATE THE BRITISH DESIGNATION'S RANKING 
summary_df_CAT_result_COUNT <- df_CAT_result_WMAINE_EN_NR_WR %>%
  group_by(DES) %>%
  summarise(count = n())

DES_levels <- c("Site of Special Scientific Interest","National Park", "Area of Outstanding Natural Beauty","National Scenic Area",
                "Ramsar Site, Wetland of International Importance","National Nature Reserve", 
                "Local Nature Reserve", "Regional Park", "World Heritage Site (natural or mixed)")  

#STEP 11: GENERATE THE DOMINATE PROTECTED AREA WITHIN THE MONAD LOCATIONS 
PA_SOURCE_DES_50_WMAINE_EN_NR_WR_ <- df_CAT_result_WMAINE_EN_NR_WR %>%
  mutate(DES = factor(DES, levels = DES_levels, ordered = TRUE)) %>%  # Convert to ordered factor
  group_by(tile_name) %>%
  slice_max(order_by = DES_Percentage, with_ties = TRUE) %>%  # Keep highest designation Percentage
  slice_max(order_by = DES, with_ties = FALSE) %>%  # Resolve ties by highest ranked designation 
  ungroup() %>%
  rename(Most_LIKELY_DES = DES)

#STEP 12: SAVE THE BRITISH DESIGNATION DATASET FOR REFERENCE
write.csv(PA_SOURCE_DES_50_WMAINE_EN_NR_WR_, "PA_SOURCE_DES_50_WMAINE_EN_NR_WR_.csv")

#STEP 13:FILTER THE IUCN CATGORIES AND REMOVE THE MONADS WITH LESS THAN 50%
IUCN_CAT <- PA_SOURCE_DES_50_WMAINE_EN_NR_WR_HC %>% 
  filter(IUCN_CAT %in% c("IV", "V" , "III", "II", "VI", "Ia"))

PA_SOURCE_IUCN_50_WMAINE <- IUCN_CAT %>%
  filter(LAND_Percentage >= 50)

#STEP 14: CALCULATE THE PERCENTAGE COVER FOR IUCN CATGORIES
PA_SOURCE_IUCN_50_WMAINE$Mon_Cat <- paste(PA_SOURCE_IUCN_50_WMAINE$tile_name, PA_SOURCE_IUCN_50_WMAINE$IUCN_CAT, sep = "_")

df_CAT_Dis <- PA_SOURCE_IUCN_50_WMAINE %>% group_by(Mon_Cat) %>% summarize(Sum = sum(AREA_COVER))


split <- strsplit(df_CAT_Dis$Mon_Cat, "_")
df_CAT_Dis$tile_name <- sapply(split, `[`, 1)
df_CAT_Dis$IUCN<- sapply(split, `[`, 2)


df_CAT_INFO <- PA_SOURCE_IUCN_50_WMAINE[, c("tile_name", "area")]


df_CAT_INFO  <- df_CAT_INFO  %>% group_by(tile_name) %>% summarize(tile_ARA = sum(area))

# Merge the data frames using merge()
df_CAT_result_WMAINE_IUCN <- merge(df_CAT_Dis, df_CAT_INFO, by = "tile_name")


df_CAT_result_WMAINE_IUCN <- df_CAT_result_WMAINE_IUCN[, c("tile_name", "Sum", "tile_ARA", "IUCN")]

names(df_CAT_result_WMAINE_IUCN)[names(df_CAT_result_WMAINE_IUCN) == "Sum"] <- "IUCN_area"
names(df_CAT_result_WMAINE_IUCN)[names(df_CAT_result_WMAINE_IUCN) == "tile_ARA"] <- "Title_Area"

df_CAT_result_WMAINE_IUCN$Title_Area <- df_CAT_result_WMAINE_IUCN$Title_Area/1000000
df_CAT_result_WMAINE_IUCN$IUCN_area <- df_CAT_result_WMAINE_IUCN$IUCN_area/1000000
df_CAT_result_WMAINE_IUCN$Title_Area <- round(df_CAT_result_WMAINE_IUCN$Title_Area, digits = 2)
df_CAT_result_WMAINE_IUCN$IUCN_area<- round(df_CAT_result_WMAINE_IUCN$IUCN_area, digits = 2)
df_CAT_result_WMAINE_IUCN$IUCN_Percentage <- (df_CAT_result_WMAINE_IUCN$IUCN_area/df_CAT_result_WMAINE_IUCN$Title_Area)*100
df_CAT_result_WMAINE_IUCN$IUCN_Percentage <- round(df_CAT_result_WMAINE_IUCN$IUCN_Percentage, digits = 2)

#STEP 15: SAVE THE IUCN DATASET FOR REFERENCE

write.csv(df_CAT_result_WMAINE_IUCN, "PA_IUCN_50_WMARINE_ALL_DATA.csv")


#STEP 16: CALCULATE THE MOST DOMINATE IUCN CATGORY PER MONAD
IUCN_levels <- c( "VI","V" ,"IV", "III", "II", "Ia")  

df_CAT_result_EXCLUIUCN <- df_CAT_result_WMAINE_IUCN %>%
  mutate(IUCN = factor(IUCN, levels = IUCN_levels, ordered = TRUE)) %>%  # Convert to ordered factor
  group_by(tile_name) %>%
  slice_max(order_by = IUCN_Percentage, with_ties = TRUE) %>%  # Keep highest IUCN_Percentage
  slice_max(order_by = IUCN, with_ties = FALSE) %>%  # Resolve ties by highest IUCN category
  ungroup() %>%
  rename(Most_LIKELY_IUCN = IUCN)

#STEP 17: SAVE THE IUCN DATASET FOR REFERENCE 
Sum_EXCLUIUCN <- left_join(df_CAT_result_EXCLUIUCN, PA_SOURCE_IUCN_50_WMAINE, by = "tile_name")
Sum_EXCLUIUCN <- Sum_EXCLUIUCN[!duplicated(Sum_EXCLUIUCN$tile_name), ]
write.csv(Sum_EXCLUIUCN, "PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA.csv")

# STEP 18: JOIN THE PROTECTED AREA DATA TO FUNCTIONAL DIVERSITY DATASET (FDM_DIVERSITY)

PA_SOURCE$Protected_status <- "Protected_Area"
PA_SOURCE_Com <- merge(FDM_DIVERSITY,PA_SOURCE, by = "tile_name", all.x = TRUE)

PA_SOURCE_Com$Protected_status[is.na(PA_SOURCE_Com$Protected_status)] <- "Non-Protected Area"

PA_SOURCE_Com <- unique(PA_SOURCE_Com)

PA_SOURCE_Com <- data.frame(tile_name = PA_SOURCE_Com$tile_name, 
                            Protected_status = PA_SOURCE_Com$Protected_status,
                            Native_status = PA_SOURCE_Com$Native_status,
                            FDIS = PA_SOURCE_Com$FDis,
                            FRIC = PA_SOURCE_Com$FRIC,
                            River_com = PA_SOURCE_Com$River_com,
                            Canal_com = PA_SOURCE_Com$Canal_com,
                            Geo_div = PA_SOURCE_Com$Geo_div,
                            lan_div = PA_SOURCE_Com$lan_div,
                            SD_div = PA_SOURCE_Com$SD_div,
                            Ele_range = PA_SOURCE_Com$Ele_range,
                            slo_range = PA_SOURCE_Com$slo_range,
                            GEO_SUM = PA_SOURCE_Com$GEO_SUM)

#STEP 19: SAVE THE ALL PROTECTED AREAS AS COMBINED PROTECTED AREAS

PA_SOURCE_DES_50_ALL <- PA_SOURCE_DES_50_WMAINE_EN_NR_WR %>%
  filter(!IUCN_CAT %in% c("Ia"))

FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA <- merge(PA_SOURCE_Com, PA_SOURCE_DES_50_ALL, by = "tile_name", all.x = TRUE)
FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA   <- unique(FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA)

write.csv(FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA, "FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA.csv")



#STEP 20: GENERATE AND SAVE THE INDIVIDUAL BRITSH PROTECTED AREAS 
PA_SOURCE_DES_50_EXO <- PA_SOURCE_DES_50_WMAINE_EN_NR_WR_HC %>%
  filter(!IUCN_CAT %in% c("Ia", "II"))


PA_SOURCE_DES_50_EXO <- PA_SOURCE_DES_50_EXO %>%
  filter(!DESIG_ENG_ALTERED %in% c("World Heritage Site 
                                   (natural or mixed)"))


FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA <- merge(PA_SOURCE_Com, PA_SOURCE_DES_50_EXO, by = "tile_name", all.x = TRUE)
FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA   <- unique(FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA)

write.csv(FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA, "FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA.csv")

#STEP 21: GENERATE AND SAVE THE INDIVIDUAL IUCN CATGORIES WITH THE FUNCTIONAL DIVERSITY AND LANDSCAPE DATA

PA_SOURCE_IUCN_50 <- PA_SOURCE_DES_50 %>% 
  filter(IUCN_CAT %in% c("IV", "V" , "III"))


Tile_WDPA_IUCN <- data.frame(tile_name = PA_SOURCE_IUCN_50$tile_name,
                             WDPA_PID = PA_SOURCE_IUCN_50$WDPA_PID,
                             IUCN_CAT = PA_SOURCE_IUCN_50$IUCN_CAT)

FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA <- merge(PA_SOURCE_Com, PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA, by = "tile_name", all.x = TRUE)
FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA  <- unique(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA)
FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA$Most_LIKELY_IUCN[is.na(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA$Most_LIKELY_IUCN)] <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA$Protected_status[is.na(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA$Most_LIKELY_IUCN)]

FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA %>%
  filter(!Most_LIKELY_IUCN %in% c("Protected_Area"))

write.csv(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA, "FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO__IUCN_DATA.csv")
