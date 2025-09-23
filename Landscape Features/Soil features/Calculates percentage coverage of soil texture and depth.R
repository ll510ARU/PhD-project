#The following scripts describes how to calculate pentage coverage for soil texture and depth
#After calculating the area coverage of soil in Great Britain using British Geological Society via Edina Digimap 
# The csv file (SOIL_UNION_AREA) was pre-processed through QGIS to calculate 
#the soil area and monad locations should also contain SOIL_DEPTH and SOIL_TEXT Columns

#STEP 1: INSTALL AND PACKAGES 

install.packages("tidyverse")
library("tidyverse")

### CALCULATING SOIL DEPTH ### ------

#STEP 2:FILTER FOR THE SOIL DEPTH CATEGORIES 
SOIL_UNION_AREA_filtered_deep <- SOIL_UNION_AREA[grep("DEEP", SOIL_UNION_AREA$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_deep <- SOIL_UNION_AREA[grep("\\bDEEP\\b", SOIL_UNION_AREA$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_deep <- SOIL_UNION_AREA_filtered_deep[!grepl("DEEP-INTERMEDIATE", SOIL_UNION_AREA_filtered_deep$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_deep_intermediate <- SOIL_UNION_AREA[grep("DEEP-INTERMEDIATE", SOIL_UNION_AREA$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_intermediate <- SOIL_UNION_AREA[grep("INTERMEDIATE", SOIL_UNION_AREA$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_intermediate <- SOIL_UNION_AREA_filtered_intermediate[!grepl("DEEP-INTERMEDIATE", SOIL_UNION_AREA_filtered_intermediate$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_intermediate_shallow <- subset(SOIL_UNION_AREA_filtered_intermediate, grepl("INTERMEDIATE-SHALLOW", SOIL_DEPTH, ignore.case = TRUE))
SOIL_UNION_AREA_filtered_intermediate <- SOIL_UNION_AREA_filtered_intermediate[!grepl("INTERMEDIATE-SHALLOW", SOIL_UNION_AREA_filtered_intermediate$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_shallow <- SOIL_UNION_AREA[grep("SHALLOW", SOIL_UNION_AREA$SOIL_DEPTH), ]
SOIL_UNION_AREA_filtered_shallow <- SOIL_UNION_AREA_filtered_shallow[!grepl("INTERMEDIATE-SHALLOW", SOIL_UNION_AREA_filtered_shallow$SOIL_DEPTH), ]


#STEP 3:STANDARDISE THE SPEELLING OF THE SOIL DEPTH CATEGORIES
SOIL_UNION_AREA_filtered_deep$SOIL_DEPTH <- "DEEP"
SOIL_UNION_AREA_filtered_deep_intermediate$SOIL_DEPTH <- "DEEP-INTERMEDIATE"
SOIL_UNION_AREA_filtered_intermediate$SOIL_DEPTH <- "INTERMEDIATE"
SOIL_UNION_AREA_filtered_intermediate_shallow$SOIL_DEPTH <- "INTERMEDIATE-SHALLOW"
SOIL_UNION_AREA_filtered_shallow$SOIL_DEPTH <- "SHALLOW"

#STEP 4:COMBINING THE SOIL DEPTH CATEGORIES INFORMATION INTO ONE DATAFRAME
combined_SOIL_UNION_AREA <- rbind(SOIL_UNION_AREA_filtered_deep, SOIL_UNION_AREA_filtered_deep_intermediate,
                                  SOIL_UNION_AREA_filtered_intermediate, SOIL_UNION_AREA_filtered_intermediate_shallow,
                                  SOIL_UNION_AREA_filtered_shallow)


#STEP 5: SELECT THE COLUMNS MONAD = tile_name", MONAD AREA = "Area_2", SOIL AREA = "SOIL_AREA", SOIL DEPTH = "SOIL_DEPTH"
combined_SOIL_UNION_AREA <- combined_SOIL_UNION_AREA[, c("tile_name", "Area_2", "SOIL_AREA", "SOIL_DEPTH")]

#STEP 6: CREATE ID FOR SOIL DEPTH AND MONAD LOCATIONS
combined_SOIL_UNION_AREA$Mon_SOIL_DEPTH <- paste(combined_SOIL_UNION_AREA$tile_name, combined_SOIL_UNION_AREA$SOIL_DEPTH)

#STEP 7: SUMMERISE SOIL DEPTH WITH THE MONAD LOCATIONS
combined_SOIL_UNION_AREA_Dis <- combined_SOIL_UNION_AREA %>% group_by(Mon_SOIL_DEPTH) %>% summarize(Sum = sum(SOIL_AREA))

#STEP 8: SPLIT THE ID IN SOIL DEPTH AND MONAD LOCATIONS WITH SUMMERISED AREA
split <- strsplit(combined_SOIL_UNION_AREA_Dis$Mon_SOIL_DEPTH, " ")
combined_SOIL_UNION_AREA_Dis$tile_name <- sapply(split, `[`, 1)
combined_SOIL_UNION_AREA_Dis$SOIL_DEPTH <- sapply(split, `[`, 2)

#STEP 8.5: SAVE AND RELOAD THE FILE TO REMOVE THE ARTIFACTS
write.csv(combined_SOIL_UNION_AREA_Dis, "combined_SOIL_UNION_AREA_DisC.csv")

#STEP 9: TOTAL OF THE MONAD AREA INFORMATION
combined_SOIL_UNION_AREA_INFO <- combined_SOIL_UNION_AREA[, c("tile_name","Area_2")]

#STEP 10: REMOVE THE ROWS WITH NO VALUES 
combined_SOIL_UNION_AREA_DisC <- na.omit(combined_SOIL_UNION_AREA_DisC)
combined_SOIL_UNION_AREA_INFO <- na.omit(combined_SOIL_UNION_AREA_INFO)

#STEP 11: SUMMERISE MONAD LOCATIONS AREA
combined_SOIL_UNION_AREA_INFO <- combined_SOIL_UNION_AREA_INFO %>% group_by(tile_name) %>% summarize(tile_ARA = sum(Area_2))

#STEP 12: COMBINE THE MONAD AND SOIL DEPTH AREA DATASETS
SOIL_UNION_AREA_result <- merge(combined_SOIL_UNION_AREA_DisC, combined_SOIL_UNION_AREA_INFO, by = "tile_name")
SOIL_UNION_AREA_result <- unique(SOIL_UNION_AREA_result)

#STEP 13: SELECT THE REQUIRED COLUMNS  MONAD REF = "tile_name", SOIL_AREA = "Sum", MONAD AREA = "tile_ARA", SOIL TYPE = "SOIL_DEPTH"

SOIL_UNION_AREA_result <- SOIL_UNION_AREA_result[, c("tile_name", "Sum", "tile_ARA", "SOIL_DEPTH")]

names(SOIL_UNION_AREA_result)[names(SOIL_UNION_AREA_result) == "Sum"] <- "Soil_Area"
names(SOIL_UNION_AREA_result)[names(SOIL_UNION_AREA_result) == "tile_ARA"] <- "Title_Area"

#STEP 14: CONVERTS TO METER SQUARE TO KILOMETER AND ROUND TO 2 DECIMAL PLACES
SOIL_UNION_AREA_result$Title_Area <- SOIL_UNION_AREA_result$Title_Area/1000000
SOIL_UNION_AREA_result$Soil_Area <- SOIL_UNION_AREA_result$Soil_Area/1000000
SOIL_UNION_AREA_result$Title_Area  <- round(SOIL_UNION_AREA_result$Title_Area, digits = 2)
SOIL_UNION_AREA_result$Soil_Area <- round(SOIL_UNION_AREA_result$Soil_Area, digits = 2)
SOIL_UNION_AREA_result$Percentage <- (SOIL_UNION_AREA_result$Soil_Area/SOIL_UNION_AREA_result$Title_Area)*100

#STEP 15: SAVE AND RE-LOAD THE SOIL_DEPTH DATAFRAME
write.csv(SOIL_UNION_AREA_result, "SOIL_DEPTH.csv")

#STEP 16: CONVERT SOIL DEPTH DATAFRAME FROM WIDE TO LONG
Soil_Depth_long <- gather(SOIL_DEPTH, Depth, per, DEEP:SHALLOW, factor_key=TRUE)

#STEP 17: SELECTS THE MAXMINUMN PERCENTAGE COVER PER MONAD LOCATIONS

largest_area_rows_dep <- Soil_Depth_long %>%
  group_by(tile_name) %>%
  filter(per == max(per)) 

# SAVE, ADD IN THE LVLS 0 = NAs, 1 = SHALLOW, 2 = SHALLOW-INTERMEDIATE
# 3 = INTERMEDIATE, 4 = INTERMEDIATE-DEEP, 5 = DEEP AND RELOAD THE DATAFRAME 
write.csv(largest_area_rows_dep, "largest_area_rows_dep.csv")

#STEP 18: FILTER FOR THE DUPLICATE ROWS - WHERE SOIL AREA IS EQUAL FOR MULTIPLE DEPTHS 
duplicate_rows <- largest_area_rows_dep[duplicated(largest_area_rows_dep$tile_name), ]
duplicate_rows <- duplicate_rows$tile_name
DR <- largest_area_rows_dep %>% filter(tile_name %in% duplicate_rows) 

#STEP 19:  BREAK THE TIES AND RETAINS THE MONAD WITH THE DEEPEST DEPTH
largest_area_rows_DR <- DR %>%
  group_by(tile_name) %>%
  filter(Dep_lvl == max(Dep_lvl))

#STEP 20: REMOVES THE DUPLICATES AND REPLACES WITH RESOLVED INFORMATION 
filter_list <- largest_area_rows_DR$tile_name
largest_area_rows_dep  <- largest_area_rows_dep %>% filter(!tile_name %in% filter_list)
Soil_depth_resolved <-rbind(largest_area_rows_dep, largest_area_rows_DR)

write.csv(Soil_depth_resolved, "Soil_depth_resolved.csv")


### SOIL_TEXTURE ### -----
#STEP 21:FILTER FOR THE SOIL TEX CATEGORIES 
HEAVY <- SOIL_UNION_AREA %>%
  filter(grepl("\\bheavy\\b",
               SOIL_GROUP, ignore.case = TRUE))
write.csv(HEAVY, "HEAVY.csv")

MEDIUM <- SOIL_UNION_AREA %>%
  filter(grepl("MEDIUM",
               SOIL_GROUP, ignore.case = TRUE))
MEDIUM <- MEDIUM[!grepl("HEAVY TO", MEDIUM$SOIL_GROUP), ]
write.csv(MEDIUM, "MEDIUM.csv")

LIGHT <- SOIL_UNION_AREA %>%
  filter(grepl("LIGHT",
               SOIL_GROUP, ignore.case = TRUE))

LIGHTSILT <- LIGHT %>%
  filter(grepl("SILTY",
               SOIL_GROUP, ignore.case = TRUE))

LIGHTSANDY <- LIGHT %>%
  filter(grepl("SANDY",
               SOIL_GROUP, ignore.case = TRUE))

LIGHT <- LIGHT[!grepl("SANDY", LIGHT$SOIL_GROUP), ]

write.csv(LIGHT, "LIGHT.csv")
write.csv(LIGHTSANDY, "LIGHTSANDY.csv")
write.csv(LIGHTSILT, "LIGHTSILT.csv")

ALL_TEX <- SOIL_UNION_AREA %>%
  filter(grepl("ALL",
               SOIL_GROUP, ignore.case = TRUE))

write.csv(ALL_TEX, "ALL_TEX.csv")

#ENSURE THE SOIL TEXTURE CATGORIES MATCH THE soil_cat AND LVLS

#STEP 22: COMBINE THE SEPORATE CATGORIES TOGETHER 
combined_TEXTURE <- rbind(HEAVY, MEDIUM2, LIGHT, LIGHTSANDY, LIGHTSILT, ALL_TEX)

#STEP 23: SELECT THE REQUIRED COLUMNS  MONAD REF = "tile_name", SOIL_AREA = "Sum", MONAD AREA = "tile_ARA", SOIL TYPE = "TEXTURE"
combined_TEXTURE <- combined_TEXTURE[, c("tile_name", "Area_2", "SOIL_AREA", "TEXTURE")]

#STEP 23.5: REPLACE THE SPACES BETWEEN WORDS WITH -
combined_TEXTURE <- combined_TEXTURE %>%
  mutate(TEXTURE = str_replace_all(TEXTURE, " ", "-"))


#STEP 24: SUMMERISE SOIL_TEXT WITHIN MONAD LOCATIONS AREA  

combined_TEXTURE$Mon_TEXTURE <- paste(combined_TEXTURE$tile_name, combined_TEXTURE$TEXTURE)
combined_TEXTURE_Dis <- combined_TEXTURE %>% group_by(Mon_TEXTURE) %>% summarize(Sum = sum(SOIL_AREA))

#STEP 25: SELECT THE SOIL_TEX INFORMATION
combined_TEXTURE_Dis <- combined_TEXTURE_Dis[, c("tile_name",  "TEXTURE", "Sum")]


#STEP 26:SPLIT THE ID INTO SOIL TEXTURE AND MONAD LOCATIONS WITH SUMMERISED AREA

split <- strsplit(combined_TEXTURE_Dis$Mon_TEXTURE, " ")
combined_TEXTURE_Dis$tile_name <- sapply(split, `[`, 1)
combined_TEXTURE_Dis$TEXTURE <- sapply(split, `[`, 2)

#STEP 27: SAVE AND RELOAD THE FILE TO REMOVE THE ARTIFACTS
write.csv(combined_TEXTURE_Dis, "combined_TEXTURE_Dis.csv")

#STEP 28: TOTAL OF THE MONAD AREA INFORMATION
combined_SOIL_UNION_AREA_INFO <- combined_TEXTURE[, c("tile_name","Area_2")]

#STEP 29: SUMMERISE MONAD LOCATIONS AREA
combined_SOIL_UNION_AREA_INFO <- combined_SOIL_UNION_AREA_INFO %>% group_by(tile_name) %>% 
  summarize(Title_Area = sum(Area_2))


#STEP 30: COMBINE THE MONAD AND SOIL DEPTH AREA DATASETS
SOIL_TEXTURE <- right_join(combined_TEXTURE_Dis, combined_SOIL_UNION_AREA_INFO, by = "tile_name")



#STEP 31: SELECT THE REQUIRED COLUMNS  MONAD, MONAD ="tile_name", 
# SOIL_AREA = "Sum", MONAD AREA = "Title_Area", SOIL_TEXTURE = "TEXTURE"
SOIL_TEXTURE <- SOIL_TEXTURE[, c("tile_name", "Sum", "Title_Area", "TEXTURE")]

#STEP 32: 
names(SOIL_TEXTURE)[names(SOIL_TEXTURE) == "Sum"] <- "Soil_Area"
SOIL_TEXTURE$Soil_Area <- round(SOIL_TEXTURE$Soil_Area, digits = 2)
SOIL_TEXTURE$Title_Area <- round(SOIL_TEXTURE$Title_Area, digits = 2)

#STEP 33: CONVERTS TO METER SQUARE TO KILOMETER AND ROUND TO 2 DECIMAL PLACES

SOIL_TEXTURE$Title_Area <- SOIL_TEXTURE$Title_Area/1000000
SOIL_TEXTURE$Soil_Area <- SOIL_TEXTURE$Soil_Area/1000000
SOIL_TEXTURE$Percentage <- (SOIL_TEXTURE$Soil_Area/SOIL_TEXTURE$Title_Area)*100
SOIL_TEXTURE$Percentage <- round(SOIL_TEXTURE$Percentage, digits = 0)

#STEP 34: SAVE AND RE-LOAD THE SOIL_TEXTURE DATAFRAME
write.csv(SOIL_TEXTURE, "SOIL_TEXTURE.csv")

#STEP 35:SELECTS THE MAXMINUMN PERCENTAGE COVER PER MONAD LOCATIONS

largest_area_rows_tex <- SOIL_TEXTURE %>%
  group_by(tile_name) %>%
  filter(percent == max(percent)) 

write.csv(largest_area_rows_tex, "Soil_tex_sortedtr.csv")

#STEP 36: FILTER FOR THE DUPLICATE ROWS - WHERE SOIL AREA IS EQUAL FOR MULTIPLE TEXTURE
duplicate_rows <- Soil_tex_sortedtr[duplicated(Soil_tex_sortedtr$tile_name), ]
duplicate_rows <- duplicate_rows$tile_name
DR <- Soil_tex_sortedtr %>% filter(tile_name %in% duplicate_rows) 


#STEP 37: SAVE THE FILE

write.csv(Soil_tex_sortedtr, "Soil_tex_resolved.csv")