# The following script describes how to calculate pentage coverage for bedrock - Sedimentary, Igneous and Metamorphic
#After calculating the area coverage of the bedrock in Great Britain using British Geological Society via Edina Digimap 
# The csv file (BR_UNOIN_AREA) was pre-processed through QGIS to calculate 
#the bedrock area and monad locations should also contain RCS_ORGIN Columns

#STEP 1: INSTALL AND PACKAGES 

install.packages("tidyverse")
library("tidyverse")


#STEP 2:FILTER FOR THE COLUMNS MONAD = tile_name", MONAD AREA = "Area", BEDROCK AREA = "BR_AREA", BEDROCK TYPE = "RCS_ORIGIN"
BR_UNOIN_AREA_cal <- BR_UNOIN_AREA[, c("tile_name", "Area", "BR_AREA", "RCS_ORIGIN")]

#STEP 3: CREATE ID FOR BEDROCK AND MONAD LOCATIONS
BR_UNOIN_AREA_cal$Mon_BR <- paste(BR_UNOIN_AREA_cal$tile_name, BR_UNOIN_AREA_cal$RCS_ORIGIN)

#STEP 4: SUMMERISE THE BEDROCK WITHIN THE MONAD LOCATIONS
BR_UNOIN_AREA__Dis <- BR_UNOIN_AREA_cal %>% group_by(Mon_BR) %>% summarize(Sum = sum(BR_AREA))

#STEP 5: SPLIT THE ID IN THE BEDROCK AND MONAD LOCATIONS WITH SUMMERISED AREA
split <- strsplit(BR_UNOIN_AREA__Dis$Mon_BR, " ")
BR_UNOIN_AREA__Dis$tile_name <- sapply(split, `[`, 1)
BR_UNOIN_AREA__Dis$BR_TYPE <- sapply(split, `[`, 2)

#STEP 5.5: SAVE AND RELOAD THE FILE TO REMOVE THE ARTIFACTS
write.csv(BR_UNOIN_AREA__Dis, "BR_UNOIN_AREA__Dis.csv")

#STEP 6: THE MONAD AREA INFORMATION
BR_UNOIN_AREA_INFO <- BR_UNOIN_AREA[, c("tile_name","Area")]

#STEP 7: SUMMERISE MONAD LOCATIONS AREA
BR_UNOIN_AREA_INFO <- BR_UNOIN_AREA_INFO %>% group_by(tile_name) %>% summarize(Title_Area = sum(Area))

#STEP 8: COMBINE THE MONADS AND BEDROCK AREA DATASETS 
BEDROCK <- right_join(BR_UNOIN_AREA__Dis, BR_UNOIN_AREA_INFO, by = "tile_name")
BEDROCK <- BEDROCK[, c("tile_name", "Sum", "Title_Area", "BR_AREA")]


#STEP 9: CONVERTS TO METER SQUARE TO KILOMETER AND ROUND TO 2 DECIMAL PLACES
BEDROCK$Title_Area <- BEDROCK$Title_Area/1000000
BEDROCK$Soil_Area <- BEDROCK$Soil_Area/1000000
BEDROCK$BR_AREA <- round(BEDROCK$BR_AREA, digits = 2)
BEDROCK$Title_Area<- round(BEDROCK$Title_Area, digits = 2)

#STEP 10: CALCULATE THE PERCENTAGE COVERAGE
BEDROCK$Percentage <- (BEDROCK$BR_AREA/BEDROCK$Title_Area)*100
BEDROCK <- BEDROCK[, c("tile_name", "IGNEOUS", "METAMORPHIC", "SEDIMENTARY")]
BEDROCK  <- spread(BR_AREA, BR_TYPE, Percentage)
write.csv(BEDROCK, "BEDROCK.csv")
