#The following script describes how to calculate the percentage coverage of superficial deposit sources (SD)
#From British Geological Society via Edina Digimap after creating map of SD in Great Britain
#then calculating the area coverage of SD and the monad in QGIS
# SD_AREA - FILE SHOULD CONTAIN
#MONAD = title_name, 
#SD_TYPE = FM_EQ_D,
#MONAD AREA = Area, 
#SD AREA = SD_Area

#STEP 1: INSTALL AND LOAD THE PACKAGES:
install.packages("tidyverse")
library("tidyverse")


#STEP 2: CREATE ID FOR THE LOCATION OF MONAD AND SD
SD_AREA$FM_Mon <- paste(SD_AREA$tile_name, "-", SD_AREA$FM_EQ_D)

#STEP 3: SELECT COLUMNS (MONAD = title_name, SD_TYPE = FM_EQ_D, MONAD AREA = Area, SD AREA = SD_Area) AND 
# Summarise  SDs within the same location

SD_AREA_FM_Mon_cal <-SD_AREA[, c("FM_Mon", "tile_name", "FM_EQ_D","Area", "SD_Area")]
SD_AREA_FM_Mon_cal_DIS <- SD_AREA_FM_Mon_cal %>% group_by(FM_Mon) %>% summarize(Sum = sum(SD_Area))

#STEP 4: SPLIT THE INTO THE FOLLOWING COLUMN WITH THE TOTAL SD_AREA
#MONAD = title_name, 
#SD_TYPE = FM_EQ_D, 
#MONAD AREA = Area, 
#SD AREA = SD_Area

split <- strsplit(SD_AREA_FM_Mon_cal_DIS$FM_Mon, "-")
SD_AREA_FM_Mon_cal_DIS$tile_name <- sapply(split, `[`, 1)
SD_AREA_FM_Mon_cal_DIS$FM_EQ_D <- sapply(split, `[`, 2)


#STEP 5:EXTRACT MONAD AREA INFO AND SAVE THE GROUP SD_INFO RELOAD TO REMOVE ARTIFACTS
SD_AREA_FM_Mon_cal_INFO <- SD_AREA[, c("tile_name","Area")]
write.csv(SD_AREA_FM_Mon_cal_DIS, "SD_AREA_FM_Mon_cal_DIS.csv")
SD_AREA_FM_Mon_cal_DIs <- na.omit(SD_AREA_FM_Mon_cal_DIS)

#STEP 6: JOIN GROUP SD_INFO WITH AREA_INFO AND RENAME COLUMNS
SD_result_FM <- right_join(SD_AREA_FM_Mon_cal_DIs, SD_AREA_FM_Mon_cal_INFO, by = "tile_name")
SD_result_FM <- SD_result_FM[, c("tile_name", "FM_EQ_D","Area","Sum")]
SD_result_FM <- na.omit(SD_result_FM)
names(SD_result_FM)[names(SD_result_FM) == "Sum"] <- "SD_Area"


#STEP 7: CONVERT UNITS FROM METERS SQUARE TO KIOMETERS AND ROUNDS THE VALUES
SD_result_FM$Area <- SD_result_FM$Area/1000000
SD_result_FM$SD_Area <- SD_result_FM$SD_Area/1000000
SD_result_FM$Area  <- round(SD_result_FM$Area, digits = 2)
SD_result_FM$SD_Area  <- round(SD_result_FM$SD_Area, digits = 2)

#STEP 8: CALCULATE THE PERCENTAGE COVERAGE 
SD_result_FM$Percentage <- (SD_result_FM$SD_Area/SD_result_FM$Area)*100
write.csv(SD_result_FM, "SD_result_FMCOMPLETE.csv")


#STEP 9:GROUPS CALCULATE THE PERCENTAGE COVERAGE FOR THE SAME MONAD 
SD_Sum <- SD_result_FMCOMPLETE %>%
  group_by(tile_name, FM_EQ_D) %>%
  summarize(Total_Count = sum(Percentage, na.rm = TRUE))

write.csv(SD_Sum, "SD_Sum.csv")

#STEP 10: ADD THE SOURCE CLASSIFICATIONS - SEE SD_CAT.csv
SD_Sum <- merge(SD_Sum,
                SD_cat,
                by = "FM_EQ_D", 
                all.x = TRUE)


#STEP 11:SUMMERISE THE SD INTO THE SOURCE CLASSIFICATIONS
SD_Board <- SD_Sum %>%
  group_by(tile_name, Board_group) %>%
  summarize(Total_Count = sum(Total_Count, na.rm = TRUE))
write.csv(SD_Board, "SD_Board.csv")


#STEP 12: CONVERTS FROM LONG TO WIDE FORMAT 
SD_Board_wide <- spread(SD_Board, Board_group, Total_Count)
SD_Board_wide[is.na(SD_Board_wide)] <- 0

# STEP 13: SAVE THE FILE SD_BOARD_WIDE

write.csv(SD_Board_wide, "SD_Board_wide.csv")

