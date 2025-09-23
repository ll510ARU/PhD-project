#The following script are the conversion for climate variables after manually processing the files in QGIS

#Install And Load
install.packages("sf")
library("sf")

### Mean Temperature ### -------------
#STEP 1: Load shape file and extract monad and mean temperature columns
shapefile_path <- "insert filepath way to Mean_Tem_or.shp"
Mean_Tem_or <- st_read(dsn = shapefile_path)
Mean_Tem_or <- Mean_Tem_or[, c("tile_name", "DN")]


# STEP 2: The conversion into degrees Celsius
Mean_Tem_or$DN <- (Mean_Tem_or$DN*0.1) - 273.15

#STEP 3: Rename the column and save the file
colnames(Mean_Tem_or)[colnames(Mean_Tem_or) == "DN"]<- "Mean_Tem"
write.csv(Mean_Tem_or, "Mean_Tem.csv")

### Mean Precipitation ### -------------
#STEP 1: Load shape file and extract monad and mean precipitation columns
shapefile_path <- "insert filepath way to Mean_prep_or.shp"
Mean_prep_or <- Mean_prep_or[, c("tile_name", "Bio12")]

# STEP 2: The conversion into Kg m-2 year-1
Mean_prep_or$Bio12 <- (Mean_prep_or$Bio12*0.1)

# STEP 3: Rename the column and save the file
colnames(Mean_prep_or)[colnames(Mean_prep_or) == "Bio12"]<- "Mean_Precip"
write.csv(Mean_prep_or, "Mean_prep.csv")

### Seasonality Temperature ### -------------
#STEP 1: Load shape file and extract monad and mean temperature columns
shapefile_path <- "insert filepath way to Seasonality_tem_or.shp"
Seasonality_tem_or <- st_read(dsn = shapefile_path)
Seasonality_tem_or <- Seasonality_tem_or[, c("tile_name", "DN")]

# STEP 2: The conversion into degrees Celsius divided by 100
Seasonality_TEM <- Seasonality_tem_or[, c("tile_name_2", "DN")]
Seasonality_TEM$DN <- (Seasonality_TEM$DN*0.1)
Seasonality_TEM$DN <- (Seasonality_TEM$DN/100)

# STEP 3: Rename the column and save the file
colnames(Seasonality_TEM)[colnames(Seasonality_TEM) == "DN"]<- "Tem_Season"
Seasonality_TEM <- Seasonality_TEM %>% group_by(tile_name_2) %>% summarize(Tem_Season = mean(Tem_Season))
write.csv(Seasonality_10km_TEM, "Seasonality_10km_TEM.csv")


### Seasonality Precipitation ### -------------
#STEP 1: Load shape file and extract monad and mean temperature columns
shapefile_path <- "insert filepath way to Seasonality_Prec_or.shp"
Seasonality_Prec_or <- st_read(dsn = shapefile_path)
Seasonality_Prec_or <- Seasonality_Prec_or[, c("tile_name", "DN")]

# STEP 2: The conversion into degrees Celsius divided by 100
Seasonality_Prec <- Seasonality_Prec_or[, c("tile_name_2", "DN")]
Seasonality_Prec$DN <- (Seasonality_Prec$DN*0.1)
Seasonality_Prec$DN <- (Seasonality_Prec$DN/100)

# STEP 3: Rename the column and save the file
colnames(Seasonality_Prec)[colnames(Seasonality_Prec) == "DN"]<- "Prec_Season"
Seasonality_Prec <- Seasonality_Prec %>% group_by(tile_name_2) %>% summarize(Prec_Season = mean(Prec_Season))
write.csv(Seasonality_Prec, "Seasonality_Prec_TEM.csv")


