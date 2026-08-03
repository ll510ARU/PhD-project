## The script describes pipline code for the Univerate PERMANOVA analysis of functional diversity across native status in protected and non-protected areas for following datasets
# The datasets generated are for all protected areas (FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA), the British designate protected areas (FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA) 
# and the IUCN framework (FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA).
# The visualisations of Univerate PERMANOVA using geom_violin plots.

#The analysis of relationship between landscape measures (geodiversity and percentage cover) 
# using Ordinary least squares (OLS) linear regression models. 
# The visualisations of regression models using ggplots


#STEP 1: INSTALL AND LOAD PACKAGES
install.packages("FSA")
library(FSA)

install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(tidyverse)


#STEP 2: CREATE A GENERALISED PROTECTED AREA ID
FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA$PERM_IND <- paste0(FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA$Most_LIKELY_DES)
FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$PERM_IND <- paste0(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Most_LIKELY_IUCN)
FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA$PERM_IND <- paste0(FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA$Protected_status)


#STEP 3: LOOP THROUGH THE PROTECTED AREAS TO PERFORM UNIVERATE PERMANOVE FOR FUNCTIONAL DIVERSITY AND GEODIVERSITY

listsets <- list(FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA = FD_GEO_PA_PA_SOURCE_DES_50_ALL_DATA,
  FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA,
  FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA = FD_GEO_PA_PA_SOURCE_DES_50_EXO_DATA)

sink("ALL_PERMANOVAS_AND_DUNN_DATA.txt")

for (name in names(listsets)) {
  lists <- listsets[[name]]
  
  cat("\n==============================\n")
  cat("Dataset:", name, "\n")
  cat("==============================\n")
  
  # Try to fit the GAM model safely
  tryCatch({
    modelA <- aovp(FDIS  ~ PERM_IND, data = lists, perm = "Prob")
    
    print(summary(modelA))
    
    modelB <- dunnTest(FDIS ~ interaction(Native_status, PERM_IND), 
                       data = lists, 
                       method = "bh")
    
    
    
    modelB <- modelB[["res"]]
    
    print(modelB)
    
    
    
    modelC <- aovp(GEO_SUM ~ PERM_IND, data = lists, perm = "Prob")
    
    print(summary(modelC))
    
    
    modelD <- dunnTest(GEO_SUM ~ interaction(PERM_IND), 
                       data = lists, 
                       method = "bh")
    
    
    modelD <- modelD[["res"]]
    
    print(modelD)
    
    
    
  }, error = function(e) {
    cat("Model failed for", name, "due to error:\n", conditionMessage(e), "\n")
  })
}


#STEP 4:



y_vars <- c("FDIS", "FRIC")


y_labels <- c(FDIS = "Functional Dispersion", FRIC = "Functional Richness")


for (y in y_vars) {
  
  png(
    paste0(y_labels[[y]], "_Violin_British_Designation_EXD_PRO_DATA.png"),
    width = 10400,
    height = 5800,
    res = 300
  )
  
  p <- ggplot(
    FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA,
    aes(
      x = Most_LIKELY_DES_NS,
      y = .data[[y]],        # ✅ log transform
      fill = factor(Native_status)
    )
  ) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.12, fill="white")+
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 15,
      size = 4,
      color = "black",
      position = position_dodge(0.8)
    ) +
    scale_x_discrete(labels = c(
      "Regional ParkNATIVE" = "Regional Parks: Native Species", "Regional ParkNON-NATIVE" =  "Regional Parks: Non-Native Species",
      "National Scenic AreaNATIVE" = "National Scenic Areas: Native species",
      "National Scenic AreaNON-NATIVE" = "National Scenic Areas: Non-Native species",
      "Ramsar Site, Wetland of International ImportanceNATIVE" = "Ramsar Sites: Native species",
      "Ramsar Site, Wetland of International ImportanceNON-NATIVE" = "Ramsar Sites: Non-Native species",
      "Local Nature ReserveNATIVE"  = "Local Nature Reserves: Native species",
      "Local Nature ReserveNON-NATIVE"  = "Local Nature Reserves: Non-Native species",
      "National ParkNATIVE" = "National Parks: Native Species",
      "National ParkNON-NATIVE" = "National Parks: Non-Native Species",
      "Site of Special Scientific InterestNATIVE" = "Sites of Special Scientific Interest: Native Species",
      "Site of Special Scientific InterestNON-NATIVE" = "Sites of Special Scientific Interest: Non-Native Species",
      "Area of Outstanding Natural BeautyNATIVE" = "National Landscapes: Native Species",
      "Area of Outstanding Natural BeautyNON-NATIVE" = "National Landscapes: Non-Native Species",
      "Non-Protected AreaNATIVE" = "Non-Protected Areas: Native",
      "Non-Protected AreaNON-NATIVE" = "Non-Protected Areas: Non-Native")) +
    labs(
      x = "Protected Status",
      y =  y_labels[[y]]
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 65, hjust = 1, size = 23, face = "bold"),
      axis.text.y = element_text(size = 23, face = "bold"),
      axis.title = element_text(size = 25, face = "bold"),
      text = element_text(size = 23)
    )
  
  print(p)
  dev.off()
}


#IUCN FRAMEWORK


for (y in y_vars) {
  
  png(
    paste0(y_labels[[y]], "_Viloin_EXD_PRO_IUCN_DATA.png"),
    width = 10400,
    height = 5800,
    res = 300
  )
  
  p <- ggplot(
    FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA,
    aes(
      x = DES_NS,
      y = .data[[y]],        # ✅ log transform
      fill = factor(Native_status)
    )
  ) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.1, fill="white")+
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 15,
      size = 4,
      color = "black",
      position = position_dodge(0.8)
    ) +
    scale_x_discrete(labels = c(
      "IIINATIVE" = "Category III: Native Species",  "IIINON-NATIVE"= "Category III: Non-Native Species", 
      "IVNATIVE" =  "Category IV: Native Species",   "IVNON-NATIVE" = "Category IV: Non-Native Species",
      "VNATIVE" =  "Category V: Native Species", "VNON-NATIVE" = "Category V: Non-Native Species",
      "Non-Protected AreaNATIVE" = "Non-Procted Areas: Native Species", "Non-Protected AreaNON-NATIVE"= "Non-Procted Areas: Non-Native Species"
    )) +
    labs(
      x = "Protected Status",
      y =  y_labels[[y]]
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 65, hjust = 1, size = 23, face = "bold"),
      axis.text.y = element_text(size = 23, face = "bold"),
      axis.title = element_text(size = 25, face = "bold"),
      text = element_text(size = 23)
    )
  
  print(p)
  dev.off()
}



# STEP 5: LOOP THROUGH THE GEODIVERSITY METRICS TO GENERATE VILIOLIN PLOTS FOR BRITISH DESIGNATIONS AND IUCN CATGORIES

y_labels <- c(Log_river = "Log River Complexity", Log_Canal = "Log Canal Complexity",
              Geo_div = "Geological Diversity", lan_div = "Landform Diversity",
              SD_div = "Superficial Deposit Diversity", Ele_range  = "Elevation Range (m)",
              slo_range = "Slope Range (m)", GEO_SUM = "Geodiversity score")


for (y in y_vars) {
  
  png(
    paste0(y_labels[[y]], "_GEOVILIOLIN_British_Designation_EXD_PRO_DATA.png"),
    width = 4400,
    height = 3800,
    res = 300
  )
  
  p <- ggplot(
    FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA,
    aes(
      x = Most_LIKELY_DES,
      y = .data[[y]],        # ✅ log transform
      fill = factor(Most_LIKELY_DES)
    )
  ) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.12, fill="white")+
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 15,
      size = 4,
      color = "black",
      position = position_dodge(0.8)
    ) +
    scale_x_discrete(
      labels = c("Regional Parks", "National Scenic Areas",
                 "Ramsar Site, Wetland of International Importances",
                 "Local Nature Reserves","National Parks",
                 "Site of Special Scientific Interests",
                 "Area of Outstanding Natural Beautys",
                 "Non-Protected Areas"
      )
    ) +
    scale_fill_manual(values = c(
      "Regional Park"= "#999900", "National Scenic Area" = "#CC9933",
      "Ramsar Site, Wetland of International Importance"= "#669999",
      "Local Nature Reserve"= "#CC9999","National Park"= "#C66999",
      "Site of Special Scientific Interest"= "#FF9909",
      "Area of Outstanding Natural Beauty"= "#996633"
    )) +
    labs(
      x = "Designation Status",
      y =  y_labels[[y]]
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 65, hjust = 1, size = 20, face = "bold"),
      axis.text.y = element_text(size = 20, face = "bold"),
      axis.title = element_text(size = 20, face = "bold"),
      text = element_text(size = 20)
    )
  
  print(p)
  dev.off()
}


#IUCN FRAMEWORK


for (y in y_vars) {
  
  png(
    paste0(y_labels[[y]], "_GEOVILIOLIN_IUCN_EXD_PRO_DATA.png"),
    width = 4400,
    height = 3800,
    res = 300
  )
  
  p <- ggplot(
    FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA,
    aes(
      x = Most_LIKELY_IUCN,
      y = .data[[y]],        # ✅ log transform
      fill = factor(Most_LIKELY_IUCN)
    )
  ) +
    geom_violin(trim=FALSE)+
    geom_boxplot(width=0.12, fill="white")+
    stat_summary(
      fun = mean,
      geom = "point",
      shape = 15,
      size = 4,
      color = "black",
      position = position_dodge(0.8)
    ) +
    scale_x_discrete(
      labels = c("III" = "Category III", 
                 "IV" =  "Category IV",   
                 "V" =  "Category V", 
                 "Non-Protected Area" = "Non-Procted Areas"
      )
    ) +
    scale_fill_manual(values = c("III" = "#CC99FF",
                                 "V"   = "#99CCCC",
                                 "IV"  = "#99CC99"
    )) +
    labs(
      x = "Protected area IUCN Category",
      y =  y_labels[[y]]
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 65, hjust = 1, size = 30, face = "bold"),
      axis.text.y = element_text(size = 30, face = "bold"),
      axis.title = element_text(size = 30, face = "bold"),
      text = element_text(size = 30)
    )
  
  print(p)
  dev.off()
}



#STEP 6: SUBSET THE PROTECTED AREA DATASETS BY THE INDIVIDUAL IUCN CATGORIES AND BRITISH DESIGNATIONS BY NATIVE STATUS

# DESINGATIONS
FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$PER <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$DES_Percentage

FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native <- subset(FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA, Native_status != "NATIVE")

FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native <- subset(FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA, Native_status != "NON-NATIVE")

unique(FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES)


DES_model_Regional_Park_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "Regional Park", ]
DES_model_National_Park_nat_EXO  <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "National Park", ]
DES_model_Local_Nature_Reserve_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "Local Nature Reserve", ]
DES_model_National_Nature_Reserve_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "National Nature Reserve", ]
DES_model_Ramsar_Site_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "Ramsar Site, Wetland of International Importance", ]
DES_model_National_Scenic_Area_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "National Scenic Area", ]
DES_model_AONB_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES =="Area of Outstanding Natural Beauty", ]
DES_model_SSI_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_native$Most_LIKELY_DES == "Site of Special Scientific Interest", ]

DES_model_Regional_Park_non_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "Regional Park", ]
DES_model_National_Park_non_nat_EXO  <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "National Park", ]
DES_model_Local_Nature_Reserve_non_nat_EXO  <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "Local Nature Reserve", ]
DES_model_National_Nature_Reserve_non_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "National Nature Reserve", ]
DES_model_Ramsar_Site_non_nat_EXO  <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "Ramsar Site, Wetland of International Importance", ]
DES_model_National_Scenic_Area_non_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "National Scenic Area", ]
DES_model_AONB_non_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES =="Area of Outstanding Natural Beauty", ]
DES_model_SSI_non_nat_EXO <- FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native[FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA_Non_native$Most_LIKELY_DES == "Site of Special Scientific Interest", ]


#IUCN FRAMEWORK
FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$PER <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$IUCN_Percentage

FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native <- subset(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA, Native_status != "NATIVE")

FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native <- subset(FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA, Native_status != "NON-NATIVE")


DES_model_III_Non_native  <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native$Most_LIKELY_IUCN == "III", ]
DES_model_V_Non_native <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native$Most_LIKELY_IUCN == "V", ]
DES_model_IV_Non_native <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_Non_native$Most_LIKELY_IUCN == "IV", ]


DES_model_III_native  <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native$Most_LIKELY_IUCN == "III", ]
DES_model_V_native <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native$Most_LIKELY_IUCN == "V", ]
DES_model_IV_native <- FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native[FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA_native$Most_LIKELY_IUCN == "IV", ]




# STEP 7:GENERATE A LIST OF DATASETS AND LOOP THROUGH THE DATASETS TO CREATE REGRESSION MODELS

datasets <- list(DES_model_III_native = DES_model_III_native,
                 DES_model_V_native = DES_model_V_native,
                 DES_model_IV_native = DES_model_IV_native,
                 
                 DES_model_III_Non_native = DES_model_III_Non_native,
                 DES_model_V_Non_native = DES_model_V_Non_native,
                 DES_model_IV_Non_native = DES_model_IV_Non_native,
                 
  DES_model_Regional_Park_nat_EXO = DES_model_Regional_Park_nat_EXO,
  DES_model_National_Park_nat_EXO = DES_model_National_Park_nat_EXO,
  DES_model_Local_Nature_Reserve_nat_EXO = DES_model_Local_Nature_Reserve_nat_EXO,
  DES_model_National_Nature_Reserve_nat_EXO = DES_model_National_Nature_Reserve_nat_EXO, 
  DES_model_Ramsar_Site_nat_EXO = DES_model_Ramsar_Site_nat_EXO, 
  DES_model_National_Scenic_Area_nat_EXO = DES_model_National_Scenic_Area_nat_EXO, 
  DES_model_AONB_nat_EXO = DES_model_AONB_nat_EXO, DES_model_SSI_nat_EXO = DES_model_SSI_nat_EXO,
  
  DES_model_Regional_Park_non_nat_EXO = DES_model_Regional_Park_non_nat_EXO ,
  DES_model_National_Park_non_nat_EXO  = DES_model_National_Park_non_nat_EXO ,
  DES_model_Local_Nature_Reserve_non_nat_EXO  = DES_model_Local_Nature_Reserve_non_nat_EXO ,
  DES_model_National_Nature_Reserve_non_nat_EXO  = DES_model_National_Nature_Reserve_non_nat_EXO , 
  DES_model_Ramsar_Site_non_nat_EXO  = DES_model_Ramsar_Site_non_nat_EXO , 
  DES_model_National_Scenic_Area_non_nat_EXO  = DES_model_National_Scenic_Area_non_nat_EXO , 
  DES_model_AONB_non_nat_EXO = DES_model_AONB_non_nat_EXO, DES_model_SSI_non_nat_EXO  = DES_model_SSI_non_nat_EXO
)


sink("lm_FD_summaries_PROTECTED AREAS.txt")

for (name in names(datasets)) {
  data <- datasets[[name]]
  
  cat("\n==============================\n")
  cat("Dataset:", name, "\n")
  cat("==============================\n")
  
  # Try to fit the lm model safely
  tryCatch({
    modelA <- lm(GEO_SUM ~ FDIS, data = data)
    print(summary(modelA))
    print(AIC(modelA))
    
    modelB <- lm(River_com ~ FDIS, data = data)
    print(summary(modelB))
    print(AIC(modelB))
    
    modelC <- lm(Canal_com ~ FDIS, data = data)
    print(summary(modelC))
    print(AIC(modelC))
    
    modelD <- lm(Geo_div ~ FDIS, data = data)
    print(summary(modelD))
    print(AIC(modelD))
    
    modelE <- lm(lan_div ~ FDIS, data = data)
    print(summary(modelE))
    print(AIC(modelE))
    
    modelF <- lm(SD_div ~ FDIS, data = data)
    print(summary(modelF))
    print(AIC(modelF))
    
    modelG <- lm(Ele_range ~ FDIS, data = data)
    print(summary(modelG))
    print(AIC(modelG))
    
    modelH <- lm(slo_range ~ FDIS, data = data)
    print(summary(modelH))
    print(AIC(modelH))
    
    modelX <- lm( FDIS ~ PER, data = data)
    print(summary(modelX))
    print(AIC(modelX))
    
  }, error = function(e) {
    cat("Model failed for", name, "due to error:\n", conditionMessage(e), "\n")
  })
}
sink() 


#STEP 8: SUBSECTION DATASETS AND USE LOOP TO GENERATE
# COMBINE GRAPHS OF THE PROTECTED AREA DATASETS OF THE REGRESSIONS RELATIONSHIPS

DES_Sub <- data.frame(tile_name = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$tile_name,
                      FDIS = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$FDIS,
                      GEO_SUM = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$GEO_SUM,
                      River_com = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$River_com, 
                      Canal_com = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$Canal_com,
                      Geo_div = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$Geo_div, 
                      lan_div = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$lan_div,
                      SD_div = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$SD_div, 
                      Ele_range = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$Ele_range,
                      slo_range = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$slo_range,
                      NS = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$Native_status,
                      DES = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$Most_LIKELY_DES,
                      DES_NS = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$DES_NS,
                      PER = FD_GEO_PA_DES_50_WMARINE_EXD_PRO_DATA$DES_Percentage)


IUCN_Sub <- data.frame(tile_name = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$tile_name,
                       FDIS = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$FDIS,
                       GEO_SUM = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$GEO_SUM,
                       River_com = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$River_com, 
                       Canal_com = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Canal_com,
                       Geo_div = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Geo_div, 
                       lan_div = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$lan_div,
                       SD_div = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$SD_div, 
                       Ele_range = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Ele_range,
                       slo_range = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$slo_range,
                       NS = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Native_status,
                       DES = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$Most_LIKELY_IUCN,
                       DES_NS = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$DES_NS,
                       PER = FD_GEO_PA_IUCN_50_WMARINE_EXD_PRO_IUCN_DATA$IUCN_Percentage)


Group <- rbind(DES_Sub, IUCN_Sub)


Group <- Group[Group$DES != "Non-Protected Area", ]



x_vars <- c(
  "River_com", "Canal_com",
  "Geo_div", "lan_div",
  "SD_div", "Ele_range",
  "slo_range", "GEO_SUM", "PER"
)


x_labels <- c(River_com = "River Complexity", Canal_com = "Canal Complexity",
              Geo_div = "Geological Diversity", lan_div = "Landform Diversity",
              SD_div = "Superficial Deposit Diversity", Ele_range = "Elevation Range (m)",
              slo_range = "Slope Range (m)", GEO_SUM = "Geodiversity Score",  PER = "Percentage Coverage of Protected Area per 1km grid square"
)



for (x in x_vars) {
  
  png(
    paste0("FDIS_", x_labels[[x]], "_Group_graph_edits.png"),
    width = 2400,
    height = 1800,
    res = 300
  )
  
  p <- ggplot(
    Group,
    aes_string(
      x = x,
      y = "FDIS",
      color = "factor(DES)"
    )
  ) +
    geom_point(size = 0.9) +
    geom_smooth(method = "lm", linewidth = 1.1) +
    scale_color_manual(values = c(
      "III" = "#CC99FF",
      "V"   = "#99CCCC",
      "IV"  = "#99CC99",
      "Regional Park"= "#99CC34", "National Nature Reserve"= "seagreen", "National Scenic Area" = "#999900",
      "Ramsar Site, Wetland of International Importance"= "skyblue",
      "Local Nature Reserve"= "lightgreen","National Park"= "#009966",
      "Site of Special Scientific Interest"= "#CC9933",
      "Area of Outstanding Natural Beauty"= "#996633"
    )) +
    facet_wrap(~ DES, scales = "free_x") +  theme_classic() +
    theme(
      legend.position = "none",
      legend.text = element_text(size = 15),
      strip.text = element_text(size = 15, face = "bold"),
      axis.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 15, face = "bold")) +
    labs(
      x = x_labels[[x]],
      y = "Functional Dispersion"
    ) +
    theme(legend.position = "none")
  
  print(p)
  dev.off()
}








