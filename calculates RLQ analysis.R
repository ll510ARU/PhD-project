#The following script describes how to preform RLQ analysis and create visualasation of the relationships between species traits and the landscapes
# The species occurrences (species_occ file) was generated using Botentical Society of Britain and Ireland - see species occurrence script
# The landscape features (Landscapes_fea) was generated using Edina Digimap, OS ordnance survey and research from Bailey et al.  (2017)  - see landscape feature branch
# The species traits (species_Tra) was generated using TRY, BIEN, LEDA and BROT - see species trait branch

#STEP 1: INSTALL AND LOAD THE PACKAGES

install.packages("ade4")
library("ade4")


#STEP 2: REMOVE THE ID COLUMN, CREATE VECTOR OF THE MONAD LOCATIONS AND ORDER THE SPECIES NAMES ALPHEBETICALLY
species_occ <- species_occ[, -1]
locations<- species_occ$Monad
Spin_D <- species_occ[, order(colnames(species_occ))]

#STEP 3: CREATE A MATICES OF THE SPECIES AND MONAD LOCATIONS
Spin_D<-as.matrix(Spin_D)



# STEP 4: PERFORM CORRESPONDENCE ANALYSIS (COA) OF SPECIES 
coa.abu <- dudi.coa(Spin_D, scannf = FALSE, nf = 2)


# STEP 5:SUMMARY OF COA RESULTS
summary(coa.abu)

#STEP 6: FORMATTING TO TRAIT DATABASE AND PERFORM PRINCIPLE CORRESPONDENCE ANALYSIS OF THE TRAITS  
species_Tra <- as.data.frame(species_Tra)
rownames(species_Tra) <- species_Tra[, 1]
species_Tra <- species_Tra[, -1]
pca.trait <- dudi.pca(species_Tra, scannf = FALSE, 
                      row.w = coa.abu$cw)

#STEP 7: PERFORM PRINCIPLE CORRESPONDENCE ANALYSIS OF THE LANDSCAPE
pca.env <- dudi.pca(Landscapes, scannf = FALSE, 
                    row.w = coa.abu$lw)


#STEP 8:PERFORM THE RLQ ANALYSIS

rlqF <- rlq(dudiR= pca.env,
            dudiL= coa.abu, 
            dudiQ= pca.trait, 
            scannf = FALSE)


### CREATE THE RLQ GRAPH ### -----

#STEP 9: LOAD AND INSTALL

install.packages("ggplot2")
library("ggplot2")

#STEP 10: RETRIEVE THE RLQ SCORE FOR THE LANDSCAPE AND SPECIES TRAITS

LAND_scores  <- rlqF$l1
species_traits <- rlqF$c1

#STEP 11:STANDARDISED THE LANDSCAPE AND SPECIES TRAIT LABELS
rownames(LAND_scores) <- toupper(rownames(LAND_scores))
rownames(LAND_scores) <- c("GF_IGN", "GF_MET",
                          "GF_SED", "GMF_M_ELEV", "HF_RF",
                          "GMF_FLAT", "GMF_VAL", "GMF_SLO", "GMF_PEAK_RIDGES",
                          "GMF_HOL", "GMF_PIT", "HF_CL", "CL_M_TEM", "GF_DEP",
                          "GF_TEX", "HF_RL", "CL_M_PREP", "GF_ANT", "GF_GEO",
                          "GF_HYD", "GF_GEOM", "GF_ORG", "GF_PET", "GF_WEA",
                          "CL_S_PREP", "GMF_ASP", "CL_S_TEM")  


LAND_scores  <- as.data.frame(LAND_scores )
species_df_traits <- as.data.frame(species_traits)
rownames(species_df_traits) <- c("Seed Mass", "LDMC", "SLA",
                          "Plant Hieght", "Human", "Animal",
                          "Water", "Self-dispersal", "Wind")  

#STEP 12: ATTACH THE ASSOCIATED COLOUR TO THE DIFFERENT LANDSCAPE FEATURE CATAGORY

LAND_scores  <- LAND_scores  %>%
  mutate(color_group = case_when(
    grepl("^GMF_", rownames(LAND_scores )) ~ "#FF6600",
    grepl("^HF_", rownames(LAND_scores )) ~ "#0066FF",
    grepl("^GF_", rownames(LAND_scores )) ~ "#336633",
    grepl("^CL_", rownames(LAND_scores )) ~ "#990000",
    TRUE ~ "black"  # Default color
  ))


#STEP 13: CREATE A GRAPH OF THE RLQ ANALYSIS

ggplot() +
  # trait scores as points
  geom_point(data = species_df_traits, aes(x = CS1, y = CS2), color = "black") +
  
  # Species labels
  geom_text(data = species_df_traits, aes(x = CS1, y = CS2, label = rownames(species_df_traits)), 
            color = "black", vjust = 1.5, cex = 6, fontface = "bold") + 
  geom_segment(data = species_df_traits, 
               aes(x = 0, y = 0, xend = CS1, yend = CS2), 
               arrow = arrow(length = unit(0.2, "cm")),  
               size = 0.9, color = "black") +
  
  # Landscape feature scores with labels
  geom_text(data = LAND_scores , 
            aes(x = RS1, y = RS2, label = rownames(LAND_scores ), color = color_group), 
            cex = 5.5, fontface = "bold") +
  scale_color_identity() +
  
  # Labels and title
  labs(x = "PCA 1 (70.98%)", y = "PCA 2 (13.95%)", title = "Vascular Plants") +
  
  # Apply minimal theme
  theme_minimal() + 
  
  # Border of the plot
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 2)) + 
  
  # Axis text properties
  theme(axis.text = element_text(size = 10, color = "black", face = "bold")) + 
  
  # Axis title properties
  theme(axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold")) +
  
  # Plot title bold
  theme(plot.title = element_text(face = "bold"))



