# The following script describes how preform fourth corner analysis and the visualations for all the studied species 
# Note species traits were seporated in to native status in excel prior to the analysis
# The species occurrences (species_occ file) was generated using Botentical Society of Britain and Ireland - see species occurrence script
# The landscape features (Landscapes_fea) was generated using Edina Digimap, OS ordnance survey and research from Bailey et al.  (2017)  - see landscape feature branch
# The species traits (species_Tra) was generated using TRY, BIEN, LEDA and BROT - see species trait branch


### ALL STUDY SPECIES ### -----

#STEP 1: INSTALL AND LOAD PACKAGES

install.packages("ade4")
library(ade4)

install.packages("ggplot2")
library(ggplot2)

install.packages("dplyr")
library(dplyr)

install.packages("ggtext")
library(ggtext)

install.packages("stringr")
library(stringr)

# STEP 2: REMOVE THE UNQIUE ID COLUMN FROM THE DATASET 
species_occ_rm_id <- species_occ
species_occ_rm_id <- species_occ_rm_id[, -1]
species_Tra <- species_Tra[, -1]
species_Tra <- data.frame(lapply(species_Tra, as.numeric))

# STEP 3: CREATE A LIST OF DATASETS 

dataset_list <- list(
  Envi = Landscapes_fea,
  abu = species_occ_rm_id,
  traits = species_Tra)


#STEP 4: PERFORM THE FOURTH CORNER ANALYSIS 
FCA2 <- fourthcorner(dataset_list$Envi, dataset_list$abu, dataset_list$traits,nrepet=999, modeltype=2)
FCA4 <- fourthcorner(dataset_list$Envi, dataset_list$abu, dataset_list$traits,nrepet=999, modeltype=4)
four.comb <- combine.4thcorner(FCA2, FCA4)

#STEP 5: RETRIEVE THE PEARSON CORRELATIONS AND SIGNIFICANCE 

FCA <- summary(four.comb)

#STEP 6: SAVE AND INSPECT THE FILE
write.csv(FCA, "FCA_1km.csv")

#STEP 7: ISOLATE THE LANDSCAPE FEATURES AND SPECIES TRAITS TO SEPORATE COLUMNS 
split <- strsplit(FCA_1km$Test, "/")
FCA_1km$Land <- sapply(split, `[`, 1)
FCA_1km$Trait <- sapply(split, `[`, 2)



#STEP 8: ADD THE ANONATION FOR THE SIGNIFICANGE
FCA_1km$Significant <- ifelse(FCA_1km$Pvalue < 0.01, "**", 
                                        ifelse(FCA_1km$Pvalue < 0.05, "*", ""))


#STEP 9: SUBSET FOR THE SIGNIFICANT CORRELATIONS

FCA_1km_Significant <- FCA_1km %>% filter(Significant %in% c("*", "**"))  # Adjust condition if needed

#STEP 10: FORMAT THE LANDSCAPE FEATURE LABELS AND ADD THE ASSOCIATED COLOURS
FCA_1km_Significant <- FCA_1km_Significant %>%
  mutate(Land = str_to_upper(Land))

FCA_1km_Significant <- FCA_1km_Significant %>%
  mutate(Land_Colored = case_when(
    grepl("^GMF_", Land) ~ paste0("<span style='color:#FF6600;'>", Land, "</span>"),  
    grepl("^HF_", Land)  ~ paste0("<span style='color:#0066FF;'>", Land, "</span>"),  
    grepl("^GF_", Land)  ~ paste0("<span style='color:#336633;'>", Land, "</span>"),  
    grepl("^CL_", Land)  ~ paste0("<span style='color:#990000;'>", Land, "</span>"),  
    TRUE ~ Land  
  )) %>%
  arrange(Land) %>%  # Ensure alphabetical order
  mutate(Land_Colored = factor(Land_Colored, levels = unique(Land_Colored)))  


#STEP 10: ORDER THE SPECIES TRAITS ON THE X-AXIS
FCA_1km_Significant <- FCA_1km_Significant %>%
  mutate(Trait = factor(Trait, levels = c(" Aneochory_Chameachor", " Autochor",
                                          " Hemerochor", " Hydrochory"," Zoochor", " Seed_Mass.y",
                                          " LDMC_or", " plant_hieght", " SLA_or")))

#STEP 11: CREATES A FOURTH CORNER ANALYSIS GRAPH WITH THE ONLY THE SIGNIFICANT RESULT

library(stringr)

# Create the plot
ggplot(FCA_1km_Significant, aes(x = Trait, y = Land_Colored, fill = Obs)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0, 
    name = "Correlation",
    guide = guide_colorbar(barwidth = 1, barheight = 10)
  ) +
  geom_text(aes(label = Significant), color = "black", size = 10, vjust = 0.5) +
  labs(x = "Plant Species Traits", 
       y = "Environmental Features", 
       title = "") +
  theme_classic() +
  theme(
    axis.text.y = element_markdown(size = 20),  
    axis.text.x = element_text(size = 20),
    legend.position = "right",
    legend.text = element_text(size = 15), legend.title = element_text(size = 15),
  ) + 
  scale_x_discrete(labels = c(
    " Aneochory_Chameachor" = "Wind",  # Convert the labels
    " Autochor" = "Self-dispersal", 
    " Hemerochor" = "Human",
    " Hydrochory" = "Water",
    " Zoochor" = "Animal",
    " Seed_Mass.y" = "Seed Mass",
    " LDMC_or" = "LDMC",
    " plant_hieght" = "Plant Height",
    " SLA_or" = "SLA"
  )) +
  theme(panel.border = element_rect(color = "black", 
                                    fill = NA, 
                                    size = 2), plot.margin = margin(5, 1, 5, 1)) +
  # Add vertical lines at the boundaries of each column, excluding the first one
  geom_vline(xintercept = seq(1.5, length(unique(FCA_1km_Significant$Trait)) - 0.5), 
             color = "black", size = 1) + theme(axis.text.x = element_text(face="bold", color="black", 
                                                                           size=20)) +
  theme(
    axis.title.x = element_text(size= 20, face="bold"),
    axis.title.y = element_text(size= 20, face="bold"))


# OPTIONAL TO PLOT ALL RESULTS


ggplot(FCA_1km, aes(x = Trait, y = Land, fill = Obs)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", 
    mid = "white", 
    high = "red", 
    midpoint = 0, 
    name = "Observation",  # Customizing the legend title
    guide = guide_colorbar(
      barwidth = 1, 
      barheight = 10
    )  # Adjust the size of the color bar
  ) +
  labs(title = "Pearson Correlation Plot For Vascular plants") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 8),
    legend.position = "right",  # Position the legend on the right
    legend.text = element_text(angle = 0)  # Ensure legend text is not rotated
  ) +
  geom_text(aes(label = Significant), color = "black", size = 10, vjust = 0.5)


