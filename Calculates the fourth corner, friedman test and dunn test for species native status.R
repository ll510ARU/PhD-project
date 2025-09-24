## The following script describes how preform fourth corner analysis for native status following fridman and dunn test 
# Note species traits were seporated in to native status in excel prior to the analysis
# The species occurrences (species_occ file) was generated using Botentical Society of Britain and Ireland - see species occurrence script
# The landscape features (Landscapes_fea) was generated using Edina Digimap, OS ordnance survey and research from Bailey et al.  (2017)  - see landscape feature branch
# The species traits (species_Tra) was generated using TRY, BIEN, LEDA and BROT - see species trait branch



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


install.packages("dunn.test")
library(dunn.test)

### NATIVE SPECIES ### -----

#STEP 2: SELECT SPECIES OCCURRENCE FOR NATIVR SPECIES 

column_list <- species_Tra_NATIVE$Accepted_names
species_occ_rm_id_NATIVE <- species_occ[, names(species_occ) %in% column_list]

# STEP 3: ADD THE MONAD LOCATION TO NATIVE SPECIES OCCURRENCE 
species_occ_rm_id_NATIVE$tile_name <- species_occ$tile_name
species_occ_rm_id_NATIVE  <- species_occ_rm_id_NATIVE %>% filter(tile_name %in% Locations)

# STEP 4: FORMAT THE NATIVE SPECIES TRAIT DATA FRAME
species_Tra_NATIVE <- species_Tra_NATIVE[, -1]
species_Tra_NATIVE <- data.frame(lapply(species_Tra_NATIVE, as.numeric))

#STEP 5: CREATE A LIST OF NATIVE SPECIES TRAITS, LANDSCAPE AND OCCURRENCES
dataset_list_NATIVE <- list(
  Envi = Landscapes_fea,
  abu = species_occ_rm_id_NATIVE,
  traits = species_Tra_NATIVE)


#STEP 6: PERFORM THE FOURTHCORNER ANALYSIS FOR NATIVE SPECIES 
FCA2_NATIVE_ <- fourthcorner(dataset_list_NATIVE$Envi, dataset_list_NATIVE$abu, dataset_list_NATIVE$traits,nrepet=999, modeltype=2)
FCA4_NATIVE_ <- fourthcorner(dataset_list_NATIVE$Envi, dataset_list_NATIVE$abu, dataset_list_NATIVE$traits,nrepet=999, modeltype=4)
four.comb_NAtive_ <- combine.4thcorner(FCA2_NATIVE_, FCA4_NATIVE_)

FCANat <- summary(four.comb_NAtive_)
write.csv(FCANat, "FCANat3.csv")


### ARCHAEOPHYTES ### -----

# STEP 7: SELECT SPECIES OCCURRENCE FOR NATIVR SPECIES 

column_list <- species_Tra_aro$Accepted_names
species_occ_rm_id_aro <- species_occ[, names(species_occ) %in% column_list]

# STEP 8: ADD THE MONAD LOCATION TO NATIVE SPECIES OCCURRENCE 

species_occ_rm_id_aro$tile_name <- species_occ$tile_name
species_occ_rm_id_aro  <- species_occ_rm_id_aro %>% filter(tile_name %in% Locations)

# STEP 9: FORMAT THE NATIVE SPECIES TRAIT DATA FRAME
species_Tra_aro <- species_Tra_aro[, -66]
species_Tra_aro <- species_Tra_aro[, -1]
species_Tra_aro <- data.frame(lapply(species_Tra_aro, as.numeric))

# STEP 10: CREATE A LIST OF NATIVE SPECIES TRAITS, LANDSCAPE AND OCCURRENCES
dataset_list_aro <- list(
  Envi = Landscapes_fea,
  abu = species_occ_rm_id_aro,
  traits = species_Tra_aro)


# STEP 11: PERFORM THE FOURTHCORNER ANALYSIS FOR ARCHAEOPHYTES
FCA2_ARCO_ <- fourthcorner(dataset_list_aro$Envi, dataset_list_aro$abu, dataset_list_aro$traits,nrepet=999, modeltype=2)
FCA4_ARCO_ <- fourthcorner(dataset_list_aro$Envi, dataset_list_aro$abu, dataset_list_aro$traits,nrepet=999, modeltype=4)
four.comb_aro_ <- combine.4thcorner(FCA2_ARCO_, FCA4_ARCO_)

FCAaro <- summary(four.comb_aro_)
write.csv(FCAaro, "FCAaro3.csv")


### NEOPHYTES ### -----

# STEP 12: SELECT SPECIES OCCURRENCE FOR NEOPHYTES
column_list <- species_Tra_Neo$Accepted_names
species_occ_rm_id_Neo <- species_occ[, names(species_occ) %in% column_list]

# STEP 13: ADD THE MONAD LOCATION TO NEOPHYTES SPECIES OCCURRENCE 

species_occ_rm_id_Neo$tile_name <- species_occ$tile_name
species_occ_rm_id_Neo  <- species_occ_rm_id_Neo %>% filter(tile_name %in% Locations)

# STEP 14: FORMAT THE NEOPHYTES SPECIES TRAIT DATA FRAME
species_occ_rm_id_Neo  <- species_occ_rm_id_Neo[, -638]
species_Tra_Neo <- species_Tra_Neo[, -1]
species_Tra_Neo <- data.frame(lapply(species_Tra_Neo, as.numeric))

# STEP 15: CREATE A LIST OF NATIVE SPECIES TRAITS, LANDSCAPE AND OCCURRENCES
dataset_list_Neo <- list(
  Envi = Landscapes_fea,
  abu = species_occ_rm_id_Neo,
  traits = species_Tra_Neo)


# STEP 16: PERFORM THE FOURTHCORNER ANALYSIS FOR NEOPHYTES
FCA2_Neo_ <- fourthcorner(dataset_list_Neo$Envi, dataset_list_Neo$abu, dataset_list_Neo$traits,nrepet=999, modeltype=2)
FCA4_Neo_ <- fourthcorner(dataset_list_Neo$Envi, dataset_list_Neo$abu, dataset_list_Neo$traits,nrepet=999, modeltype=4)
four.comb_Neo_ <- combine.4thcorner(FCA2_Neo_, FCA4_Neo_)

FCANeo <- summary(four.comb_Neo_)

write.csv(FCANeo, "FCANeo3.csv")

# STEP 17:ADD ANONATION TO IDENTIFY THE NATIVE STATUS

FCAaro3$GROUP <- "ARO"
FCANeo3$GROUP <- "NEO"
FCANat3$GROUP <- "NAT"

# STEP 18: COMBINE THE FOURTH CORNER DATASETS

NS_ANALYSIS <- bind_rows(
  FCAaro3,
  FCANat3,
  FCA3,
  FCANeo3
)

write.csv(NS_ANALYSIS, "NS_ANALYSIS.csv")



# STEP 19: PERFORM THE FRIEDMAN TEST

result <- friedman.test(Obs ~ Test | GROUP, data = NS_ANALYSIS)

# STEP 20: PERFORM THE DUNN TEST
H <- dunn.test(NS_ANALYSIS$Obs, g = interaction(NS_ANALYSIS$Test, NS_ANALYSIS$GROUP), method = "bonferroni")
PH <- data.frame(H$comparisons, H$P, H$chi2, H$Z)

# STEP 21: FORMAT THE RESULTS FROM THE DUNN TEST
split <- strsplit(PH$H.comparisons, "/")
PH$LAND <- sapply(split, `[`, 1)
PH$Trait_NS <- sapply(split, `[`, 2)
PH$Trait_NSpt2 <- sapply(split, `[`, 3)
split <- strsplit(PH$Trait_NS, "-")
PH$Trait_NS <- sapply(split, `[`, 1)
PH <- PH %>%
  mutate(Merged = paste(Trait_NS, Trait_NSpt2, sep = "-"))

#save the Dunn test results 
write.csv(PH, "DUnn_test_results_NSc.csv")



### NATIVE STATUS FOUTH CORNER CORELATION GRAPH### ----

# STEP 22: ORDER NATIVE STATUS
NS_ANALYSIS$GROUP <- factor(NS_ANALYSIS$GROUP, levels = c("NAT", "ARO", "NEO"))

# STEP 23: CREATE A GRAPH FOR EACH TRAIT
plots <- list()
for (trait in unique(NS_ANALYSIS$Trait)) {
  plot <- ggplot(subset(NS_ANALYSIS, Trait == trait), aes(x = Land, y = Obs, fill = GROUP)) +
    geom_bar(stat = "identity", position = "dodge", width = 0.8) +
    labs(title = paste("Friedman Test Results - Trait:", trait),
         x = "Test", y = "Mean Observed Value") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
    theme(axis.ticks.x = element_line(size = 0.5)) +
    scale_fill_brewer(palette = "RdYlBu") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.1)
  
  plots[[trait]] <- plot
  
  # Save each plot as a PNG file
  ggsave(paste("plot_", trait, ".png", sep = ""), plot, width = 6, height = 4, units = "in")
}
