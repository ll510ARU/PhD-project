# The following script describes how to calculate functional dispersion and functional richness using the package FD at a given resolution and species group
# The species occurrences that well sampled (well_sampled_species_occ file) were generated using Botanical Society of Britain and Ireland - see species occurrence script
#The species traits (species_Tra) were generated using TRY, BIEN, LEDA and BROT - see species trait branch


# STEP 1: Check that well sampled locations is confined to the given resolution

well_sampled_species_occ_filter <- 
  well_sampled_species_occ[
    nchar(as.character(well_sampled_species_occ$tile_name)) == 'enter the number of charcters the grid reference', ]


# STEP 2: Set row names of trait matrix in order of the accepted names of species_Tra and well_sampled_species_occ
#and convert the datasets to matrixes 

# remove the ID column of species_Tra
traits_data <- species_Tra[,-1]

# Set row names
rownames(traits_data) <- c(species_Tra$Accepted_names)

# create a seporate dataset
OCU_df <- as.data.frame(well_sampled_species_occ)

# Convert to matrix if necessary
occurrence_data <- as.matrix(OCU_df[, -1])

# Set row names
rownames(OCU_df) <- OCU_df$Accepted_names


# STEP 3: Check of filter out the zero-abundance species from traits_data and check 'traits_data' has columns corresponding to species names.

zero_abundance_species <- colSums(occurrence_data) == 0

traits_data_filtered <- traits_data[, !colnames(traits_data) %in% colnames(occurrence_data)[zero_abundance_species]]

#Check occurrence_data is also filtered to match the traits_data and remove zero-abundance species
occurrence_data_filtered <- occurrence_data[, !zero_abundance_species]

occurrence_data_filtered <- occurrence_data[, !zero_abundance_species]

#STEP 4: Ensure the traits_data includes only species present in the filtered occurrence_data
# and reorder traits_data_filtered to match the order of occurrence_data_filtered columns

traits_data_filtered <- traits_data[rownames(traits_data) %in% colnames(occurrence_data_filtered), ]

traits_data_filtered <- traits_data_filtered[match(colnames(occurrence_data_filtered), rownames(traits_data_filtered)), ]

#STEP 5: Run the dbFD function with the filtered data to calculate functional diversity
result <- dbFD(traits_data_filtered, occurrence_data_filtered, stand.x = TRUE, calc.FRic = TRUE)

fdisps_result <- as.data.frame(result[["FDis"]])
fdispR_result <- as.data.frame(result[["FRic"]])

# STEP 6: Combine 1km grid references (monad) and functional diversity into a dataset
columns_to_keep <- colnames(occurrence_data_filtered)

filtered_OCU_df<- OCU_df[, c(1, which(colnames(OCU_df) %in% columns_to_keep))]

FDM_<- cbind(Monad = filtered_OCU_df$tile_name, FDIS = fdisps_result$`result[["FDis"]]`, FRIC = fdispR_result$`result[["FRic"]]`)


# STEP 7: Round the functional diversity to 2 decimal places and remove locations with zero functional diversity to prevent inflation of results. 

FDM$FDIS <- round(FDM$FDIS, 2)
FDM$FRIC <- round(FDM$FRIC, 2)

FDM <- FDM[FDM$FDIS != 0, ]
FDM <- FDM[FDM$FRIC != 0, ]

write.csv(FDM, "FDM_STANDISED_NO_ZEROS.csv")

# STEP 8: Repeat this process for the remaining resolutions and species groups (e.g native species at the 10km and 1km resolution)
#by subsitituting well_sampled_species_occ  and species_Tra