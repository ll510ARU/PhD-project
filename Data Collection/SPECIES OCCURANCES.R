###Introduction - This script describes how to create species occurrences using ###
## Botanical Society of Britain & Ireland database (Distribution Database) from 1990-2023
#In this process a series of file directories and output directories and are use
#To complete the following script please set up these file directories before running the code

# 1.Orginal_Files - description, the individual occurrence records from BSBI save as csv files total 34
# 2.Extracted_Files - description, will have the individual occurrence records with only taxon, record.status, easting, northing, monad, date columns 
#3.Change_to_wide_format - description, will have the he individual occurrence records of species x monad

#4.Add_missing_species description, will have the he individual occurrence records with added species
#5.changed_species - description, will have the individual occurrence records with no artifacts 
#6.Ordered_species - description, will have the individual occurrence records with ordered alphabetically species names
#7.Intrum_Files - description, will have the individual occurrence records converts site × species matrix into a species × site matrix
#Reason for the Intrum_Files is so possible to converge columns with the same name 
#8.resolved_summerised_species_names_wide description, will have the individual occurrence records with summarised resolved species names no duplicates

#STEP 1: EXTRACT THE SPECIES, LOCATION AND DATE COLUMNS

# List of file names from the file directory 1.Orginal_Files 
dataFiles <- list.files(path = "insert the file directory to 1.Orginal_Files", pattern = "\\.csv", full.names = TRUE)

# Output directory - 2.Extracted_Files
output_dir <- "insert the file directory to 2.Extracted_Files"  # Set the desired output folder



# Loop through each file to extract the columns taxon, record.status, easting, northing, monad
for (file in dataFiles) {
  
  # Read the CSV file without row names
  data <- read.csv(file, header = TRUE, row.names = NULL)
  
  # Select specific columns
  selected_data <- data[, c("taxon", "record.status", "easting", "northing", "monad", "date"), drop = FALSE]
  
  # Rename the "record.status" column to "record_status"
  colnames(selected_data)[colnames(selected_data) == "record.status"] <- "record_status"
  
  # Add a unique identifier column (e.g., row_number)
  selected_data$unique_id <- seq_len(nrow(selected_data))
  
  # Create output file name
  output_file <- file.path(output_dir, paste0("Extracted_", basename(file)))
  
  # Save the selected data to a new CSV file with unique identifiers and without row names
  write.csv(selected_data, file = output_file, row.names = FALSE)
  
  cat("Processed file:", file, "\n")
}

cat("Processing complete.\n")



#STEP 2: CALCULATE THE SPECIES OCCURRENCE FOR EACH MONAD LOCATIONS
#AND TO CONVERT THE DATASETS TO WIDE FORMAT SPECIES AND MONAD LOCATIONS

dataFiles <- list.files(path = "insert the file directory to 2.Extracted_Files/", pattern = "\\.csv", full.names = TRUE)
output_dir <- list.files(path = "insert the file directory to 3.Change_to_wide_format /", pattern = "\\.csv", full.names = TRUE) 

#Install and load the package dplyr
install.packages("dplyr")
library(dplyr)

# Create an empty list to store the output files for when species occurrences have been counted
results_list <- list()

# This loop does the following
#Replace within the monad column the blank values with NA as otherwise will interpt as values.
#Creates a new ID (OID) that combines monad and taxon and then counts the number of times these occur
# the occurrence, monad and taxon are transposed into the wide format


for (file in dataFiles) {
  # Read the dataset from 2.Extracted_Files directory
  data <- read.csv(file)
  
  # Replace within the monad column the blank values with NA 
  data$monad[data$monad == ""] <- NA
  
  #Creates a new ID
  data$OID <- paste(data$monad, data$taxon, sep = " ")
  #counts the number of times the species occurs within a monad
  TA <- count(data, OID)
  
  #Splits the combined OID back into two separate columns: Monad and Taxon.
  # Extract the only Monad, Taxon, and the count n.
  TA[c('Monad', "Taxon")] <- str_split_fixed(TA$OID, ' ', 2)
  TA1 <- TA[, c('Monad', "Taxon", "n")]
  
  #Convert to wide format (species × monad matrix) where rows are monads and columns are species
  data_wide <- spread(TA1, Taxon, n, fill = 0)
  
  # Store the wide format species × monad matrix in the list
  results_list[[file]] <- data_wide
  
  # Saves the  data_wide as a new CSV in output_dir with prefix wide_
  output_file <- file.path(output_dir, paste0("wide_", basename(file)))
  
  # Save the selected data to a new CSV file
  write.csv(data_wide, file = output_file, row.names = FALSE)
  
  cat("Processed file:", file, "\n")
}

cat("Processing complete.\n")

# Inspect the files to check the number of locations
num_rows <- sapply(results_list, nrow)
sum(num_rows)



#STEP 3: ADD TO EACH YEAR THE SPECIES THAT DID NOT OCCUR THAT YEAR 
#BUT WERE IN THE PRESENT IN THE TIMEFRAME OF THE STUDY

#This step consists of 3 loops
# LOOP 1: Compares the species column names across each year
# LOOP 2: Adds the missing species to each year and put those species occurrence as zero
# LOOP 3: Save harmonised files

#Ensuring the file directory of the wide format is read in and the out directory is
dataFiles <- list.files(path = "insert the file directory to 3.Change_to_wide_format/", pattern = "\\.csv", full.names = TRUE)
output_dir <- list.files(path = "insert the file directory to 4.Add_missing_species/", pattern = "\\.csv", full.names = TRUE)


# Create an empty list to store each individual occurrence data frames
data_frames <- list()

# Read each file and store the individual occurrence data frames in the list
for (file in dataFiles) {
  tryCatch({
    current_df <- read.csv(file, sep = ",", stringsAsFactors = FALSE)
    data_frames <- c(data_frames, list(current_df))
  }, error = function(e) {
    cat("Error reading file:", file, "\n")
    # Handle the error as needed
  })
}

# LOOP 1: Compares the species column names across each year

# Check if there are at least two individual occurrence dataframe  in the list
if (length(data_frames) >= 2) {
  
  # Create an empty list to store missing species column names for each pair
  missing_columns_list <- vector("list", length = choose(length(data_frames), 2))
  
  # Counter for indexing the missing_columns_list
  counter <- 1
  
  # Iterate through each pair of individual occurrence dataframe 
  for (i in 1:(length(data_frames) - 1)) {
    for (j in (i + 1):length(data_frames)) {
      # Extract the current pair of data frames
      df1 <- data_frames[[i]]
      df2 <- data_frames[[j]]
      
      # Identify missing species column names in each dataframe
      missing_columns_df1 <- setdiff(names(df2), names(df1))
      missing_columns_df2 <- setdiff(names(df1), names(df2))
      
      # Store the missing column names in the list
      missing_columns_list[[counter]] <- list(
        DataFrames = c(i, j),
        MissingInDF1 = missing_columns_df1,
        MissingInDF2 = missing_columns_df2
      )
      
      # Increment the counter
      counter <- counter + 1
    }
  }
  
  # Print the species names to check the proccess
  for (k in 1:length(missing_columns_list)) {
    cat(
      "Data Frames", missing_columns_list[[k]]$DataFrames[1],
      "and", missing_columns_list[[k]]$DataFrames[2],
      "have missing column names:\n",
      "  Missing in DF1:", paste(missing_columns_list[[k]]$MissingInDF1, collapse = ", "), "\n",
      "  Missing in DF2:", paste(missing_columns_list[[k]]$MissingInDF2, collapse = ", "), "\n\n"
    )
  }
} else {
  cat("There are not enough data frames in the list for comparison.\n")
}

# LOOP 2: Adds the missing species to each year and put those species occurrence as zero

# Check if there are at least two individual occurrence dataframe  in the list
#Finds all unique species names across all files
if (length(data_frames) >= 2) {
  all_column_names <- unique(unlist(lapply(data_frames, names)))
  
  # Iterate through each data frame
  #Ensures every occurrence dataframe has the same set of species names by 
  #adding the missing ones fills the occurrence with NA.
  #Prints out the dimensions of each dataframe afterwards.To check that all the same.
  
   for (i in seq_along(data_frames)) {
    # Identify missing columns in the current data frame
    missing_columns <- setdiff(all_column_names, names(data_frames[[i]]))
    
    # Add missing columns with NA values
    data_frames[[i]][missing_columns] <- NA
  }
  
  # Verify the result
  for (i in seq_along(data_frames)) {
    cat("Data Frame", i, "dimensions after adding missing columns:",
        paste(dim(data_frames[[i]]), collapse = " x "), "\n")
  }
} else {
  cat("There are not enough data frames in the list for comparison.\n")
}



# Converts all NA values in the remaining columns to 0 expect from first 1 column which is the monad
if (length(data_frames) > 0) {
  data_frames <- lapply(data_frames, function(df) {
    df[, -c(1)][is.na(df[, -c(1)])] <- 0
    return(df)
  })
  
  # Verify the changes in the dataset
  invisible(lapply(seq_along(data_frames), function(i) {
    cat("Data Frame", i, "dimensions after replacing NA with 0 (except first 1 column):",
        paste(dim(data_frames[[i]]), collapse = " x "), "\n")
  }))
} else {
  cat("No data frames in the list.\n")
}


# LOOP 3: Save harmonised files


# Loop through each data frame
for (i in seq_along(data_frames)) {
  file_path <- file.path(output_dir, paste0("SSN_wide_", i, ".csv"))
  write.csv(data_frames[[i]], file = file_path, row.names = FALSE)
  cat("Data Frame", i, "saved to", file_path, "\n")
}

cat("Saving complete.\n")

#Clean the environment
rm()

#STEP 4: RESOLVES THE SPECIES NAMES 

#The following proccess creates a resolved species list of names 
#Please note that part of this proccess was mannually achieved through excel
#and the World Checklist of Vascular Plants

#Install and load the following packages
install.packages("devtools")
install.packages('taxize')
install.packages('magrittr')
install.packages("reshape")
install.packages("tidyr")
install.packages("taxize")  
install.packages("bdc") 


library("usethis")
library("devtools")
library('taxize')
library('magrittr')
library("reshape")
library("tidyr")
library("taxize")
library("bdc")


# Concatenate data frames
combined_df <- bind_rows(data_frames)
CN<-as.data.frame(colnames(combined_df))

# load SSN_wide_1.csv and create a dataset with species names
all_column_names <- names(SSN_wide_1)[-1]
species_names_list <- data.frame(Specie_name = all_column_names)
head(species_names_list)

#Seporate the names into subeset for 1000 row, this is to speed up the proccess
#And to check that each subset.
subset1_row <-0:1000
subset2_row <- 1001:2000
subset3_row <-2001:3000
subset4_row <- 3001:4000
subset5_row <- 4001:5000
subset6_row <- 5001:5441

SUBSET1 <- species_names_list$Specie_name[subset1_row]
Specie_name_pt1 <- as.data.frame(Specie_name_pt1 = SUBSET1)
SUBSET2 <- species_names_list$Specie_name[subset2_row]
Specie_name_pt2 <- as.data.frame(Specie_name_pt2 = SUBSET2)
SUBSET3 <- species_names_list$Specie_name[subset3_row]
Specie_name_pt3 <- as.data.frame(Specie_name_pt3 = SUBSET3)
SUBSET4 <- species_names_list$Specie_name[subset4_row]
Specie_name_pt4 <- as.data.frame(Specie_name_pt4 = SUBSET4)
SUBSET5 <- species_names_list$Specie_name[subset5_row]
Specie_name_pt5 <- as.data.frame(Specie_name_pt5 = SUBSET5)
SUBSET6 <- species_names_list$Specie_name[subset6_row]
Specie_name_pt6 <- as.data.frame(Specie_name_pt6 = SUBSET6)


# Resolve the species using GBIF Backbone Taxonomy
result.long1 <- Specie_name_pt1$SUBSET1 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
#Changes the column name user_supplied_name → "Species names"
colnames(result.long1)[colnames(result.long1) == "user_supplied_name"]<- "Species_names"

#Joins the resolved species names to the orginal species_names_list
results_df<- species_names_list %>% left_join(result.long1, by = "Species_names")

#Copies the column matched_name2 (resolved/standardized name) into a new column FillAccepted_names.
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
#Fills the missing accepted names
fillp1_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp1_result_df <- fillp1_result_df %>% distinct(Specie_name, .keep_all = TRUE)


#Proccess is repeated for the other subsets
result.long2 <- Specie_name_pt2$SUBSET2 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
colnames(result.long2)[colnames(result.long2) == "user_supplied_name"]<- "Species names"
results_df<- species_names_list %>% left_join(result.long2, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp2_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp2_result_df <- fillp2_result_df %>% distinct(Specie_name, .keep_all = TRUE)

result.long3 <- Specie_name_pt3$SUBSET3 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
colnames(result.long3)[colnames(result.long3) == "user_supplied_name"]<- "Species names"
results_df<- species_names_list %>% left_join(result.long3, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp3_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp3_result_df <- fillp3_result_df %>% distinct(Specie_name, .keep_all = TRUE)


result.long4 <- Specie_name_pt4$SUBSET4 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
colnames(result.long4)[colnames(result.long4) == "user_supplied_name"]<- "Species names"
results_df<- species_names_list %>% left_join(result.long4, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp4_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp4_result_df <- fillp4_result_df %>% distinct(Specie_name, .keep_all = TRUE)

result.long5 <- Specie_name_pt5$SUBSET5 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
colnames(result.long5)[colnames(result.long5) == "user_supplied_name"]<- "Species names"
results_df<- species_names_list %>% left_join(result.long5, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp5_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp5_result_df <- fillp5_result_df %>% distinct(Specie_name, .keep_all = TRUE)

result.long6 <- Specie_name_pt6$SUBSET6 %>% gnr_resolve(data_source_ids = c(167), with_canonical_ranks=T)
colnames(result.long6)[colnames(result.long6) == "user_supplied_name"]<- "Species names"
results_df<- species_names_list %>% left_join(result.long6, by = "Species names")
results_df<- results_df %>% mutate(FillAccepted_names = matched_name2)
fillp6_result_df <- results_df %>% mutate(`Accepted names` = coalesce(`Accepted names`, FillAccepted_names))
dis_fillp6_result_df <- fillp6_result_df %>% distinct(Specie_name, .keep_all = TRUE)


# Save files for reference and check resolusions
write.csv(dis_fillp1_result_df, "dis_fillp1_result_df.csv")
write.csv(dis_fillp2_result_df, "dis_fillp2_result_df.csv")
write.csv(dis_fillp3_result_df, "dis_fillp3_result_df.csv")
write.csv(dis_fillp4_result_df, "dis_fillp4_result_df.csv")
write.csv(dis_fillp5_result_df, "dis_fillp5_result_df.csv")
write.csv(dis_fillp6_result_df, "dis_fillp6_result_df.csv")


Species_df <- rbind(dis_fillp1_result_df, dis_fillp2_result_df,
                    dis_fillp3_result_df, dis_fillp4_result_df,
                    dis_fillp5_result_df, dis_fillp6_result_df)
# Collate the rows with missing species names 
Unresolved_names <- Species_df %>%
  filter(is.na(`Accepted names`)) %>%
  select(Species_names)   # keep only the species name column

# check for the resolved names through ncbi
SP1<- c(Unresolved_names$Species_names)
names_har1<- bdc_query_names_taxadb(
  SP1,
  replace_synonyms = TRUE,
  suggest_names = TRUE,
  suggestion_distance = 0.9,
  db = "ncbi",
  parallel = TRUE,
  ncores = 2,
  export_accepted = FALSE)

write.csv(names_har1, "UN_name_check.csv")


# MANNUAL - Within excel addin the UN_name_check to main species and fill in the 
#main gaps with World Checklist of Vascular Plants 
# Have a final list csv that contrains Species names column and the accepted name column
# ENSURE THAT THE SPECIES NAME COLUMN IS IN ALPHEBICAL ORDER AND CHECK THE HYBRID SPECIES REMAIN THE SAME

#STEP 5: REVERT THE HYBRID SPECIES NAMES DUE TO AN ARTIFACT 

csv_files <- list.files(path = "insert the file directory to 4.Add_missing_species/", pattern = "\\.csv$", full.names = TRUE)
output_dir <- list.files(path = "insert the file directory to 5.changed_species/", pattern = "\\.csv", full.names = TRUE)

# changes the hybrid species name 
column_number <- 4957  # Example column number
new_column_name <- "Narcissus.x.cyclazetta"

# changes the hybrid name to correct format across of the databases
# Loop through each CSV file
for (i in seq_along(csv_files)) {
  file <- csv_files[i]
  # Read the CSV
  data <- read.csv(file)
  # Renames the desired column
  colnames(data)[column_number] <- new_column_name
  
  # Save file to the out directory
  output_file <- file.path(output_dir, paste0("changed_", tools::file_path_sans_ext(basename(file)), ".csv"))
  # Save modified CSV
  write.csv(data, file = output_file, row.names = FALSE)
  
  cat("Processed and saved file:", output_file, "\n")
}


#STEP 6: ARRANGES THE SPECIES NAMES IN THE SAME ORDER AS 
#ORGINAL NAMES IN THE FINAL RESOLVED SPECIES LIST

csv_files <- list.files(path = "insert the file directory to 5.changed_species", pattern = "\\.csv$", full.names = TRUE)
output_dir <- list.files(path = "insert the file directory to 6.Ordered_species/", pattern = "\\.csv", full.names = TRUE)


desired_order <- c(Final_species_list$Specie_name)

# Loop through each occurrence file
for (file in csv_files) {
  # Read the CSV file
  SSN <- read.csv(file)
  
  # Reorder columns according to desired_order
  df_ordered <- SSN %>%
    select(Monad, all_of(desired_order))
  
  #  Save the reordered occurrence file
  output_file <- file.path(output_dir, paste0("df_ordered_", tools::file_path_sans_ext(basename(file)), ".csv"))
  write.csv(df_ordered, file = output_file, row.names = FALSE)
  
  cat("Processed and saved file:", output_file, "\n")
}


# STEP 7: CHANGES ALL THE ORGINAL NAMES TO THE RESOLVED SPECIES NAMES 
# WITHIN THE INDIVIDUAL OCCURRENCE FILES
data_files <- list.files(path = "insert the file directory to 6.Ordered_species", pattern = "\\.csv$", full.names = TRUE)
output_dir <- list.files(path = "insert the file directory to 7. Intrum_files", pattern = "\\.csv", full.names = TRUE)

# Loop through each CSV file
for (file in data_files) {
  # Read the CSV file
  df_ordered <- read.csv(file)
  
  # Removes the first column, monad of the dataframe.
  df_ordered <- df_ordered[-1]
  
  #Renames the orginal species name to accepted resolved names.
  CN <- colnames(df_ordered)
  CTC <- CN[-1]
  NCN <- c(Final_species_list$Accepted_names)
  colnames(df_ordered)[match(CTC, colnames(df_ordered))] <- NCN
  
  #Sets the first row of the transposed data as accepted resolved names.
  #converts a site × species matrix into a species × site matrix
  transposed_data <- t(df_ordered)
  colnames(transposed_data) <- transposed_data[1, ]
  transposed_data <- transposed_data[-1, ]
  
  
  
    #  Save the reordered occurrence file
    output_file <- file.path(output_dir, paste0(tools::file_path_sans_ext(basename(file)), "_Tr.csv"))
    write.csv(transposed_data, file = output_file, row.names = FALSE)
    
    cat("Processed and saved file:", output_file, "\n")
  }
  

# STEP 8: SUMMARISES SPECIES NAME COLUMNS 

data_files <- list.files(path = "insert the file directory to 7.Intrum_files", pattern = "\\.csv$", full.names = TRUE)  
output_dir <- list.files(path = "insert the file directory to 8.resolved_summerised_species_names_wide", pattern = "\\.csv", full.names = TRUE)


for (file in data_files) {
  # Read data
  transposed_data <- read.csv(file)
  # Add a name Taxon to the species column
  names(transposed_data)[1] <- "Taxon"
  
  #Summarises the species name columns
  summary_data <- transposed_data %>%
    group_by(Taxon) %>%
    summarise(across(everything(), sum))
  
  #Converts to site × species matrix 
  RESOLVED_SSN_WIDE <- t(summary_data)
  colnames(RESOLVED_SSN_WIDE) <- RESOLVED_SSN_WIDE[1, ]
  RESOLVED_SSN_WIDE <- RESOLVED_SSN_WIDE[-1, ]
  RESOLVED_SSN_WIDE <- as.data.frame(RESOLVED_SSN_WIDE)
  colnames(RESOLVED_SSN_WIDE) <- iconv(colnames(RESOLVED_SSN_WIDE), "UTF-8", "ASCII", sub = "")
  
  # Write output CSV file
  write.csv(RESOLVED_SSN_WIDE, paste0("RESOLVED_SSN_WIDE_", tools::file_path_sans_ext(basename(file)), ".csv"))
}



# STEP 9: SUMMERISING THE OCCURRANCES OVER THE STUDY PERIOD FROM 1990-2023
# list of data frames named data_frames_list
dataFiles <- list.files(path = "insert the file directory to 8.resolved_summerised_species_names_wide", pattern = "\\.csv", full.names = TRUE)

# create an empty list to store occurrence files
data_frames_list <- list()

# Read each occurrence file and store the dataframe in the list
for (file in dataFiles) {
  {
    current_df <- read.csv(file, sep = ",", stringsAsFactors = FALSE)
    data_frames_list <- c(data_frames_list, list(current_df))
  }
}

# create an empty list to store all the data together
list_of_long_data<-list()


# Use a loop to iterate over each data frame
for (i in seq_along(data_frames_list)) {
  # Get the column indices for gathering
  col_indices <- 2:4952  # Adjust the range of column indices as needed
  
  # Gathers the occurrences and store within the list
  long_data <- gather(data_frames_list[[i]], species, ocur, 
                      all_of(col_indices), factor_key = TRUE)
  
  list_of_long_data[[i]] <- long_data
}

#Combines all the monads, species, and occurrence values.

combined_df2 <- bind_rows(list_of_long_data)

#Calculate mean species count per X, X = monads
subsets <- split(combined_df2, combined_df2$species)
mean_species <- lapply(subsets, function(subset) {
  subset %>%
    group_by(X, species) %>%
    summarise(mean_species_count = mean(ocur, na.rm = TRUE), .groups = "drop")
})

#Clear the following
rm(combined_df2, long_data, list_long_data)


# Combine the results into a single data frame
mean_species_df <- bind_rows(mean_species)

# Converts to the formmating and saves the file
data_wide <- spread(mean_species_df, species, mean_species_count)
write.csv(data_wide, "Occurance_mean_years1km.csv")


# STEP 10: CREATES A OCCURRENCE DATASET USEING CATORGIES 

for (i in 3:4953) {
  Occurance_mean_years1km[, i] <- ifelse(Occurance_mean_years1km[, i] > 0.000000000, "present", "absence")
}
write.csv(Occurance_mean_years1km, "Occurance_presence-absence_mean_years1km.csv")

#RELOAD Occurance_mean_years1km

for (i in 3:4953) {
  Occurance_mean_years1km[, i] <- ifelse(Occurance_mean_years1km[, i] > 0.000000000, 1, 0)
}
write.csv(Occurance_mean_years1km, "Occurance_Freq_years1km.csv")


# STEP 11: ASSESSMENT FOR WELL SAMPLED SITES FOR Occurance_Freq_years1km
#Removing the under sampled areas
# Calculate species presence counts across locations
sample_assessment <- Occurance_Freq_years1km[, 3:4953] %>%
  mutate(assessment = ifelse(rowSums(select(., everything()) >= 2.5), "keep", "eliminate"))

#Filter for the rows to keep
Occur<-bind_cols(Occurance_Freq_years1km$X, sample_assessment)

names<- "keep"
Keeps<- Occur %>% filter(assessment %in% names)

write.csv(Keeps, "Occurance_mean_years1km_well_sampliedcorrected_PA.csv")


