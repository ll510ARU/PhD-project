The repository contains the scripts used to generate the following: Understanding the role of British landscape features in shaping vascular plant traits across native and non-native species. 

# Folders
1. Species occurrences sourced from the Botanical Society of Britain and Ireland (BSBI)
2. Corresponding Species traits sourced from the TRY, Botanical Information and Ecology Network (BIEN), life history traits of the Northwest European flora
(LEDA) and TR8
3. Landscape features
4. Analysis
   
Please be aware that several steps were performed manually in either Excel or QGIS
### Species occurrences folder contains calculating species occurrences, resolving species names and removing crop species ####
Note - To efficiently calculate species occurrence, the following working directories were created before running the code
1. Original_Files - the individual occurrence records from BSBI are saved as CSV files
   # The following directories should be empty before starting the script
2.Extracted_Files - description: the individual occurrence records with only taxon, record status, easting, northing, monad, and date columns 
3.Change_to_wide_format - description: the individual occurrence records of species x monad
4.Add_missing_species description: the individual occurrence records with added species
5.changed_species - description: the individual occurrence records with no artefacts 
6.Ordered_species - description: the individual occurrence records with ordered alphabetically species names
7.Intrum_Files - description: the individual occurrence records convert site × species matrix into a species × site matrix
8.resolved_summerised_species_names_wide - description: the individual occurrence records with summerised resolved species names, no duplicates

This script also describes resolving the species names, but does not remove the hybrids or crops. 

Manual steps include - Checking unresolved species in the World Checklist of Vascular Plants database.

## Removing the crop species from the study species 

Note - common crop species were sourced from the European Search Catalogue for Plant Genetic Resources.

### Corresponding Species traits folder contains how the individual traits were calculated and the imputation process 
## Species traits 
Note - The traits can be calculated in any order, but to produce a single file of the species traits, performing the scripts in the following order is advised

1. Specific leaf area - SLA
2. Leaf dry matter content - LDMC
3. Seed mass
4. Plant height
5. dispersal modes
   
 Important - please check the units within the different databases being used, to make sure they match using conversions.

## Imputation Process
Note— m represents the number of decision trees and can be changed based on the percentage of missing trait information.

###  Landscape features folder contains the individual landscape feature scripts and how they are all joined together
Note: Before starting any of the scripts, the data was gathered manually in QGIS, where a whole map of Great Britain was generated, and the area of the landscape features was calculated. Other manual steps include summarising the superficial deposit sources using the British Geological Society Lexicon of Named Rock Units (2020).

### The Analysis folder contains a script for calculating the fourth corner analysis, the Friedman test, the Dunn test, and graph visualisations.
Note—There are two scripts for the fourth corner analysis, one for all species and one for each native species. 

