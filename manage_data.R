#calculate shannon diversity index 
#The formula is H' = - Σ (pi * ln(pi)), where 'pi' 
#represents the proportion of individuals belonging to species 'i' and 'ln' 
#is the natural logarithm.

#calculate simpson biodiversity index
#D = 1 - Σ (n/N)²:
#Where:
  #D: is Simpson's Diversity Index
#n: is the number of individuals of a particular species
#N: is the total number of individuals of all species
#Σ: (sigma) indicates the sum of the values for each species 


library(tidyverse)

#put functions here
add_rogue_species <- function(df, df_to_put_in_df) {
  if (!"ESVId" %in% colnames(df)) {
    stop("The column 'Accession' is missing in the first dataframe (df).")
  }
  if (!"ESVId" %in% colnames(df_to_put_in_df)) {
    stop("The column 'Accession' is missing in the second dataframe (df_to_put_in_df).")
  }
  # Filter rogue_insects to remove rows with ESVId already in only_insecta
  new_rows <- df_to_put_in_df %>%
    filter(!(ESVId %in% df$ESVId))
  
  # Combine only_insecta with the new rows from rogue_insects
  new_data <- bind_rows(df, new_rows)
  
  return(new_data)
}
delete_repeats <- function(df) {
  # Remove rows with duplicate ESVId, keeping the first occurrence
  df_unique <- df %>%
    distinct(Accession, .keep_all = TRUE)
  
  return(df_unique)
}


#clean insect data to only show species from arthropoda
insect_data <- read_csv(file.choose())
crust_data <- read.csv(file.choose())
only_arth_data <- filter(insect_data, str_detect(Phylum, 'Arthropoda'))
only_insecta_data <- filter(only_arth_data, str_detect(Class, 'Insecta'))
only_insecta_data <- delete_repeats(only_insecta_data)
write_csv(only_insecta_data, "JVB4500-BRS-insects-esv-data(with only insecta).csv")

#clean crusteacean data to only have the classes (
#"Branchiopoda", "Remipedia", "Cephalocarida", "Maxillopoda", "Ostracoda", 
#"Malacostraca", "Mystacocarida", "Pentastomida", "Stomatopoda", "Leptostraca", 
#"Branchiura", "Thecostraca")
crustacea_classes <- c(
  "Branchiopoda", "Remipedia", "Cephalocarida", "Maxillopoda", "Ostracoda", 
  "Malacostraca", "Mystacocarida", "Pentastomida", "Stomatopoda", "Leptostraca", 
  "Branchiura", "Thecostraca"
)
print(colnames(crust_data))

only_crustacea_class_data <- filter(crust_data, str_detect(Class, paste0(crustacea_classes, collapse = "|")))
only_crustacea_class_data <- delete_repeats(only_crustacea_class_data)

#check for insecta in crust_data:
rogue_insects <- filter(crust_data, str_detect(Class, 'Insecta'))
print(colnames(rogue_insects))
#theres several rogue insects so add 
new_insect_data <- add_rogue_species(only_insecta_data, rogue_insects)
new_insect_data <- delete_repeats(new_insect_data)
#save file with all insects in insect data and crusteacea data
write_csv(new_insect_data, "JVB4500-BRS-insects-esv-data(with only insecta and insecta rows from crustacean dataframe).csv")

#check for crustaceans in insect data
rogue_crust <- filter(insect_data, str_detect(Class, paste0(crustacea_classes, collapse = "|")))
#add rogue crust to only_crusteacean class data
new_crust_data <- add_rogue_species(only_crustacea_class_data, rogue_crust)
new_crust_data <- delete_repeats(new_crust_data)
write_csv(new_crust_data, "JVB4500-UniCOI-crustaceans-esv-data(with filtered crustacean classes and added crustaceans from insecta dataset)")

#check for non insect and non crustaceans in both datasets
no_insect_no_crust_insect_data <- insect_data %>%
  filter(!str_detect(Class, 'Insecta')) %>%
  filter(!str_detect(Class, paste0(crustacea_classes, collapse = "|")))
# find unique phylum values
print(unique(no_insect_no_crust_insect_data$Phylum))
#The following are the phylumns found with rows that are not insects or crusts in insect dataset
  #Bacillariophyta (Diatoms)
  #Arthropoda (Insects, Spiders, Crustaceans, etc.)
  #Porifera (Sponges)
  #Cnidaria (Jellyfish, Corals, Sea Anemones, Hydra)
  #Rhodophyta (Red Algae)
  #Discosea (Amoeboid Protists)
  #Chlorophyta (Green Algae)
  #Onychophora (Velvet Worms)
  #Chordata (Vertebrates and Some Invertebrates)
  #Hemichordata (Acorn Worms & Pterobranchs)
  #unk_phylum (Unknown Phylum)
  #Rotifera (Rotifers)
  #Annelida (Segmented Worms)
  #Mollusca (Snails, Clams, Octopuses)
  #Oomycota (Water Molds)
  #Nemertea (Ribbon Worms)
  #Streptophyta (Land Plants & Some Green Algae)
  #Nematoda (Roundworms)
  #Basidiomycota (Mushrooms, Shelf Fungi)
  #Blastocladiomycota (A Group of Fungi)
  #Evosea (Protist Group)
  #Zoopagomycota (Fungi)
  #Mucoromycota (Molds)
  #Gastrotricha (Gastrotrichs)
  #Ascomycota (Sac Fungi)
  #Tardigrada (Water Bears)
  #Tubulinea (Amoeboid Protists)
  #Chytridiomycota (Chytrid Fungi)
  #Haptophyta (Single-Celled Algae)
  #Echinodermata (Sea Stars, Urchins, Cucumbers)
  #Platyhelminthes (Flatworms)
  #Bryozoa (Moss Animals)
  #Endomyxa (Protist Group)
  #Cercozoa (Single-Celled Protists)
no_insect_no_crust_crust_data <- crust_data %>%
  filter(!str_detect(Class, 'Insecta')) %>%
  filter(!str_detect(Class, paste0(crustacea_classes, collapse = "|")))
print(unique(no_insect_no_crust_insect_data$Phylum))
#unique phyla in crust dataset
  #[1] "Bacillariophyta"    "Arthropoda"         "Porifera"           "Cnidaria"          
  #[5] "Rhodophyta"         "Discosea"           "Chlorophyta"        "Onychophora"       
  #[9] "Chordata"           "Hemichordata"       "unk_phylum"         "Rotifera"          
  #[13] "Annelida"           "Mollusca"           "Oomycota"           "Nemertea"          
  #[17] "Streptophyta"       "Nematoda"           "Basidiomycota"      "Blastocladiomycota"
  #[21] "Evosea"             "Zoopagomycota"      "Mucoromycota"       "Gastrotricha"      
  #[25] "Ascomycota"         "Tardigrada"         "Tubulinea"          "Chytridiomycota"   
  #[29] "Haptophyta"         "Echinodermata"      "Platyhelminthes"    "Bryozoa"           
  #[33] "Endomyxa"           "Cercozoa" 

# In both datasets several of these are phyto plankton which is 



# checking file lengths
print(nrow(new_insect_data))
print(nrow(only_insecta_data))
print(nrow(crust_data))
print(nrow(rogue_insects))
print(nrow(only_crustacea_class_data))
print(nrow(rogue_crust))
print(nrow(new_crust_data))






