library(dplyr)

# Function to update nomenclature of lipid species
Nomenclature_update <- function(Data) {
  
  # Load Nomenclature Key
  Nomenclature_key <- read.csv("https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/lipid_nomenclature_conversion_v2.csv") # CSV containing old names and new names
  
  # Create named vector for nomenclature mapping
  nomenclature_map <- setNames(Nomenclature_key$updated_precursor_name, Nomenclature_key$precursor_name)
  
  # Ensure formatting of species is correct
  Target_Columns <- c("CE", "CER", "MAG", "DAG", "FFA", "LCER", "HCER", "LPC", "LPE", "LPI", "LPG", "LPS", "PE", "PC", "PS", "PG", "PI", "SM", "TAG")
  
  colnames(Data) <- sapply(colnames(Data), function(Formatting) {
    if (any(sapply(Target_Columns, function(target) startsWith(Formatting, target)))) {
      Formatting <- gsub(".", ":", sub(".$", ")", sub(".", "(", Formatting, fixed = TRUE)), fixed = TRUE)
    }
    return(Formatting)
  })
  
  # Update Nomenclature
  colnames(Data) <- sapply(colnames(Data), function(nomenclature) {
    if (nomenclature %in% names(nomenclature_map)) {
      return(nomenclature_map[nomenclature]) # replaces old nomenclature with updated nomenclature
    } else {
      return(nomenclature) # keeps original nomenclature if no match is found in nomenclature key
    }
  })
  return(Data)
}
