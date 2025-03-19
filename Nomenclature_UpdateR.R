library(dplyr)

# Function to update nomenclature of lipid species
Nomenclature_update <- function(Data) {
  
  # Load Nomenclature Key
  Nomenclature_key <- read.csv("https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/lipid_nomenclature_conversion_v2.csv", check.names = FALSE) # CSV containing old names and new names
  
  # Create named vector for nomenclature mapping
  nomenclature_map <- setNames(Nomenclature_key$updated_precursor_name, Nomenclature_key$precursor_name)
  
  ## Ensure formatting of species is correct
  Target_Columns <- c("CE", "CER", "MAG", "DAG", "FFA", "LCER", "HCER", "LPC", "LPE", "LPI", "LPG", "LPS", "PE", "PC", "PS", "PG", "PI", "SM", "TAG", "DCER")
  colnames(Data) <- sapply(colnames(Data), function(Formatting) {
    if (any(sapply(Target_Columns, function(target) startsWith(Formatting, target)))) {
      if (grepl("\\.", Formatting)) {
        Formatting <- gsub("\\.", ":", Formatting)
        if (startsWith(Formatting, "PE:P") || startsWith(Formatting, "PE:O")) {
          Formatting <- sub("^PE:P:", "PE:P-", Formatting)
          Formatting <- sub("^PE:O:", "PE:O-", Formatting)
        }
        Formatting <- sub("\\:$", ")", Formatting)
        Formatting <- sub(":", "(", Formatting, fixed = TRUE)
      }
    }
    return(Formatting)
  })
  
  # Start counters and lists
  updated_count <- 0
  not_updated <- list()
  
  # Update Nomenclature
  colnames(Data) <- sapply(colnames(Data), function(nomenclature) {
    if (nomenclature %in% names(nomenclature_map)) {
      updated_count <<- updated_count + 1
      return(nomenclature_map[nomenclature]) # replaces old nomenclature with updated nomenclature
    } else {
      not_updated <<- c(not_updated, nomenclature)
      return(nomenclature) # keeps original nomenclature if no match is found in nomenclature key
    }
  })
  
  # Print the results
  cat("Number of column names updated:", updated_count, "\n")
  cat("Column names not updated:", paste(not_updated, collapse = ", "), "\n")
  
  return(Data)
}
