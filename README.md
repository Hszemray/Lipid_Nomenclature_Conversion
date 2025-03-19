---

# Lipid_Nomenclature_Conversion

This repository contains a function to update the nomenclature of lipid species in your dataset for the ANPC targeted lipid method

Many thanks to Alanah Grant-St James for providing the nomenclature key. 

## Usage

To use the function, follow these steps

   ```r
   library(dplyr)

   # Source the function from the GitHub repository:
   source_url <- "https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/Nomenclature_UpdateR.R"
   source(source_url)

   # Load your data
   Data <- read.csv("path_to_your_data.csv")

   # Call the function:
   Updated_Data <- Nomenclature_update(Data)

   # Save the updated data if needed:
   #write.csv(Updated_Data, "path_to_save_updated_data.csv", row.names = FALSE)
   ```

Replace `"path_to_your_data.csv"` with the path to your data file and `"path_to_save_updated_data.csv"` with the path where you want to save the updated data.

---
