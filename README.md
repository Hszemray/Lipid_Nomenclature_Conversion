---

# Lipid_Nomenclature_Conversion

This repository contains a function to update the nomenclature of lipid species in your dataset.

## Usage

To use the function, follow these steps:

1. **Load the necessary library**:
   ```r
   library(dplyr)
   ```

2. **Source the function from the GitHub repository**:
   ```r
   source_url <- "https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/Nomenclature_UpdateR.R"
   source(source_url)
   ```

3. **Load your data**:
   ```r
   Data <- read.csv("path_to_your_data.csv", check.names = FALSE)
   ```

4. **Call the function**:
   ```r
   Updated_Data <- Nomenclature_update(Data)
   ```

5. **Save the updated data if needed**:
   ```r
   write.csv(Updated_Data, "path_to_save_updated_data.csv", row.names = FALSE)
   ```

Replace `"path_to_your_data.csv"` with the path to your data file and `"path_to_save_updated_data.csv"` with the path where you want to save the updated data.

---
