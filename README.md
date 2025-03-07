# Lipid_Nomenclature_Conversion

# Function Usage 
Copy and paste the following into your R script 
#Load necessary library
library(dplyr)

#Source the function GitHub repository
source_url <- "https://raw.githubusercontent.com/Hszemray/Lipid_Nomenclature_Conversion/main/Nomenclature_UpdateR.R"
source(source_url)

#Load your data
Data <- read.csv("path_to_your_data.csv")

#Call the function
Updated_Data <- Nomenclature_update(Data)

#Save the updated data if needed
write.csv(Updated_Data, "path_to_save_updated_data.csv", row.names = FALSE)
