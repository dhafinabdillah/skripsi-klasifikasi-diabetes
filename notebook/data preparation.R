# Library
library(dplyr) # cleaning data

# Load dataset
Diabetes <- read.csv("D:/Dhafin's/Kuliah/Skripsi/R/diabetes.csv")

# Data cleaning
Diabetes$Outcome[Diabetes$Outcome == "1"] <- "2"
Diabetes$Outcome[Diabetes$Outcome == "0"] <- "1" # feature engineering

str(Diabetes) # check if there's wrong type

Diabetes <- Diabetes %>%
  filter(
    !if_any(-Pregnancies, ~ . == 0 | is.na(.))
  )
rownames(Diabetes) <- NULL # cleaning missing data

Diabetes <- Diabetes %>% distinct() # cleaning duplicate data

write.csv(Diabetes, "D:/Dhafin's/Kuliah/Skripsi/R/Diabetes_cleaned.csv", row.names = FALSE)
