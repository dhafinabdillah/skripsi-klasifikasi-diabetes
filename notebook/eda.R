# Library
library(ggplot2) # Membuat chart
library(RColorBrewer) # Memberi warna pada chart
library(dplyr)

# Load Dataset
Diabetes <- read.csv("D:/Dhafin's/Kuliah/Skripsi/R/Diabetes_cleaned.csv")

# EDA
colors <- brewer.pal(2, "Set2")

ggplot(Diabetes, aes(x = Outcome, fill = Outcome)) +
  geom_bar(position = "dodge", width = 0.5, color = "black") +
  scale_fill_manual(
    values = brewer.pal(2, "Set2"),
    labels = c("1" = "Negatif Diabetes", "2" = "Positif Diabetes")
  ) +
  labs(
    x = "Kondisi Diabetes", 
    y = "Jumlah Penduduk", 
    fill = "Kondisi",
    title = "Distribusi Kondisi Diabetes Berdasarkan Hasil Pemeriksaan"
  ) +
  geom_text(
    stat = "count", 
    aes(label = ..count..), 
    position = position_dodge(width = 0.5), 
    vjust = -0.5, 
    color = "black", size = 3
  ) +
  theme_minimal() # Histogram Variabel Respon

descriptive_stats <- Diabetes %>%
  group_by(Outcome) %>%                     
  summarise(across(everything(),            
                   list(mean = ~mean(.),    
                        sd = ~sd(.)),       
                   .names = "{col}_{fn}"))

View(descriptive_stats) # Statistika deskriptif variabel penjelas