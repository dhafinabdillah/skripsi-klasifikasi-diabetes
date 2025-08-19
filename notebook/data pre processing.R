# Library
library(dplyr) # Membuat data frame di uji normal multivariat
library(biotools) # Menghitung nilai chi di uji kesamaan matriks kovarian
library(mvoutlier) # Mendeteksi outlier

# Load Dataset
Diabetes <- read.csv("D:/Dhafin's/Kuliah/Skripsi/R/Diabetes_cleaned.csv")

# Data pre processing
group1 <- Diabetes[Diabetes$Outcome == "1", c("Pregnancies", "Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age")]
group2 <- Diabetes[Diabetes$Outcome == "2", c("Pregnancies", "Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age")]

mean_group1 <- colMeans(group1)
mean_group2 <- colMeans(group2) # Vektor rata-rata sampel

cov_group1 <- cov(group1)
cov_group2 <- cov(group2) # Matriks kovarian sampel

n1 <- nrow(group1)
n2 <- nrow(group2)

spooled <- ((n1 - 1) * cov_group1 + (n2 - 1) * cov_group2) / (n1 + n2 - 2) # Calculate the pooled covariance matrix

mean_diff <- as.matrix(mean_group1 - mean_group2)

t2 <- (n1 * n2) / (n1 + n2) * t(mean_diff) %*% solve(spooled) %*% mean_diff
t2 # T2 Hotelling

F_critical <- qf(0.95, 8, 383)

T2_critical <- (8 * (392 - 2)) / (392 - 8 - 1) * F_critical
T2_critical

# Uji Asumsi
md_Diabetes<-mahalanobis(Diabetes[1:8],colMeans(Diabetes[1:8]),cov(Diabetes[1:8]))

chikuadrat_Diabetes <- qchisq((nrow(Diabetes[1:8]) - seq_len(nrow(Diabetes[1:8])) + 0.5) / nrow(Diabetes[1:8]), 
                              df = ncol(Diabetes[1:8]))

qqplot(chikuadrat_Diabetes,md_Diabetes,main="Q-Q Plot",ylab="Jarak Kuadrat",xlab="Chi-square")
abline(a=0,b=1) # Plot normal multivariat

normalitas_Diabetes <- data.frame(md_Diabetes)
normalitas_Diabetes$chikuadrat_Diabetes <- chikuadrat_Diabetes
normalitas_Diabetes$results<-ifelse(normalitas_Diabetes$md_Diabetes <= normalitas_Diabetes$chikuadrat_Diabetes, 'True', 
                                    ifelse(normalitas_Diabetes$chikuadrat_Diabetes > normalitas_Diabetes$md_Diabetes, 'No', 'None'))

sum_md_less_than_chi_Diabetes<-length(which(normalitas_Diabetes$results=="True"))
n_Diabetes<-nrow(normalitas_Diabetes)
(sum_md_less_than_chi_Diabetes/n_Diabetes)*100 # Normal multivariat

boxM <- boxM(as.matrix(Diabetes[1:8]), Diabetes$Outcome)
boxM

v<-0.5*ncol(Diabetes[1:8])*(ncol(Diabetes[1:8])+1)*(2-1)
chisq<-qchisq(c(0.05),df=v,lower.tail=FALSE)
chisq # Kesamaan matriks kovarian

# Detecting Outliers
outlier <- dd.plot(Diabetes[, 1:8], quan = 0.5114796)
outlier_detect <- outlier$outliers

Diabetes$outlier <- outlier_detect
mahalanobis_outlier<-outlier$md.cla
robust_distances<-outlier$md.rob

outlierr <- data.frame(mahalanobis_outlier)
outlierr$robust_distances <- robust_distances

Outliers <- subset(Diabetes, outlier == 'TRUE')
No_Outliers <- subset(Diabetes, outlier == 'FALSE')
str(Outliers)
str(No_Outliers)
