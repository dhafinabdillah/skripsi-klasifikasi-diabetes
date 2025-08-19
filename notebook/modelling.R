# Library
library(dplyr) # Membuat data frame
library(MASS) # Membuat aturan klasifikasi
library(rrcov) # Memuat MCD estimator

# Import Dataset
Diabetes <- read.csv("D:/Dhafin's/Kuliah/Skripsi/R/Diabetes_cleaned.csv")

# Modelling
group1 <- Diabetes[Diabetes$Outcome == "1", c("Pregnancies", "Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age")]
group2 <- Diabetes[Diabetes$Outcome == "2", c("Pregnancies", "Glucose","BloodPressure","SkinThickness","Insulin","BMI","DiabetesPedigreeFunction","Age")]

mean_group1 <- colMeans(group1)
mean_group2 <- colMeans(group2) # Vektor rata-rata sampel

cov_group1 <- cov(group1)
cov_group2 <- cov(group2) # Matriks kovarian sampel

# Analisis Diskriminan Kuadratik
det_Sigma1 <- det(cov_group1)
det_Sigma2 <- det(cov_group2) # Determinan matriks kovarian

inv_Sigma1 <- solve(cov_group1) 
inv_Sigma2 <- solve(cov_group2) # Inverse matriks kovarian

term1 <- 0.5 * log(det_Sigma1 / det_Sigma2)
term2 <- 0.5 * (t(mean_group1) %*% inv_Sigma1 %*% mean_group1 - t(mean_group2) %*% inv_Sigma2 %*% mean_group2)
k <- term1 + term2

compute_score <- function(row) {
  lhs <- -0.5 * t(row) %*% (inv_Sigma1 - inv_Sigma2) %*% row +
    (t(mean_group1) %*% inv_Sigma1 - t(mean_group2) %*% inv_Sigma2) %*% row - 
    k
  return(lhs)
} # Aturan klasifikasi analisis diskriminan kuadratik

data_matrix <- as.matrix(Diabetes[1:8])
scores <- apply(data_matrix, 1, compute_score)

predicted_classes <- ifelse(scores >= 0, "1", "2")

adk <- data.frame(
  score = as.numeric(scores),
  decision_boundary = 0,
  predicted_class = predicted_classes,
  actual_class = Diabetes$Outcome
)

head(adk)

# Recall
confusion_matrix <- table(Predicted = adk$predicted_class, Actual = adk$actual_class)
print(confusion_matrix) # Confusion matrix

FN <- confusion_matrix["1", "2"] 
TP <- confusion_matrix["2", "2"] 

Recall_adk <- (TP / (TP + FN))* 100
print(paste("Recall:", Recall_adk,"%")) # Recall

# Analisis diskriminan kuadratik robust
mcd1 <- CovMcd(group1, alpha = 0.5171756, nsamp = 500)
mcd2 <- CovMcd(group2, alpha = 0.5346154, nsamp = 500) # alpha = h1/n

mean_mcd_group1 <- mcd1@center
mean_mcd_group2 <- mcd2@center

cov_mcd_group1 <- mcd1@cov
cov_mcd_group2 <- mcd2@cov

det_mcd_Sigma1 <- det(cov_mcd_group1)
det_mcd_Sigma2 <- det(cov_mcd_group2)

inv_mcd_Sigma1 <- solve(cov_mcd_group1)
inv_mcd_Sigma2 <- solve(cov_mcd_group2)

term1 <- 0.5 * log(det_mcd_Sigma1 / det_mcd_Sigma2)
term2 <- 0.5 * (t(mean_mcd_group1) %*% inv_mcd_Sigma1 %*% mean_mcd_group1 - t(mean_mcd_group2) %*% inv_mcd_Sigma2 %*% mean_mcd_group2)
k <- term1 + term2

compute_score_mcd <- function(row) {
  lhs <- -0.5 * t(row) %*% (inv_mcd_Sigma1 - inv_mcd_Sigma2) %*% row +
    (t(mean_mcd_group1) %*% inv_mcd_Sigma1 - t(mean_mcd_group2) %*% inv_mcd_Sigma2) %*% row -
    k
  return(lhs)
}

scores_mcd <- apply(data_matrix, 1, compute_score_mcd)

predicted_classes_mcd <- ifelse(scores_mcd >= 0, "1", "2")

adkr <- data.frame(
  score = as.numeric(scores_mcd),
  decision_boundary = 0,
  predicted_class = predicted_classes_mcd,
  actual_class = Diabetes$Outcome
)

head(adkr)

# Recall
confusion_matrix_mcd <- table(Predicted = adkr$predicted_class, Actual = adkr$actual_class)
print(confusion_matrix_mcd) # Confusion matrix

FN <- confusion_matrix_mcd["1", "2"] 
TP <- confusion_matrix_mcd["2", "2"] 

Recall_adkr <- (TP / (TP + FN))* 100
print(paste("Recall:", Recall_adkr,"%")) # Recall