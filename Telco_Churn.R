# =====================================================================
# TELECOM CUSTOMER CHURN ANALYSIS
# =====================================================================

# 0. Setup and Packages
required_packages <- c("tidyverse", "ggplot2", "caret", "rpart", "rpart.plot", "pROC", "cluster", "factoextra")
for(pkg in required_packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg, dependencies = TRUE)
  }
}
library(tidyverse)
library(ggplot2)
library(caret)
library(rpart)
library(pROC)
library(cluster)
library(factoextra)

# =====================================================================
# OBJECTIVE 1: Data Cleaning and Exploratory Data Analysis (EDA)
# =====================================================================
cat("\n--- Running Objective 1: Cleaning & EDA ---\n")

df <- read.csv("/Users/gauravsarwe/Downloads/Telco-Customer-Churn.csv", stringsAsFactors = FALSE)

# Clean TotalCharges and convert categorical variables
df$TotalCharges <- as.numeric(df$TotalCharges)
df <- df %>% drop_na(TotalCharges)
df$customerID <- NULL
df <- df %>% mutate(across(where(is.character), as.factor))
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# Overall churn rate
overall_churn <- mean(df$Churn == "Yes") * 100
cat("Overall Churn Rate:", round(overall_churn, 2), "%\n")

# Churn by Contract (Printed directly)
contract_churn <- df %>% group_by(Contract) %>% summarise(Churn_Rate = mean(Churn == "Yes") * 100)
p1 <- ggplot(contract_churn, aes(x = Contract, y = Churn_Rate, fill = Contract)) +
  geom_bar(stat = "identity", width = 0.6) + theme_minimal() +
  labs(title = "Churn Rate by Contract Type", y = "Churn Rate (%)", x = "Contract") + theme(legend.position = "none")
print(p1)

# Churn by Internet Service (Printed directly)
internet_churn <- df %>% group_by(InternetService) %>% summarise(Churn_Rate = mean(Churn == "Yes") * 100)
p2 <- ggplot(internet_churn, aes(x = InternetService, y = Churn_Rate, fill = InternetService)) +
  geom_bar(stat = "identity", width = 0.6) + theme_minimal() +
  labs(title = "Churn Rate by Internet Service", y = "Churn Rate (%)", x = "Internet Service") + theme(legend.position = "none")
print(p2)

# Churn by Tenure Group (Printed directly)
df <- df %>%
  mutate(tenure_group = case_when(
    tenure <= 12 ~ "0-12 months",
    tenure <= 24 ~ "13-24 months",
    tenure <= 48 ~ "25-48 months",
    TRUE ~ "49+ months"
  ))
tenure_churn <- df %>% group_by(tenure_group) %>% summarise(Churn_Rate = mean(Churn == "Yes") * 100)
p3 <- ggplot(tenure_churn, aes(x = tenure_group, y = Churn_Rate, fill = tenure_group)) +
  geom_bar(stat = "identity", width = 0.6) + theme_minimal() +
  labs(title = "Churn Rate by Tenure Group", y = "Churn Rate (%)", x = "Tenure Group") + theme(legend.position = "none")
print(p3)


# =====================================================================
# OBJECTIVE 2: Statistical Tests
# =====================================================================
cat("\n--- Running Objective 2: Statistical Tests ---\n")

getCramersV <- function(tbl) {
  chi2 <- chisq.test(tbl)$statistic
  n <- sum(tbl)
  k <- min(nrow(tbl), ncol(tbl))
  v <- sqrt(chi2 / (n * (k - 1)))
  return(as.numeric(v))
}

# Chi-Square: Contract
tbl_contract <- table(df$Churn, df$Contract)
cat("Chi-Square Test (Contract vs Churn):\n")
print(chisq.test(tbl_contract))
cat("Cramer's V:", round(getCramersV(tbl_contract), 3), "\n\n")

# Chi-Square: PaymentMethod
tbl_payment <- table(df$Churn, df$PaymentMethod)
cat("Chi-Square Test (PaymentMethod vs Churn):\n")
print(chisq.test(tbl_payment))
cat("Cramer's V:", round(getCramersV(tbl_payment), 3), "\n\n")

# Chi-Square: InternetService
tbl_internet <- table(df$Churn, df$InternetService)
cat("Chi-Square Test (InternetService vs Churn):\n")
print(chisq.test(tbl_internet))
cat("Cramer's V:", round(getCramersV(tbl_internet), 3), "\n\n")

# Wilcoxon Test: MonthlyCharges
cat("Wilcoxon Test (MonthlyCharges vs Churn):\n")
print(wilcox.test(MonthlyCharges ~ Churn, data = df))

# Wilcoxon Test: tenure
cat("\nWilcoxon Test (Tenure vs Churn):\n")
print(wilcox.test(tenure ~ Churn, data = df))


# =====================================================================
# OBJECTIVE 3: Churn Prediction Model
# =====================================================================
cat("\n--- Running Objective 3: Churn Models ---\n")

df_model <- df %>% select(-tenure_group)
df_model$Churn <- factor(df_model$Churn, levels = c("No", "Yes"))

set.seed(42)
trainIndex <- createDataPartition(df_model$Churn, p = 0.7, list = FALSE)
trainData <- df_model[trainIndex, ]
testData  <- df_model[-trainIndex, ]

# Logistic Regression
log_model <- glm(Churn ~ ., data = trainData, family = binomial)
log_prob <- predict(log_model, newdata = testData, type = "response")
log_pred <- factor(ifelse(log_prob > 0.5, "Yes", "No"), levels = c("No", "Yes"))

cat("--- Logistic Regression Confusion Matrix ---\n")
print(confusionMatrix(log_pred, testData$Churn, positive = "Yes"))

# Decision Tree
tree_model <- rpart(Churn ~ ., data = trainData, method = "class")
tree_pred <- predict(tree_model, newdata = testData, type = "class")

cat("\n--- Decision Tree Confusion Matrix ---\n")
print(confusionMatrix(tree_pred, testData$Churn, positive = "Yes"))

# ROC Curve & AUC (Rendered directly)
roc_obj <- roc(as.numeric(testData$Churn), log_prob)
cat("Logistic Regression AUC:", round(auc(roc_obj), 3), "\n")
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)

# Top Odds Ratios
odds_ratios <- exp(coef(log_model))
cat("\n--- Top Factors Influencing Churn (Odds Ratios) ---\n")
print(head(sort(odds_ratios, decreasing = TRUE), 6))


# =====================================================================
# OBJECTIVE 4: Customer Segmentation (K-Means Clustering)
# =====================================================================
cat("\n--- Running Objective 4: Customer Segmentation ---\n")

cluster_data <- df %>% select(tenure, MonthlyCharges, TotalCharges)
scaled_data <- scale(cluster_data)

# Elbow Plot (Rendered directly)
print(fviz_nbclust(scaled_data, kmeans, method = "wss") + labs(title = "Elbow Plot for Optimal k"))

set.seed(42)
km_result <- kmeans(scaled_data, centers = 3, nstart = 25)
df$Cluster <- as.factor(km_result$cluster)

cluster_profile <- df %>%
  group_by(Cluster) %>%
  summarise(
    Count = n(),
    Avg_Tenure = mean(tenure),
    Avg_Monthly_Charges = mean(MonthlyCharges),
    Avg_Total_Charges = mean(TotalCharges),
    Churn_Rate = mean(Churn == "Yes") * 100
  )

cat("--- Cluster Profiles ---\n")
print(cluster_profile)

# Cluster Plot (Rendered directly)
print(fviz_cluster(km_result, data = scaled_data, geom = "point", ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal()) +
        labs(title = "Customer Segmentation Clusters"))

