# Telecom Customer Churn Analysis 📊📞

A comprehensive end-to-end business analytics project built in **R** to analyze customer attrition, identify churn drivers, build predictive machine learning models, and segment high-risk customers for telecommunication retention campaigns.

---

## 📌 Business Problem
Customer acquisition in the telecom industry costs 5 to 7 times more than retaining existing subscribers. High churn rates directly erode recurring revenue streams. This project analyzes **7,032 customer accounts** to answer key strategic questions:
- What behavioral and demographic factors drive customer churn?
- Can we accurately predict which customers are at risk of leaving?
- How can we segment customers to design targeted retention strategies?

---

## 🛠️ Tech Stack & Tools
- **Language:** R
- **Core Libraries:** `tidyverse`, `ggplot2`, `caret`, `rpart`, `pROC`, `cluster`, `factoextra`
- **Methodologies:** Exploratory Data Analysis (EDA), Chi-Square Tests ($\chi^2$), Cramer's V, Wilcoxon Rank-Sum Tests, Logistic Regression, Decision Trees, K-Means Clustering.

---

## 📁 Repository Structure

```text
telecom-churn-analysis/
├── data/
│   └── telco_churn.csv      # IBM Telco Customer Churn dataset (Kaggle)
├── scripts/
│   ├── 01_cleaning_and_eda.R         # Data cleaning & initial EDA plots
│   ├── 02_statistical_tests.R        # Chi-Square, Cramer's V, Wilcoxon tests
│   ├── 03_churn_model.R             # Logistic Regression & Decision Trees
│   └── 04_customer_segmentation.R    # K-Means clustering & profiling
├── outputs/                         # Saved visual charts & ROC curves
├── Telecom_Customer_Churn_Report.docx # Comprehensive business report
├── requirements.txt                 # R package dependencies
└── README.md                        # Project documentation