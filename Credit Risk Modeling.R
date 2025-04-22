# Load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)
library(tidyr)
library(purrr)  # for walk()

# 1. Load and clean data --------------------------------------------------
data <- read.csv("D:/BCA 2 SEM/R PROGRAMMING/RAKESH G 2401201064 R programming Project/loan_data_set.csv")

# Check column names
names(data)

# Rename Loan_Status to default for simplicity
data <- data %>%
  rename(default = Loan_Status)

# Check missing values
cat("\nMissing Values Before Cleaning:\n")
print(colSums(is.na(data)))

# Clean data
data_clean <- data %>%
  na.omit() %>%
  mutate(default = as.factor(default))  # Convert target to factor

cat("\nMissing Values After Cleaning:\n")
print(colSums(is.na(data_clean)))

# 2. EDA ------------------------------------------------------------------
# Select numeric variables
numeric_vars <- data_clean %>%
  select(where(is.numeric)) %>%
  names()

# Plot histograms for numeric features
walk(numeric_vars, ~{
  print(
    ggplot(data_clean, aes(x = !!sym(.x), fill = default)) +
      geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
      labs(title = paste("Distribution of", .x),
           x = .x, fill = "Loan Status") +
      theme_minimal()
  )
})

# Example bar plot for categorical feature (Education)
ggplot(data_clean, aes(x = Education, fill = default)) +
  geom_bar(position = "fill") +
  labs(title = "Loan Status by Education",
       y = "Proportion", x = "Education") +
  theme_minimal()

# 3. Model Building -------------------------------------------------------
# Split data
set.seed(123)
train_index <- createDataPartition(data_clean$default, p = 0.7, list = FALSE)
train <- data_clean[train_index, ]
test <- data_clean[-train_index, ]

# Logistic regression (excluding Loan_ID and character vars)
model <- glm(default ~ ., 
             data = train %>% select(-Loan_ID), 
             family = binomial)

cat("\nModel Summary:\n")
print(summary(model))

# 4. Evaluation -----------------------------------------------------------
# Predict probabilities
probs <- predict(model, newdata = test, type = "response")
pred_class <- ifelse(probs > 0.5, "Y", "N") %>% as.factor()

# Confusion matrix
cat("\nConfusion Matrix:\n")
cm <- confusionMatrix(pred_class, test$default, positive = "Y")
print(cm)

# ROC curve
roc_obj <- roc(response = test$default, predictor = probs, levels = c("N", "Y"))
ggroc(roc_obj, legacy.axes = TRUE) +
  geom_abline(linetype = "dashed") +
  labs(title = paste("ROC Curve (AUC =", round(auc(roc_obj), 3), ")"),
       x = "False Positive Rate", y = "True Positive Rate") +
  theme_minimal()

# 5. Interpretation -------------------------------------------------------
# Odds Ratios
cat("\nOdds Ratios:\n")
odds_ratios <- exp(coef(model))
print(data.frame(
  Predictor = names(odds_ratios),
  Odds_Ratio = round(odds_ratios, 3)
))
