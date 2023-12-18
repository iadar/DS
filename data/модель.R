install.packages("tidyverse")
install.packages("rsample")
install.packages('openintro')
install.packages("knitr")
install.packages("rsample")
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")



# Importing libraries
library(tidyverse)
library(rsample)
library(openintro)
library(readr)
library(broom)
library(dplyr)
library(knitr)
library(ggplot2)
library(gridExtra)


# Importing CSV-file
diabetes <- read.csv("E:/foto/diabetes_prediction_dataset.csv")

df2 <- data.frame(diabetes)
glimpse(df2)


dim(df2)

# Peek at response variable
head(df2$diabetes)


# Create training (70%) and test (30%) sets
df_split <- initial_split(df2, prop = .7, strata = 'diabetes')
df_train <- training(df_split)
df_test <- testing(df_split)


nrow(df_train)
nrow(df_test)


# Multiple logistic regression
model3 <- glm(
  diabetes ~ gender + age + hypertension + bmi + smoking_history + HbA1c_level + blood_glucose_level, family = 'binomial',
  data = df_train
)
tidy(model3)

# Создать Confusion Matrix
conf_matrix <- table(predicted_classes, df_train$diabetes)

# Преобразовать Confusion Matrix в датафрейм
conf_matrix_df <- as.data.frame.matrix(conf_matrix)
conf_matrix_df <- cbind(Actual = rownames(conf_matrix_df), conf_matrix_df)

# Преобразовать широкий формат в длинный формат для ggplot2
conf_matrix_long <- reshape2::melt(conf_matrix_df, id.vars = "Actual")

# Создать новую переменную для идентификации ячеек
conf_matrix_long$CellType <- ifelse(conf_matrix_long$Actual == "0" & conf_matrix_long$variable == "0", "TN",
                                    ifelse(conf_matrix_long$Actual == "1" & conf_matrix_long$variable == "1", "TP",
                                           ifelse(conf_matrix_long$Actual == "1" & conf_matrix_long$variable == "0", "FP", "FN")))

# Визуализировать Confusion Matrix с ggplot2 без названий осей и увеличенным шрифтом
ggplot(data = conf_matrix_long, aes(x = Actual, y = variable, fill = CellType)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), vjust = 1, size = 8) +  # Увеличьте размер шрифта по вашему усмотрению
  scale_fill_manual(values = c("TN" = "#78C850", "TP" = "#78C850", "FP" = "#5A8F29", "FN" = "#5A8F29"), guide = "none") +
  labs(title = "Confusion Matrix") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        text = element_text(size = 25))  # Увеличьте размер шрифта по вашему усмотрению


# Вычислить Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Вычислить Precision и Recall
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])  # True Positives / (True Positives + False Positives)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])     # True Positives / (True Positives + False Negatives)

cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")

# Вычислить F1-меру
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("F1 Score:", f1_score, "\n")




