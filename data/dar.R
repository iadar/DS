
# Installing packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages('openintro')
install.packages("knitr")
install.packages("rsample")
install.packages("broom")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gridExtra")

# Importing libraries
library(tidyverse)
library(openintro)
library(readr)
library(dplyr)
library(knitr)
library(ggplot2)

#PRE-Proccesing Data

# Importing CSV-file
df <- read.csv("E:/foto/diabetes_prediction_dataset.csv")

# Show Data
head(df)
glimpse(df)

variable_types <- sapply(df, class)
print(variable_types)

# Вывести типы данных в виде красивой таблицы
kable(as.data.frame(sapply(df, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'diabetes'")


# Changing Data Types from character to factor
df <- data.frame(lapply(df, function(x) {
  if (is.character(x)) {
    as.factor(x)
  } else {
    x
  }
}))

# Вывести типы данных в виде красивой таблицы
kable(as.data.frame(sapply(df, class)), 
      col.names = c("Column Name", "Data Type"), 
      caption = "Data Types in 'diabates'")

# Checking for missing data
sum(is.na(df))


# Подсчет количества нулевых (пропущенных) значений в каждом столбце
null_counts <- df %>%
  summarise_all(~ sum(is.na(.)))

# Преобразование результатов в удобный формат
null_counts_df <- as.data.frame(t(null_counts))

print(null_counts_df)

# Determine the Target Variable
outcome_counts <- df %>%
  group_by(diabetes) %>%
  summarise(count = n())

outcome_counts

unique_values <- unique(df$ColumnName)
count_unique <- length(unique_values)

summary(df)

# Assuming 'train' is your dataset
unique_counts <- sapply(df, function(x) length(unique(x)))
print(unique_counts)

# Assuming 'data' is your dataframe
outcome_counts <- df %>%
  group_by(diabetes) %>%
  summarise(count = n())

df$diabetes <- as.factor(df$diabetes)
glimpse(df)

# Create the pie chart with percentage labels
ggplot(outcome_counts, aes(x = "", y = count, fill = factor(diabetes))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#78C850", "#4E8234")) +
  labs(title = "Distribution of Diabetes Cases",
       fill = "Diabetics Status") +
  theme_void() +
  geom_text(aes(label = scales::percent(count / sum(count)), y = count), position = position_stack(vjust = 0.5), color = "white")




install.packages('corrplot')
library (corrplot)

corrplot(cor(df))

# Checking the levels of all categorical columns
gender_levels <- levels(df$gender)
print(gender_levels)

unique_values <- unique(df$gender)

# Выводим уникальные значения
print(count(unique_values))



# Using filter() to filter out all rows of comics with level that have very low counts
df <- df %>%
  filter(gender != "Other")

# Create a side-by-side bar chart with 'sex' on the x-axis and 'alignment' as fill
ggplot(data = df, aes(x = gender, fill = diabetes)) + 
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("#78C850", "#4E8234")) +
  labs(title = "Side-by-Side Bar Chart of Gender and Diabetes",
       x = "Gender",
       y = "Count")

glimpse(df)

ggplot(data = df, aes(x = smoking_history, fill = diabetes)) + 
  geom_bar(position = "dodge", stat = "count") +
  scale_fill_manual(values = c("#78C850", "#4E8234", "#5A8F29", "#6D935C")) +
  labs(title = "Side-by-Side Bar Chart of Smoking history and Diabetes",
       x = "smoking_history",
       y = "Count")


ggplot(df, aes(x = age, fill = diabetes)) +
  geom_histogram(position = "stack", alpha = 0.7, bins = 30,  color = "white") +
  scale_fill_manual(values = c("#78C850", "#4E8234")) +
  labs(title = "Age Feature Distribution") +
  theme_minimal() +
  xlim(0, 80)
  

# Создание признака 'Age_grp'
age_bins <- c(0, 12, 19, 35, 60,80, Inf)
age_labels <- c('Child', 'Teenager', 'Young Adult', 'Middle-Aged', 'Senior','Pensioner')
df$Age_grp <- cut(df$age, breaks = age_bins, labels = age_labels, right = FALSE)

glimpse(df)


ggplot(df, aes(x = Age_grp, fill = diabetes)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Diabetes vs. Age group", x = "Age group", y = "Count") +
  scale_fill_manual(values = c("#78C850", "#4E8234"))

theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Выбор только числовых переменных из набора данных
numeric_diabetes <- diabetes[, sapply(diabetes, is.numeric)]

# Создание тепловой карты корреляции с заданными цветами
corr_matrix <- cor(numeric_diabetes)
# Создание палитры оттенков зеленого
green_palette <- colorRampPalette(c("#BCDEA9", "#4E8234"))(100)
var_names <- colnames(numeric_diabetes)

# Создание тепловой карты с настройкой цвета
corrplot(corr_matrix, method = "color", tl.col = "white", tl.srt = 45, addCoef.col = "white", col = green_palette, diag.col = "black", colnames = var_names)


# Определение границ для категорий
level_bins <- c(-Inf, 5.7, 6.5, Inf)

# Определение меток для категорий
level_labels <- c('Normal', 'Pre-Diabetic', 'Diabetic')

# Создание нового столбца HbA1c_level
df$HbA1c_level_group <- cut(df$HbA1c_level, breaks = level_bins, labels = level_labels)

# Построение гистограммы с использованием ggplot2
ggplot(df, aes(x = HbA1c_level, fill = diabetes)) +
  geom_bar(position = "dodge", color = "white") +
  labs(title = "Diabetes vs. HbA1c_level_group", x = "HbA1c level group", y = "Count") +
  scale_fill_manual(values = c("#78C850", "#4E8234")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))



# Создание нового столбца weight_type
df$weight_type <- cut(df$bmi, 
                      breaks = c(-Inf, 18.5, 24.9, 29.9, Inf), 
                      labels = c('underweight', 'normal', 'overweight', 'obesity'))

# Разбиение графиков по типам веса
par(mfrow=c(2, 2))  # Устанавливаем 2 строки и 2 столбца для 4 графиков

# Подготовка данных для каждого типа веса
underweight_data <- table(df[df$weight_type == 'underweight', 'diabetes'])
normal_data <- table(df[df$weight_type == 'normal', 'diabetes'])
overweight_data <- table(df[df$weight_type == 'overweight', 'diabetes'])
obesity_data <- table(df[df$weight_type == 'obesity', 'diabetes'])

# Построение круговых диаграмм
pie(underweight_data, labels = c('no diabetes', 'diabetes'), 
    main = 'Underweight', col = c('#78C850', '#4E8234'), startangle = 45)
legend('topright', legend = c('no diabetes', 'diabetes'), fill = c('#78C850', '#4E8234'))

pie(normal_data, labels = c('no diabetes', 'diabetes'), 
    main = 'Normal', col = c('#78C850', '#4E8234'), startangle = 45)
legend('topright', legend = c('no diabetes', 'diabetes'), fill = c('#78C850', '#4E8234'))

pie(overweight_data, labels = c('no diabetes', 'diabetes'), 
    main = 'Overweight', col = c('#78C850', '#4E8234'), startangle = 45)
legend('topright', legend = c('no diabetes', 'diabetes'), fill = c('#78C850', '#4E8234'))

pie(obesity_data, labels = c('no diabetes', 'diabetes'), 
    main = 'Obesity', col = c('#78C850', '#4E8234'), startangle = 45)
legend('topright', legend = c('no diabetes', 'diabetes'), fill = c('#78C850', '#4E8234'))


num_var <- table(df$hypertension)
count_dict <- as.data.frame(num_var)



ggplot(count_dict, aes(x = "", y = Freq, fill = as.factor(Var1))) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", (Freq/sum(Freq))*100)),
            position = position_stack(vjust = 0.5)) +
  coord_polar("y", start = 0) +
  labs(title = "Percentage of patients with hypertension",
       fill = "Hypertension",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c('#78C850', '#4E8234'), name = "Hypertension") +
  theme_minimal() +
  theme(legend.position = c(0.85, 0.85))  # Расположение легенды



# Создаем копию датафрейма для избежания изменения оригинала
df_encoded <- df

# Применяем label encoding к категориальным столбцам
df_encoded <- lapply(df_encoded, function(x) {
  if (is.factor(x)) {
    levels <- levels(x)
    as.numeric(factor(x, levels = levels))
  } else {
    x
  }
})

# Преобразуем обратно в датафрейм
df_encoded <- as.data.frame(df_encoded)

# Теперь df_encoded содержит кодированные метки для категориальных столбцов

# Строим матрицу корреляции
cor_matrix <- cor(df_encoded)
print(cor_matrix)
# Загружаем необходимую библиотеку
library(ggplot2)

# Построение тепловой карты с использованием ggplot2
ggplot(data = as.data.frame(as.table(cor_matrix)), aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "#4E8234") +
  geom_text(aes(label = sprintf("%.1f", Freq)), vjust = 1) +
  labs(title = "Correlation Heatmap",
       x = "Variables",
       y = "Variables") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

glimpse(df)


gender_levels <- levels(df$gender)
print(gender_levels)
# Using droplevels() to drop the unused levels from the dataframe
df$gender <- droplevels(df$gender)
gender_levels <- levels(df$gender)
print(gender_levels)

smoking_history_levels <- levels(df$smoking_history)
print(smoking_history_levels)

# Пример графика с ограниченным диапазоном по оси x
ggplot(df, aes(x = bmi)) +
  geom_density(fill = "#78C850", alpha = 0.7) +
  labs(title = "Kernel Density Estimate of BMI", x = "Value") +
  xlim(25, 35)  


df$diabetes <- as.factor(df$diabetes)
glimpse(diabetes)

# Пример двух графиков ящика с усами
ggplot(df, aes(x = diabetes, y = bmi, fill = factor(diabetes))) +
  geom_boxplot() +
  labs(title = "Box Plot of BMI by Diabetes Status", x = "Diabetes", y = "BMI") +
  scale_fill_manual(values = c("#78C850", "#5A8F29")) 



glimpse(df)

# Преобразование df$diabetes в числовую переменную
df$diabetes <- as.numeric(df$diabetes)

# Проверка изменений
glimpse(df)

# Преобразование бинарной переменной в фактор
df$diabetes <- as.factor(df$diabetes)

# Пример графика рассеяния с использованием цвета для бинарной переменной
ggplot(df, aes(x = bmi, y = age, color = diabetes)) +
  geom_point() +
  labs(title = "Scatter Plot of Numeric and Binary Variables", 
       x = "bmi", y = "age")


ggplot(df, aes(x = bmi, fill = age)) +
  geom_histogram(position = "stack", alpha = 0.7, bins = 30,  color = "white") +
  scale_fill_manual(values = c("#78C850", "#5A8F29")) +
  labs(title = "Age Feature Distribution") +
  theme_minimal() +
  xlim(10, 50)

# Пример диаграммы рассеяния
plot(df$Age, df$diabetes, 
     main = "Scatter Plot of Numeric Features",
     xlab = "Age", ylab = 'diabetes')

