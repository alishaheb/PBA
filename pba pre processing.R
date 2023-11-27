library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(caret)
# exploratory data analysis
df=merged_data1
#first 5 rows
print(head(df,n=5))
#shape of the data
print(dim(df))
#checking for null values
print(any(is.na(df)))
#summary of the data
print(summary(df))
View(summary(df))

#some rows in the patient_smoker collumn have spellt yes as 'yess', therefore it is important to replace these:
df <- df %>%
  mutate(Patient_Smoker = ifelse(Patient_Smoker == "YESS", "YES", Patient_Smoker))


#Exploring distributions

#Distribution of survival (0 = not survived after on year, 1 = survived after one year)
ggplot(df, aes(x = factor(Survived_1_year))) +
  geom_bar() +
  labs(title = "Distribution of Survival after one year",
       x = "Survived_1_year",
       y = "Count")

#Distribution of age
ggplot(df, aes(x = Patient_Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Patient Age",
       x = "Age",
       y = "Count")


# distribution of patients based on smoking status
ggplot(df, aes(x = Patient_Smoker, fill = factor(Survived_1_year))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Patients based on Smoking Status and Survival",
       x = "Smoker",
       y = "Count",
       fill = "Survived_1_year")


# distribution of patients based on Rural/Urban status
ggplot(df, aes(x = Patient_Rural_Urban, fill = factor(Survived_1_year))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Patients based on Rural/Urban Status and Survival",
       x = "Rural/Urban",
       y = "Count",
       fill = "Survived_1_year")

#distribution of the number of previous conditions
ggplot(df, aes(x = Number_of_prev_cond, fill = factor(Survived_1_year))) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Number of Previous Conditions and Survival",
       x = "Number of Previous Conditions",
       y = "Count",
       fill = "Survived_1_year")



#  box plot for the relationship between BMI and survival
ggplot(df, aes(x = factor(Survived_1_year), y = Patient_Body_Mass_Index, fill = factor(Survived_1_year))) +
  geom_boxplot() +
  labs(title = "Relationship between BMI and Survival",
       x = "Survival Status",
       y = "Body Mass Index (BMI)",
       fill = "Survived_1_year") +
  theme_minimal()



# Display the correlation matrix for numeric columns
numeric_data <- df[, sapply(df, is.numeric)]
correlation_matrix <- cor(numeric_data)
print(correlation_matrix)

#splitting the data

# Set the seed for reproducibility
set.seed(123)

-# Create an index for splitting the data, using a 70-30 split
index <- createDataPartition(df$Survived_1_year, p = 0.7, list = FALSE)

# Create training and testing sets
train_data <- df[index, ]
test_data <- df[-index, ]

# Assuming 'train_data' and 'test_data' are your training and testing datasets
write.csv(train_data, file = "train_data.csv", row.names = FALSE)
write.csv(test_data, file = "test_data.csv", row.names = FALSE)



