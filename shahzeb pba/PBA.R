your_data_frame <- read.csv('train_data.csv')

str(your_data_frame)

# Step 1: Handle Missing Values
# Check for missing values
missing_values <- sapply(your_data_frame, function(x) sum(is.na(x)))
print(missing_values)

# Impute missing values with mean for numeric columns
your_data_frame$Patient_Age[is.na(your_data_frame$Patient_Age)] <- mean(your_data_frame$Patient_Age, na.rm = TRUE)
your_data_frame$Patient_Body_Mass_Index[is.na(your_data_frame$Patient_Body_Mass_Index)] <- mean(your_data_frame$Patient_Body_Mass_Index, na.rm = TRUE)

# Step 2: Convert Categorical Variables
# Convert categorical variables to factors
your_data_frame$Patient_Smoker <- as.factor(your_data_frame$Patient_Smoker)
your_data_frame$Patient_Rural_Urban <- as.factor(your_data_frame$Patient_Rural_Urban)

# Step 3: Encode Labels (if 'Survived_1_year' is categorical)


your_data_frame$Survived_1_year <- as.factor(your_data_frame$Survived_1_year)

# Step 4: Feature Scaling
# Standardize numeric features
your_data_frame$Patient_Age <- scale(your_data_frame$Patient_Age)
your_data_frame$Patient_Body_Mass_Index <- scale(your_data_frame$Patient_Body_Mass_Index)

# Step 5: Combine and Engineer Features (if needed)

# Step 6: Remove Unnecessary Columns
# Remove unnecessary columns
your_data_frame <- your_data_frame[, !(names(your_data_frame) %in% c("ID_Patient_Care_Situation", "Diagnosed_Condition", "Patient_ID","Patient_mental_condition"))]

# Print the structure of the preprocessed data frame
str(your_data_frame)

#--------------------------------------------------------------------------------------------------------------------------------

#  Step 1: Encode Categorical Variables using model.matrix
# One-hot encode 'Treated_with_drugs'
your_data_frame_encoded <- cbind(your_data_frame[, -which(names(your_data_frame) == "Treated_with_drugs")],model.matrix(~ Treated_with_drugs - 1, data = your_data_frame))

# Encode labels (if 'Survived_1_year' is categorical)
your_data_frame_encoded$Survived_1_year <- as.factor(your_data_frame_encoded$Survived_1_year)

# Step 2: Scale Numerical Features
# Standardize numerical features
your_data_frame_encoded$Patient_Age <- scale(your_data_frame_encoded$Patient_Age)
your_data_frame_encoded$Patient_Body_Mass_Index <- scale(your_data_frame_encoded$Patient_Body_Mass_Index)

# Step 3: Handle Outliers
# Identify and address outliers using winsorizing
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, c(trim, 1 - trim), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# Apply winsorizing to numerical features
your_data_frame_encoded$Patient_Age <- winsorize(your_data_frame_encoded$Patient_Age)
your_data_frame_encoded$Patient_Body_Mass_Index <- winsorize(your_data_frame_encoded$Patient_Body_Mass_Index)

# Print the structure of the preprocessed data frame
str(your_data_frame_encoded)

#--------------------------------------------------------------------------------------------------------------------------------

install.packages(c("ggplot2", "dplyr"))

# Load libraries
library(ggplot2)
library(dplyr)


your_data_frame$Patient_Age <- as.numeric(your_data_frame$Patient_Age)
your_data_frame$Patient_Body_Mass_Index <- as.numeric(your_data_frame$Patient_Body_Mass_Index)

# Plot histograms
ggplot(your_data_frame, aes(x = Patient_Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram - Patient Age", x = "Patient Age", y = "Frequency")

ggplot(your_data_frame, aes(x = Patient_Body_Mass_Index)) +
  geom_histogram(binwidth = 2, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(title = "Histogram - Body Mass Index", x = "Body Mass Index", y = "Frequency")



# Bar chart for 'Patient_Smoker'
ggplot(your_data_frame, aes(x = Patient_Smoker, fill = Patient_Smoker)) +
  geom_bar() +
  labs(title = "Bar Chart - Patient Smoker", x = "Patient Smoker", y = "Count") +
  scale_fill_manual(values = c("YES" = "orange", "NO" = "blue"))


# Box plot for 'Patient_Age'
ggplot(your_data_frame, aes(x = 1, y = Patient_Age)) +
  geom_boxplot(fill = "lightcoral", color = "black") +
  labs(title = "Box Plot - Patient Age", x = NULL, y = "Patient Age")


ggplot(your_data_frame, aes(x = "", fill = Survived_1_year)) +
  geom_bar(width = 1, color = "white") +
  coord_polar("y") +
  labs(title = "Survival Distribution", fill = "Survived 1 year") +
  theme_void()

your_data_frame$Treated_with_drugs <- factor(your_data_frame$Treated_with_drugs)

# Stacked bar chart for 'Treated_with_drugs' with respect to 'Survived_1_year'
your_data_frame %>%
  ggplot(aes(x = Survived_1_year, fill = Treated_with_drugs)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Treatment and Survival", x = "Survived 1 year", y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Treated with Drugs"))

ggplot(your_data_frame, aes(x = Survived_1_year, y = Patient_Age, fill = Survived_1_year)) +
  geom_boxplot() +
  labs(title = "Box Plot - Patient Age by Survival Status", x = "Survived 1 year", y = "Patient Age") +
  theme_minimal()


install.packages("corrplot")
library(corrplot)

# Select relevant numerical columns for correlation matrix
numerical_columns <- your_data_frame[, c("Patient_Age", "Patient_Body_Mass_Index", "Number_of_prev_cond")]

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_columns)

# Create a heatmap for the correlation matrix
corrplot(correlation_matrix, method = "color", addCoef.col = "black", title = "Correlation Matrix")


ggplot(your_data_frame, aes(x = Survived_1_year, y = Patient_Body_Mass_Index, fill = Survived_1_year)) +
  geom_boxplot() +
  labs(title = "Box Plot - Body Mass Index by Survival Status", x = "Survived 1 year", y = "Body Mass Index") +
  theme_minimal()

your_data_frame$Survived_1_year <- factor(your_data_frame$Survived_1_year)

# Box plot for 'Patient_Body_Mass_Index' with respect to 'Survived_1_year'
ggplot(your_data_frame, aes(x = Survived_1_year, y = Patient_Body_Mass_Index, fill = Survived_1_year)) +
  geom_boxplot() +
  labs(title = "Box Plot - Body Mass Index by Survival Status", x = "Survived 1 year", y = "Body Mass Index") +
  theme_minimal()

install.packages(c("GGally", "viridis"))

# Load libraries
library(GGally)
library(viridis)

# Select relevant numerical columns for pairs plot
numerical_columns_pairs <- your_data_frame[, c("Patient_Age", "Patient_Body_Mass_Index", "Number_of_prev_cond")]

# Ensure 'Survived_1_year' is a factor
your_data_frame$Survived_1_year <- factor(your_data_frame$Survived_1_year)

# Create a pairs plot with color gradient
ggpairs(
  data = your_data_frame,
  columns = c("Patient_Age", "Patient_Body_Mass_Index", "Number_of_prev_cond"),
  aes(color = Survived_1_year),
  lower = list(continuous = "points", combo = "facetdensity"),
  upper = list(continuous = "cor"),
  title = "Scatter Plot Matrix with Color Gradient",
  palette = "viridis"
)

#-------------------------------------------------------------------------------------------------------------------------------#
#random forest


# Install and load the randomForest package if not already installed
library(randomForest)

# Set seed for reproducibility
set.seed(123)



train_indices <- sample(1:nrow(your_data_frame_encoded), 0.7 * nrow(your_data_frame_encoded))
train_data <- your_data_frame_encoded[train_indices, ]
test_data <- your_data_frame_encoded[-train_indices, ]
# Ensure valid column names for the formula
colnames(train_data) <- make.names(colnames(train_data))

# Define the model
rf_model <- randomForest(Survived_1_year ~ .,ntree=700, data = train_data)
# Ensure valid column names for the formula
colnames(test_data) <- make.names(colnames(test_data))
# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate the model (you can use other metrics as needed)
confusion_matrix <- table(predictions, test_data$Survived_1_year)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# Print the results
print("Confusion Matrix:")
print(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Make predictions on the test set
predictions_rf <- predict(rf_model, newdata = test_data)

# Evaluate the model
confusion_matrix_rf <- table(predictions_rf, test_data$Survived_1_year)
accuracy_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)



# Print the results
cat("Random Forest Metrics:\n")
cat("Confusion Matrix:\n", confusion_matrix_rf, "\n")
cat("Accuracy:", accuracy_rf, "\n")

#--------------------------------------------------------------------------------------------------------------------#

#Logistic regression

# Load required libraries
library(caret)
library(glmnet)


# Set the seed for reproducibility
set.seed(123)
# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Define the hyperparameter grid for logistic regression
grid <- expand.grid(alpha = seq(0, 1, 0.1),
                    lambda = seq(0.001, 0.1, length = 10))

# Ensure valid column names for the formula
colnames(your_data_frame_encoded) <- make.names(colnames(your_data_frame_encoded))

# Perform logistic regression using grid search and cross-validation
model_lr <- train(Survived_1_year ~ ., data = your_data_frame_encoded, method = "glmnet",
                  trControl = ctrl, tuneGrid = grid)

# Display the best model and its parameters
print(model_lr)

# Make predictions on the entire dataset
predictions_lr <- predict(model_lr, newdata = your_data_frame_encoded)

# Evaluate the model 
confusion_matrix_lr <- table(predictions_lr, your_data_frame_encoded$Survived_1_year)
accuracy_lr <- sum(diag(confusion_matrix_lr)) / sum(confusion_matrix_lr)

# Print the results
cat("Confusion Matrix:\n", confusion_matrix_lr, "\n")
cat("Accuracy:", accuracy_lr, "\n")

