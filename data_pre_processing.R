#installing needed libraries
install.packages("dplyr")
library(dplyr)

#importing file to environment
pharma_testing <- read.csv("Testing_set_advance.csv", header = TRUE)
pharma_training <- read.csv("Training_set_advance.csv", header = TRUE)

#merging training & testing dataset.

library(data.table)

merged_data <- merge(pharma_testing,pharma_training,all = TRUE)

print(merged_data)


#Filling Null values with randomly generated 1 & 0.
merged_data <- merged_data %>%
  mutate(A = ifelse(is.na(A), sample(0:1, 1, replace = TRUE), A))

merged_data <- merged_data %>%
  mutate(B = ifelse(is.na(B), sample(0:1, 1, replace = TRUE), B))

merged_data <- merged_data %>%
  mutate(C = ifelse(is.na(C), sample(0:1, 1, replace = TRUE), C))

merged_data <- merged_data %>%
  mutate(D = ifelse(is.na(D), sample(0:1, 1, replace = TRUE), D)) 

merged_data <- merged_data %>%
  mutate(E = ifelse(is.na(E), sample(0:1, 1, replace = TRUE), E))

merged_data <- merged_data %>%
  mutate(F = ifelse(is.na(F), sample(0:1, 1, replace = TRUE), F))

merged_data <- merged_data %>%
  mutate(Z = ifelse(is.na(Z), sample(0:1, 1, replace = TRUE), Z))

merged_data <- merged_data %>%
  mutate(Number_of_prev_cond = ifelse(is.na(Number_of_prev_cond), sample(0:1, 1, replace = TRUE), Number_of_prev_cond))

merged_data <- merged_data %>%
  mutate(Survived_1_year = ifelse(is.na(Survived_1_year), sample(0:1, 1, replace = TRUE), Z))

#removing unwanted rows
merged_data <- merged_data %>% 
  
  slice(-(34402:34409))

print(merged_data)

#exporting merged_data as an excel file.
library(writexl)
write_xlsx(merged_data, "merged_data.xlsx")
































