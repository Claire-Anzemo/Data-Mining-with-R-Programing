# Load required libraries
library(e1071)      # For SVM model
library(caret)      # For model evaluation

# Load the dataset
data <- read.csv("C:/Users/stacy/Downloads/DataSets/Patients to be re-admited-UCI data/diabetic_Readmission_data.csv")

# Checking the structure of the dataset
str(data)
# Summary statistics
summary(data)

# Split the dataset into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Print column names of the dataset
#print(colnames(train_data))


# Define predictors and target variable
predictors <- c("encounter_id", "patient_nbr", "race","gender", "age", "weight", "admission_type_id","discharge_disposition_id",
                 "admission_source_id","time_in_hospital", "payer_code", "medical_specialty","num_lab_procedures","num_procedures","num_medications","number_outpatient",       
                 "number_emergency","number_inpatient","diag_1","diag_2","diag_3","number_diagnoses","max_glu_serum","A1Cresult",               
                 "metformin","repaglinide","nateglinide","chlorpropamide","glimepiride","acetohexamide","glipizide","glyburide","tolbutamide","pioglitazone","rosiglitazone","acarbose",                
                 "miglitol","troglitazone","tolazamide","examide","citoglipton","insulin","glyburide.metformin","glipizide.metformin",     
                 "glimepiride.pioglitazone","metformin.rosiglitazone","metformin.pioglitazone","change","diabetesMed")
target <- "readmitted"

# Check unique values of categorical variables
unique_values <- sapply(train_data[, predictors], function(x) length(unique(x)))

# Identify single-level variables
single_level_vars <- names(unique_values[unique_values == 1])

# Print single-level variables
print("Single-level variables:")
print(single_level_vars)

# Drop single-level variables from predictors
predictors <- setdiff(predictors, single_level_vars)

# Convert 'readmitted' to factor
train_data$readmitted <- factor(train_data$readmitted)
test_data$readmitted <- factor(test_data$readmitted)

# Find levels in test dataset not present in training dataset
new_levels <- setdiff(levels(test_data$medical_specialty), levels(train_data$medical_specialty))

# Convert new levels to NA
test_data$medical_specialty[test_data$medical_specialty %in% new_levels] <- NA


# Check levels in training dataset
levels_train <- levels(train_data$medical_specialty)

# Check levels in test dataset
levels_test <- levels(test_data$medical_specialty)

# Print levels not present in training dataset but present in test dataset
new_levels <- setdiff(levels_test, levels_train)
print(new_levels)

# Remove medical_specialty from predictors
predictors <- setdiff(predictors, "medical_specialty")

# Define predictors without diag_1, diag_2, and diag_3
predictors <- setdiff(predictors, c("diag_1", "diag_2", "diag_3"))

# Train SVM model
svm_model <- svm(as.factor(readmitted) ~ ., data = train_data[, c(predictors, "readmitted")], kernel = "radial")

# Make predictions on test set
test_predictions <- predict(svm_model, newdata = test_data)

# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = test_data$readmitted)

# Print confusion matrix
print(conf_matrix)

# Print performance metrics
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
f_measure <- conf_matrix$byClass['F1']
kappa <- kappa2(conf_matrix$table)

print(paste("Accuracy:", round(accuracy, 3)))
print(paste("Sensitivity:", round(sensitivity, 3)))
print(paste("Specificity:", round(specificity, 3)))
print(paste("F-Measure:", round(f_measure, 3)))
print(paste("Cohen's Kappa:", round(kappa$value, 3)))
