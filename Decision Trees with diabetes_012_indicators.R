# Load required libraries
library(caret)   # For decision tree model
library(pROC)    # For ROC curve and AUC
library(caretEnsemble) # For confusionMatrix function
library(vcd)
library(irr)

# Install the irr package if not already installed
if (!requireNamespace("irr", quietly = TRUE)) {
  install.packages("irr")
}


# Install the vcd package if not already installed
if (!requireNamespace("vcd", quietly = TRUE)) {
  install.packages("vcd")
}

# Load the dataset
data <- read.csv("C:/Users/stacy/Downloads/DataSets/diabetes_012_health_indicators_BRFSS2015.csv") 

# Checking the structure of the dataset
str(data)

# Summary statistics
summary(data)

# Split the dataset into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define predictors and target variable
predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", 
                "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", 
                "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")
target <- "Diabetes_012"  # Replace "Diabetes_012" with the actual name of your target variable

# Print data types of predictor variables
print("Data types of predictor variables:")
print(sapply(train_data[, predictors], class))

# Print data type of target variable
print("Data type of target variable:")
print(class(train_data$Diabetes_012))

# Inspect a sample of the data
print("Sample of the data:")
print(head(train_data))

# Check for missing values
print("Missing values in predictor variables:")
print(sapply(train_data[, predictors], function(x) sum(is.na(x))))

print("Missing values in target variable:")
print(sum(is.na(train_data$Diabetes_012)))

# Convert Diabetes_012 to a factor with consistent levels
train_data$Diabetes_012 <- factor(train_data$Diabetes_012, levels = c("0", "1", "2"))

# Check unique values of the target variable
print("Unique values of the target variable:")
print(unique(train_data$Diabetes_012))

# Train decision tree model
decision_tree_model <- train(Diabetes_012 ~ .,
                             data = train_data,
                             method = "rpart",
                             trControl = trainControl(method = "cv", number = 10))

# Make predictions on test set
test_predictions <- predict(decision_tree_model, newdata = test_data)

# Ensure consistent levels in the test set
test_data$Diabetes_012 <- factor(test_data$Diabetes_012, levels = levels(train_data$Diabetes_012))



# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = test_data$Diabetes_012)
accuracy <- conf_matrix$overall['Accuracy']
sensitivity <- conf_matrix$byClass['Sensitivity']
specificity <- conf_matrix$byClass['Specificity']
f_measure <- conf_matrix$byClass['F1']
kappa <- kappam.fleiss(conf_matrix$table)  # Using Fleiss' kappa

# Print confusion matrix
print(conf_matrix)

print(conf_matrix$byClass)

# Print performance metrics
print(paste("Accuracy:", round(accuracy, 3)))
print(paste("Sensitivity:", round(sensitivity, 3)))
print(paste("Specificity:", round(specificity, 3)))
print(paste("F-Measure:", round(f_measure, 3)))
print(paste("Cohen's Kappa:", round(kappa$value, 3)))

# Define colors for the plot
colors <- c("skyblue", "salmon", "lightgreen")

# Plot the confusion matrix
heatmap(conf_matrix$table, 
        col = colors,
        main = "Confusion Matrix",
        xlab = "Actual",
        ylab = "Predicted",
        symm = TRUE, 
        density.info = "histogram")


