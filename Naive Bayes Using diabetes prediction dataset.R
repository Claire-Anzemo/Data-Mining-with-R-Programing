# Load required libraries
library(e1071)  # For Naive Bayes model
library(caret)   # For model evaluation
library(ggplot2) # For data visualization

# Load the dataset
data <- read.csv("C:/Users/stacy/Downloads/DataSets/diabetes_prediction_data.csv")

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
predictors <- c("Age", "Sex", "HighChol", "CholCheck", "BMI", "Smoker", "HeartDiseaseorAttack", 
                "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "GenHlth", "MentHlth", 
                "PhysHlth", "DiffWalk", "Stroke", "HighBP")
target <- "Diabetes"  # Replace "Diabetes" with the actual name of your target variable

# Train Naive Bayes model
naive_bayes_model <- naiveBayes(as.formula(paste(target, "~", paste(predictors, collapse = "+"))), 
                                data = train_data)

print(naive_bayes_model)

# Make predictions on test set
test_predictions <- predict(naive_bayes_model, newdata = test_data)

# Ensure consistent levels in the test set
test_data$Diabetes <- factor(test_data$Diabetes, levels = levels(test_predictions))

# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = test_data$Diabetes)

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

# Visualization of the data (example with Age and BMI)
ggplot(data, aes(x = Age, y = BMI, color = Diabetes)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. BMI by Diabetes Status",
       x = "Age",
       y = "BMI",
       color = "Diabetes Status")


# Visualize confusion matrix
plot(conf_matrix$table, col = c("skyblue", "salmon", "lightgreen"), 
     main = "Confusion Matrix", xlab = "Predicted", ylab = "Actual")
legend("topright", legend = rownames(conf_matrix$table), fill = c("skyblue", "salmon", "lightgreen"))

