# Load required libraries
library(readr)      # For reading data
library(caret)      # For logistic regression model
library(pROC)       # For ROC curve and AUC
library(caret)      # For model evaluation
library(caretEnsemble) # For confusionMatrix function
library(irr)

# Load the dataset
data <- read.csv("C:/Users/stacy/Downloads/DataSets/diabetes_prediction_data.csv")  # Replace "path_to_dataset1.csv" with the actual path to your dataset file

# Checking the structure of the dataset
str(data)
# Summary statistics
summary(data)

# Split the dataset into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data), 0.7*nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define predictors and target variable
predictors <- c("Age", "Sex", "HighChol", "CholCheck", "BMI", "Smoker", "HeartDiseaseorAttack", 
                "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "GenHlth", "MentHlth", 
                "PhysHlth", "DiffWalk", "Stroke", "HighBP")
target <- "Diabetes"  # Replace "Diabetes" with the actual name of your target variable

# Convert "Diabetes" to a factor with levels "0" and "1"
train_data$Diabetes <- factor(train_data$Diabetes, levels = c("0", "1"))

# Train logistic regression model
logistic_model <- train(as.formula(paste(target, "~", paste(predictors, collapse = "+"))),
                        data = train_data,
                        method = "glm",
                        trControl = trainControl(method = "cv", number = 10), 
                        family = binomial)

summary(logistic_model)


# Make predictions on test set
test_predictions <- predict(logistic_model, newdata = test_data, type = "raw")

# Convert predicted values to factors with the same levels as the reference values
test_predictions <- factor(test_predictions, levels = c("0", "1"))

# Convert test_data$Diabetes to factor with the same levels as test_predictions
test_data$Diabetes <- factor(test_data$Diabetes, levels = c("0", "1"))

# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = test_data$Diabetes)

# Print confusion matrix
print(conf_matrix)


# Calculate Cohen's Kappa
kappa <- kappa2(conf_matrix$table)

# Print Cohen's Kappa
print(paste("Cohen's Kappa:", round(kappa$value, 3)))

# Visualize confusion matrix with custom colors
plot(conf_matrix$table, 
     col = c("yellowgreen", "darkgreen", "lightgreen"), 
     main = "Confusion Matrix", 
     xlab = "Predicted", 
     ylab = "Actual")
legend("topright", 
       legend = rownames(conf_matrix$table), 
       fill = c("yellowgreen", "darkgreen", "lightgreen"))







# Predict probabilities on the test set
test_probabilities <- predict(logistic_model, newdata = test_data, type = "raw")

# Create a ROC curve object
roc_curve <- roc(test_data$Diabetes, test_probabilities[,2])  # Use the probabilities of the positive class

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Logistic Regression Model",
     col = "blue", lwd = 2)

# Add diagonal reference line
abline(a = 0, b = 1, lty = 2, col = "red")

# Add legend
legend("bottomright", legend = c("ROC Curve", "Random Guess"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 1))




