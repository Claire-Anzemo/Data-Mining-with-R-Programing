# Load required libraries
library(readr)      # For reading data
library(caret)      # For model training and evaluation
library(caretEnsemble) # For confusionMatrix function
library(e1071)      # For SVM model

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

# Train SVM model
svm_model <- svm(as.factor(Diabetes) ~ ., data = train_data[, c(predictors, target)], kernel = "radial")

# Make predictions on test set
test_predictions <- predict(svm_model, newdata = test_data[, predictors])

# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = test_data$Diabetes)

# Print confusion matrix
print(conf_matrix)

# Visualize confusion matrix with custom colors
plot(conf_matrix$table, 
     col = c("yellowgreen", "darkgreen", "lightgreen"), 
     main = "Confusion Matrix", 
     xlab = "Predicted", 
     ylab = "Actual")
legend("topright", 
       legend = rownames(conf_matrix$table), 
       fill = c("yellowgreen", "darkgreen", "lightgreen"))

