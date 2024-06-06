# Load required libraries
library(caret)  # For modeling and evaluation
library(ggplot2)  # For visualization

# Load the dataset
data <- read.csv("C:/Users/stacy/Downloads/DataSets/diabetes_012_health_indicators_BRFSS2015.csv")

# Print structure of the data
str(data)

# Summary statistics
summary(data)

# Close any open plot devices
#dev.off()

# Scatter plot of Age vs. BMI
ggplot(data, aes(x = Age, y = BMI, color = as.factor(Diabetes_012))) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. BMI", x = "Age", y = "BMI", color = "Diabetes") +
  theme_minimal()

# Split the dataset into training and testing sets (70% training, 30% testing)
set.seed(123)  # For reproducibility
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define predictors and target variable
predictors <- c("HighBP", "HighChol", "CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack",
                "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost",
                "GenHlth", "MentHlth", "PhysHlth", "DiffWalk", "Sex", "Age", "Education", "Income")
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

# Convert Diabetes_012 to a factor with consistent levels
train_data$Diabetes_012 <- factor(train_data$Diabetes_012, levels = c("0", "1", "2"))

# Check unique values of the target variable
print("Unique values of the target variable:")
print(unique(train_data$Diabetes_012))

# Train k-NN model for multiclass classification
knn_model <- train(Diabetes_012 ~ .,
                   data = train_data,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 10),
                   preProcess = c("center", "scale"))

# Make predictions on test set
test_predictions <- predict(knn_model, newdata = test_data)

# Evaluate performance
conf_matrix <- confusionMatrix(data = test_predictions, reference = factor(test_data$Diabetes_012, levels = c("0", "1", "2")))

# Print confusion matrix
print(conf_matrix)

# Calculate and print accuracy, sensitivity, specificity, and F-measure for each class
print(conf_matrix$byClass)

# Explicitly load the confusionMatrixPlot function
library(caret)



# Visualize confusion matrix
print(conf_matrix$table)

# Visualize confusion matrix
plot(conf_matrix, col = c("skyblue", "salmon", "lightgreen"), 
     main = "Confusion Matrix", xlab = "Predicted", ylab = "Actual")
legend("topright", legend = rownames(conf_matrix), fill = c("skyblue", "salmon", "lightgreen"))
