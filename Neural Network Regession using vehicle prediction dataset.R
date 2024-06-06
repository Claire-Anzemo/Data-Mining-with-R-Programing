# Load required libraries
library(caret)
library(nnet)
library(corrplot)

# Read the large dataset. Replace with your dataset path
large_data <- read.csv("C:/Users/stacy/Downloads/Machine learning portfolio/car_prices.csv")

# Specify the desired number of rows to keep (e.g., 10,000 rows)
desired_rows <- 50000

# Check if the dataset has more rows than the desired number
if (nrow(large_data) > desired_rows) {
  # If yes, sample rows randomly to reduce the number
  sampled_data <- large_data %>% sample_n(desired_rows)
} else {
  # If not, keep the original dataset
  sampled_data <- large_data
}

# Exclude rows with missing values
sampled_data <- na.omit(sampled_data)

# Convert character variables to factors
sampled_data$make <- as.factor(sampled_data$make)
sampled_data$model <- as.factor(sampled_data$model)
sampled_data$trim <- as.factor(sampled_data$trim)
sampled_data$body <- as.factor(sampled_data$body)
sampled_data$transmission <- as.factor(sampled_data$transmission)
sampled_data$vin <- as.factor(sampled_data$vin)
sampled_data$state <- as.factor(sampled_data$state)
sampled_data$color <- as.factor(sampled_data$color)
sampled_data$interior <- as.factor(sampled_data$interior)
sampled_data$seller <- as.factor(sampled_data$seller)
sampled_data$saledate <- as.factor(sampled_data$saledate)

# Split data into numeric and non-numeric columns
#numeric_data <- sampled_data[, sapply(sampled_data, is.numeric)]

# Generate correlation matrix
#correlation_matrix <- cor(numeric_data)
#corrplot(correlation_matrix, method="circle", type="upper", tl.cex=0.7)

# Split data into training and testing sets
set.seed(123) # for reproducibility
train_index <- createDataPartition(numeric_data$sellingprice, p = 0.8, list = FALSE)
train_data <- numeric_data[train_index, ]
test_data <- numeric_data[-train_index, ]

# Train the neural network regression model
nn_model <- nnet::nnet(sellingprice ~ ., data = train_data, size = 10, linout = TRUE)

summary(nn_model)

# Make predictions on the test set
predictions <- predict(nn_model, newdata = test_data)

# Evaluate performance (you can use RMSE, R-squared, etc.)
rmse <- sqrt(mean((predictions - test_data$sellingprice)^2))
r_squared <- cor(predictions, test_data$sellingprice)^2
mape <- mean(abs((test_data$sellingprice - predictions) / test_data$sellingprice)) * 100
rss <- sum((test_data$sellingprice - predictions)^2)

# Print performance measures
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")
cat("MAPE:", mape, "\n")
cat("RSS:", rss, "\n")
