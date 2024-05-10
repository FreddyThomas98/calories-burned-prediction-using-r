#Load Dataset

calories_data = read.csv("C:/Users/FREDDY/Downloads/archive (29)/calories.csv",header=TRUE)
exercise_data=read.csv("C:/Users/FREDDY/Downloads/archive (29)/exercise.csv",header = TRUE)

# Merging datasets based on User_ID

merged_data <- merge(calories_data, exercise_data, by="User_ID")

# Remove the User_ID column

merged_data <- merged_data[, -which(names(merged_data) == "User_ID")]
head(merged_data)

# Summary statistics for numerical variables
summary(merged_data[, c("Calories", "Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp")])

library(ggplot2)

# Create a histogram of Calories using ggplot2
ggplot(merged_data, aes(x = Calories)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Calories", x = "Calories", y = "Frequency")

# Check for missing values
missing_values <- colSums(is.na(merged_data)) #no missing values


# Scatter plot: Calories vs. Age
ggplot(merged_data, aes(x = Age, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Age", x = "Age", y = "Calories")

# Scatter plot: Calories vs. Height
ggplot(merged_data, aes(x = Height, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Height", x = "Height", y = "Calories")

# Scatter plot: Calories vs. Weight
ggplot(merged_data, aes(x = Weight, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Weight", x = "Weight", y = "Calories")

# Scatter plot: Calories vs. Duration
ggplot(merged_data, aes(x = Duration, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Duration", x = "Duration", y = "Calories")

# Scatter plot: Calories vs. Heart Rate
ggplot(merged_data, aes(x = Heart_Rate, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Heart Rate", x = "Heart Rate", y = "Calories")

# Scatter plot: Calories vs. Body Temperature
ggplot(merged_data, aes(x = Body_Temp, y = Calories)) +
  geom_point() +
  labs(title = "Calories vs. Body Temperature", x = "Body Temperature", y = "Calories")



# Calculate correlation matrix
correlation_matrix <- cor(merged_data[, c("Calories", "Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp")])

library(reshape2)

# Convert correlation matrix to long format for ggplot
correlation_df <- melt(correlation_matrix)


# Create heatmap
ggplot(correlation_df, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 10, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap")



# Box plot: Age
ggplot(merged_data, aes(y = Age)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age")

# Box plot: Height
ggplot(merged_data, aes(y = Height)) +
  geom_boxplot() +
  labs(title = "Box Plot of Height")

# Box plot: Weight
ggplot(merged_data, aes(y = Weight)) +
  geom_boxplot() +
  labs(title = "Box Plot of Weight")

# Box plot: Duration
ggplot(merged_data, aes(y = Duration)) +
  geom_boxplot() +
  labs(title = "Box Plot of Duration")

# Box plot: Heart Rate
ggplot(merged_data, aes(y = Heart_Rate)) +
  geom_boxplot() +
  labs(title = "Box Plot of Heart Rate")

# Box plot: Body Temperature
ggplot(merged_data, aes(y = Body_Temp)) +
  geom_boxplot() +
  labs(title = "Box Plot of Body Temperature")


# Define numerical variables
numerical_vars <- c("Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp")

# Loop through numerical variables
for (var in numerical_vars) {
  # Calculate Q1 (25th percentile) and Q3 (75th percentile)
  Q1 <- quantile(merged_data[[var]], 0.25)
  Q3 <- quantile(merged_data[[var]], 0.75)
  
  # Calculate IQR (Interquartile Range)
  IQR <- Q3 - Q1
  
  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Identify outliers
  outliers <- merged_data[[var]] < lower_bound | merged_data[[var]] > upper_bound
  
  # Remove outliers from the dataset
  merged_data <- merged_data[!outliers, ]
}

# Show the dimensions of the dataset after removing outliers
dim(merged_data)


#encoding
# Convert Gender to a factor
merged_data$Gender <- factor(merged_data$Gender)

# Perform label encoding
merged_data$Gender_encoded <- as.numeric(merged_data$Gender)

# Print the encoded dataset
print(merged_data)
merged_data <- merged_data[, !names(merged_data) %in% "Gender"]

# Print the updated dataset
print(merged_data)


#scaling
# Select numerical variables for scaling
numerical_vars <- c("Age", "Height", "Weight", "Duration", "Heart_Rate", "Body_Temp")

# Perform standardization (or Min-Max scaling)
merged_data_scaled <- merged_data
merged_data_scaled[, numerical_vars] <- scale(merged_data[, numerical_vars])

# Print the scaled dataset
head(merged_data_scaled)


#Train Test Split

# Load caret library

library(caret)

# Set seed for reproducibility
set.seed(123)
train_proportion <- 0.7
# Perform train-test split
train_index <- createDataPartition(merged_data_scaled$Calories, p = train_proportion, list = FALSE)
train_data <- merged_data_scaled[train_index, ]
test_data <- merged_data_scaled[-train_index, ]

# Print the dimensions of train and test sets
print(dim(train_data))
print(dim(test_data))


# Train the Linear Regression model
linear_model <- lm(Calories ~ ., data = train_data)

# Print the model summary
summary(linear_model)

# Predict on the testing dataset
predictions_linear <- predict(linear_model, newdata = test_data)

# Calculate R-squared
rsquared_linear <- cor(predictions_linear, test_data$Calories)^2

# Print R-squared
cat("R-squared (R²) for Linear Regression:", rsquared_linear, "\n")

