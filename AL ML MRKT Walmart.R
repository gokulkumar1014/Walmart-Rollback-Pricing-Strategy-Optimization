#G25385029
getwd()
setwd("C:\\Program Files\\RStudio")
walmart <- read.csv("C:\\Program Files\\RStudio\\Case_Study_I_WalMart_Data.csv", header = TRUE)
head(walmart)
walmart$Gender <- as.factor(walmart$Gender)
summary(walmart)

# MISSING VALUES
colSums(is.na(walmart))
# Check if any blank ("") values exist in each column
colSums(walmart == "")
# Convert blank values and string "NA" to actual NA
walmart[walmart == ""] <- NA
sum(is.na(walmart$Gender))
sum(is.na(walmart$Age))
colSums(is.na(walmart))

# IDENTITY ROWS THAT ORIGINALLY HAS MISSING GENDER
missing_gender_rows <- which(is.na(walmart$Gender))
# Print the rows before filling (if stored separately)
print(missing_gender_rows)
# Count Gender before filling
table_before <- table(walmart$Gender, useNA = "always")  # Shows NA count
table_before
# Plot before filling
barplot(table_before, main = "Gender Distribution (Before Filling)",
        col = c("pink", "blue", "gray"), ylim = c(0, max(table_before) + 20))

# IDENTITY ROWS THAT ORIGINALLY HAS MISSING AGE
hist(walmart$Age, main = "Age Distribution (Before Filling)", col = "lightblue", breaks = 20)
missing_age_rows <- which(is.na(walmart$Age))
# Print the rows before filling (if stored separately)
print(missing_age_rows)
# CREATE BINS FOR AGE GROUP
bins <- c(0, 20, 30, 40, 50, 60, 70, 80, 100)
labels <- c("0-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-100")
# Convert Age into categorical bins before filling
walmart_before <- walmart  # Make a copy of dataset before filling Age
walmart_before$Age_Group <- cut(walmart_before$Age, breaks = bins, labels = labels, include.lowest = TRUE)
# Count Age groups before filling
age_distribution_before <- table(walmart_before$Age_Group, useNA = "ifany")  # NA will show separately
# Print distributions
print("Age Distribution Before Filling:")
print(age_distribution_before)


#PLOTTING THE CORRELATION
# Total Male and Female customers in the dataset
table(walmart$Gender)
#Check if Males and Females spend differently on Total purchases
library(ggplot2)
ggplot(walmart, aes(x = Gender, y = Total, fill = Gender)) + 
  geom_boxplot() +
  labs(title = "Total Spending by Gender", x = "Gender", y = "Total Spending") +
  theme_minimal()
boxplot(MarkDown1 ~ Gender, data = walmart, main = "MarkDown1 Spending by Gender")
boxplot(MarkDown2 ~ Gender, data = walmart, main = "MarkDown2 Spending by Gender")
boxplot(MarkDown3 ~ Gender, data = walmart, main = "MarkDown3 Spending by Gender")
boxplot(MarkDown4 ~ Gender, data = walmart, main = "MarkDown4 Spending by Gender")
boxplot(MarkDown5 ~ Gender, data = walmart, main = "MarkDown5 Spending by Gender")
boxplot(R ~ Gender, data = walmart, main = "Recency by Gender")
boxplot(F ~ Gender, data = walmart, main = "Frequency by Gender")
boxplot(M ~ Gender, data = walmart, main = "Monetary Score by Gender")

# CORRELATION
# Convert Gender to numeric
walmart$Gender_numeric <- ifelse(walmart$Gender == "M", 1, 0)
# Compute correlation between Gender and spending behavior
correlation_values <- cor(walmart[, c("Gender_numeric", "Total", "Age", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5", "R", "F", "M")], use = "complete.obs")
# Print correlations
correlation_values

#FEATURE SCALING
# Load necessary library
library(caret)
# Select numeric columns to normalize
normalize_cols <- c("Total", "Age", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")
# Store the original min and max values BEFORE normalizing
original_mins <- apply(walmart[, normalize_cols], 2, min, na.rm = TRUE)
original_maxs <- apply(walmart[, normalize_cols], 2, max, na.rm = TRUE)
# Define normalization function using stored min-max values
normalize <- function(x, min_val, max_val) {
  return((x - min_val) / (max_val - min_val))
}
# Apply normalization using stored min-max values
for (col in normalize_cols) {
  walmart[[col]] <- normalize(walmart[[col]], original_mins[col], original_maxs[col])
}
# Check summary to confirm normalization
summary(walmart[normalize_cols])

# LOGISTIC REGRESSION
# Train logistic regression model using Total and Age
gender_model <- glm(Gender_numeric ~ Total + Age + MarkDown1 + MarkDown2 + MarkDown3 + MarkDown4 + MarkDown5 + R + F + M, data = walmart[!is.na(walmart$Gender), ], family = binomial)
# Check model summary
summary(gender_model)


# KNN FOR GENDER
# Load necessary library for KNN
# install.packages("class")
library(class)
# Define features to use for KNN
features <- c("Total", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5")
# Extract training data (where Gender is known)
train_data <- walmart[!is.na(walmart$Gender), ]
train_labels <- train_data$Gender  # Target variable (Gender)
# Extract test data (where Gender is missing)
test_data <- walmart[is.na(walmart$Gender), ]
test_features <- test_data[, features]  # Only select features for prediction
# Check for missing values in training data
colSums(is.na(train_data[features]))
# Check for missing values in test data
colSums(is.na(test_features))
# Function to fill missing values with median
fill_na_with_median <- function(df, cols) {
  for (col in cols) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  return(df)
}
# Apply to training and test data
train_data <- fill_na_with_median(train_data, features)
test_features <- fill_na_with_median(test_features, features)
# Verify missing values are gone
colSums(is.na(train_data[features]))
colSums(is.na(test_features))
# Run KNN to predict missing Gender values
predicted_gender <- knn(train = train_data[, features], 
                        test = test_features, 
                        cl = train_labels, 
                        k = 5)
# Assign predicted values back to the dataset
walmart$Gender[is.na(walmart$Gender)] <- predicted_gender
sum(is.na(walmart$Gender))  # Should return 0
# Recreate Gender_numeric after filling missing Gender values
walmart$Gender_numeric <- ifelse(walmart$Gender == "M", 1, 0)
# Display the 10 rows where Gender was filled
walmart[missing_gender_rows, ]
# Count Gender after filling
table_after <- table(walmart$Gender)
# Plot after filling
barplot(table_after, main = "Gender Distribution (After Filling)",
        col = c("pink", "blue"), ylim = c(0, max(table_after) + 20))
table_before
table_after


# KNN FOR AGE
# Define features to use for KNN
features <- c("Total", "MarkDown1", "MarkDown2", "MarkDown3", "MarkDown4", "MarkDown5", "Gender_numeric")
# Extract training data (where Age is known)
train_data_age <- walmart[!is.na(walmart$Age), ]
train_labels_age <- train_data_age$Age  # Target variable (Age)
# Extract test data (where Age is missing)
test_data_age <- walmart[is.na(walmart$Age), ]
test_features_age <- test_data_age[, features]  # Only select features for prediction
# Check for missing values in training and test datasets
colSums(is.na(train_data_age[features]))
colSums(is.na(test_features_age[features]))
# Function to fill missing values with median (Reused from Gender)
fill_na_with_median <- function(df, cols) {
  for (col in cols) {
    df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
  }
  return(df)
}
# Apply to training and test datasets for Age prediction
train_data_age <- fill_na_with_median(train_data_age, features)
test_features_age <- fill_na_with_median(test_features_age, features)
# Verify that missing values are gone
colSums(is.na(train_data_age[features]))
colSums(is.na(test_features_age[features]))
# Load necessary library
# install.packages("FNN")
library(FNN)
# Run KNN Regression to predict missing Age values
predicted_age <- knn.reg(train = train_data_age[, features], 
                         test = test_features_age, 
                         y = train_labels_age, 
                         k = 5)  # Using k=5 neighbors
# Assign predicted values back to the dataset
walmart$Age[is.na(walmart$Age)] <- predicted_age$pred
walmart$Age <- round(walmart$Age)
# Verify that missing Age values are filled
sum(is.na(walmart$Age))  # Should return 0



# Denormalization function
denormalize <- function(x, min_val, max_val) {
  return(x * (max_val - min_val) + min_val)
}
# Apply denormalization to all normalized columns
for (col in normalize_cols) {
  walmart[[col]] <- walmart[[col]] * (original_maxs[col] - original_mins[col]) + original_mins[col]
}
# Check summary to confirm denormalization worked
summary(walmart[normalize_cols])

# Display the 10 rows where AGE was filled
walmart[missing_age_rows, ]
# Convert Age into categorical bins after filling
walmart_after <- walmart  # Using dataset after filling Age
walmart_after$Age_Group <- cut(walmart_after$Age, breaks = bins, labels = labels, include.lowest = TRUE)
# Count Age groups after filling
age_distribution_after <- table(walmart_after$Age_Group)
print("Age Distribution After Filling:")
print(age_distribution_after)
# Create a barplot comparison
par(mfrow = c(1, 2))  # Split plot area into 2 columns
barplot(age_distribution_before, main = "Age Distribution (Before Filling)", col = "red", ylim = c(0, max(age_distribution_after) + 10))
barplot(age_distribution_after, main = "Age Distribution (After Filling)", col = "blue", ylim = c(0, max(age_distribution_after) + 10))






# Define predictor variables
features <- c("Age", "Gender_numeric", "R", "F", "M")

# Train separate regression models for each markdown level
lm_markdown1 <- lm(MarkDown1 ~ Age+Gender_numeric+R+F+M, data = walmart)
lm_markdown2 <- lm(MarkDown2 ~ Age+Gender_numeric+R+F+M, data = walmart)
lm_markdown3 <- lm(MarkDown3 ~ Age+Gender_numeric+R+F+M, data = walmart)
lm_markdown4 <- lm(MarkDown4 ~ Age+Gender_numeric+R+F+M, data = walmart)
lm_markdown5 <- lm(MarkDown5 ~ Age+Gender_numeric+R+F+M, data = walmart)

# Check model summaries
summary(lm_markdown1)
summary(lm_markdown2)
summary(lm_markdown3)
summary(lm_markdown4)
summary(lm_markdown5)

# Train new regression models with only significant predictors (R, M)
lm_markdown1 <- lm(MarkDown1 ~ R + M, data = walmart)
lm_markdown2 <- lm(MarkDown2 ~ R + M, data = walmart)
lm_markdown3 <- lm(MarkDown3 ~ R + M, data = walmart)
lm_markdown4 <- lm(MarkDown4 ~ R + M, data = walmart)
lm_markdown5 <- lm(MarkDown5 ~ R + M, data = walmart)

# Check updated model summaries
summary(lm_markdown1)
summary(lm_markdown2)
summary(lm_markdown3)
summary(lm_markdown4)
summary(lm_markdown5)


# Generate 10 random new customers
set.seed(123)  # For reproducibility
new_customers <- data.frame(
  Age = sample(walmart$Age, 10, replace = TRUE),  # Sample from existing Age values
  Gender_numeric = sample(walmart$Gender_numeric, 10, replace = TRUE),  # Random M/F
  R = sample(walmart$R, 10, replace = TRUE),  # Recency
  F = sample(walmart$F, 10, replace = TRUE),  # Frequency
  M = sample(walmart$M, 10, replace = TRUE)   # Monetary
)

# View the new random customers
print(new_customers)


# Predict purchases for each markdown level
new_customers$MarkDown1 <- predict(lm_markdown1, newdata = new_customers)
new_customers$MarkDown2 <- predict(lm_markdown2, newdata = new_customers)
new_customers$MarkDown3 <- predict(lm_markdown3, newdata = new_customers)
new_customers$MarkDown4 <- predict(lm_markdown4, newdata = new_customers)
new_customers$MarkDown5 <- predict(lm_markdown5, newdata = new_customers)

# View predicted purchases
print(new_customers)

# Calculate total predicted revenue for each new customer
new_customers$Total_Revenue <- new_customers$MarkDown1 + 
  new_customers$MarkDown2 + 
  new_customers$MarkDown3 + 
  new_customers$MarkDown4 + 
  new_customers$MarkDown5

# View the final predicted revenue per customer
print(new_customers)

# Summarize total predicted revenue for all 10 new customers
total_predicted_revenue <- sum(new_customers$Total_Revenue)
print(paste("Total predicted revenue from 10 new customers:", total_predicted_revenue))
