# load in necessary packages
install.packages('ggfortify')
library(ggfortify)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
install.packages("moments")
library(moments)
install.packages("tidyr")
library(tidyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library(corrplot)
install.packages("glmnet")
library(glmnet)
install.packages("impute")
library(impute)


### Exercise 6.1:

# (a) Start R and use these commands to load the data:
install.packages("caret")
library(caret)
data(tecator) # use ?tecator to see more details

# (b) In this example the predictors are the measurements at the individual
# frequencies. Because the frequencies lie in a systematic order (850–1,050 nm),
# the predictors have a high degree of correlation. Hence, the data lie in a
# smaller dimension than the total number of predictors (215). Use PCA
# to determine the effective dimension of these data. What is the effective
# dimension? 


# Argument data=teacator created warning message:
# extra argument 'data' will be disregarded
# removed 'data=tecator' and warning resolved
tecator_pca <- prcomp(absorp, scale. = TRUE)
summary(tecator_pca)

# Visualize scatter plot relationship between PC1 and PC2
autoplot(tecator_pca) + ggtitle("Scatter plot of PC1 vs. PC2 in Percentages")

xyplot(PC2 ~ PC1, data = as.data.frame(tecator_pca$x),
       xlab = "Principal Component #1",
       ylab = "Principal Component #2",
       main = "Scatter plot of PC1 vs. PC2",
       type = c("p", "g"), aspect = 0.5)


# Visualize variance in each principal component
# compute the total variance for each component
percent_variance = (tecator_pca$sdev^2/sum(tecator_pca$sdev^2)) * 100
percent_variance[1:4]   # first 4 components account for 99% of variance

plot(percent_variance, xlab="Component", ylab="Percentage of Total Variance",
     type="l", main="Scree Plot")

# Zoomed in Scree plot for better view of "elbow"
plot(percent_variance, xlab="Component", ylab="Percentage of Total Variance",
     type="l", main="Scree Plot of Absorp", 
     xlim=c(1, 5),
     ylim=c(0, max(percent_variance[1:5])))


# (c) Split the data into a training and a test set, pre-process the data, and
# build each variety of models described in this chapter. For those models with
# tuning parameters, what are the optimal values of the tuning parameter(s)?

# Splitting data set will be easier on one data frame
tecator_data <- data.frame(absorp, fat_percentage = endpoints[,2])

set.seed(42)
trainIndex <- createDataPartition(tecator_data$fat_percentage, p=0.8, list=FALSE)
training_data <- tecator_data[trainIndex, ]
testing_data <- tecator_data[-trainIndex, ]


# Pre-processing
sum(is.na(tecator_data)) # check for missing values
str(training_data) # ensure columns are numerical

# Data is moderately and severely right-skewed based on output
skewness(training_data) 
histogram(training_data$fat_percentage,
          main = "Historgram Distribution of Fat Percentage",
          xlab = "Fat Percentage",
          ylab = "Frequency")
# Reshape the data to a long format for easier visualization
# Unable to view Fat Percentage column clearly in this format
reshaped <- pivot_longer(training_data, 
                          cols = -fat_percentage,
                          names_to = "Variable", 
                          values_to = "Value")
# Plot the histograms using ggplot2 with facet_wrap
ggplot(reshaped, aes(x = Value)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Histogram Distribution of Predictors in Training Data",
    x = "Predictors",
    y = "Frequency"
  ) +
  theme(
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5))

# Apply box-cox transformation to normalize data
boxcox_preprocess <- preProcess(training_data,
                                method = c("BoxCox", "center", "scale"))

training_data_transformed <- predict(boxcox_preprocess, training_data)


# Review transformed data
# Majority of skewness is near 0. 
skewness(training_data_transformed) 


# Review normalized visualizations
histogram(training_data_transformed$fat_percentage,
          main = "Historgram Distribution of Normalized Fat Percentage",
          xlab = "Fat Percentage",
          ylab = "Frequency")
second_reshaped <- pivot_longer(training_data_transformed, 
                         cols = -fat_percentage,
                         names_to = "Variable", 
                         values_to = "Value")
# Plot the histograms using ggplot2 with facet_wrap
ggplot(second_reshaped, aes(x = Value)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Histogram Distribution of Normalized Predictors in Training Data",
    x = "Predictors",
    y = "Frequency"
  ) +
  theme(
    axis.text.x = element_text(size = 5),
    axis.text.y = element_text(size = 5))

# Check for multi-colinearity
# Identify highly correlated features (correlation > 0.8)
trainingCorr <- cor(training_data_transformed)
corrplot(trainingCorr, order = "hclust", tl.cex = .35)

# Apply PCA to the transformed data to solve multi-colinearity issues
training_pca <- prcomp(training_data_transformed, scale. = TRUE)
summary(training_pca)

training_variance = (training_pca$sdev^2/sum(training_pca$sdev^2)) * 100
training_variance[1:4]   # first 4 components account for 99% of variance

plot(training_variance, xlab="Component", ylab="Percentage of Total Variance",
     type="l", main="Scree Plot")
# Zoomed in Scree plot for better view of "elbow"
plot(training_variance, xlab="Component", ylab="Percentage of Total Variance",
     type="l", main="Scree Plot of Training Data", 
     xlim=c(1, 5),
     ylim=c(0, max(training_variance[1:5])))

# Linear Regression
training_lm_model <- lm(fat_percentage ~ training_pca$x[, 1] +
                          training_pca$x[, 2] +
                          training_pca$x[, 3], data = training_data)

summary(training_lm_model)

# Assumptions Visuals 
par(mfrow = c(2, 2))
plot(training_lm_model) 

# Lasso Regression
# defining predictor and response variable
x <- as.matrix(training_pca$x[, 1:4])
y <- training_data$fat_percentage

# perform k-fold cross-validation to find optimal lambda value
lasso_cv <- cv.glmnet(x, y, alpha = 1)

# find optimal lambda value that minimizes test MSE
best_lambda <- lasso_cv$lambda.min
best_lambda
# produce plot of test MSE by lambda value
par(mfrow = c(1, 1))
plot(lasso_cv) 

# find coefficients of best model
lasso_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
print(coef(lasso_model))

# Build lasso model
train_predictions <- predict(lasso_model, s = best_lambda, newx = x)
train_predictions
# Calculate R squared for evaluation
sst_lasso <- sum((y - mean(y))^2)
sse_lasso <- sum((train_predictions - y)^2)
rsq_lasso <- 1 - sse_lasso/sst_lasso
rsq_lasso


# Ridge Regression

# perform cross-validation to find optimal lambda value
ridge_cv <- cv.glmnet(x, y, alpha = 0)
# find optimal lambda value that minimizes test MSE
best_lambda_ridge <- ridge_cv$lambda.min
best_lambda_ridge
# produce plot of test MSE by lambda value for Ridge
plot(ridge_cv)
# find coefficients of best Ridge model
ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
print(coef(ridge_model))
# Build lasso model
train_ridge_predictions <- predict(ridge_model, s = best_lambda_ridge, newx = x)
train_ridge_predictions
# Calculate R-squared for the training data for evaluation
sst_ridge <- sum((y - mean(y))^2)
sse_ridge <- sum((train_ridge_predictions - y)^2)
rsq_ridge <- 1 - sse_ridge/sst_ridge
rsq_ridge


### Exercise 6.2:

# (a) Start R and use these commands to load the data:

data(permeability)

# b) The fingerprint predictors indicate the presence or absence of substructures 
# of a molecule and are often sparse meaning that relatively few of the
# molecules contain each substructure. Filter out the predictors that have
# low frequencies using the nearZeroVar function from the caret package.
# How many predictors are left for modeling?
near_zero <- nearZeroVar(fingerprints)
filtered_fingerprints <- fingerprints[, -near_zero]
ncol(filtered_fingerprints)

# c) Split the data into a training and a test set, pre-process the data, and
# tune a PLS model. How many latent variables are optimal and what is
# the corresponding resampled estimate of R2?

set.seed(54)
# Combine predictors and the target variable into one data frame
combined_data <- data.frame(filtered_fingerprints, permeability = permeability)
# Split data into training and testing data
permeability_trainIndex <- createDataPartition(combined_data$permeability,
                                  p = 0.8,
                                  list = FALSE)
perm_training_data <- combined_data[permeability_trainIndex, ]
perm_testing_data <- combined_data[-permeability_trainIndex, ]

# pre-processing
sum(is.na(combined_data)) # check for missing values
str(perm_training_data) # ensure columns are numerical

# Data is moderately and severely right-skewed based on output
skewness(perm_training_data)
histogram(perm_training_data$permeability,
          main = "Historgram Distribution of Permeability",
          xlab = "Permeability",
          ylab = "Frequency")


### TODO: 

# Exercise 6.2: (c) in-progress, (d)
