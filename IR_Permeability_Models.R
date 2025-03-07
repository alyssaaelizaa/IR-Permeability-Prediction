# load in necessary packages
install.packages('ggfortify')
library(ggfortify)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)

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
     type="l", main="Scree Plot", 
     xlim=c(1, 5),
     ylim=c(0, max(percent_variance[1:5])))


# (c) Split the data into a training and a test set, pre-process the data, and
# build each variety of models described in this chapter. For those models with
# tuning parameters, what are the optimal values of the tuning parameter(s)?



### TODO: 
# Exercise 6.1: (c) - (e)
# Exercise 6.2: (a) - (d)
