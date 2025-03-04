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

# Visualize relationship between PC1 and PC2

# comparing differences between scatter plots
autoplot(tecator_pca)
transparentTheme(pchSize = ., trans = .20)

# Visualize variance in each principal component
# TODO: build scree plot for variance visualization



### TODO: 
# Exercise 6.1: (c) - (e)
# Exercise 6.2: (a) - (d)
