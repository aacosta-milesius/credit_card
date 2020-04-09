library(ranger)
library(caret)
library(data.table)
creditcard_data <- read.csv("C:/Users/andre/Downloads/creditcard.csv")

# Get summary statistics on the data
table(creditcard_data$Class)
summary(creditcard_data$Amount)
names(creditcard_data)
var(creditcard_data$Amount)

# Standardize the data
creditcard_data$Amount=scale(creditcard_data$Amount)
NewData=creditcard_data[,-c(1)]

# After we have standardized our entire dataset, we will split our 
# dataset into training set as well as test set with a split ratio 
# of 0.80. This means that 80% of our data will be attributed to the
# train_data whereas 20% will be attributed to the test data. 
# We will then find the dimensions using the dim() function
library(caTools)
set.seed(123)
data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)

# Fitting Logistic Regression Model
Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

# In order to assess the performance of our model, we will 
# delineate the ROC curve. ROC is also known as Receiver Optimistic 
# Characteristics. For this, we will first import the ROC package 
# and then plot our ROC curve to analyze its performance.
library(pROC)
lr.predict <- predict(Logistic_Model,train_data, probability = TRUE)
auc.gbm = roc(test_data$Class, lr.predict, plot = TRUE, col = "blue")

