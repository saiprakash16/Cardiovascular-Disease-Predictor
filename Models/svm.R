# To get the working directory
getwd()

#importing the dataset
HeartData_svm = read.csv('cleaned_data.csv')

# To see the dataset
View(HeartData_svm)

#To find the Structure of dataset
str(HeartData_svm)

#To find the summary of the data
summary(HeartData_svm)
#To see first 3 and last 3 values of the dataset
head(HeartData_svm,3)
tail(HeartData_svm,3)

#Factorizing the variables
HeartData_svm$gender <- as.factor(HeartData_svm$gender)
HeartData_svm$chestpain <- as.factor(HeartData_svm$chestpain)
HeartData_svm$fastingbloodsugar <- as.factor(HeartData_svm$fastingbloodsugar)
HeartData_svm$restingrelectro <- as.factor(HeartData_svm$restingrelectro)
HeartData_svm$exerciseangia <- as.factor(HeartData_svm$exerciseangia)
HeartData_svm$slope <- as.factor(HeartData_svm$slope)
HeartData_svm$target <- as.factor(HeartData_svm$target)

#Again checking the structure of the data
str(HeartData_svm)

#Splitting train and test data

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split_svm=sample.split(HeartData_svm,SplitRatio=0.80)
svm_train_set=subset(HeartData_svm,split_svm==TRUE)
svm_test_set=subset(HeartData_svm,split_svm==FALSE)

# Creating SVM classifier model 

library(e1071)
svm_classifier = svm(formula = target ~ .,
                     data = svm_train_set,
                     type = 'C-classification',
                     kernel = 'linear')

# Predicting the Test set results
svm_pred=predict(svm_classifier, svm_test_set)
svm_pred

#Gives first 6 and last 6 rows of the prediction set results
head(svm_pred)
tail(svm_pred)
# creating confusion matrix and finding accuracy of the model
cm_svm = table(svm_test_set[,12], svm_pred)
cm_svm
acr_svm = (sum(diag(cm_svm))/sum(cm_svm))
(acr_svm*100)

#Accuracy, Precision, Recall and Fscore
TN =cm_svm[1,1]
TP =cm_svm[2,2]
FP =cm_svm[1,2]
FN =cm_svm[2,1]

precision_svm =(TP)/(TP+FP)
precision_svm*100
accuracy_svm  =(TP+TN)/(TP+TN+FP+FN)
accuracy_svm*100
recall_svm=(TP)/(TP+FN)
recall_svm*100
Fscore_svm=2*((precision_svm*recall_svm)/(precision_svm+recall_svm))
Fscore_svm*100

