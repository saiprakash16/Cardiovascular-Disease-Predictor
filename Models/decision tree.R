getwd()
heart_dataset = read.csv('cleaned_data.csv')
# To see dataset
View(heart_dataset)
#Structure of dataset
str(heart_dataset)
#Converting character and int values to factors.

heart_dataset$gender = as.factor(heart_dataset$gender)

heart_dataset$target = as.factor(heart_dataset$target)

heart_dataset$chestpain = as.factor(heart_dataset$chestpain)

heart_dataset$fastingbloodsugar = as.factor(heart_dataset$fastingbloodsugar)

heart_dataset$exerciseangia = as.factor(heart_dataset$exerciseangia)

heart_dataset$restingrelectro = as.factor(heart_dataset$restingrelectro)

heart_dataset$slope = as.factor(heart_dataset$slope)
heart_dataset$target = as.factor(heart_dataset$target)                                

library(caTools)
set.seed(1200000)
split=sample.split(heart_dataset,SplitRatio=0.75)
training_set=subset(heart_dataset,split==TRUE)
testing_set=subset(heart_dataset,split==FALSE)


#Decision tree classifier

# install.packages('rpart')
library(rpart)
decision_classifier1 = rpart(target ~ ., 
                             control= rpart.control(minsplit = 20, cp = 0.01,  maxdepth= 30),
                             data = training_set)
library(rpart.plot)
rpart.plot(decision_classifier1)

# Predicting the Test set results
y_pred1 = predict(decision_classifier1, newdata = testing_set, type = 'class')

# Making the Confusion Matrix
cm_d2 = table(testing_set[, 12], y_pred1)
cm_d2
accr_d1 =  (sum(diag(cm_d2))/sum(cm_d2))
(accr_d1*100)

