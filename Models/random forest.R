# Load the required library
library(randomForest)

# Define the h_rf function
h_rf <- function(){
  # Read the dataset
  heart_data_rf <- read.csv('cleaned_data.csv')
  
  # Factorize the variables
  heart_data_rf$gender <- as.factor(heart_data_rf$gender)
  heart_data_rf$chestpain <- as.factor(heart_data_rf$chestpain)
  heart_data_rf$fastingbloodsugar <- as.factor(heart_data_rf$fastingbloodsugar)
  heart_data_rf$restingrelectro <- as.factor(heart_data_rf$restingrelectro)
  heart_data_rf$exerciseangia <- as.factor(heart_data_rf$exerciseangia)
  heart_data_rf$slope <- as.factor(heart_data_rf$slope)
  heart_data_rf$target <- as.factor(heart_data_rf$target)
  
  # Split the data into train and test sets
  set.seed(123)
  split_rf <- sample.split(heart_data_rf$target, SplitRatio = 0.80)
  train_set_rf <- subset(heart_data_rf, split_rf == TRUE)
  test_set_rf <- subset(heart_data_rf, split_rf == FALSE)
  
  # Build the Random Forest model
  RandomForest_Classifier <- randomForest(target ~ ., data = train_set_rf)
  
  # Make predictions on the test set
  rf_pred <- predict(RandomForest_Classifier, test_set_rf)
  
  # Create confusion matrix
  cm_rf <- table(test_set_rf$target, rf_pred)
  return(cm_rf)
}

# Define the hrf_accuracy function
hrf_accuracy <- function(cm_rf){
  # Calculate accuracy
  acr_rf <- sum(diag(cm_rf))/sum(cm_rf)
  return(acr_rf * 100)
}

# Get confusion matrix
cm <- h_rf()

# Calculate accuracy
accuracy <- hrf_accuracy(cm)
print(accuracy)
