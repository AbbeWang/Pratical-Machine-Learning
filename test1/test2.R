# SPAM Example: Data splitting
library(caret)
library(kernlab)
inTrain <- createDataPartition(y = spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# SPAM Example: Fit a model
set.seed(32343)
modelFit <- train(type ~., data = training, method = "glm")
modelFit
modelFit$finalModel

# SPAM Example: Prediction
predictions <- predict(modelFit, newdata = testing)
predictions

# SPAM Example: Confusion Matrix
confusionMatrix(predictions, testing$type)

