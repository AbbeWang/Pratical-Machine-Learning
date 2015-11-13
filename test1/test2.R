# SPAM Example: Data splitting
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

# SPAM Example: K-fold
set.seed(32323)
folds <- createFolds(y = spam$type, k=10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# SPAM Example: Return test
set.seed(32323)
folds <- createFolds(y = spam$type, k=10, list = TRUE, returnTrain = FALSE)
sapply(folds, length)
folds[[1]][1:10]

# SPAM Example: Resampling
set.seed(32323)
folds <- createResample(y=spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

# SPAM Example: Time Slices
set.seed(32323)
tme <- 1:10000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)
folds$train[[1]]
folds$test[[1]]


# SPAM Example: Fit a model
modelFit <- train(type ~., data = training, method = "glm")
args(train.default)
args(trainControl)

set.seed(1235)
modelFit2 <- train(type ~., data = training, method = "glm")
modelFit2

modelFit$finalModel



# SPAM Example: Prediction
predictions <- predict(modelFit, newdata = testing)
predictions


# Example: Wage data
library(ISLR)
library(ggplot2)
library(caret)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training)
dim(testing)

# Feature plot
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot = "pairs")
# Qplot
qplot(age, wage, data = training)

# Qplot with color
qplot(age, wage, colour=jobclass, data=training)

# Add regression smoothers
qq <- qplot(age,wage,colour=education, data=training)
qq + geom_smooth(method="lm",formula=y~x)


# cut2, making factors
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
table(cutWage)

# Boxplots with cut2


# SPAM Example: Confusion Matrix
confusionMatrix(predictions, testing$type)

