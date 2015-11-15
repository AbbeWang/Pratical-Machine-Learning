# Question 1
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]


# Question 2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

sp <- concrete$Superplasticizer
summary(sp)
table(sp)
hist(sp)
lgsp <- log(sp)


# Qustion 3
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(training)

subtrain <- training[c(58:69)]
prComp <- prcomp(subtrain)
prComp$sdev
0.9*sum(prComp$sdev)<sum(prComp$sdev[c(1:8)])

# using caret
preProc <- preProcess(training[,c(58:69)], method="pca")
trainPC <- predict(preProc, training[,c(58:69)])
sdev <- sapply(trainPC[c(1:10)], sd)
0.9*sum(sdev)<sum(sdev[c(1:9)])


# Question 4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]

# using the predictors as they are
subtrain <- training[c(1,58:69)]
modelFit <- train(subtrain$diagnosis ~ ., method="glm", data=subtrain[,-1])
confusionMatrix(testing$diagnosis, predict(modelFit, testing[,-1]))


# pca - 80%
preProc <- preProcess(subtrain[,-1], method="pca", pcaComp = 8)
trainPC <- predict(preProc, subtrain[,-1])
sdev <- sapply(trainPC, sd)
0.8*sum(sdev)<sum(sdev[c(1:8)])
modelFit <- train(subtrain$diagnosis ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, testing[,-1])
confusionMatrix(testing$diagnosis, predict(modelFit, testPC))

