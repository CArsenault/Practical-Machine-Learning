library(caret)
library(rattle)
setwd("C:/Users/Songbird2015/Desktop/Coursera/Practical Machine Learning")
DatTrain <- read.csv(file="pml-training.csv", header=TRUE, sep=",")
dim(DatTrain)

DatVal <- read.csv(file="pml-testing.csv", header=TRUE, sep=",")
dim(DatVal)
##Data Partition
set.seed(1000)
DatTrim <-createDataPartition(y=DatTrain$classe, p=.75, list=FALSE)
TrainSamp <- DatTrain[DatTrim,]
TestSamp <- DatTrain[-DatTrim,]

##Get rid of NA and missing values                                      
DatTrim_NZ <- sapply(names(DatVal), function(x) all(is.na(DatVal[,x])==TRUE))
NZTestDat <- names(DatTrim_NZ)[DatTrim_NZ==FALSE]
NZTestDat <- NZTestDat[-(1:7)]
NZTestDat <- NZTestDat[1:(length(NZTestDat)-1)]

fitControl <- trainControl(method="cv", number=5)
model_cart <- caret::train(
        classe ~ ., 
        data=TrainSamp[, c('classe', NZTestDat)],
        trControl=fitControl,
        method='rpart'
)
save(model_cart, file='./ModelFitCART.RData')
model_gbm <- caret::train(
        classe ~ ., 
        data=TrainSamp[, c('classe', NZTestDat)],
        trControl=fitControl,
        method='gbm'
)
save(model_gbm, file='./ModelFitGBM.RData')
model_rf <- caret::train(
        classe ~ ., 
        data=TrainSamp[, c('classe', NZTestDat)],
        trControl=fitControl,
        method='rf',
        ntree=100
)
save(model_rf, file='./ModelFitRF.RData')

##Cross validation
fitControl <- trainControl(method='cv', number = 3)

##look at OOS error
predCART <- predict(model_cart, newdata=TestSamp)
cmCART <- confusionMatrix(predCART, TestSamp$classe)
predGBM <- predict(model_gbm, newdata=TestSamp)
cmGBM <- confusionMatrix(predGBM, TestSamp$classe)
predRF <- predict(model_rf, newdata=TestSamp)
cmRF <- confusionMatrix(predRF, TestSamp$classe)
AccuracyResults <- data.frame(
        Model = c('CART', 'GBM', 'RF'),
        Accuracy = rbind(cmCART$overall[1], cmGBM$overall[1], cmRF$overall[1])
)
print(AccuracyResults)

predictTEST <- predict(model_rf, newdat=DataVal)
predictTEST

ValidationPredictionResults <- data.frame(
        problem_id=DatVal$problem_id,
        predicted=predictTEST
)
print(ValidationPredictionResults)
