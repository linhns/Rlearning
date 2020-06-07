library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~.,data=training, method="glm")
modelFit

predictions <- predict(modelFit,newdata=testing)
predictions

confusionMatrix(predictions,testing$type)

# Preprocessing with PCA
typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainingIL <- training[, grep("^IL|diagnosis", names(training))]
testingIL <- testing[, grep("^IL|diagnosis", names(testing))]

model1 <- train(diagnosis ~ ., data = trainingIL, method = "glm")
predictions1 <- predict(model1, newdata = testingIL)
confusionMatrix(predictions1, testingIL$diagnosis)

model2 <- train(diagnosis ~ ., data = trainingIL, method = "glm", 
                preProcess = "pca",
                trControl = trainControl(preProcOptions = list(thresh = 0.8)))
predictions2 <- predict(model2, newdata = testingIL)
confusionMatrix(predictions2, testingIL$diagnosis)



procObj <- preProcess(trainingIL, method = "pca", thresh = 0.9)

data(segmentationOriginal)
set.seed(125)

training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]

fit <- train(Class ~ ., data = training, method = "rpart")
plot(fit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

## Combining predictors
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
# Create a building data set and validation set
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

dim(training)
dim(testing)
dim(validation)

mod1 <- train(wage ~.,method="glm",data=training)
mod2 <- train(wage ~.,method="rf",
              data=training, 
              trControl = trainControl(method="cv"),number=3)

pred1 <- predict(mod1,testing); pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)

pred1V <- predict(mod1,validation); pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)

sqrt(sum((pred1V-validation$wage)^2))
sqrt(sum((pred2V-validation$wage)^2))
sqrt(sum((combPredV-validation$wage)^2))


## Forecasting
library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="yahoo", from = from.dat, to = to.dat)
head(GOOG)

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

plot(decompose(ts1),xlab="Years+1")

ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))

plot(ts1Train)
lines(reorder(ts1Train,order=3),col="red")

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")

accuracy(fcast,ts1Test)