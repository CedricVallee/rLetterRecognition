data = read.csv('Letters.csv')
summary(data)
str(data)

# a.i) Create variable IsB
data$IsB=(data$Letter=="B")
summary(data)

# a.i) Randomly split the dataset
library(caTools)
set.seed(9)
spl = sample.split(data, SplitRatio = 0.50)
dataTrain = subset(data, spl==TRUE)
dataTest = subset(data, spl==FALSE)
summary(dataTest)

# a.ii) Build a CART without overfitting or underfitting
library(rpart)
library(rpart.plot)
library(randomForest)
BTree1 = rpart(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                    + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=20)
prp(BTree1)
BTree2 = rpart(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
               + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=25)
prp(BTree2)
BTree3 = rpart(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
               + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=30)
prp(BTree3)
BTree4 = rpart(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
               + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=50)
prp(BTree4)
BTree5 = rpart(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
               + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=100)
prp(BTree5)

BPredict1 = predict(BTree1, newdata=dataTest,type="class")
table(dataTest$IsB, BPredict1)
BPredict2 = predict(BTree2, newdata=dataTest,type="class")
table(dataTest$IsB, BPredict2)
BPredict3 = predict(BTree3, newdata=dataTest,type="class")
table(dataTest$IsB, BPredict3)
BPredict4 = predict(BTree4, newdata=dataTest,type="class")
table(dataTest$IsB, BPredict4)
BPredict5 = predict(BTree5, newdata=dataTest,type="class")
table(dataTest$IsB, BPredict5)

# a.iii) Build a random forest model
dataTrain$IsB = as.factor(dataTrain$IsB)   #We store the outcome variable as a factor
dataTest$IsB = as.factor(dataTest$IsB)
BForest = randomForest(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                       + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 200, nodesize = 15)
BPredictForest = predict(BForest, newdata = dataTest)
table(dataTest$IsB,BPredictForest)

BForest2 = randomForest(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                       + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 200, nodesize = 50)
BPredictForest2 = predict(BForest2, newdata = dataTest)
table(dataTest$IsB,BPredictForest2)

BForest3 = randomForest(IsB ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                       + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 100, nodesize = 15)
BPredictForest3 = predict(BForest3, newdata = dataTest)
table(dataTest$IsB,BPredictForest3)

# b.i) Baseline model for Letter
summary(dataTest)

# b.ii) Build a CART
ABPRTree1 = rpart(Letter ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
               + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=20)
prp(ABPRTree1)
ABPRPredict1 = predict(ABPRTree1, newdata=dataTest,type="class")
table(dataTest$Letter, ABPRPredict1)

ABPRTree2 = rpart(Letter ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                  + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, method="class", data=dataTrain, minbucket=10)
prp(ABPRTree2)
ABPRPredict2 = predict(ABPRTree2, newdata=dataTest,type="class")
table(dataTest$Letter, ABPRPredict2)

# b.iii) Build a random forest model
dataTrain$Letter = as.factor(dataTrain$Letter)   #We store the outcome variable as a factor
dataTest$Letter = as.factor(dataTest$Letter)
ABPRForest = randomForest(Letter ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                       + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 200, nodesize = 15)
ABPRPredictForest = predict(ABPRForest, newdata = dataTest)
table(dataTest$Letter,ABPRPredictForest)

ABPRForest2 = randomForest(Letter ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                          + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 100, nodesize = 15)
ABPRPredictForest2 = predict(ABPRForest2, newdata = dataTest)
table(dataTest$Letter,ABPRPredictForest2)

ABPRForest3 = randomForest(Letter ~ Xbox + Ybox + Width + Height + Onpix + Xbar + Ybar + X2bar + Y2bar + XYbar + X2Ybar 
                           + XY2bar + Xedge + XedgeYcor + Yedge + YedgeXcor, data=dataTrain, ntree = 100, nodesize = 50)
ABPRPredictForest3 = predict(ABPRForest3, newdata = dataTest)
table(dataTest$Letter,ABPRPredictForest3)
