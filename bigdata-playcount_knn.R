#Big Data Project - Full Dataset

raw = read.csv(file.choose())
attach(data)
sad <- as.factor(sad)
data$sad <- as.factor(data$sad)
data$playcount = 0

#take only complete cases
raw.comp = raw[complete.cases(raw),]
dim(raw.comp) #number of complete cases: 340,826
dim(raw.comp[raw.comp$sad == 1,]) #number of sad songs: 13,762

#find unique cases and duplicates
msd <- raw.comp[!duplicated(raw.comp[,3]),] #remove all cases where the song ID is the same
dim(msd) #now the number of cases is 340,192
dim(msd[msd$sad == 1,]) #now, the number of sad songs is 13,617

#make sure that variable are correct
class(msd$sad)
msd$sad <- as.factor(msd$sad)
msd$beautiful <- as.factor(msd$beautiful)
msd$happy <- as.factor(msd$happy)
msd$key <- as.factor(msd$key)
msd$mode <- as.factor(msd$mode)
summary(msd)

#find and remove cases where tempo = 0 
length(which(msd$tempo == 0))
msd[237,]
subset = which(msd$tempo == 0)
#row_sub = apply(msd, 1, function(row) all(row !=0 ))
#msd.sub = msd[apply(msd[,9],1,function(x) !all(x==0)),]
#msd.sub = apply(msd,1,function(x)msd[!subset,]
#msd.sub = msd[rowSums(msd[, -1] > 0) != 0, ]
msd.sub = msd[-subset,]
length(which(msd.sub$tempo == 0))
msd <- msd.sub
summary(msd)

#find cases where familiarity is 0
length(which(msd$familiarity == 0))
which(msd$familiarity == 0)
msd[718,]

#balance the dataset by undersampling the nonzero cases
indx <- which(msd$sad == 0)
sad_num <- dim(msd[msd$sad == 1,])[1]
set.seed(12)
msdNS <- msd[sample(indx,sad_num),]
msdS <- msd[msd$sad == 1,]
summary(msdNS)
dim(msdS)
dim(msdNS)
msdSamp <- rbind(msdNS,msdS)
dim(msdSamp)
prop.table(table(msdSamp$sad))
summary(msdSamp)

#divide the data into training and test based on an 80/20 split
num = round((dim(msdSamp)[1])*(.8))
num #training = 271,595
set.seed(13)
train = sample(dim(msdSamp)[1],num)
msdSamp.train = msdSamp[train,]
table(msdSamp.train$sad) #there are 10,900 sad songs in the training set
sad_num <- dim(msdSamp.train[msdSamp.train$sad == 1,])[1]
msdSamp.test = msdSamp[-train,]
dim(msdSamp.test) #test split = 67,899
class(msdSamp.test$sad)
prop.table(table(msdSamp.train$sad))
prop.table(table(msdSamp.test$sad))


#Dealing with unbalanced data 
#the number of songs classified as sad is 83/3169
#therefore, we need to oversample the unrepresented cases
#One way of doing this is to use SMOTE

install.packages("DMwR")
library("DMwR")
msdSmote <- msd.train
ncols<-ncol(msdSmote)
msdSmote<-cbind(msdSmote[2:ncols],msdSmote[1])
msdSmote.train<-SMOTE(sad~familiarity+hotness+tempo+duration+mode+key+loudness,msd.train,perc.over = 100,perc.under=200) #undersample the non-sad cases
#msd.smote<-SMOTE(sad~familiarity+hotness+tempo+duration+mode+key+loudness,msdSmote,k=5,perc.over = 1400,perc.under=140) #oversample the sad cases
table(msd.smote$sad) #now the data is 21,802 not sad, 
table(msdSmote.train$sad)
dim(msd.smote)
msdSmote<-cbind(msd.smote[ncols],msd.smote[1:ncols-1]) #now there are equal number of sad and not sad

#Another way of dealing with assymmetric class size 
#is to incrase the weight of certain factors 
m <- svm(x, y, class.weights = c(A = 0.3, B = 0.7)) #this weights B more than A


#run the Support Vector Machine using linear kernel 
install.packages("e1071")
library("e1071")
set.seed(1234)
lsvm.sad=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdSamp.train, kernel="linear",cost=1)
plot(svm.sad,msdSmote.train)
summary(lsvm.sad)
train.pred = predict(lsvm.sad,msdSamp.train)
table(msdSamp.train$sad,train.pred) #Percent correct 15949/21,766 = 73.27% (WORSE THAN RADIAL)
test.pred = predict(lsvm.sad,msdSamp.test)
table(msdSamp.test$sad,test.pred) #Percent correct: 3958/5442, 72.7% (WORSE THAN RADIAL)

# Obtain feature weights
weights.sad.linear = t(lsvm.sad$coefs) %*% lsvm.sad$SV
weights.sad.linear

#run SVM using linear kernel with SCALED and CENTERED DATA
install.packages("e1071")
library("e1071")
set.seed(1234)

install.packages("caret")
library(caret)
msdSamp.train$tempo <-scale(msdSamp.train$tempo) #Method 1

preObj1 <- preProcess(msdSamp.train2[,c(0:4,7)], method=c("center", "scale")) #Method 2
msdNorm.train <- predict(preObj1,msdSamp.train2[,c(0:4,7)])
sd(msdNorm.train$loudness)
summary(msdNorm.train)
msdNorm.train <- cbind(msdNorm.train,msdSamp.train2$mode,msdSamp.train2$key,msdSamp.train2$sad)
names(msdNorm.train)[8] <- "sad"
names(msdNorm.train)[7] <- "key"
names(msdNorm.train)[6] <- "mode"

preObj2 <- preProcess(msdSamp.test[,c(6,8,9,11,12)], method=c("center", "scale")) #Method 2
msdNorm.test <- predict(preObj2,msdSamp.test[,c(6,8,9,11,12)])
sd(msdNorm.test$loudness)
summary(msdNorm.test)
dim(msdNorm.test)
msdNorm.test <- cbind(msdNorm.test,msdSamp.test$mode,msdSamp.test$key,msdSamp.test$sad)
names(msdNorm.test)[8] <- "sad"
names(msdNorm.test)[7] <- "key"
names(msdNorm.test)[6] <- "mode"

class(msdNorm.train$sad)
class(msdNorm.test$sad)

lsvm.norm=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdNorm.train, kernel="linear",cost=100)
summary(lsvm.norm)
train.pred = predict(lsvm.norm,msdNorm.train)
table(msdNorm.train$sad,train.pred) #Percent correct 15947/21,766 = 73.27% (SAME AS BEFORE NORM)
test.pred = predict(lsvm.norm,msdNorm.test)
table(msdNorm.test$sad,test.pred) #Percent correct: 3957/5442, 72.7% (SAME AS BEFORE NORM)

# Obtain feature weights
weights.norm.linear = t(lsvm.norm$coefs) %*% lsvm.norm$SV
weights.norm.linear <- as.data.frame(weights.norm.linear)


#ROC Curves
install.packages("pROC")
library(pROC)
auc <- roc(msdSamp.test$sad,test.pred)

library (ROCR)
rocplot =function (pred , truth , ...){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf ,...)}

svmsad.opt=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdSamp.train, kernel="radial",gamma=1,cost=1,decision.values=T)
fitted = attributes(predict(svmsad.opt,msdSamp.train,decision.values =TRUE))$decision.values
par(mfrow =c(1,2))
rocplot(fitted,msdSamp.train$sad, main="Training Data")

fitted = attributes(predict(svmsad.opt,msdSamp.test,decision.values =TRUE))$decision.values
rocplot(fitted,msdSamp.train$sad,main=" Training Data")
rocplot(fitted,msdSamp.test$sad,add=T,col="red")


#make the training set with only the variables that we are interested in
keeps <- c("familiarity","hotness","tempo","duration","mode","key","loudness","sad")
msdSamp.train2 <- msdSamp.train[keeps]
msdSamp.test2 <- msdSamp.train[keeps]
head(msdSamp.train2)

#cross validation to find the best choice for lambda and cost
set.seed(11)
tune.rad = tune(svm,sad~.,data=msdSamp.train,kernel="radial",ranges = list(cost=c(0.1,1,10,100),gamma=c(0.5,1,2,3)))

tune.out = tune.svm(sad~., data = msdSmote.train, kernel = "radial", ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

summary(tune.out)
svmbest=svm(sad~familiarity+hotness+tempo+duration+mode+key+playcount,data=msdSamp.train, kernel="radial",gamma=tune.out$best.parameters$gamma,cost=tune.out$best.parameters$cost, cross = 10)
summary(svmbest)
train.pred = predict(svmbest,msdSamp.train)
table(msdSamp.train$sad,train.pred)
test.pred = predict(svmbest,msdSamp.test)
table(msdSamp.test$sad,test.pred)


#Try SVM using Caret Package 
install.packages('caret')
library(caret)
tunegrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))
control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs=TRUE,summaryFunction=twoClassSummary)
set.seed(825)
svm.caret <- train(sad~familiarity+hotness+tempo+duration+mode+key+loudness, data=msdSamp.train, method="svmRadial", trControl=control, tunelength = 8, metric = "ROC")
testProbs <- predict(svm.caret, newdata = msdSamp.test, type = "prob")

confusionMatrix(data = plsClasses, testing$Class)


#KNN
library(class)
attach(msdSamp.train)
attach(msdSamp.test)
train.x = msdSamp.train[,keeps]
train.x <- train.x[,0:7]
train.sad <- msdSamp.train$sad

test.x = msdSamp.test[,keeps]
test.x <- test.x[,0:7]

train.x = msdNorm.train[,keeps]
train.x <- train.x[,0:7]
train.sad <- msdNorm.train$sad
test.x = msdNorm.test[,keeps]
test.x <- test.x[,0:7]

set.seed(12345)
knn.pred1=knn(train.x,test.x,train.sad,k=1)
table(knn.pred,msdNorm.test$sad)
mean(knn.pred==msdNorm.test$sad)

knn.pred2=knn(train.x,test.x,train.sad,k=2)
table(knn.pred2,msdSamp.test$sad)
mean(knn.pred2==msdSamp.test$sad)

knn.pred3=knn(train.x,test.x,train.sad,k=3)
table(knn.pred3,msdSamp.test$sad)
mean(knn.pred3==msdSamp.test$sad)

knn.pred5=knn(train.x,test.x,train.sad,k=5)
table(knn.pred5,msdSamp.test$sad)
mean(knn.pred5==msdSamp.test$sad)

knn.pred10=knn(train.x,test.x,train.sad,k=200) #best appears to be with k=200 and its only 57% accurate
table(knn.pred10,msdSamp.test$sad)
mean(knn.pred10==msdSamp.test$sad)

#Linear Regression/Logistic Regression with playcount for sad songs 
install.packages('languageR')
library(languageR)
install.packages('lme4')
library(lme4)
s.h = which(msd$sad==1 & msd$happy ==1)
summary(msd)

#take only the ones that are sad and not sad and happy together
#find examples of both sad and happy 
s.h = which(msd$sad==1 & msd$happy ==1)
sad_hap = tweets[s.h,2:3]
msd.hs = msd[-s.h,] #take only cases that are not both sad and happy
msd.hs$valence <- ifelse(msd.hs$sad == 1, "1",
                         ifelse(msd.hs$happy == 1, "0","NA"))
msd.hs$valence <- as.factor(msd.hs$valence)
summary(msd.hs)
which(msd.hs$sad==1 & msd.hs$happy ==1)
tweets.hs = tweets.hs[(tweets.hs$valence==0 | tweets.hs$valence == 1),]
dim(tweets.hs)

#Predicting playcount 
lm.fit <- lm(playcount~sad, data = msd.hs)
summary(lm.fit)

lm.fit <- lm(playcount~beautiful, data = msd.hs)
summary(lm.fit)

lm.fit <- lm(playcount~duration+mode+tempo+loudness, data = msd.hs)
summary(lm.fit)

lm.fit <- lm(playcount~duration+mode+tempo+loudness, data = msd.hs)
summary(lm.fit)

lm.fit1 <- lm(playcount~familiarity+hotness+duration+mode+tempo+loudness, data = msd.hs)
summary(lm.fit1)

lm.fit2 <- lm(playcount~sad+beautiful+happy, data = msd.hs)
summary(lm.fit2) #all three are significant, suggesting that just tagging a song as something is associated with higher play count

anova(lm.fit,lm.fit1)

#select only sad songs, and look at the linear regression for play count
sad <- which(msd.hs$sad == 1)
head(sad)
msd.sad <- msd.hs[sad,]
summary(msd.hs$sad)
summary(msd.sad)

lm.sad <- lm(playcount~familiarity+hotness+duration+mode+tempo+loudness, data = msd.sad)
summary(lm.sad) #only loudness was significant with playcount

lm.sad <- lm(playcount~duration+mode+tempo+loudness+key, data = msd.sad)
summary(lm.sad)

lm.sad <- lm(playcount~familiarity+hotness+beautiful+loudness+duration+tempo, data = msd.sad)
summary(lm.sad)

lm.sad <- lm(playcount~beautiful, data = msd.sad)
summary(lm.sad) #songs tagged beautiful are more likely to have higher play count (when already tagged sad)

lm.sadall <- lm(playcount~beautiful+duration+mode+tempo+loudness, data =msd.sad)
summary(lm.sadall) 
#with key included: Multiple R-squared:  0.007287,  Adjusted R-squared:  0.005974
#with key not included: Multiple R-squared:  0.006845,  Adjusted R-squared:  0.006435 


anova(lm.sadall,lm.sad)
anova(lm.sad,lm.sadall)

#run a SVM regression and regression tree
#split msd.sad into train and test 
num = round((dim(msd.sad)[1])*(.8))
num #training = 9,690
set.seed(13)
train = sample(dim(msd.sad)[1],num)
msdSad.train = msd.sad[train,]
msdSad.test = msd.sad[-train,]
dim(msdSad.test) #test split = 2,422
class(msdSnorm.test$beautiful)
prop.table(table(msdSad.train$beautiful))
prop.table(table(msdSad.test$beautiful))
summary(msdSad.train)
str(msdSad.train)
str(msdSad.test)

library(randomForest)
set.seed (1)
bag.count = randomForest(playcount~familiarity+hotness+tempo+duration+mode+key+loudness+beautiful,data=msdSad.train,mtry=3,importance =TRUE)
plot(bag.count) #out of bag (OOG) error estimate 
importance(bag.count)

yhat.bagsad = predict(bag.count,newdata=msdSad.test)
mean((yhat.bagsad - msdSad.test$playcount)^2)

par(mfrow=c(2,1))
par(pty="s")
varImpPlot(bag.count, type=1, pch=19, col=1, cex=.5, main="")
varImpPlot(bag.count, type=2, pch=19, col=1, cex=.5, main="")

