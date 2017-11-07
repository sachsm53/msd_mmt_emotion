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

summary(msd)

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
train = sample(dim(msdSamp)[1],num)
msdSamp.train = msdSamp[train,]
table(msdSamp.train$sad) #there are 10,900 sad songs in the training set
sad_num <- dim(msdSamp.train[msdSamp.train$sad == 1,])[1]
msdSamp.test = msdSamp[-train,]
dim(msdSamp.test) #test split = 67,899
class(msdSamp.test$sad)
prop.table(table(msdSamp.train$sad))
prop.table(table(msdSamp.test$sad))

#balance the dataset by undersampling the nonzero cases
indx <- which(msd.train$sad == 0)
msdNS.train <- msd.train[sample(indx,sad_num),]
msdS.train <- msd.train[msd.train$sad == 1,]
summary(msdNS.train)
dim(msdS.train)
dim(msdNS.train)
msdSamp.train <- rbind(msdNS.train,msdS.train)
dim(msdSamp.train)
prop.table(table(msdSamp.train$sad))
summary(msdSamp.train)


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

dim(msdSamp.train)

#run the Support Vector Machine using radial kernel 
install.packages("e1071")
library("e1071")
set.seed(1234)
svm.sad=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdSamp.train, kernel="radial",gamma=1,cost=1)
plot(svm.sad,msdSmote.train)
summary(svm.sad)
train.pred = predict(svm.sad,msdSamp.train)
table(msdSamp.train$sad,train.pred) #Percent correct 17,966/21,800 = 82.4%
test.pred = predict(svm.sad,msdSamp.test)
table(msdSamp.test$sad,test.pred) #Percent correct: 49373/67899, 72.7% (this is without undersampling testing)
table(msdSamp.test$sad,test.pred) #Percent correct: 4202/5442, 77.2% (with undersampling the testing)

dim(msdSamp)

#ROC Curves
install.packages("pROC")
library(pROC)
auc <- roc(msdSamp$sad,test.pred)

library (ROCR)
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth )
  perf = performance (predob , "tpr ", "fpr ")
  plot(perf ,...)}
svmsad.opt=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdSamp.train, kernel="radial",gamma=1,cost=1,decision.values=T)
fitted = attributes(predict(svmsad.opt,msdSamp.train, decision.values =TRUE))$decision.values
par(mfrow =c(1,2))
rocplot(fitted,msdSamp.train$sad, main="Training Data")

fitted = attributes(predict(svmsad.opt,msdSamp.train, decision.values =TRUE))$decision.values
par(mfrow =c(1,2))
rocplot(fitted,msdSamp.train$sad, main="Training Data")

#make the training set with only the variables that we are interested in
keeps <- c("familiarity","hotness","tempo","duration","mode","key","loudness","sad")
msdSmote.train <- msdSmote[keeps]
msdSamp.train <- msdSamp.train[keeps]
msdSamp.test <- msdSamp.test[keeps]
testing = msdSamp.train[keeps]
head(testing)

#cross validation to find the best choice for lambda and cost
set.seed(11)
tune.out = tune(svm,sad~., data = msdSamp.train, kernel = "radial", ranges = list(cost=c(0.01,0.1,1,10,100),gamma=c(0.5,1,2,3,4)))

tune.out = tune.svm(sad~., data = msdSmote.train, kernel = "radial", ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))

summary(tune.out)
svmbest=svm(sad~.,data=msdSamp.train, kernel="radial",gamma=tune.out$best.parameters$gamma,cost=tune.out$best.parameters$cost)
summary(svmbest) #gamma = 
train.pred = predict(svmbest,msdSamp.train)
table(msdSamp.train$sad,train.pred) #Percent Correct: 16,384/21766 = 75.27%
test.pred = predict(svmbest,msdSamp.test)
table(msdSamp.test$sad,test.pred) #Percent Correct: 3998/5442 = 73.5%


#Try SVM using Caret Package 
install.packages('caret')
library(caret)
tunegrid <- data.frame(.sigma = sigDist[1], .C = 2^(-2:7))
control <- trainControl(method="repeatedcv", number=10, repeats=3,classProbs=TRUE,summaryFunction=twoClassSummary)
set.seed(825)

msdSamp.train$val <- ifelse(msdSamp.train$sad == 0, "NS","S")
msdSamp.test$val <- ifelse(msdSamp.test$sad == 0, "NS","S")
class(msdSamp.train$val)
msdSamp.train$val <- as.factor(msdSamp.train$val)
msdSamp.test$val <- as.factor(msdSamp.test$val)
prop.table(table(msdSamp.train$val))
prop.table(table(msdSamp.test$val))

rsvm.caret <- train(val~familiarity+hotness+tempo+duration+mode+key+loudness, data=msdSamp.train, method="svmRadial", trControl=control, tunelength = 5, metric = "ROC")
rsvm.caret
predictedClasses <- predict(rsvm.caret,testing)
testProbs <- predict(rsvm.caret, newdata = testing, type = "prob")

confusionMatrix(data = predictedClasses, testing$sad)

#Logistic Regressions 
msd 
summary(msd)

#find examples of both sad and happy 
s.h = which(msd$sad==1 & msd$happy ==1)
sad_hap = msd[s.h,4:5]
msd.hs = msd[-s.h,] #take only cases that are not both sad and happy
msd.hs$valence <- ifelse(msd.hs$sad == 1, "1",
                         ifelse(msd.hs$happy == 1, "0","NA"))
msd.hs$valence <- as.factor(msd.hs$valence)
summary(msd.hs)

#Centered and Scaled (mean = 0, sd = 1)

install.packages("caret")
library(caret)
set.seed(1234)

dim(msdS)
preObj1 <- preProcess(msd.hs[,c(6,8,9,11,12)], method=c("center", "scale")) #Method 2
msdScale <- predict(preObj1,msd.hs[,c(6,8,9,11,12)])
sd(msdNorm$loudness)
summary(msdScale)
msdScale <- cbind(msdScale,msd.hs$mode,msd.hs$key,msd.hs$sad, msd.hs$happy, msd.hs$beautiful)
names(msdScale)[8] <- "sad"
names(msdScale)[7] <- "key"
names(msdScale)[6] <- "mode"
names(msdScale)[9] <- "happy"
names(msdScale)[10] <- "beautiful"

#Lets run some logistic regressions (or ANOVAs) on each predictor individually on sad versus happy 

which(msdNo$sad==1 & msdNorm$happy ==1) #check and make sure there are no sad and happy cases

#Is there sig corr between loudness and sad v happy tag? 
glm.loud<- glm(sad ~ loudness, family = binomial, data = msdScale)
print(glm.loud)
summary(glm.loud) #negative correlation between sad (1) vs. not sad (0) and loudness

#Is there sig corr between mode and sad v happy tag? 
glm.mode<- glm(sad ~ mode, family = binomial, data = msdNorm)
print(glm.mode)
summary(glm.mode) #negative correlation between sad (1) vs. happy (0) and mode (0 = minor, 1 = major)

#Is there sig corr between tempo and sad v happy tag? 
glm.tempo<- glm(sad ~ tempo, family = binomial, data = msdNorm)
print(glm.tempo)
summary(glm.tempo) #negative correlation between sad (1) vs. happy (0) and tempo (slower = sad)

#Is there sig corr between duration and sad v happy tag? 
glm.duration<- glm(sad ~ duration, family = binomial, data = msdNorm)
print(glm.duration)
summary(glm.duration) #positive correlation between sad (1) vs. happy (0) and duration (longer = sad)

#Interactions 
glm.inter<- glm(sad ~ duration+mode+loudness+tempo+familiarity+hotness+beautiful, family = binomial, data = msdScale)
print(glm.inter)
summary(glm.inter) #positive correlation between sad (1) vs. happy (0) and duration (longer = sad)

summary(msdNorm)
exp(coef(glm.inter))
exp(cbind(OR = coef(glm.inter), confint(glm.inter)))


yhat.bag = predict(bag.sad,newdata=msdSamp.test)
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
importance()

