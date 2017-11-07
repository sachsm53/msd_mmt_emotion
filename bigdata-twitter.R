#Twitter Data Now Bithcessss

raw = read.csv(file.choose())
summary(raw)
dim(raw)
raw$hotness <- as.numeric(levels(raw$hotness))[raw$hotness]
raw$loudness <- as.numeric(levels(raw$loudness))[raw$loudness]
raw$duration <- as.numeric(levels(raw$duration))[raw$duration]
raw$tempo <- as.numeric(levels(raw$tempo))[raw$tempo]
raw$familiarity <- as.numeric(levels(raw$familiarity))[raw$familiarity]

raw$sad <- as.factor(raw$sad)
raw$happy <- as.factor(raw$happy)
raw$beautiful <- as.factor(raw$beautiful)
raw$mode <- as.factor(raw$mode)
raw$key <- as.factor(raw$key)


tweets<-raw[complete.cases(raw),]

head(tweets)
tweets$sad <- as.factor(tweets$sad)
tweets$happy <- as.factor(tweets$happy)
tweets$beautiful <- as.factor(tweets$beautiful)
class(tweets$weekday)
class(tweets$mode)
tweets$mode <- as.factor(tweets$mode)
tweets$key <- as.factor(tweets$key)
summary(tweets)
tweets.comp<-tweets[complete.cases(tweets),]
summary(tweets.comp)
attach(tweets.comp)
summary(tweets.comp)
dim(tweets.comp)
dim(tweets)

#find unique cases and duplicates
dup_tweets <- tweets[duplicated(tweets),]
unique_tweets <- unique(tweets)
dup_tweets_sort <- dup_tweets[order(dup_tweets$song_name),]
tweets <- unique_tweets

#convert time to factor variable of morning, afternoon, evening
head(tweets.comp)
class(time)

tweets$time2 <- strptime(tweets$time,"%H:%M:%S")

ref = "2015-04-21 00:00:00 PDT"
ref <- strptime(ref,"%Y-%m-%d %H:%M:%S")
ref
as.double(tweets$time2[1]-ref)
tweets$time2[2]-ref
as.double(difftime(tweets$time2[1],ref,units="hours"))

# tweets.comp$tod <- as.double(tweets.comp$time2-ref)
# tweets.comp$tod <- apply(tweets.comp[,14],1,function(x) x-ref)
# apply(tweets.comp,2,function(x) x-ref)

time <- tweets$time2
class(time)
class(ref)
# diff <- lapply(time,function(x) as.double(x - ref))

time_diff <- rep(0,length(time))
for (i in 1:length(time)){
  time_diff[i] = as.double(difftime(time[i],ref,units="hours"))
}

tweets$time_diff <- time_diff

tweets$tod <- ifelse(tweets$time_diff > 5 & tweets$time_diff < 12, "1",
                     ifelse(tweets$time_diff > 12 & tweets$time_diff < 22, "2","3"))
dim(tweets[tweets$tod == "1",]) #number of morning
dim(tweets[tweets$tod == "2",]) #number of afternoon
dim(tweets[tweets$tod == "3",]) #number of night
class(tweets$tod)
tweets$tod <- as.factor(tweets$tod)
class(tweets$sad)

#turn the date of the tweet into the months
install.packages("lubridate")
library(lubridate)
dates <- tweets$date
head(dates)
d <- as.Date(dates, format = "%m/%d/%Y")
head(d)
month <- month(d)
tweets$month <- month
tweets$month <- as.factor(tweets$month)

#turn into seasons
getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}
head(getSeason(dates), 24)

#convert the countries into regions 
install.packages("countrycode")
library(countrycode)
cc <- tweets$country_code
head(cc)
region <- countrycode(cc,"iso2c","region")
head(region)
tweets$region <- region
dim(tweets)
#tweets.comp<-tweets[complete.cases(tweets),]
#dim(tweets.comp2)

tweets$happy <- as.factor(tweets$happy)
tweets$mode <- as.factor(tweets$mode)
tweets$key <- as.factor(tweets$key)
tweets$weekday <- as.factor(tweets$weekday)

tweets$familiarity <- as.numeric(tweets$familiarity)
tweets$hotness <- as.numeric(tweets$hotness)
tweets$tempo <- as.numeric(tweets$tempo)
tweets$duration <- as.numeric(tweets$duration)
tweets$loudness <- as.numeric(tweets$loudness)

as.numeric(levels(f))[f]
summary(tweets)

#remove cases where tempo is zero 
#find and remove cases where tempo = 0 
length(which(tweets$tempo == 0))
tweets[237,]
subset = which(tweets$tempo == 0)
tweets.sub = tweets[-subset,]
length(which(tweets.sub$tempo == 0))
tweets <- tweets.sub
summary(tweets)
dim(tweets)

#take only the ones that are sad and not sad and happy together
#find examples of both sad and happy 
s.h = which(tweets$sad==1 & tweets$happy ==1)
sad_hap = tweets[s.h,2:3]
tweets.hs = tweets[-s.h,] #take only cases that are not both sad and happy
tweets.hs$valence <- ifelse(tweets.hs$sad == 1, "1",
                            ifelse(tweets.hs$happy == 1, "0","NA"))
tweets.hs$valence <- as.factor(tweets.hs$valence)
summary(tweets.hs)
tweets.hs = tweets.hs[(tweets.hs$valence==0 | tweets.hs$valence == 1),]
dim(tweets.hs)


##run some logistic regressions 
install.packages("languageR")
library(languageR)
install.packages("lme4")
library(lme4)

#Is there a sig corr between time of day, month of the year, and tweeting about sad music? 
glm.time<- glm(sad~tod, family = binomial, data = tweets)
print(glm.time)
summary(glm.time)

glm.time2<- glm(valence~tod, family = binomial, data = tweets.hs)
print(glm.time2)
summary(glm.time2)
exp(coef(glm.time2))


#Is there sig corr between country and listening to sad music? 
glm.region<- glm(sad ~ region, family = binomial, data = tweets)
print(glm.region)
summary(glm.region) #positively correlated with South America, South-Eastern Asia, Southern Asia, Western Asia (collectivist countries)

glm.region2<- glm(valence~region, family = binomial, data = tweets.hs)
print(glm.region2)
summary(glm.region2)

#Is there sig corr between month and listening to sad music? 
glm.month<- glm(sad ~ month, family = binomial, data = tweets)
print(glm.month)
summary(glm.month)
exp(coef(glm.month))

glm.month2<- glm(valence~month, family = binomial, data = tweets.hs)
print(glm.month2)
summary(glm.month2) #in this case, month 5 (May) is negatively correlated with sad (1) vs. happy (0), people listen to happy music in May

#Is there a sig interaction between time of day, month of the year, region, and tweeting about sad music? 
glm.all<- glm(sad ~ tod+month+region, family = binomial, data = tweets)
print(glm.all)
summary(glm.all)
exp(coef(glm.all))

glm.all<- glm(sad ~ tod+month+region, family = binomial, data = tweets)
print(glm.all)
summary(glm.all)

#Is there sig corr between key and listening to sad music? 
glm.key<- glm(sad ~ key, family = binomial, data = tweets)
print(glm.key)
summary(glm.key)

glm.key<- glm(valence ~ key, family = binomial, data = tweets.hs)
print(glm.key)
summary(glm.key)

#Is there sig corr between day of week and listening to sad music? 

glm.key<- glm(sad ~ key, family = binomial, data = tweets)
print(glm.key)
summary(glm.key)

#take only the northern hemisphere regions to look at the importance of month 
south.hem = which(tweets$region=="South America" | tweets$region =="Southern Africa" | tweets$region =="Southern Africa"| tweets$region =="Melanesia" | tweets$region =="Southern Africa" | tweets$region =="South-Eastern Asia" | tweets$region =="Western Africa" | tweets$region =="Eastern Africa" | tweets$region =="Middle Africa" )
south.hem[1]
tweets[9,]
tweets.hem = tweets[-south.hem,] #take only cases that are in the northern hemisphere
summary(tweets.hem)

south.hem2 = which(tweets.hs$region=="South America" | tweets.hs$region =="Southern Africa" | tweets.hs$region =="Southern Africa"| tweets.hs$region =="Melanesia" | tweets.hs$region =="Southern Africa" | tweets.hs$region =="South-Eastern Asia" | tweets.hs$region =="Western Africa" | tweets.hs$region =="Eastern Africa" | tweets.hs$region =="Middle Africa" )
tweets.hs.hem = tweets.hs[-south.hem2,] #take only cases that are not both sad and happy
summary(tweets.hs.hem)

#Now, is there sig corr between month and listening to sad music (now that we are only in north hem)? 
glm.month<- glm(sad ~ month, family = binomial, data = tweets.hem)
print(glm.month)
summary(glm.month)

glm.month2<- glm(valence ~ month, family = binomial, data = tweets.hs.hem)
print(glm.month2)
summary(glm.month2) #Listen to happy music more in March and May 

#Is there a sig interaction between time of day, month of the year, region, and tweeting about sad music? 
glm.all.north<- glm(sad ~ tod+month+region, family = binomial, data = tweets.hem)
print(glm.all.north)
summary(glm.all.north)
exp(coef(glm.all.north))
exp(cbind(OR = coef(glm.all.north), confint(glm.all.north)))

glm.all<- glm(valence ~ tod+month+region, family = binomial, data = tweets.hs)
summary(glm.all)
exp(coef(glm.all))

glm.all.north2<- glm(valence ~ tod+month+region, family = binomial, data = tweets.hs.hem)
print(glm.all.north2)
summary(glm.all.north2) #Still Southern Asia and Western Asia are signficantly associated with sad music, as is March and May with happy music

#Associations with beautiful 
glm.all.beaut<- glm(beautiful ~ tod+month+region, family = binomial, data = tweets.hem)
print(glm.all.beaut)
summary(glm.all.beaut)

glm.all.beaut<- glm(beautiful ~ month+region, family = binomial, data = tweets.hs)
print(glm.all.beaut)
summary(glm.all.beaut)




##SVMs - Split into training and testing

#balance the sad (1) cases and the not sad (0) cases 
#balance the dataset by undersampling the nonzero cases
indx <- which(tweets$sad == 0)
sad_num <- dim(tweets[tweets$sad == 1,])[1]
tweetsNS <- tweets[sample(indx,sad_num),]
tweetsS <- tweets[tweets$sad == 1,]
summary(tweetsNS)
dim(tweetsS)
dim(tweetsNS)
tweetSamp <- rbind(tweetsNS,tweetsS)
dim(tweetSamp)
prop.table(table(tweetSamp$sad))


#divide the data into training and test based on an 80/20 split
num = round((dim(tweetSamp)[1])*(.8))
num #training = 15,888
set.seed(13)
train = sample(dim(tweetSamp)[1],num)
tweets.train = tweetSamp[train,]
table(tweets.test$sad) #there are 7,944 sad songs in the training set
sad_num <- dim(msdSamp.train[msdSamp.train$sad == 1,])[1]
tweets.test = tweetSamp[-train,]
dim(tweets.test) #test split = 3972
dim(tweets.train)
prop.table(table(tweets.train$sad))
prop.table(table(tweets.test$sad))

#do the same for the sad v. happy
num = round((dim(tweets)[1])*(.8))
num #training = 24017
set.seed(13)
train = sample(dim(tweets)[1],num)
beau.train = tweets[train,]
table(beau.train$beautiful) #there are 6,076 sad songs in the training set
beau.test = tweets[-train,]
dim(beau.train) #test split = 6004
prop.table(table(beau.train$beautiful))
prop.table(table(beau.test$beautiful))



#make the training set with only the variables that we are interested in
keeps <- c("familiarity","hotness","tempo","duration","mode","key","loudness","sad","tod","month","region")
tweets.train <- tweets.train[keeps]
tweets.test <- tweets.test[keeps]


#once again, take only the complete cases 
tweets.train<-tweets.train[complete.cases(tweets.train),]
tweets.test<-tweets.test[complete.cases(tweets.test),]

tweets.hstrain<-tweets.hstrain[complete.cases(tweets.hstrain),]
tweets.hstest<-tweets.hstest[complete.cases(tweets.hstest),]



#take away Melanesia, Micronesia, and Middle Africa
reg <- which(tweets.train$region == 'Melanesia' | tweets.train$region == 'Micronesia' | tweets.train$region == 'Middle Africa')
tweets.train <- tweets.train[-reg,]
summary(tweets.train$region)

reg <- which(tweets.test$region == 'Melanesia' | tweets.test$region == 'Micronesia' | tweets.test$region == 'Middle Africa')
tweets.test <- tweets.test[-reg,]
summary(tweets.test$region)

#SVM 1- Sad vs. not sad
#run the Support Vector Machine using radial kernel 
install.packages("e1071")
library("e1071")
set.seed(1235)
svm.sad=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness+tod+month+region,data=tweets.train, kernel="radial",gamma=1,cost=1)
train1.pred = predict(svm.sad,tweets.train,na.action = na.exclude)
table(tweets.train$sad,train1.pred) #Percent correct 15,550/15,827 = 98.2% (when we add region, now our accuracy is 15757/15,827 = 99.5%)
test1.pred = predict(svm.sad,tweets.test,na.action = na.exclude)
table(tweets.test$sad,test1.pred) #Percent correct: 3480/3965, 87.7%! (when add region as predictor:3397/3964 = 85.7%)

tweets.test$region <- as.factor(x, levels = c("red", "green", "blue")) 
tweets.test$region <- as.factor(tweets.test$region)
tweets.train$region <- as.factor(tweets.train$region)
summary(tweets.test)
summary(tweets.train)

dim(tweets.hstrain)
#SVM 2- Sad vs. happy (using tweets.hs)
svm.val=svm(valence~familiarity+hotness+tempo+duration+mode+key+loudness+tod+month,data=tweets.hstrain, kernel="radial",gamma=1,cost=1)
plot(svm.tweet2,tweets.hstrain)
summary(svm.tweet2)
train.pred = predict(svm.val,tweets.hstrain,na.action = na.exclude)
table(tweets.hstrain$valence,train.pred) #Percent correct 11,539/11636 = 99.16%
test.pred = predict(svm.val,tweets.hstest,na.action = na.exclude)
table(tweets.hstest$valence,test.pred) #Percent Correct: 2690/2916 = 92.2%

#SVM 3- Beautiful
summary(beau.train)
dim(beau.train)
dim(beau.test)
summary(beau.test)
reg <- which(beau.test$region == 'Melanesia' | beau.test$region == 'Micronesia' | beau.test$region == 'Middle Africa')
beau.test <- beau.test[-reg,]
reg <- which(beau.train$region == 'Melanesia' | beau.train$region == 'Micronesia' | beau.train$region == 'Middle Africa')
beau.train <- beau.train[-reg,]
beau.train$region <- factor(beau.train$region)
beau.test$region <- factor(beau.test$region)
beau.train<-beau.train[complete.cases(beau.train),]
beau.test<-beau.test[complete.cases(beau.test),]
str(beau.test)
str(beau.train)
summary(beau.test$region)
summary(beau.train$region)

#make the training set with only the variables that we are interested in
keeps <- c("familiarity","hotness","tempo","duration","mode","key","loudness","beautiful","tod","month","region")
beau.train <- beau.train[keeps]
beau.test <- beau.test[keeps]

svm.beaut=svm(beautiful~familiarity+hotness+tempo+duration+mode+key+loudness+tod+month+region,data=beau.train, kernel="radial",gamma=1,cost=1)
plot(svm.tweet2,tweets.hstrain)
summary(svm.beaut)
train.pred = predict(svm.beaut,beau.train,na.action = na.exclude)
table(beau.train$beautiful,train.pred) #Percent Correct:23736/23941 = 99.1% (23367/23941 = 97.6% without region)
test.pred = predict(svm.beaut,beau.test,na.action = na.exclude)
table(beau.test$beautiful,test.pred) #Percent Correct: 5266/5988 = 87.9% (5192/5986 86.7% with region as a parameter:)

#SVM 4- Predict location of tweet(region): Multiclass Problem!
#run the Support Vector Machine using radial kernel 
set.seed(5432)
svm.tweet4=svm(region~familiarity+hotness+tempo+duration+mode+key+loudness+month+tod+sad+beautiful+happy,data=tweets.train, kernel="radial",gamma=1,cost=1)
plot(svm.tweet1,tweets.train)
summary(svm.tweet1)
dim(tweets.train$sad)
train2.pred = predict(svm.tweet1,tweets.train)
table(tweets.train$sad,train.pred) 
test.pred = predict(svm.tweet1,tweets.test)
table(tweets.test$sad,test.pred)


##Random Forests
#Try it first without taking away sad + happy
library(randomForest)
set.seed (1)
bag.sad1 =randomForest(sad~key+familiarity+hotness+tempo+duration+mode+loudness+tod+month,data=tweets.train,mtry=3,importance =TRUE)
plot(bag.sad) #out of bag (OOG) error estimate 
importance(bag.sad1)
yhat.bagsad = predict(bag.sad,newdata=tweets.test)
table(tweets.test$sad,yhat.bagsad) #Accuracy is 3763/3964 = 94.9%
getTree(bag.sad, 1, labelVar=TRUE)
?getTree
summary(tweets.train)

par(mfrow=c(2,1))
par(pty="s")

varImpPlot(bag.sad1, type=1, pch=19, col=1, cex=.5, main="")
varImpPlot(bag.sad1, type=2, pch=19, col=1, cex=.5, main="")

#Again with sad vs. happy
library(randomForest)
set.seed (1)
bag.val1 = randomForest(valence~familiarity+hotness+tempo+duration+mode+key+loudness+tod+month+region,data=tweets.hstrain,mtry=3,importance =TRUE)
plot(bag.val1) #out of bag (OOG) error estimate 
importance(bag.val1)
yhat.bagval = predict(bag.val1,newdata=tweets.test)
table(tweets.test$sad,yhat.bagval) #Accuracy is 3220/3964 = 81.2%

summary(tweets.hstrain)
tweets.hstrain$valence <- factor(tweets.hstrain$valence)
tweets.hstrain$valence <- factor(tweets.hstrain$valence)

par(mfrow=c(2,1))
par(pty="s")

varImpPlot(bag.val1, type=1, pch=19, col=1, cex=.5, main="")
varImpPlot(bag.val1, type=2, pch=19, col=1, cex=.5, main="")

#Again with predicting beautiful 
set.seed(5)
bag.beaut=randomForest(beautiful~familiarity+hotness+tempo+duration+mode+key+loudness+tod+month+region,data=beau.train,mtry=3,importance =TRUE)
plot(bag.beaut)
importance(bag.beaut)
yhat.bagbeaut = predict(bag.beaut,newdata=beau.test)
table(beau.test$beautiful,yhat.bagbeaut) #Accuracy: 5741/5986 = 95.9%

par(mfrow=c(2,1))
par(pty="s")
varImpPlot(bag.beaut, type=1, pch=19, col=1, cex=.5, main="")
varImpPlot(bag.beaut, type=2, pch=19, col=1, cex=.5, main="")



library(randomForest)
set.seed(2)
bag.val=randomForest(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdNorm.train,mtry=3,importance =TRUE)
plot(bag.val)
importance(bag.val)
yhat.bagval = predict(bag.val,newdata=msdNorm.test)
table(msdNorm.test$sad,yhat.bagval) #Accuracy is 3116/4567 = 68% accuracy
mean((yhat.bagval-msdNorm.test)^2)

par(mfrow=c(2,1))
par(pty="s")

varImpPlot(bag.val, type=1, pch=19, col=1, cex=.5, main="")
varImpPlot(bag.val, type=2, pch=19, col=1, cex=.5, main="")








#**************************
#return the rules of a tree
#**************************
getConds<-function(tree){
  #store all conditions into a list
  conds<-list()
  #start by the terminal nodes and find previous conditions
  id.leafs<-which(tree$status==-1)
  j<-0
  for(i in id.leafs){
    j<-j+1
    prevConds<-prevCond(tree,i)
    conds[[j]]<-prevConds$cond
    while(prevConds$id>1){
      prevConds<-prevCond(tree,prevConds$id)
      conds[[j]]<-paste(conds[[j]]," & ",prevConds$cond)
      if(prevConds$id==1){
        conds[[j]]<-paste(conds[[j]]," => ",tree$prediction[i])
        break()
      }
    }
    
  }
  
  return(conds)
}

#**************************
#find the previous conditions in the tree
#**************************
prevCond<-function(tree,i){
  if(i %in% tree$right_daughter){
    id<-which(tree$right_daughter==i)
    cond<-paste(tree$split_var[id],">",tree$split_point[id])
  }
  if(i %in% tree$left_daughter){
    id<-which(tree$left_daughter==i)
    cond<-paste(tree$split_var[id],"<",tree$split_point[id])
  }
  
  return(list(cond=cond,id=id))
}

#remove spaces in a word
collapse<-function(x){
  x<-sub(" ","_",x)
  
  return(x)
}
tree<-getTree(bag.sad, k=1, labelVar=TRUE)
colnames(tree)<-sapply(colnames(tree),collapse)
rules<-getConds(tree)
print(rules)