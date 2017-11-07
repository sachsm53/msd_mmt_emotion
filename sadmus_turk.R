#Sad Music Survey Mechanical Turk --------------------------------------------
#Import packages
library(caret)
library(FactoMineR)
library(corrplot)
library(psych)
library(nFactors)
library(MASS)
library(fpc)
library(cluster)
library(gplots)
library(usdm)
library(tree)
library(rpart.plot)
library(maptree)
library(cluster)
library(WRS2)
library(foreign)

#Load Data
raw = read.csv('/Users/mattsachs/Documents/Sad_Music_Listening_Survey_Turk.csv')
raw <- raw[-which(raw$sex == 6),]
raw$sex <- as.factor(raw$sex)
head(raw)
summary(raw)
dim(raw)
#raw = raw[-c(1,2),]
#raw$Q12.1 <- NULL



##########################################
# ------------ EFA -----------------------
##########################################

#GEM-9 and Factor Analysis
gem_col <- grep("gem_", colnames(raw))
gem <- raw[,min(gem_col):max(gem_col)]
gem.cor <- cor(gem)
fa.parallel(gem.cor, n.obs=dim(gem)[1],fm="pa", fa = "both") #suggests six factors, 4 components

set.seed(1)
gem.fa <- factanal(na.omit(gem), factors = 3, rotation = "varimax", scores = "regression")
print(gem.fa, digits = 2, cutoff = .35, sort = TRUE)
gem.pro <- update(gem.fa, rotation = "promax")
print(gem.pro, digits = 2, cutoff = .35, sort = TRUE)
head(gem.pro$scores) 
head(gem.fa$scores)
summary(gem.pro$scores)
summary(gem.fa$scores)
colnames(gem.pro$scores)<-c("gem_subl","gem_vital","gem_unease")

#Music Genres
stomp_col1 <- grep("alt",colnames(raw))
stomp_col2 <- grep("singer",colnames(raw))
stomp <- raw[,stomp_col1:stomp_col2]

#Situations for listening to sad music
f <- grep("angry_h", colnames(raw))
l <- grep("get_way_e", colnames(raw))
sit <- raw[,as.numeric(f):as.numeric(l)]
dim(sit)
describe(sit)
sapply(sit, function(x) sum(is.na(x)))
sit[is.na(sit)] <- 0
sit.cor <- cor(sit.sad,use="complete.obs")
sad <- grep("_s", colnames(sit))
happy <- grep("_h", colnames(sit))
sh <- append(sad,happy)
sit.sad <- (sit[,sad])
sit.hap <- (sit[,happy])
sit.sh <- (sit[,sh])
describe(sit.sad)
describe(sit.sh)
dim(sit.sh)
sit.cor <- cor(sit.sad)
sit.cor <- cor(sit.sh)

ev <- eigen(cor(sit.sad)) # get eigenvalues
ap <- parallel(subject=nrow(sit.sad),var=ncol(sit.sad),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
fa.parallel(sit.cor, n.obs=dim(raw)[1],fm="pa", fa = "both") #suggests six factors, 4 components
fa.parallel(sit.sad,fm="pa", fa = "both")

set.seed(1)
sit.fa <- factanal(na.omit(sit.sad), factors = 3, rotation = "varimax", scores = "regression")
print(sit.fa, digits = 2, cutoff = .3, sort = TRUE)
sit.pro <- update(sit.fa, rotation = "promax")
print(sit.pro, digits = 2, cutoff = .25, sort = TRUE)
colnames(sit.pro$scores)<-c("distract_factor","lonely_factor","stress_factor")
colnames(sit.pro$scores)<-c("sad_factor","celebrate_factor","angry_factor","distract_factor")

set.seed(2)
sit.sh.fa <- factanal(na.omit(sit.sh), factors = 9, rotation = "varimax", scores = "regression")
print(sit.sh.fa, digits = 2, cutoff = .3, sort = TRUE)
sit.sh.pro <- update(sit.sh.fa, rotation = "promax")
print(sit.sh.pro, digits = 2, cutoff = .35, sort = TRUE)

ml5.out <- factanal(covmat = cor(situation, use = "complete.obs"),factors = 5, rotation = "none")

#Reasons to listen to sad music
r1 <- grep("better_understand", colnames(raw))
r2 <- grep("calm", colnames(raw))
reason <- raw[,r1:r2]
reas.cor <- cor(reason,use = "complete.obs")
reas2 <- na.omit(reason)
normalization <- preProcess(reas2)
reasN <- predict(normalization,reas2)
reasN <- as.data.frame(reasN)
summary(reasN)


# Determine Number of Factors to Extract
ev <- eigen(cor(reas2)) # get eigenvalues
ap <- parallel(subject=nrow(reas2),var=ncol(reas2),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
fa.parallel(reas.cor, n.obs=dim(raw)[1],fm="pa", fa = "both") 
fa.parallel(reas2,fm="pa", fa = "both") 
reas.pca <- principal(reason,2,n.obs=dim(raw)[1],rotate="Promax")
reas.pca <- princomp(reas2, center = TRUE, scale = TRUE)
log.reas <- log(reas2)
reas.pca <- princomp(log.reas, center = TRUE, scale = TRUE, scores = TRUE)
reas.pca <- prcomp(log.reas, center = TRUE, scale = TRUE)
summary(reas.pca)
print(reas.pca)
plot(reas.pca)
reason[is.na(reason)] <- 0

reason.fa <- factanal(na.omit(reason), factors = 2, rotation = "varimax", scores = "regression")
reason.fa <- factanal(reas2, factors = 1, rotation = "varimax", scores = "regression")
print(reason.fa, digits = 2, cutoff = .3, sort = TRUE)
reason.promax <- update(reason.fa,rotation="promax")
print(reason.promax, digits = 2, cutoff = .3, sort = TRUE)
colnames(reason.promax$loadings)<-c("regulate_factor","purge_factor") 
colnames(reason.promax$scores)<-c("regulate_factor","purge_factor") 
head(reason.promax$scores)

# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(mydata),cex=.7) # add variable names

#PCA

pca1 <- PCA(situation)
pca2 <- PCA(inv_sit)
barplot(pca1$eig[,1], main = "Eigenvalues",names.arg = paste("Dim", 1:nrow(pca1$eig), sep = ""))
dimdesc(pca2, proba = 0.2)
plot(pca2)
pca1 <- PCA(inv_reas)
plot(pca1)

#AES-M scale
chill = read.csv('/Users/mattsachs/Downloads/chill_r.csv')
head(chill)
describe(chill)
chill <- na.omit(chill)
chill_norm <- scale(chill, center = TRUE, scale = TRUE)
fa.parallel(chill_norm,fm="pa", fa = "both")

ev <- eigen(cor(chill_norm)) # get eigenvalues
ap <- parallel(subject=nrow(chill_norm),var=ncol(chill_norm),rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)
chill.pca1 <- principal(chill_norm,2,n.obs=dim(raw)[1],rotate="Promax")
chill.pca2 <- princomp(chill_norm, scores = TRUE)
log.reas <- log(reas2)
reas.pca <- princomp(log.reas, center = TRUE, scale = TRUE, scores = TRUE)
reas.pca <- prcomp(log.reas, center = TRUE, scale = TRUE)
summary(chill.pca1)
print(chill.pca1)
summary(chill.pca2)
plot(chill.pca1)

pca1 <- PCA(situation)
pca2 <- PCA(inv_sit)
barplot(pca1$eig[,1], main = "Eigenvalues",names.arg = paste("Dim", 1:nrow(pca1$eig), sep = ""))
dimdesc(pca2, proba = 0.2)
plot(pca2)
pca1 <- PCA(inv_reas)
plot(pca1)


##########################################
# ------ Make Datasets  ------------------
##########################################

# 1. Basic data set called df (and df.norm) with all personality measures and important sad music questions
rum <-raw[["ruminate"]]
ref <- raw[["reflect"]]
nostal <- raw[["nostal"]]
empathy <- raw[["global"]]
depression <- raw[["dep_sum"]]
open <- raw[["openness"]]
agree <- raw[["agree"]]
extra <- raw[['extraver']]
stab <- raw[["emo_stab"]]
sad_hours <- raw[["sad_hours"]]
sad_enjoy <- raw[["sad_enjoy"]]
absorp <- raw[["absorp"]]
emp_conc <- raw[["empathic"]]
fantasy <- raw[["fantasy"]]
sad_mus_percent <- raw[["sad_mus_percent"]]
age <- raw[["age"]]
sex <- raw[["sex"]]
mus_years <- raw[["mus_years"]]
mus_onset <- raw[["mus_onset"]]
mus_import <- raw[["mus_import"]]
mus_ability <- raw[["mus_ability"]]
raw$mus_years[is.na(raw$mus_years)] <- 0
mus_years <- raw$mus_years
feelsad <- raw$feel_sad
beautsad <- raw$sad_beaut
chills <- raw$chills
strong_emo <- raw$strong_emo
sad_feelbet <- raw$sad_feelbetter
stay_sad <- raw$stay_sad

df <- raw[,266:dim(raw)[2]]
df <- cbind(df,age,sex,mus_ability,mus_import,mus_years,sad_feelbet, beautsad, stay_sad, chills,strong_emo, sad_enjoy)
df <- cbind(df, gem.pro$scores)
df$global <- NULL
#df$gender <- raw$sex
#df <- df[-which(df$gender == 6),]
#df$gender <- as.factor(df$gender)

#df <- cbind(df,sit.pro$scores)
normalization <- preProcess(df)
df.norm <- predict(normalization,df)
df.norm <- as.data.frame(df.norm)
summary(df.norm)
describe(df.norm) # df.norm: n = 219, all personality + sad music related questions

## 2) Make a dataset with only people who enjoy listening to sad music
enj <- raw[-which(raw$sad_enjoy <= 2),]
gem.enj <- gem.pro$scores[-which(raw$sad_enjoy <= 2),]
sapply(sitc.sad, function(x) sum(is.na(x))) <- enj[,266:dim(enj)[2]] # dataset of only personality with n = 167 (only people who like sad music)
enj$mus_years[is.na(enj$mus_years)] <- 0
gem_col <- grep("gem_", colnames(enj))
gem2 <- enj[,min(gem_col):max(gem_col)] 
df.enj$global <- NULL
#df.enj <- cbind(df.enj,reason.promax$scores,enj$mus_years,enj$mus_import,enj$sad_feelbetter, enj$stay_sad, enj$sad_enjoy)
df.enj <- cbind(df.enj,gem.enj,enj$sex,enj$mus_years,enj$mus_import,enj$sad_feelbetter, enj$stay_sad, enj$sad_enjoy)
normalization <- preProcess(df.enj)
df.enjN <- predict(normalization,df.enj)
df.enjN <- as.data.frame(df.enjN)
summary(df.enjN)
describe(df.enjN)

#Dataset with only people who like sad music + prolong question for reasons
df.prolonglr <- cbind(df.enj,enj$mus_years,enj$age,enj$sad_feelbetter, enj$stay_sad, enj$sad_enjoy)
df.prolonglr$gender <- as.factor(enj$sex)
df.prolonglr <- df.enj
df.prolonglr$prolong <- reas2$prolong
df.prolonglr$touched <- reas2$touched
df.prolonglr <- df.prolonglr[-which(df.prolonglr$gender == 6),]
df.prolonglr$gender <- factor(df.prolonglr$gender)
dim(df.prolonglr)
df.prolonglr$sad_mus_percent <- NULL
df.prolonglr[["enj$stay_sad"]] <- NULL
df.prolonglr[["enj$stay_sad"]] <- NULL
df.prolonglr[["enj$sad_feelbetter"]] <- NULL
df.prolonglr[["enj$sad_enjoy"]] <- NULL
df.prolonglr$Factor1 <- NULL
df.prolonglr$global <- NULL
summary(df.prolonglr)
normalization <- preProcess(df.prolonglr)
df.prolonglrN <- predict(normalization,df.prolonglr)
df.prolonglrN <- as.data.frame(df.prolonglrN)
summary(df.prolonglrN)

#Dataset including sad situations EFA
df.sit <- na.omit(df
df.sit <- cbind(df.sit,sit.pro$scores)
normalization <- preProcess(df.sit)
df.sitN <- predict(normalization,df.sit)
df.sitN <- as.data.frame(df.sitN)
summary(df.sitN)

#Dataset of only people who like sad music + all the reasons
df.reas2 <- cbind(df.enj,reas2,enj$mus_years,enj$mus_import,enj$sad_feelbetter, enj$stay_sad, enj$sad_enjoy)
df.reas <- cbind(reas2,df.enj)
normalization <- preProcess(df.reas)
df.reasN <- predict(normalization,df.reas)
df.reasN <- as.data.frame(df.reasN)
summary(df.reasN)

df.pers <- enj[,265:dim(enj)[2]]
enj$mus_years[is.na(enj$mus_years)] <- 0
gem_col <- grep("gem_", colnames(enj))
gem2 <- enj[,min(gem_col):max(gem_col)]
df.pers <- cbind(df.pers,enj$mus_years,enj$age,reason.promax$scores)
df.pers <- cbind(df.pers,enj$mus_years,enj$age)
df.pers$gender <- enj$sex
df.pers$sad_enjoy <- reason.promax$scores[,4]
df.pers <- df.pers[-which(df.pers$gender == 6),]
df.pers$gender <- as.factor(df.pers$gender)
normalization <- preProcess(df.pers)
df.persN <- predict(normalization,df.pers)
df.persN <- as.data.frame(df.persN)
summary(df.persN)
df.persN$global <- NULL
head(df.persN)

#List of datasets
df
df.enj #only people who like listening to sad music 
df.enjlr #
df.gem
df.prolonglr #n = 166 (remove gender): all predictors plus prolong question
df.sit


##########################################
# ------------ Correlation Plots ---------
##########################################

##Correlation Plots
pcor.all <- rcorr(as.matrix(survey.norm),type = "pearson")
cor.all <- cor(as.matrix(survey.norm))
cor.all
symnum(cor.all)

corrplot(cor.all, type = "upper", order = "hclust", tl.col="black", tl.srt=45)

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(df.norm)
M <- cor(as.matrix(reas2), method = 'spearman')
head(M)
head(round(M,2))
head(p.mat[, 1:5])

# Leave blank on no significant coefficient
par(mfrow = c(1,1),mar = rep(.01, 4))
corrplot(M, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05, insig = "blank")
corrplot(M,p.mat = p.mat, sig.level = 0.003, insig = "blank")

sapply(sadmus, function(x) sum(is.na(x)))#find missing values
sadmus[is.na(sadmus)] <- 0

##########################################
# ------------ Multi Regressions ---------
##########################################

enj.lr <- lm(sad_enjoy ~ ., data=df.persN)

enj.lmer <- lmer(sad_enjoy ~ . -gender + (1|gender), data=df.persN)
summary(enj.lr)
summary(enj.lmer)
layout(matrix(c(1,2,3,4),2,2))
plot(enj.lr)

# 1) Enjoyment of sad music
df.enjlr <- df.norm
df.enjlr$sad_mus_percent <- NULL
df.enjlr$chills <- NULL
df.enjlr$stay_sad <- NULL
df.enjlr$beautsad <- NULL
df.enjlr$sad_feelbet <- NULL
df.enjlr$mus_import <- NULL
df.enjlr$global <- NULL
df.enjlr$strong_emo <- NULL
dim(df.enjlr) #dataset (n = 220) for multiregressions with all personality + demographic + gem + sad music questions

enj.lr1 <- lm(sad_enjoy ~ ., data=df.enjlr)
summary(enj.lr1)

#Check everything
enj.lr2 <- lm(sad_enjoy ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + gem_subl + gem_vital + gem_unease + age + mus_years + sex, data=df.enjlr)
summary(enj.lr2)

#Same but without GEM
enj.lr3 <- lm(sad_enjoy ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + age + mus_years, data=df.enjlr)
summary(enj.lr3)

anova(enj.lr1,enj.lr2,enj.lr3)

enj.lr <- lm(sad_enjoy ~ ., data=df.norm)
summary(enj.lr)

step(enj.lr2, direction = "both")
step <- stepAIC(enj.lr2, direction="both")
step$anova # display results

#Model based on AIC
enj.lr4 <- lm(sad_enjoy ~ dep_sum + fantasy + perspective + absorp + gem_subl + gem_unease + age, data=df.enjlr)
summary(enj.lr4)

enj.lr5 <- lm(sad_enjoy ~ dep_sum + fantasy + perspective + gem_subl + gem_unease + age, data=df.enjlr)
summary(enj.lr5)

anova(enj.lr4,enj.lr5)

# 2) Linear Regression to Predict GEM Factors
df.gem <- raw[,265:dim(raw)[2]]
df.gem <- cbind(df.gem,mus_years,age)
df.gem$gender <- raw$sex
#df.gem <- cbind(df.gem, gem.pro$scores)
df.gem$gem_factor <- gem.pro$scores[,2] #select the factor that you want to test here
df.gem <- df.gem[-which(df.gem$gender == 6),]
df.gem$gender <- as.factor(df.gem$gender)
df.gem$mus_years[is.na(df.gem$mus_years)] <- 0
normalization <- preProcess(df.gem)
df.gemN <- predict(normalization,df.gem)
df.gemN <- as.data.frame(df.gemN)
summary(df.gemN)
df.gemN$global <- NULL
head(df.gemN)

gem1.lr <- lm(gem_factor ~ ., data=df.gemN)
summary(gem1.lr)

step <- stepAIC(gem1.lr, direction = "both")
step$anova
gem1.lr <- lm(gem_factor ~ emo_stab + fantasy + absorp + nostal + mus_years, data=df.gemN)
summary(gem1.lr)

gem2.lr <- lm(gem_factor ~ ., data=df.gemN)
summary(gem2.lr)

step <- stepAIC(gem2.lr, direction = "both")
step$anova
gem2.lr <- lm(gem_factor ~ agree + emo_stab + dep_sum + fantasy + pers_distress + ruminate + age, data=df.gemN)
summary(gem2.lr)

gem3.lr <- lm(gem_factor ~ ., data=df.gemN)
summary(gem3.lr)

step <- stepAIC(gem3.lr, direction = "both")
step$anova
gem3.lr <- lm(gem_factor ~ fantasy + perspective + empathic + pers_distress + nostal + age, data=df.gemN)
summary(gem3.lr)


# 3) Linear Regression to predict stay sad question 
df.ss <- cbind(df,mus_years,age)
df.ss$gender <- raw$sex
df.ss$stay_sad <- raw$stay_sad
df.ss <- df.ss[-which(df.ss$gender == 6),]
df.ss$gender <- as.factor(df.ss$gender)
df.ss$mus_years[is.na(df.ss$mus_years)] <- 0
normalization <- preProcess(df.ss)
df.ssN <- predict(normalization,df.ss)
df.ssN <- as.data.frame(df.ssN)
summary(df.ssN)
df.ssN$global <- NULL
df.ssN$sad_mus_percent <- NULL
head(df.ssN)

ss.lr <- lm(stay_sad ~ ., data=df.ssN)
summary(ss.lr)

# 4) Multiple Linear Regression with prolong 

prolong.lr <- lm(prolong ~ ., data=df.prolonglrN)
summary(prolong.lr)

step <- stepAIC(prolong.lr, direction="both")
step$anova # display results

prolong2.lr <- lm(prolong ~ extraver + dep_sum + fantasy + ruminate + perspective, data=df.prolonglrN)
summary(prolong2.lr)

# 4) Multiple Linear Regression with touched, moved, awed
touched.lr <- lm(touched ~ ., data=df.prolonglrN)
summary(touched.lr)

step <- stepAIC(touched.lr, direction="both")
step$anova # display results
prolong2.lr <- lm(touched ~ emo_stab + empathic + absorp + ruminate + reflect, data=df.prolonglrN)
summary(prolong2.lr)

#5) Multiple regression with situation factors 
df.sitN$gem_subl <- NULL
df.sitN$gem_unease <- NULL
df.sitN$gem_vital <- NULL

sit.lr1 <- lm(lonely_factor ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + gem_subl + gem_vital + gem_unease + age + mus_years + gender, data = df.sitN)      
summary(sit.lr1)
step <- stepAIC(sit.lr1, direction="both")
step$anova # display results
sit.step <- lm(lonely_factor ~ fantasy + gem_subl + gem_vital, data=df.sitN)
summary(sit.step)

sit.lr2 <- lm(distract_factor ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + gem_subl + gem_vital + gem_unease + age + mus_years + gender, data = df.sitN)      
summary(sit.lr2)
step <- stepAIC(sit.lr2, direction="both")
step$anova # display results
sit.step <- lm(lonely_factor ~ empathic + pers_distress + gem_subl + gem_vital + gem_unease + age + mus_years, data=df.sitN)
summary(sit.step)

sit.lr3 <- lm(stress_factor ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + gem_subl + gem_vital + gem_unease + age + mus_years + gender, data = df.sitN)      
summary(sit.lr3)
step <- stepAIC(sit.lr3, direction="both")
step$anova # display results
sit.step <- lm(lonely_factor ~ empathic + ruminate + gem_subl + gem_vital + reflect + age, data=df.sitN)
summary(sit.step)

sit.lr4 <- lm(stress_factor ~ dep_sum + fantasy + perspective + empathic + pers_distress + absorp + ruminate + reflect + nostal + age + mus_years + gender, data = df.sitN)      
summary(sit.lr4)
step <- stepAIC(sit.lr4, direction="both")
step$anova # display results
sit.step <- lm(lonely_factor ~ empathic + ruminate + reflect + age, data=df.sitN)
summary(sit.step)



##########################################
# ------------ Cluster Analysis---------
##########################################

reas.clus <- na.omit(reason)
reas.scale <- scale(reas.clus)
inv_reas <- t(reas.scale)

sit.sad <- scale(sit.sad)
inv.sit <- t(sit.sad)

#Methods for Determing the Number of Clusters

#1) Look at Scree Plot
wss <- (nrow(reas.scale)-1)*sum(apply(reas.scale,2,var)) 
wss <- (nrow(inv_reas)-1)*sum(apply(inv_reas,2,var)) 
for (i in 2:15) wss[i] <- sum(kmeans(reas.scale,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

#2) Partitioning around medois to estimate the number of clusters

pamk.best <- pamk(reas.scale)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(reas.scale, pamk.best$nc))
#2 clusters

#3) PAM 
pamk.best <- pamk(inv.sit)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")

#4) Cascade KM
install.packages('vegan')
require(vegan)
fit <- cascadeKM(scale(inv.sit, center = TRUE,  scale = TRUE), 1, 10, iter = 1000)
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# 2 clusters!

# K-Means Cluster Analysis
reas.km1 <- kmeans(reas.scale, 4)
reas.km2 <- kmeans(reas.scale, 2)

set.seed(2)
sit.km <- kmeans(sit.sad,4)
aggregate(sit.sad,by=list(sit.km$cluster),FUN=mean)

# get cluster means 
aggregate(reas.scale,by=list(reas.km$cluster),FUN=mean)
fit.reas$cluster
aggregate(inv_sit,by=list(fit.sit$cluster),FUN=mean)
fit.sit$cluster

#plotting cluster results
clusplot(reas.scale, reas.km$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# comparing 2 cluster solutions
d <- dist(reas.scale)
cluster.stats(d, reas.km1$cluster, reas.km2$cluster)

# append cluster assignment
inv_sit$fit.sit.cluster.1 <- NULL
inv_sit$fit.sit.cluster <- NULL
inv_reas <- data.frame(inv_reas, fit.reas$cluster)
sad_sit_c1 <- apply(inv_sit,1,function(x) mean(x ==1))

## ----------Hierarchical Cluster Analysis 

#Determine number of clusters
install.packages('NbClust')
require(NbClust)
nb <- NbClust(data=sit.sadN,distance="euclidean",min.nc=2,max.nc=10,method='ward.D2',index='alllong', alphaBeal=0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

reas <- scale(reas, center = TRUE, scale = TRUE)
nb <- NbClust(data=reas,distance="euclidean",min.nc=2,max.nc=10,method='ward.D2',index='alllong', alphaBeal=0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))

gem <- scale(gem, center = TRUE, scale = TRUE)
nb <- NbClust(data=gem,distance="euclidean",min.nc=2,max.nc=10,method='ward.D2',index='alllong', alphaBeal=0.1)
hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))


#Do the cluster analysis for sad situations
inv_sit <- t(sit.sad)
sit.sadN <- scale(sit.sad, center = TRUE, scale = TRUE)
set.seed(4)
d <- dist(as.matrix(sit.sadN), method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)
groups <- cutree(hc, k=3)# cut tree into 4 clusters
summary(as.factor(groups))
means <- aggregate(sit.sad,by=list(groups),FUN=mean)
means$Group.1 <- NULL
means <- t(means)
means <- data.frame(row.names(means), means, row.names = NULL) 
means[order(-means$X1),]

#Do the cluster analysis for the reasons
reas <- na.omit(reason)
d <- dist(as.matrix(reas), method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)
reas.clus <- cutree(hc, k=4) # cut tree into k clusters
summary(as.factor(reas.clus))
reas.M <- aggregate(reas,by=list(reas.clus),FUN=mean)
reas.M$Group.1 <- NULL
reas.M <- t(reas.M)
reas.M <- data.frame(row.names(reas.M), reas.M, row.names = NULL) 
reas.M[order(-reas.M$X1),]

#Do the cluster analysis for the GEM
d <- dist(as.matrix(gem), method = "euclidean")
hc <- hclust(d, method = "ward.D2")
plot(hc)
gem.clus <- cutree(hc, k=2) # cut tree into 4 clusters
summary(as.factor(gem.clus))
gem.M <- aggregate(gem,by=list(gem.clus),FUN=mean)
gem.M$Group.1 <- NULL
gem.M <- t(gem.M)
gem.M <- data.frame(row.names(gem.M), gem.M, row.names = NULL) 
gem.M[order(-gem.M$X2),]

# draw dendogram with red borders around the 5 clusters 
rect.hclust(hc, k=2, border="red")
means <- melt(means ,  id.vars = 'row.names.means.', variable.name = 'situation')

# draw dendogram with red borders around the 5 clusters 
rect.hclust(hc, k=4, border="red")

##########################################
# ------------ MANCOVA ---------
##########################################

#-------MANCOVA (used to test multiple continous dependent variables with both continueous and ordinar dependent variables)

df.aov <- df
df.aov$groups <- groups
df.aov$groups <- as.factor(df.aov$groups)
df.aov$sad_mus_percent <- NULL
normalization <- preProcess(df.aov)
df.aovN <- predict(normalization,df.aov)
df.aovN <- as.data.frame(df.aovN)
dv.aov <- cbind(df.aovN$fantasy, df.aovN$perspective, df.aovN$empathic, df.aovN$pers_distress)
aov.fit <- manova(dv.aov ~ df.aovN$groups*df.aovN$age*df.aovN$gender*df.aovN$mus_years)
summary.aov(aov.fit)

dv.aov <- cbind(df.aovN$openness, df.aovN$conscient, df.aovN$extraver, df.aovN$agree, df.aovN$emo_stab)
dv.aov <- cbind(df.aovN$nostal,df.aovN$dep_sum, df.aovN$ruminate, df.aovN$reflect, df.aovN$absorp)
aov.fit <- manova(dv.aov ~ df.aovN$groups*df.aovN$age*df.aovN$gender*df.aovN$mus_years)
summary.aov(aov.fit)

which(,)
g1 <- df.aov[which(df.aov$groups == 1),]
mean(g1$dep_sum)
g2 <- df.aov[which(df.aov$groups == 2),]
mean(g2$dep_sum)
g3 <- df.aov[which(df.aov$groups == 3),]
mean(g3$dep_sum)

#plot results 

clusters <- factor(df.aovN$groups)
plotmeans(df.aovN$perspective~ clusters, xlab = "Group", ylab = "Perspective Taking")

interaction.plot(clusters,df.aovN$gender,df.aovN$nostal, type="b",leg.bty="o",leg.bg="beige",lwd=2)

install.packages('ggplot')
require(ggplot2)
require(reshape)
f <- data.frame(time = 1:10,a = cumsum(rnorm(10)),b = cumsum(rnorm(10)),c = cumsum(rnorm(10)))

#Testing Multicollinearlity 
vif(df.norm) #anything over 4 is generally assumed to be multicollinear and should be removed before fitting model

# Robust Version of MANCOVA 
#Robust measures of ANCOVA
head(df.aovN)
ancova(fantasy ~ groups, data = df.aovN, tr = 0.2, fr1 = 1, fr2 = 1, pts = NA)
ancboot(fantasy ~ groups, data = df.aovN)
model = lm(fantasy ~ groups*age*gender*mus_years, data = df.aovN)
summary.aov(model)

ancova
ancsm
Qancsm
ancpb # like ancova but with a percentile bootstrap used 
ancbbpb
ancboot # like ancova but with a bootstrap t method 


## -----------Decision trees
sit_c1 <- cbind(raw$cry_need, raw$breakup, raw$missing, raw$sad)
reas_c1 <- cbind(raw$cry,raw$grieve)
sit_c2 <- cbind(raw$happy,raw$friends,raw$celebrate, raw$relaxed, raw$distract, raw$traveling)
reas_c2 <- cbind(raw$good,raw$calm,raw$better,raw$cheered_up,raw$soothing,raw$relaxed,raw$distance,raw$safe_space,raw$less_alone)
grep("Q11.1", colnames(raw))

sad_sit_c1 <- apply(sit_c1,1,function(x) mean(x ==1))
sad_reas_c1 <- apply(reas_c1,1,function(x) mean(x ==1))
sad_sit_c2 <- apply(sit_c2,1,function(x) mean(x ==1))
sad_reas_c2 <- apply(reas_c2,1,function(x) mean(x ==1))
test <- cbind(sad_sit_c1, sad_reas_c1, sad_sit_c2, sad_reas_c2)

hap_sit_c1 <- apply(sit_c1,1,function(x) mean(x !=1))
hap_reas_c1 <- apply(reas_c1,1,function(x) mean(x !=1))
hap_sit_c2 <- apply(sit_c2,1,function(x) mean(x !=1))
hap_reas_c2 <- apply(reas_c2,1,function(x) mean(x !=1))
tes2 <- cbind(hap_sit_c1, hap_reas_c1, hap_sit_c2, hap_reas_c2)

happy <- apply(reason,1,function(x) sum(x ==2))
other <- apply(reason,1,function(x) sum(x ==3))
mix <- apply(reason,1,function(x) sum(x ==4))
none <- apply(reason,1,function(x) sum(x ==5))

length(which(raw$sad == 1))
length(which(raw$sad != 1))

length(which(raw$happy == 1))
raw$happy == 1


tr = tree(sad_enjoy ~ extraver + global + ruminate + nostal + dep_sum + reflect + absorp, data = raw)
plot(tr); text(tr)

names(df.enjsadS)[names(df.enjsadS) == 'perspective'] <- 'persp_taking'
names(df.enjsadS)[names(df.enjsadS) == 'dep_sum'] <- 'dep_sev'
df.enjsadS$Class <- NULL
enjsad.dt = rpart(sadenj ~ ., data = df.enjsadS, method = "class")
printcp(enjsad.dt, digits=getOption("digits") - 2)
printcp(enjsad.dt)
plotcp(enjsad.dt)
plotcp(enjsad.dt, minline = TRUE, lty = 3, col = 1, upper = c("size", "splits", "none"), args)
plot(enjsad.dt, uniform = TRUE, margin = .1); text(enjsad.dt, use.n = TRUE, cex = 1)

prune.enjsad <- prune(enjsad.dt, cp = enjsad.dt$cptable[which.min(enjsad.dt$cptable[,"xerror"]),"CP"])
fancyRpartPlot(prune.enjsad, uniform=TRUE,main="Pruned Classification Tree")
plot(prune.enjsad, uniform=TRUE, margin = .1); text(prune.enjsad, use.n = TRUE, cex = .8)
fancyRpartPlot(enjsad.dt)


# Feel Sad 
names(df.sad)[names(df.fsadS) == 'empathic'] <- 'empathic_concern'
names(df.sad)[names(df.fsadS) == 'dep_sum'] <- 'dep_sev'
names(df.sad)[names(df.sad) == 'aes_trans'] <- 'transc_exp'
feelsad.dt = rpart(feel_sad ~ ., data = df.fsadS, method = "class")
fsad.dt <- rpart(feel_sad ~ ., data = df.sad, method = "class")
printcp(feelsad.dt, digits=getOption("digits") - 2)
plotcp(feelsad.dt)
plot(feelsad.dt, compress = TRUE,uniform = TRUE, margin = .1); text(feelsad.dt, use.n = TRUE, cex = 1)
fancyRpartPlot(feelsad.dt, )
plot(fsad.dt, compress = TRUE,uniform = TRUE, margin = .1); text(fsad.dt, use.n = TRUE, cex = 1)

# prune the tree 
pfit<- prune(feelsad.dt, cp= feelsad.dt$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
# plot the pruned tree 
plot(pfit, uniform=TRUE, compress = TRUE, margin = .2)
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", title = "Pruned Classification Tree for Kyphosis")

# MAPTREE
draw.tree(clip.rpart(rpart(ra), best=7),nodeinfo=TRUE, units="species",cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)

#Party 
output.tree <- ctree(feel_sad ~ ., data = df.fsadN)
plot(output.tree)

plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)

chills.dt = rpart(chill.freq~ ., data = df.chills_norm, method = "class")
printcp(chills.dt, digits=getOption("digits") - 2)
plotcp(chills.dt)
plot(chills.dt, compress = FALSE, margin = .2)
text(chills.dt, use.n = TRUE)

df.nostS$Class <- NULL
nost.dt = rpart(mus_nostal ~ ., data = df.nostN, method = "class")
printcp(nost.dt, digits=getOption("digits") - 2)
plotcp(nost.dt)
plot(nost.dt, compress = FALSE, margin = .1); text(nost.dt, use.n = TRUE, cex= 1)
par(mfrow = c(1,1), mar = rep(2, 4))


enjsad.dt = rpart(sadenj ~ ., data = df.enjsadS, method = "class")
printcp(enjsad.dt, digits=getOption("digits") - 2)
plotcp(enjsad.dt)
plot(enjsad.dt, compress = FALSE, margin = .2); text(enjsad.dt, use.n = TRUE, cex= .8)

aest.dt = rpart(aem ~ ., data = df.aestN, method = "class")
printcp(enjsad.dt, digits=getOption("digits") - 2)
plotcp(enjsad.dt)
plot(aest.dt, compress = FALSE, margin = .3)
text(aest.dt, use.n = TRUE, cex= .8)

##---------------------------------------------------------------
#take only complete cases
raw.comp = raw[complete.cases(raw),]
dim(raw.comp) #number of complete cases: 340,826
dim(raw.comp[raw.comp$sad == 1,]) #number of sad songs: 13,762

#find unique cases and duplicates
msd <- raw.comp[!duplicated(raw.comp[,3]),] #remove all cases where the song ID is the same
dim(msd) #now the number of cases is 340,192
dim(msd[msd$sad == 1,]) #now, the number of sad songs is 13,617


#find and remove cases where tempo = 0 
length(which(msd$tempo == 0))
msd[237,]
subset = which(msd$tempo == 0)
msd.sub = msd[-subset,]
length(which(msd.sub$tempo == 0))
msd <- msd.sub
summary(msd)


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


#ROC Curves
auc <- roc(data.test$sad,test.pred)
rocplot =function (pred , truth , ...){
  predob = prediction (pred , truth )
  perf = performance (predob , "tpr ", "fpr ")
  plot(perf ,...)}
svmsad.opt=svm(sad~familiarity+hotness+tempo+duration+mode+key+loudness,data=msdSamp.train, kernel="radial",gamma=1,cost=1,decision.values=T)

#make the training set with only the variables that we are interested in
keeps <- c("familiarity","hotness","tempo","duration","mode","key","loudness","sad")
msdSmote.train <- msdSmote[keeps]

#cross validation to find the best choice for lambda and cost
set.seed(11)
tune.out = tune(svm,sad~., data = msdSmote.train, kernel = "radial", ranges = list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))


train.pred = predict(svmbest,data.train)
table(data.train$sad,train.pred)
test.pred = predict(svmbest,data.test)
table(data.test$sad,test.pred)

