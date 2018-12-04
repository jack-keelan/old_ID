
library(tidyverse)
library(scatterplot3d)
library(ggplot2)
library(rhdf5)     # fail
library(stats)
library(psych)
library(MASS)
library(gbm)
library(caTools)
library(rpart)
library(caret)
library(rJava)  # fail
library(RWeka)  #java problem?
library(mda)
library(kernlab)  # spam is here
library(ISLR)     # wage is here
library(Hmisc)
library(gridExtra)
library(splines)
library(rattle)
library(rpart)
library(rpart.plot)
library(ElemStatLearn)
library(klaR)

#setwd("C:/Users/turk_la/Documents/SSAM/Data")
setwd("C:/Users/mooreet_la/Documents/Data Science/data")

###############  test yaml
con <- file("isotopicsMLSDRD/yaml/BABA05.yaml")
v <- readLines(con,15:20)
close(con)
v
Ba133modelB05<- read.csv("isotopicsMLSDRD/yaml/BABA05.yaml")
View(Ba133modelB05)
Ba133modelB20<- read.csv("isotopicsMLSDRD/yaml/BABA20.yaml")
View(Ba133modelB20)
testyaml<- read.csv("asymptotics/SSAM03Mar2017.yaml")
View(testyaml)
############################
############################
#machine learning R clss
training <- testing <- 0

data("spam")

View(spam)
############################
# week2 Lecture1&2
############ Data slicing
# funky problem with "spam"

inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

training <- spam[inTrain,]
testing <- spam[-inTrain,]

spamPerm <- spam
spamtraining <- training
spamtesting <- testing

dim(testing)
dim(training)

modFit2 <- train(type ~ ., data = training, method="glm")
modFit2

modFit2$finalModel
View(testing)

predictions <- predict(modFit2, newdata=testing)
predictions
confusionMatrix(predictions,testing$type)
########## K-folds

set.seed(32323)

folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
#folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
#folds <- createResample(y=spam$type, times=10, list=TRUE)

sapply(folds,length)
folds[[1]][1:10]
View(folds)
########## time slices
tme <- 0
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 20)
names(folds)
folds$train[[1]]
folds$test[[1]]
############ Data slicing


#####################
# week2 Lecture4







###############
# week2 Lecture5
########### preprocessing
hist(training$capitalAve,main="",xlab = "ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
View(spam)
trainCapAve <- trainCapAveS <- 0
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
# the test set will have to use the same standardinzation
testCapAve <- testCapAveS <- 0
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS)
sd(testCapAveS)

#preprocess package
preObj <- 0
preObj <- preProcess(training[,-58],method = c("center","scale"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)
testCapAveS <- predict(preObj,testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)
## or straight to the modelFit
set.seed(32343)
modelFit3 <- 0
modelFit3 <- train(type ~.,data = training,
                   preProcess=c("center","scale"), method="glm") ##  ????
View(training)
####

preObj <- preProcess(training[,-58],method = c("BoxCox"))
trainCapAveS <- predict(preObj,training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS); qqnorm(trainCapAveS)
mean(trainCapAveS)
sd(trainCapAveS)
#View(training)
############# imputing data - 'missing data' missing values
set.seed(13343)
# make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size = 1, prob = 0.05)==1
training$capAve[selectNA] <- NA
# imput & standardize
preObj <- preProcess(training[,-58],method = c("knnImpute"))
capAve <- predict(preObj,training[,-58])$capAve
# standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])
############# imputing data - 'missing data missing values
#############


###########################
###########################
# week 2 Lecture4
# plotting predictors
####  Wage
#wage <- read.csv("MLclass/income1.csv")

data(Wage)
summary(Wage)
View(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(testing)
dim(training)

featurePlot(x=training[,c("age","education","jobclass")], y=training$wage, plot="pairs")

qplot(age,wage,color=jobclass,data = training)
qplot(education,wage,color=jobclass,data = training)
qplot(jobclass,wage,data = training)

qq <- qplot(age,wage,color=education,data = training)
qq + geom_smooth(method = 'lm', formula = y~x)

cutWage <- cut2(training$wage, g=3)
table(cutWage)


p1 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p2 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
p1
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage,training$jobclass)
t1

prop.table(t1,1)

qplot(wage,color=education,data=training,geom="density")
## plottting predictors


###########################
##  Week2 Lecture6
########################### Covariant creation
#################### summation vs information loss
View(spam)
spam$capitalAveSq <- spam$capitalAve^2

## tidy covariates
data("Wage")
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

WagePerm <- Wage
Wagetraining <- training
Wagetesting <- testing

### dummy variables
# convert qualitative variables to quantitative
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data = training)
head(predict(dummies, newdata = training))
## removing zero variables
## variables w/o variablitity won't help in prediction
nsv1 <- nearZeroVar(training, saveMetrics = TRUE)
nsv1
## polynominal fitting data
# splines library
bsBasis <- bs(training$age, df = 3)
bsBasis # 1 age scaled, 2 something like age^2, 3age^3

# example of curve fitting
lm1 <- lm(wage ~bsBasis, data = training)
plot(training$age, training$wage, pch = 19, cex = 0.5)
points(training$age, predict(lm1, newdata = training), 
       col = "red", pch = 19, cex = 0.5)

# testing - must use exactly same procedure as on training set
predict(bsBasis, age = testing$age)
#gam in caret has some smoothing of mutiple variables each differently
########################### Covariant creation


#####################
## week2 Lecture 7
################ preposesing w/ PCA
data("spam")
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
View(spam)

M <- abs(cor(training[,-58]))
diag(M) <- 0  # why not 1?  to not consider
which(M > 0.8, arr.ind = T)
View(M)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])
## these are highly correlated create a weighted predictor
## that is a combination of the two
## single combination that has the most information least noise
X <- Y <- 0
X <- 0.71 * training$num415 + 0.71 * training$num857
Y <- 0.71 * training$num415 - 0.71 * training$num857
plot(X,Y) # shows that most information is in X not Y

# This is where single valur decomision(SVD) comes in
# the PCA are the same as the right singluar value is you scale(minus mean/sd) first
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])
prComp$rotation

# Example
data("spam")
typeColor <- ((spam$type == "spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1)) # make the data look less skewed
plot(prComp$x[,1],prComp$x[,2], col = typeColor, xlab = "pc1", ylab = "pc2")
# principle component 1 explains the most variation in data, pc2 the next most
# pc3 the next, etc...
### with caret
preProc <- preProcess(log10(spam[,-58]+1), method = "pca", pcaComp = 2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col = typeColor)

preProc <- preProcess(log10(training[,-58]+1), method = "pca", pcaComp = 2)
trainPC <- predict(preProc,log10(training[,-58]+1))
View(trainPC)

modelFit <- train(training$type ~ ., 
                   method = "glm", 
                   data = trainPC) # this fails!!!!!!!!!
#View(trainType)

testPC <- predict(preProc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelFit,testPC))

modelFit <- train(type ~ ., 
                  method = "glm",
                  preProcess = "pca",
                  data = training) 
confusionMatrix(testing$type, predict(modelFit, testing))

View(training)
################ preposesing w/ PCA

#################
# week2 Lecture8

###########  predicting with regression
data("faithful")
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,p=0.5,list=FALSE)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]
head(trainFaith)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration")

## fit a linear model
# ED = b0 + b1WT + err; b0 intercept; b1 linear factor
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=4)

#predict new value for 80
coef(lm1)[1] + coef(lm1)[2]*80
newdata <- data.frame(waiting=80)
predict(lm1,newdata)

par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
    xlab="Waiting",ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",ylab="Duration")
lines(testFaith$waiting,predict(lm1,testFaith),lwd=3)

#get errors
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2)) #training
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2)) #testing

pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col=c(1,2,2),
         lty=c(1,1,1),lwd=3)
modFit <- train(eruptions ~ waiting,data=trainFaith,method="lm")
summary(modFit$finalModel)

###########  predicting with regression

#########################
# week2 Lecture9
######### multiple covariates
data(Wage)
Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(testing)
dim(training)

featurePlot(x=training[,c("age","education","jobclass")], 
            y=training$wage, plot="pairs")
qplot(age,wage,color=jobclass,data = training)
qplot(age,wage,color=education,data = training)

## fit a linear model
# ED = b0 + b1*age + b2*I(Jobclass = "Information") +
# summation over'k'(1 to 4) of g_k*I(eduction_i=level_k)
# page 9 last lecture wk 2
modFit <- train(wage ~ age + jobclass + education, 
                method = "lm", data = training)
finMod <- modFit$finalModel
print(modFit)
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
qplot(finMod$fitted,finMod$residuals,color=race,data=training)
plot(finMod$residuals,pch=19)

# predict vs truth ideally straight line
pred <- predict(modFit,testing)
qplot(wage, pred, color=year, data=testing)

modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)

######### multiple covariates
#########################







################################
##### Week 2 Quiz
#1
library(AppliedPredictiveModeling)
data("AlzheimerDisease")
View(AlzheimerDisease)

adData <- data.frame(diagnosis,predictors)
View(adData)

trainIndex <- createDataPartition(diagnosis, p=0.5, list=FALSE)
training <- adData[trainIndex,]
testing <- adData[-trainIndex,]

dim(testing)
dim(training)

#2
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
dim(testing)
dim(training)

View(mixtures)
View(concrete)
View(training)
#https://rstudio-pubs-static.s3.amazonaws.com/198592_5f367e8480ac4631b076b983ecfd5627.html
names <- colnames(concrete)
names <- names[-length(names)]
featurePlot(x = training[, names], 
            y = training$CompressiveStrength, plot = "pairs")

index <- seq_along(1:nrow(training))
ggplot(data = training, aes(x = index, y = CompressiveStrength)) + 
  geom_point() + theme_bw()

cutCS <- cut2(training$CompressiveStrength, g = 4)
summary(cutCS)

ggplot(data = training, aes(y = index, x = cutCS)) + geom_boxplot() + 
  geom_jitter(col = "blue") + theme_bw()

featurePlot(x = training[, names], y = cutCS, plot = "box")

#3
hist(mixtures$Superplasticizer, main="superplasticizer in concrete", 
     ylab="frequency", xlab="superPlastizer", bins=30,col='skyblue',border=F)
legend('topright',c('skyblue'),bty='n',border=NA)
# or from site
ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + theme_bw()
ggplot(data = training, aes(x = Superplasticizer)) + geom_histogram() + 
  theme_bw()
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

#4
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

###
library(ggplot2)
ncol(training)

which(sapply(adData,class)=="factor")

summary(training$diagnosis)

training$diagnosis = as.numeric(training$diagnosis)
p <- prcomp(training[,grep('^IL',names(training))])
p$rotation[,1:7]

View(p$rotation)
View(p$sdev)
qplot(1:length(p$sdev),p$sdev / sum(p$sdev))

which(cumsum(p$sdev) / sum(p$sdev) <= .9)

(cumsum(p$sdev) / sum(p$sdev))[8]

preProc <- preProcess(training[,grep('^IL',names(training))],method="pca",thres=.9)
preProc

#5
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

trainSmall <- data.frame(training[,grep('^IL',names(training))],training$diagnosis)
testSmall <- data.frame(testing[,grep('^IL',names(testing))],testing$diagnosis)
preProc <- preProcess(trainSmall[-13],method="pca",thres=.8)
trainPC <- predict(preProc,trainSmall[-13])
testPC <- predict(preProc,testSmall[-13])
######## never works
PCFit <- train(trainSmall$training.diagnosis~.,data=trainPC,method="glm")
NotPCFit <- train(trainSmall$training.diagnosis~.,data=trainSmall,method="glm")

PCTestPredict <- predict(PCFit,newdata=testPC)
NotPCTestPredict <- predict(NotPCFit,newdata=testSmall)

confusionMatrix(PCTestPredict,testSmall$testing.diagnosis)

confusionMatrix(NotPCTestPredict,testSmall$testing.diagnosis)
#pred <- predict(modFit,testing)


#modFitAll <- train(wage ~ ., data=training, method="lm")
#pred <- predict(modFitAll, testing)
#qplot(wage, pred, data=testing)
plot(training$CompressiveStrength,pch=19)
fineAgg <- cut2(concrete$FineAggregate)
View(fineAgg)
qplot(concrete$CompressiveStrength, 
      color=fineAgg, data=training)



featurePlot(x=training[,c("Cement","Water","Age")], 
            y=training$CompressiveStrength, plot="pairs")
qplot(CompressiveStrength,Age, color=FlyAsh,data=training)
qplot(CompressiveStrength ,color=Age,data = training)
qplot(Water,CompressiveStrength ,color=Age,data = training)
qplot(FineAggregate,CompressiveStrength ,color=Age,data = training)

modFit <- train(CompressiveStrength ~ Age, 
                method = "lm", data = training)
finMod <- modFit$finalModel
print(modFit)
plot(finMod,1,pch=19,cex=0.5,col="#00000010")
qplot(finMod$fitted,finMod$residuals,color=Age,data=training)
plot(finMod$residuals,pch=19)




qplot(education,wage,color=jobclass,data = training)
qplot(jobclass,wage,data = training)

################################
####  Week 3 
################################

#predicting with trees
#  split into HOMOGENIOUS groups (classification err the same 
#  on both leaves)
# measures of impurity (wrong prediction??)
# page 5 lecture 1

data(iris)
names(iris)

table(iris$Species)

inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

qplot(Petal.Width, Sepal.Width, color = Species, data = training)
qplot(Petal.Width, Petal.Length, color = Species, data = training)

# "rpart" model for regression & classification trees
modFit <- train(Species ~ ., method = "rpart", data = training)
print(modFit$finalModel)
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n = TRUE, all = TRUE, cex=0.8)

library(rattle)
library(rpart)
fancyRpartPlot(modFit$finalModel)
# must install "rattle", "rpart", GTK+ (automatic), "rpart.plot"
# R in caret: party & rpart packages for trees
# outside of caret: tree

# lecture 2 - bagging (aggregateing models together)
# concept: resample w/ replacement, re-run predictions 
# average results or majority vote for classification
# similar bias but reduced variance

library(ElemStatLearn)
data(ozone, package = "ElemStatLearn")
ozone <- ozone[order(ozone$ozone),]
head(ozone)

#  bagged loess (type of curve)
ll <- matrix(NA, nrow = 10, ncol = 155)
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace = T)
  ozone0 <- ozone[ss,]
  ozone0 <- ozone0[order(ozone0$ozone),]
  loess0 <- loess(temperature ~ ozone, data = ozone0, span = 0.2)
  ll[i,] <- predict(loess0,newdata=data.frame(ozone=1:155)) 
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)
for(i in 1:10){lines(1:155, ll[i,], col="grey",lwd=2)}
lines(1:155,apply(ll,2,mean), col="red",lwd=2)
# under train fuction: "bagEarth", "treebag","bagFDA" 
# does the bagging for you.  Or use the function 'bag' - "advance use"

predictors <- data.frame(ozone=ozone$ozone)
temperature <- ozone$temperature
treebag <- bag(predictors, temperature, B=10, 
               bagControl = bagControl(fit = ctreeBag$fit, 
                                       pred = ctreeBag$pred, 
                                       aggregate = ctreeBag$aggregate))

plot(ozone$ozone, temperature, col="lightgrey", pch=19)
points(ozone$ozone, predict(treebag$fits[[1]]$fit, predictors), 
       pch=19, col="red")
points(ozone$ozone, predict(treebag,predictors), pch=19, col="blue")

# goes through a detailed description of the 'ctreeBag' function
# for each component: 'fit', 'pred', 'aggregate'

# lecture 3 is random forests 
# (like bagging - but at each split bootstrap the variables - 
# only some variables considered at every split)
# then 'grow' a forest and average/vote
# pro - accuracy, con - speed, overfitting, interrprtability

library(ggplot2)
data("iris")


inTrain <- createDataPartition(y = iris$Species, p = 0.7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

modFit <- train(Species ~ ., method = "rf", data = training, prox=TRUE)
modFit

getTree(modFit$finalModel, k=2)

# class centers
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length, col=Species), 
               size=5, shape=4, data=irisP)

pred <- predict(modFit, testing)
testing$predRight <- pred == testing$Species
table(pred, testing$Species)

qplot(Petal.Width, Petal.Length, color=predRight, 
      data=testing, main="newdata Prediction")

# week3 lecture4 - boosting
# making a wieghted average of many weak predictors
# Why not let the network handle this?
# classifiers here may be or may not different from features
# like trees from features may be a classifier
# up-weight points that are misclassified
# boosting sounds very dangerous for overtrainning

library(ISLR)
library(caret)
library(ggplot2)

data("Wage")
Wage <- subset(Wage, select = -c(logwage))

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(testing)
dim(training)

modFit <- train(wage ~ ., method = "gbm", 
                data = training, verbose=FALSE)
print(modFit)

qplot(predict(modFit,testing), wage, data=testing)

# week3 lecture5 - Model Based Prediction
# Linear Discriminate Analysis LDA is closely related to analysis 
# of variance (ANOVA) and regression analysis, which also attempt 
# to express one dependent variable as a linear combination of 
# other features or measurements.
#
# Covariance is not 'standardized' and correlation is

#naive Bayes

library(klaR)

data("iris")
names(iris)

table(iris$Species)
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

modlda <- train(Species ~ ., data=training, method="lda") #lin desc Anal
modnb <- train(Species ~ ., data=training, method="nb") # naive Bayes

plda <- predict(modlda,testing)
pnb <- predict(modnb,testing)
table(plda,pnb)

equalPredictions <- (plda==pnb)
qplot(Petal.Width, Sepal.Width, color=equalPredictions, data=testing)
# one value different by the two methods above


###############################
# Neural networks in "neuralnet"
################################
set.seed(500)
library(MASS)
data("Boston")
#x <- 0
#data <- Boston
View(Boston)

apply(data, 2, function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)




###############################
# Neural networks in "neuralnet"
################################