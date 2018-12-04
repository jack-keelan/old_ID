
library(tidyverse)
library(scatterplot3d)
library(ggplot2)
search()
#library(rhdf5)
library(stats)
library(psych)
library(MASS)
library(yaml)
library(gbm)
library(caTools)
library(rpart)
library(caret)
library(mda)
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

#######################
# Julia's Data
#######################
#benchtest <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata1_20170328114019v2.csv")
#benchtest2 <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata2_Ortec_20170328141651.csv")
#benchtest3 <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata3_Ortec_20170328162214.csv")
benchtest4 <- read.csv("juliaDataSet/BenchtopLab/EricCoCsBckCf_Ortec_20170414153501.csv")
#View(benchtest4)
dim(benchtest4)
#View(dfBaBa5m)
x<-grep("Ch_01",colnames(benchtest4)) # finding column # labelled 'ch_01'
SpecBenchtest <-t(benchtest4[,x[1]:(x[1]+1022)])  ### ???  How is this working?
SpecBenchtestCs <- subset(SpecBenchtest, select = c(1:3800))
SpecBenchtestCo <- subset(SpecBenchtest, select = c(3900:7350))
SpecBenchtestBkg <- subset(SpecBenchtest, select = c(7400:11000))
SpecBenchtestCf <- subset(SpecBenchtest, select = c(11000:12500))
 
View(SpecBenchtestCs)
#########################################
#### Create real data testing set
column.names <- c(1:1024,"iso")
row.names <- (1:100)
testLabCs <- data.frame(matrix(ncol = 1025, nrow = 100,
                                  dimnames=list(row.names,column.names)))
testjunkCs <- t(SpecBenchtestCs)
testjunkCo <- t(SpecBenchtestCo)
testjunkBkg <- t(SpecBenchtestBkg)
View(testjunkCs[,1020:1023])

for(i in 1:10){
  testLabCs[i,1:1023] <- testjunkCs[i,]
  testLabCs[i,1025] <- "Cs"
  testLabCs[i,1024] <- 0
  testLabCs[i+10,1:1023] <- testjunkCo[i,]
  testLabCs[i+10,1025] <- "nonCs"
  testLabCs[i+10,1024] <- 0
  testLabCs[i+20,1:1023] <- testjunkBkg[i+500,]
  testLabCs[i+20,1025] <- "nonCs"
  testLabCs[i+20,1024] <- 0
  testLabCs[i+30,1:1023] <- testjunkCo[i+100,]
  testLabCs[i+30,1025] <- "nonCs"
  testLabCs[i+30,1024] <- 0
  testLabCs[i+40,1:1023] <- testjunkCo[i+2500,]
  testLabCs[i+40,1025] <- "nonCs"
  testLabCs[i+40,1024] <- 0
  testLabCs[i+50,1:1023] <- testjunkCs[i+100,]
  testLabCs[i+50,1025] <- "Cs"
  testLabCs[i+50,1024] <- 0
  testLabCs[i+60,1:1023] <- testjunkBkg[i,]
  testLabCs[i+60,1025] <- "nonCs"
  testLabCs[i+60,1024] <- 0
  testLabCs[i+70,1:1023] <- testjunkCs[i+500,]
  testLabCs[i+70,1025] <- "Cs"
  testLabCs[i+70,1024] <- 0
  testLabCs[i+80,1:1023] <- testjunkCo[i+1000,]
  testLabCs[i+80,1025] <- "nonCs"
  testLabCs[i+80,1024] <- 0
  testLabCs[i+90,1:1023] <- testjunkCs[i+2000,]
  testLabCs[i+90,1025] <- "Cs"
  testLabCs[i+90,1024] <- 0
}

plot(x=AveBenchtest[1:1022,1],y=testLabCs[10,1:1022],xlab="Enerrgy", ylab="Counts", 
     log="y", main="Lab Spectrum", pch=15, col="black", type="l")

View(testLabCs[1:100,1020:1025])
#### Create real data testing set
##########################################

column.names <- c("Bin","Energy","AveCs","AveCo","AveBkg","AveCf")
row.names <- c(1:1022)
x<-grep("Ch_01",colnames(benchtest4))
AveBenchtest <- matrix(NA,nrow=1022,ncol=6,dimnames=list(row.names,column.names))
for(i in 1:1022){
  AveBenchtest[i,"Bin"]<-i
  AveBenchtest[i,"Energy"]<-(1.055+i*2.848)
  AveBenchtest[i,"AveCs"]<-mean(SpecBenchtestCs[i,])
  AveBenchtest[i,"AveCo"]<-mean(SpecBenchtestCo[i,])
  AveBenchtest[i,"AveBkg"]<-mean(SpecBenchtestBkg[i,])
#  AveBenchtest[i,"AveCf"]<-mean(SpecBenchtestCf[i,])
#  AveBenchtest[i,"Ave"]<-mean(benchtest4[,x[1]+i-1])
#  AveBenchtest[i,"Sum"]<-sum(benchtest4[,x[1]+i-1])
}

a<-(benchtest4[,'TotCount.cps.'])
hist(a, xlim=c(600,2000), ylim=c(0,1800),main="Total Counts Julia's Data", 
     ylab="Frequency",
     xlab="Total Counts", col='skyblue',breaks = 150 , border=F)
legend('topright',c('Lab'),
       fill = c('skyblue', 'red','green','coral'), bty = 'n',
       border = NA)
sumCs <- sumCo <- sumBkg <- sumCf <- 0
sumCs
plot(x=AveBenchtest[1:1022,2],y=AveBenchtest[1:1022,3],xlab="Enerrgy", ylab="Counts", 
     log="y", main="Lab Spectrum", pch=15, col="black", type="l")
lines(x=AveBenchtest[1:1022,2], y=AveBenchtest[1:1022,4], log="y",col="blue")
lines(x=AveBenchtest[1:1022,2], y=AveBenchtest[1:1022,5], log="y",col="red")
lines(x=AveBenchtest[1:1022,2], y=AveBenchtest[1:1022,6], log="y",col="coral")
lines(x=AveBenchtest[1:1000,2], y=h[1:1000,3], col="red")
legend('topright',c('Cs','Co','Cf', 'background'),
       fill = c('black', 'blue','coral','red'), bty = 'n',
       border = NA)
plot(x=AveBenchtest[1:1022,2],y=AveBenchtest[1:1022,5],xlab="Energy", ylab="Counts", 
     main="Lab Spectrum", pch=15, col="black", type="l")
lines(x=AveBenchtest[1:1022,2], y=AveBenchtest[1:1022,6], log="y",col="coral")
legend('topright',c('Background','Cf'),
       fill = c('black','coral'), bty = 'n',
       border = NA)

### Julia's Data
#######################

############################
##  Sarah's CSV Examining data

#testyaml<- read.csv("asymptotics/SSAM03Mar2017.yaml")
ModelAll <- read.csv("asymptotics/SSAM03Mar2017.csv")
testjunk <- t(ModelAll[,-c(1:19)])
#testjunk <- t(testjunk)
View(testjunk)
column.names <- c("Bin","BaBA","BaCo","BaDU","BaST","CoBA","CoCO","CoDU",
                  "CoST","CsBA","CsCO","CsDU","CsST","IrBA","IrCO","IrDU",
                  "IrST","SeBA","SeCO","SeDU","SeST")
row.names <- c(1:1024)
ModelIso40 <- data.frame(matrix(ncol = 21, nrow = 1024,
                               dimnames=list(row.names,column.names)))
for(i in 1:1024){
  ModelIso40[i,1] <- i
}

for(j in 1:20){
  ModelIso40[,j+1] <- testjunk[,3+((j-1)*4)]
  print(3+((j-1)*4))
}
View(ModelIso40)

testModIso40 <- t(ModelIso40)
View(testModIso40)

plot(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,10], xlab="Bin", ylab="Counts", 
     log="y", main="Spectrum Bare at 40m", pch=15, col="black", type="l")

lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,11], log="y", col="blue")
lines(x=ModelIso40[1:1040,1], y=ModelIso40[1:1040,12], log="y", col="red")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,13], log="y", col="green")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,18], log="y", col="coral")

legend('bottomleft',c('Ba','Co','Cs','Ir','Se'),
       fill = c('black', 'blue', 'red','green','coral'), bty = 'n',
       border = NA)

plot(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,4], xlab="Bin", ylab="Counts", 
     log="y", main="Spectrum steel at 40m", pch=15, col="black", type="l")

lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,8], log="y", col="blue")
lines(x=ModelIso40[1:1040,1], y=ModelIso40[1:1040,12], log="y", col="red")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,16], log="y", col="green")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,20], log="y", col="coral")

legend('bottomleft',c('Ba','Co','Cs','Ir','Se'),
       fill = c('black', 'blue', 'red','green','coral'), bty = 'n',
       border = NA)

## Examining Sarah's Model Data
#################################

############################
# Training on Sarah's Model data

ModelAll <- read.csv("asymptotics/SSAM03Mar2017.csv")
testjunk <- (ModelAll[,-c(1:19)])
#testjunk <- t(testjunk)
#View(testjunk[,1020:1024])
column.names <- c(1:1024,"iso")
row.names <- c("BaBA","BaCo","BaDU","BaST","CoBA","CoCO","CoDU",
               "CoST","CsBA","CsCO","CsDU","CsST","IrBA","IrCO","IrDU",
               "IrST","SeBA","SeCO","SeDU","SeST")
ModelIso40Ba <- data.frame(matrix(ncol = 1025, nrow = 20,
                                dimnames=list(row.names,column.names)))
for(j in 1:20){
  ModelIso40Ba[j,] <- testjunk[3+((j-1)*4),]
  #  print(3+((j-1)*4))
}
for(j in 1:4){
  ModelIso40Ba[j,1025] <- "Ba"
  ModelIso40Ba[j+4,1025] <- "nonBa"
  ModelIso40Ba[j+8,1025] <- "nonBa"
  ModelIso40Ba[j+12,1025] <- "nonBa"
  ModelIso40Ba[j+16,1025] <- "nonBa"
}

#View(ModelIso40Ba[,1000:1025])

ModelIso40Co <- data.frame(matrix(ncol = 1025, nrow = 20,
                                  dimnames=list(row.names,column.names)))
for(j in 1:20){
  ModelIso40Co[j,] <- testjunk[3+((j-1)*4),]
  #  print(3+((j-1)*4))
}
for(j in 1:4){
  ModelIso40Co[j,1025] <- "nonCo"
  ModelIso40Co[j+4,1025] <- "Co"
  ModelIso40Co[j+8,1025] <- "nonCo"
  ModelIso40Co[j+12,1025] <- "nonCo"
  ModelIso40Co[j+16,1025] <- "nonCo"
}

ModelIso40Cs <- data.frame(matrix(ncol = 1025, nrow = 20,
                                  dimnames=list(row.names,column.names)))
for(j in 1:20){
  ModelIso40Cs[j,] <- testjunk[3+((j-1)*4),]
  print(3+((j-1)*4))
}

for(j in 1:4){
  ModelIso40Cs[j,1025] <- "nonCs"
  ModelIso40Cs[j+4,1025] <- "nonCs"
  ModelIso40Cs[j+8,1025] <- "Cs"
  ModelIso40Cs[j+12,1025] <- "nonCs"
  ModelIso40Cs[j+16,1025] <- "nonCs"
}

plot(x=AveBenchtest[1:1022,1],y=ModelIso40Cs[11,1:1022],xlab="Enerrgy", ylab="Counts", 
     log="y", main="Lab Spectrum", pch=15, col="black", type="l")
#View(testjunk)

#View(ModelIso40)

column.names <- c(1:1024,"iso")
row.names <- c(1:100)
ModelIso40BigBa <- data.frame(matrix(ncol = 1025, nrow = 100,
                                dimnames=list(row.names,column.names)))
for(i in 1:20){
  ModelIso40BigBa[i,] <- ModelIso40Ba[i,]
  ModelIso40BigBa[i+20,] <- ModelIso40Ba[i,]
  ModelIso40BigBa[i+40,] <- ModelIso40Ba[i,]
  ModelIso40BigBa[i+60,] <- ModelIso40Ba[i,]
  ModelIso40BigBa[i+80,] <- ModelIso40Ba[i,]
}
#View(ModelIso40BigCs[,1020:1025])
ModelIso40BigCo <- data.frame(matrix(ncol = 1025, nrow = 100,
                                     dimnames=list(row.names,column.names)))
for(i in 1:20){
  ModelIso40BigCo[i,] <- ModelIso40Co[i,]
  ModelIso40BigCo[i+20,] <- ModelIso40Co[i,]
  ModelIso40BigCo[i+40,] <- ModelIso40Co[i,]
  ModelIso40BigCo[i+60,] <- ModelIso40Co[i,]
  ModelIso40BigCo[i+80,] <- ModelIso40Co[i,]
}

ModelIso40BigCs <- data.frame(matrix(ncol = 1025, nrow = 100,
                                     dimnames=list(row.names,column.names)))
for(i in 1:20){
  ModelIso40BigCs[i,] <- ModelIso40Cs[i,]
  ModelIso40BigCs[i+20,] <- ModelIso40Cs[i,]
  ModelIso40BigCs[i+40,] <- ModelIso40Cs[i,]
  ModelIso40BigCs[i+60,] <- ModelIso40Cs[i,]
  ModelIso40BigCs[i+80,] <- ModelIso40Cs[i,]
}

#View(ModelIso40Big)

column.names <- c(1:1024,"iso")
row.names <- c(1:500)
ModelIso40veryBigCs <- data.frame(matrix(ncol = 1025, nrow = 500,
                                   dimnames=list(row.names,column.names)))
for(i in 1:100){
  ModelIso40veryBigCs[i,] <- ModelIso40BigCs[i,]
  ModelIso40veryBigCs[i+100,] <- ModelIso40BigCs[i,]
  ModelIso40veryBigCs[i+200,] <- ModelIso40BigCs[i,]
  ModelIso40veryBigCs[i+300,] <- ModelIso40BigCs[i,]
  ModelIso40veryBigCs[i+400,] <- ModelIso40BigCs[i,]
}

#View(ModelIso40BigCs[,1020:1025])
#inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)

inTrain <- createDataPartition(y=ModelIso40BigCs$iso, p=0.95, list=FALSE)
trainingCs <- ModelIso40BigCs[inTrain,]
#testingBa <- ModelIso40BigBa[-inTrain,]

#inTrain <- createDataPartition(y=ModelIso40BigCo$iso, p=0.75, list=FALSE)
#trainingCo <- ModelIso40BigCo[inTrain,]
#testingCo <- ModelIso40BigCo[-inTrain,]


#testing <- spam[-inTrain,]
#View(trainingBa[,1000:1025])
modFitBa <- train(iso ~ ., data = trainingBa, method="glm")
modFitBa

modFitCo <- train(iso ~ ., data = trainingCo, method="glm")
modFitCo

modFitCs <- train(iso ~ ., data = trainingCs, method="glm")
modFitCs <- train(iso ~ ., 
                     data = trainingCs,      #train[, features],
                     method = "rf",
                     trControl = caret.control,
                     tuneLength = 1022,    # = 5 match features
                     ntree = 101,        # = 101
                     importance = TRUE)
modFitCs
modFitCs$finalModel
###  Seems to key in on the first 19 bins only for Cs137!!!!!!!!!!!!!!
######################################
### testing real data
#modFitLabTest <- train(iso ~ ., data = testLabCs, method="glm")
pred <- predict(modFitCs,newdata=testLabCs)
pred
confusionMatrix(pred,testLabCs$iso)

#View(testLabCs[,1020:1025])
#View(trainingCs[,1020:1025])

qplot(wage, pred, data=testing)

inTrain <- createDataPartition(y=testLabCs$iso, p=0.75, list=FALSE)
testingCs <- testLabCs[inTrain,]
validationCs <- testLabCs[-inTrain,]

modFitTestCs <- train(iso ~ ., data = testingCs, method="glm")
modFitTestCs

inTrain <- createDataPartition(y=testLabCs$iso, p=0.75, list=FALSE)
testingCs <- testLabCs[inTrain,]
validationCs <- testLabCs[-inTrain,]
modFitTestCo <- train(iso ~ ., data = testingCs, method="glm")
modFitTestCo

modFitTestBkg <- train(iso ~ ., data = testingCs, method="glm")
modFitTestBkg

testModIso40 <- t(ModelIso40)
#View(testModIso40)

plot(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,2], xlab="Bin", ylab="Counts", 
     log="y", main="Spectrum Csre at 40m", pch=15, col="black", type="l")

lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,6], log="y", col="blue")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,10], log="y", col="red")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,14], log="y", col="green")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,18], log="y", col="coral")

legend('bottomleft',c('Ba','Co','Cs','Ir','Se'),
       fill = c('black', 'blue', 'red','green','coral'), bty = 'n',
       border = NA)
###  Training on Sarah's model data
############################
