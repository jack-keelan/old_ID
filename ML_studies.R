##################################################################
##                         Initialize workspace
##################################################################

##   fetch libraries
setwd("C:/Users/mooreet_la/repos/isoID/")
source("get_libraries.R")
setwd("C:/Users/mooreet_la/Documents/Data Science/data")

##     fetch RAW data set(s)
setwd("C:/Users/mooreet_la/repos/isoID/")
source("julia_data.R")   ## ~1hr close Cs137,Co60,bkg,Cf252? SpecBenchtest
source("emily_data.R")
source("sarah_data.R")  ## raw *.csv format Gadras model
source("johanna_data.R")  ##  rastered & sampled data
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
###############################################################

## or call normalized data set
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
training <- read.csv("asymptotics/raster/trainingNorm.csv")
testing <- read.csv("asymptotics/raster/testingNorm.csv")
testing <- read.csv("asymptotics/raster/testLab.norm.csv")
testing <- read.csv("asymptotics/raster/testingNormPois.csv")
row.names(training) <- training$X
training <- training[,-c(1)]
row.names(testing) <- testing$X
testing <- testing[,-c(1)]
###############################################################




plot(x=c(1:1022),y=testjunkCo[10,1:1022],xlab="Energy", ylab="Counts", 
     log="y", main="Lab Spectrum", pch=15, col="black", type="l")

############################################################
##    create longer dwell spectra
for(i in 1:5){
  x <- (i-1)*30 + 1
  y <- x + 29
  labCs100s.temp <- testjunkCs[x:y,]
  temp <- colSums(labCs100s.temp)
  labCs100s[i] <- rbind(labCs100s,temp)
}
##  More regularized data sets? single source per file
setwd("C:/Users/mooreet_la/Documents/Data Science/data/")
############################################################

##  making df into matrix
x1 <- grep("Ch_01",colnames(Lab.data))
x2 <- grep("Ch_1023",colnames(Lab.data))
y<- as.matrix(Lab.data[,x1:x2])

y1 <- y

y.norm <- y
z <- dim(y)[[1]]
#for(i in 1:z){
  sum.y <- sum(y[,2:1025])
  y.norm[,2:1025] <- (y[,2:1025]/sum(y[,2:1025]))
#}
##  making into matrix
###################################################################


write.csv(Lab.data, file = "juliaDataSet/EGJdata.csv")  #, row.names = FALSE)

#  first <- sapply(strsplit(file.first, split='le', fixed=TRUE), function(x) (x[2]))
#  last <- sapply(strsplit(file.last, split='le', fixed=TRUE), function(x) (x[2]))

Lab.data[,21] <- Lab.data[,1048]
Lab.data <- Lab.data[,-c(1:20)]

## data subsetting
Lab.data.2x <- subset(Lab.data, Lab.data$DetID == "2x")
Lab.data.2x <- Lab.data.2x[,-c(1026:1029)]


plot(x=c(1:1022),y=testing[80,1:1022],xlab="Energy", ylab="Counts", 
      main="Lab Spectrum", pch=15, col="black", type="l")
lines(x=c(1:1022), y=AveBenchtest[1:1022,5], log="y",col="red")

current.dir <- paste(working.dir,sub.dir,
                     "2017_05_03/Detector2X/137Cs_28cm_56cm_112cm/",sep="")


setwd("C:/Users/mooreet_la/Documents/Data Science/data/juliaDataSet/")

setwd("C:/Users/mooreet_la/Documents/Data Science/data")
#### format real data testing set
##########################################

##############################################################################
#                Training on Sarah's Model data

# row.names <- c("BaBA","BaCo","BaDU","BaST","CoBA","CoCO","CoDU",
#                "CoST","CsBA","CsCO","CsDU","CsST","IrBA","IrCO","IrDU",
#                "IrST","SeBA","SeCO","SeDU","SeST")
# column.names <- c(1:1024,"iso")
# 
# Model.raster <- data.frame(matrix(ncol = 1025, nrow = length(row.names),
#                                   dimnames=list(row.names,column.names)))

set.seed(12345)
inTrain <- createDataPartition(y=Model.raster$Channel100, p=0.3, list=FALSE)
trainingMod <- Model.raster[inTrain,]
testingMod <- Model.raster[-inTrain,]

plot.Lab <- testLabCs[,2:1022]
plot.Cs.Lab <- subset(plot.Lab[1:1000,]); plot.Co.Lab <- subset(plot.Lab[1001:2000,])

a <- colSums(plot.Cs.Lab); b <- colSums(plot.Co.Lab)
a <- a[-c(722:1022)]; b <- b[-c(722:1022)]
setwd("C:/Users/mooreet_la/Documents/Data Science/SDRD_presentation")
png(file = "mod_lab.png")
plot(x=c(1:721),y=trainingMod[605,2:722]/864000,xlab="Energy (~3keV)", ylab="Counts", 
      main="Model vs. Lab Spectrum", pch=15, col="green", type="l",log = "y") # Co60
#lines(x=c(1:1021), y=trainingMod[1,2:1022]/86400000, log="y",col="coral") #Ba133
lines(x=c(1:721), y=trainingMod[1187,2:722]/1864000, log="y",col="coral") #Cs137
lines(x=c(1:721), y=a/300000, log="y",col="red")
lines(x=c(1:721), y=b/50000, log="y",col="blue")
legend('topright',c('Co60 -  model','Co60 -  measured',
                    'Cs137 - model', 'Cs137 - measured'),
        fill = c('green', 'blue','coral','red'), bty = 'n',
        border = NA)
dev.off()
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
#plot(x=c(2:1022),y=a/100000,xlab="Energy", ylab="Counts", 
#     log="y",main="Lab Spectrum", pch=15, col="black", type="l")
#

lines(x=c(1:1021), y=trainingMod[48,2:1022], log="y",col="blue")
lines(x=c(1:1021), y=trainingCs[9,1:1022], log="y",col="red")
# lines(x=AveBenchtest[1:1022,2], y=trainingCs[10,1:1022], log="y",col="green")
# lines(x=AveBenchtest[1:1022,2], y=trainingCs[11,1:1022], log="y",col="yellow")
# lines(x=AveBenchtest[1:1022,2], y=trainingCs[62,1:1022], log="y",col="lightblue")
# lines(x=AveBenchtest[1:1022,2], y=trainingCs[63,1:1022], log="y",col="cyan")
# lines(x=AveBenchtest[1:1022,2], y=trainingCs[64,1:1022], log="y",col="green")


#############################################
##  sample
##  Normalizing the data
## integrate under selected function & scale
testLabCs.norm <- testLabCs
z <- dim(testLabCs.norm)[[1]]
for(i in 1:z){
  sum.testLab <- sum(testLabCs[i,2:1025])
  testLabCs.norm[i,2:1025] <- (testLabCs[i,2:1025]/sum.testLab)
}

training.norm <- trainingMod
y <- dim(training.norm)[[1]]
for(i in 1:y){
  sum.train <- sum(trainingMod[i,2:1025])
  training.norm[i,2:1025] <- trainingMod[i,2:1025]/sum.train
}


##  noisy version
testingMod <- Model.sampled.temp
testing.norm <- testingMod
x <- dim(testing.norm)[[1]]
true.counts <- 0
for(i in 1:x){
  sum.test <- sum(testingMod[i,2:1025])
#  true.counts[i] <- sum.test/86400
  testing.norm[i,2:1025] <- (testingMod[i,2:1025]/sum.test)  # ave bkg 890/sec
}

write.csv(training.norm, file = "asymptotics/raster/trainingNorm.csv")
write.csv(testing.norm, file = "asymptotics/raster/testingNorm.csv")
write.csv(testLabCs.norm, file = "asymptotics/raster/testLab.norm.csv")
write.csv(testing.norm, file = "asymptotics/raster/testingNormPois.csv")
# set.seed(8763)
# draw.from <- sample(1:x,100, replace = T)
# 
# row.names <- c(1:100)
# column.names <- c(1:1024,"iso")
# testing.samples <- data.frame(matrix(ncol = 1025, nrow = 100,
#                                      dimnames=list(row.names,column.names)))
# for(j in 1:100){
#   for(i in 1:1024){
#     testing.samples[j,i] <- rpois(1,testingCs.norm[draw.from[j],i])
#   }
#   testing.samples[j,1025] <- testingCs.norm[draw.from[j],1025]
# }
# for(i in 1:100){
#   sum.test1 <- sum(testing.samples[i,1:1024])
#   testing.samples[i,1:1024] <- (testing.samples[i,1:1024]/sum.test1)
# }
# 

#########################################################################
##         Creating Models

testing <- testing[,-c(1)]
column.names <- colnames(testing)   ##  All related to EGdata
testing <- Lab.data.2x
colnames(testing) <- column.names
testing <- subset(testing,iso != "BkGD")

test.norm <- testing
z <- dim(testing)[[1]]
for(i in 1:z){
  sum.test <- sum(testing[i,2:1025])
  test.norm[i,2:1025] <- (testing[i,2:1025]/sum.test)
}



##     Generalized Linear Model
modFitglm <- train(iso ~ ., data = trainingCs, method="glm")
modFitglm
modFitglm$finalModel

#trainingCs.norm[,"iso"] <- as.factor(trainingCs.norm[,"iso"])
#testing.samples[,"iso"] <- as.factor(testing.samples[,"iso"])

##              Random Forest
# inT <- createDataPartition(training.norm$Channel5,p=.05,list=FALSE)
# train.junk <- training.norm[inT,]
# plot(x=AveBenchtest[1:1022,1],y=testing.norm[76,2:1023],xlab="Energy", ylab="Counts", 
#      log="y", main="Lab Spectrum", pch=15, col="black", type="l")

caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)
modRF <- train(iso ~ ., 
                     data = training.norm,      #train[, features],
                     method = "rf",
                     trControl = caret.control,
                     tuneLength = 5,             #match features
                     ntree = 101,        # = 101
                     importance = TRUE)
modRF
modRF$finalModel
modFit <- modFit.jo  #RF

saveRDS(modFit.jo,file = "modFit_asy_raster_train30percent.rds")  # 90-105 minutes to run
modFit <- readRDS("MLmodels/modFit_asy_raster.rds")

modFit
modFit$finalModel
##set.seed(1)
##mod <- rfe(x = iris[, 1:4], y = iris$Species, sizes = 4,
##           rfeControl = rfeControl(functions = caretFuncs, 
##                                   method = "boot",
##                                   number = 5),
##           ## pass options to train(), 
##           tuneGrid = data.frame(mtry = 2),
##           method = "cforest",
##           controls = cforest_unbiased(ntree = 10))

###########  Support Vector Machine ###################################
#ctrl <- trainControl(method = "cv", repeats = 10)  #  "repeated cv"
#set.seed(1500)
#mod <- train(iso ~., 
#             data=trainingCs, 
#             method = "svmLinear", 
#             trControl = ctrl)
library(caret)
ctrl <- trainControl(method = "cv", savePred=T, classProb=F)
modSVMLin <- train(iso ~., 
             data=trainingCs.norm, 
             method = "svmLinear",
#             metric = "ROC",
             trControl = ctrl)

head(mod$pred)
preds <- predict(mod,newdata=testing.samples)
preds <- predict(mod,newdata=testLabCs.norm)


###################K nearest nieghbors
# Train the model with preprocessing
model_knn <- train(iso ~., 
                   training, 
                   method='knn', 
                   preProcess=c("center", "scale"))

saveRDS(model_knn,file = "mod_knn_asy_raster_30percent.rds")  # ? to run
modFit <- readRDS("MLmodels/mod_knn_asy_raster_30percent.rds")

modFit <- model_knn

# Predict values
predictions <- predict.train(object=model_knn,testLabCs.norm, type="raw")


preds <- predict(modFitCs,newdata=testing.samples)


preds <- predict(modFit,newdata=testing)
preds

# Display the results of the cross validation run - Around 79% 
# mean accuracy! 
modFit


# What is the standard deviation?
cat(paste("\nCross validation standard deviation:",  
          sd(modFit$resample$Accuracy), "\n", sep = " "))


# Pull out the the trained model using the best parameters on
# all the data! Mighty!
rf.best <- modFit$finalModel


# Look at the model - this model is trained on 100% of the data!
varImpPlot(rf.best)

preds <- predict(modFit, testing, type = "raw")
#preds <- predictions
y <- dim(testing)[[1]]
Pred.iso <- data.frame(sampleID = 1:y,
                      Isotope = testing$iso, Iso.pred = preds)

con.matrix <- confusionMatrix(preds,testing$iso)

confusion.RF.pred <- con.matrix$table


# for Age
# y <- dim(testing.norm)[[1]]
# Pred.iso <- data.frame(sampleID = 1:y,
#                        Isotope = testing.norm$iso, Iso.pred = preds)

Pred.fail <- 0

Pred.fail <- testing[which(as.character(Pred.iso$Isotope)
                                  != as.character(Pred.iso$Iso.pred)),]

Pred.fail.Ba <- testing[which(Pred.fail$iso == "Ba133"),]
Pred.fail.Co <- testing[which(Pred.fail$iso == "Co60"),]
Pred.fail.Cs <- testing[which(Pred.fail$iso == "Cs137"),]
Pred.fail.Ir <- testing[which(Pred.fail$iso == "Ir192"),]
Pred.fail.Se <- testing[which(Pred.fail$iso == "Se75"),]
failure.rate <- dim(Pred.fail)[[1]]/dim(testing)[[1]]
failure.rate
failure.rate.Cs <- dim(Pred.fail.Cs)[[1]]/
  dim(subset(testing, testing$iso == "Cs137"))[[1]]
failure.rate.Cs
failure.rate.Co <- dim(Pred.fail.Co)[[1]]/
  dim(subset(testing, testing$iso == "Co60"))[[1]]
failure.rate.Co




# Pred.iso$Diff <- Pred.iso[,"Iso.pred"] - Pred.iso[,"Isotope"]
# Pred.iso$round <- round(Pred.iso[,"Iso.pred"])
# Pred.iso$corr <- Pred.iso[,"Isotope"] - round(Pred.iso[,"Iso.pred"])
# 
# count <- 0
# for(i in 1:100){
# #  if(Pred.iso[i,"corr"] != 0){
#   if(abs(Pred.iso[i,"Diff"]) < 2){
# #  if(Pred.iso[i,"Isotope"] != 17){
#         count <- count + 1
#     x <- paste(Pred.iso[i,"Isotope"],",",Pred.iso[i,"Iso.pred"],
#                ",",Pred.iso[i,"sampleID"],",",Pred.iso[i,"Diff"])
#     print(x)
# #    print(Pred.iso[i,"Isotope"]); print(Pred.iso[i,"Iso.pred"])
# #    }
#   }
# }
# 
# y <- dim(testing.norm)[[1]]
# for(i in 1:y){
#   if(Pred.iso[i,"Iso.pred"] == Pred.iso[i,"Isotope"]){
#     count = count +1}
# }
# 
# #Pred.fail.150sub <- Pred.fail
# #Pred.iso.real.B9c5c1I13s17 <- Pred.iso
# pred.cs <- subset(Pred.iso,Pred.iso$Diff > 2.5)
# pred.co <- subset(Pred.iso,abs(Pred.iso$Diff) < 2.5)
# pred.bkg <- subset(Pred.iso,Pred.iso$Diff < -3.5)
# 
# mean(pred.cs$Iso.pred); sd(pred.cs$Iso.pred)
# mean(pred.co$Iso.pred); sd(pred.co$Iso.pred)
# mean(pred.bkg$Iso.pred); sd(pred.bkg$Iso.pred)
# 
# #Pred.iso.B5c3c1I7s9 <- Pred.iso
# #Pred.iso.B9c5c1I13s17 <- Pred.iso
# #Pred.iso.B3c2c1I4s5 <- Pred.iso
# #Pred.iso.real <- Pred.iso
# hist(Pred.iso.B5c3c1I7s9$Diff,breaks = 20)
# hist(Pred.iso.B9c5c1I13s17$Diff,breaks = 20)
# hist(Pred.iso.B3c2c1I4s5$Diff,breaks = 20)  #count 13
# hist(Pred.iso.real$Diff,breaks = 20)
# 
# hist(Pred.iso$Diff,breaks = 20)
# 
# hist(Pred.iso.real.B9c5c1I13s17$Iso.pred,breaks = 50)
# hist(pred.cs$Iso.pred,breaks = 50)
# hist(pred.co$Iso.pred,breaks = 50)
# hist(pred.bkg$Iso.pred,breaks = 50)
# 
# g <- ggplot(Pred.iso.real.B9c5c1I13s17, aes(x = as.numeric(Iso.pred)))
# g + geom_histogram(aes(color=as.factor(Isotope)), binwidth = 0.7)
# f <- ggplot(Pred.iso.real.B9c5c1I13s17, aes(x = as.numeric(Diff)))
# f + geom_histogram(aes(color=as.factor(Isotope)), binwidth = 0.7)
# 
# 
# ######################################
# ### testing real data
# modFitLabTest <- train(iso ~ ., data = testLabCs, method="glm")
# pred <- predict(modFitCs,newdata=testLabCs)
# pred
# confusionMatrix(pred,testLabCs$iso)
# 
# #View(testLabCs[,1020:1025])
# #View(trainingCs[,1020:1025])
# 
# qplot(wage, pred, data=testing)
# 
# inTrain <- createDataPartition(y=testLabCs$iso, p=0.75, list=FALSE)
# testingCs <- testLabCs[inTrain,]
# validationCs <- testLabCs[-inTrain,]
# 
# modFitTestCs <- train(iso ~ ., data = testingCs, method="glm")
# modFitTestCs
# 
# inTrain <- createDataPartition(y=testLabCs$iso, p=0.75, list=FALSE)
# testingCs <- testLabCs[inTrain,]
# validationCs <- testLabCs[-inTrain,]
# modFitTestCo <- train(iso ~ ., data = testingCs, method="glm")
# modFitTestCo
# 
# modFitTestBkg <- train(iso ~ ., data = testingCs, method="glm")
# modFitTestBkg
# 
# testModIso40 <- t(ModelIso40)
# #View(testModIso40)
# 
# ############################
# ##          testing of Poisson functions
# ##set.seed(12345)
# ##vec <- vec2 <- 0
# ##for(j in 1:10000){
# ##  x <- 0
# ##  for(i in 1:1000){x <- x + rpois(1, 60)}
# ##  vec <- c(vec,x)
# ##  vec2 <- c(vec2, rpois(1,60000))
# ##}
# ##vec2 <- vec2[vec2>10000]
# ##vec <- vec[vec>10000]
# ##hist(vec2, breaks = 50)
# ##hist(vec, breaks = 50)
# 
# ###############################################################################

