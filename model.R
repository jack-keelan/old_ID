## run some random forest business on my source features
library(caret)
library(randomForest)

#you need to reset the working directory
setwd("C:/Users/turk_la/Documents/SSAM/Data/SSAM24April2017-v1.1-20170516/")
df.mod <- read.csv("Samp_spec_feat.csv")

# Split data into training and testing set
## randomly choose 60% of the data set as training data (Why 60% instead of 70%?)
set.seed(102)
train.indices <- sample(1:nrow(df.mod), 0.6*nrow(df.mod))
train <- df.mod[train.indices,]
test <- df.mod[-train.indices,]

#if you want to use all channel data, define the features as the channels
# x1 <- grep("Channel0",colnames(df.mod))
# x2 <- grep("Channel1023",colnames(df.mod))
#features <- c("SourceIDs",colnames(df.mod[,x1:x2]))

#if you want to use window metrics, type out the ones you want
features <- c("SourceIDs","CsPeak","CsWindow","CoPeak","CoWindow",
               "BaPeak","BaWindow","U235Peak","U235Window","U238Peak","U238Window",
               "BI214Peak","BI214Window","K40Peak","K40Window",
                "CsVar","CoVar","BaVar","U235Var","U238Var","K40Var","TH232Var","BI214Var",
               "CsSkew","CoSkew","BaSkew","U235Skew","U238Skew","BG",
               "CsKurt","CoKurt","BaKurt","U235Kurt","U238Kurt")


# Set seed to ensure reproducibility between runs
set.seed(12345)

# Set up caret to perform 10-fold cross validation repeated 3 times
caret.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3)

# Use caret to train a Mighty Random Forest using 10-fold cross 
# validation repeated 3 times and use 5 values for tuning the
# mtry parameter. Use 101 trees as our data is small. This code 
# returns the best model trained on all the data! Mighty!
rf.cv <- train(SourceIDs ~ ., 
               data = train[, features],
               method = "rf",
               trControl = caret.control,
               tuneLength =5,
               ntree = 101, 
               importance = TRUE,
               na.action=na.exclude)


# Display the results of the cross validation run
rf.cv


# What is the standard deviation?
cat(paste("\nCross validation standard deviation:",  
          sd(rf.cv$resample$Accuracy), "\n", sep = " "))


# Pull out the the trained model using the best parameters on
# all the data! Mighty!
rf.best <- rf.cv$finalModel
rf.best

# Look at the model - this model is trained on 100% of the data!
varImpPlot(rf.best)

# Create predictions for the test set
preds <- predict(rf.cv, test, type = "raw")


# Create dataframe of the KNOWN and PREDICTED sourceID for the test set
test.results <- data.frame(Name = test$Name, KnownSource = test$SourceIDs,
                         ModelPrediction = preds)

#only look at the cases it got wrong
test.false <- test.results[which(test.results$KnownSource!=test.results$ModelPrediction),]

#display the confusion matrix for the test set
confusionMatrix(preds,test$SourceIDs)

#save the random forest model to use later
saveRDS(rf.best, file="RF_features_long.rds")


### read in a saved model
## I actually don't think this is working
## please fix it for me :)
mod <- readRDS("RF_features_long.rds")
preds <- predict(mod, test, type = "raw")
test.results <- data.frame(Name = test$Name, KnownSource = test$SourceIDs,
                           ModelPrediction = preds)
test.false <- test.results[which(test.results$KnownSource!=test.results$ModelPrediction),]
confusionMatrix(preds,test$SourceIDs)



# make a visualiztion of the confusion matrix
confusion <- as.data.frame(as.table(r$confusion[1:5,1:5]))
plot <- ggplot(confusion)
plot + geom_tile(aes(x=Var1, y=Var2, fill=Freq)) + scale_x_discrete(name="Actual Class") + 
  scale_y_discrete(name="Predicted Class") + 
  scale_fill_gradient(low = "white", high = "darkblue", breaks=seq(from=0, to=6000, by=1000)) +
  labs(fill="Normalized\nFrequency") 
  


