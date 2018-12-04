## start feature engineering on the GADRAS model data
library(psych)
library(e1071)
library(ggplot2)

#you need to reset the working directory
setwd("C:/Users/turk_la/Documents/SSAM/Data/SSAM24April2017-v1.1-20170516/")

#load the csv of spectral data you want to investigate
#this can be asymptotic or sampled data
df.data <- read.csv("AllSampled.csv")
#df.data <- read.csv("AllRaster.csv")

#define a vector of bins to use at various points
bins <- 1:1024


#time to start generating some features
#let's do window metrics
#assuming 2.848 keV/channel
#I define a window around the peak of interest
#and a higher energy window to normalize against


#Cs is in channel 232
#Cs window 200:260, normalize to 280:980
x1 <- grep("Channel200",colnames(df.data))
x2 <- grep("Channel260",colnames(df.data))
y1 <- grep("Channel280",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
    df.data[i,"CsPeak"] <- sum(df.data[i,x1:x2])
    df.data[i,"CsWindow"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
    spec <- as.numeric(df.data[i,x1:x2])
    df.data[i,"CsMean"] <- mean(spec)
    df.data[i,"CsVar"] <- var(spec)
    df.data[i,"CsSkew"] <- skew(spec)
    df.data[i,"CsKurt"] <- kurtosis(spec)
}

#C0 window 385:510, normalize to 545:980
x1 <- grep("Channel385",colnames(df.data))
x2 <- grep("Channel510",colnames(df.data))
y1 <- grep("Channel545",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"CoPeak"] <- sum(df.data[i,x1:x2])
  df.data[i,"CoWindow"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"CoMean"] <- mean(spec)
  df.data[i,"CoVar"] <- var(spec)
  df.data[i,"CoSkew"] <- skew(spec)
  df.data[i,"CoKurt"] <- kurtosis(spec)
}

# Ba window 85:145, normalize to 170:980
x1 <- grep("Channel85",colnames(df.data))
x2 <- grep("Channel145",colnames(df.data))
y1 <- grep("Channel170",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"BaPeak"] <- sum(df.data[i,x1:x2])
  df.data[i,"BaWindow"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"BaMean"] <- mean(spec)
  df.data[i,"BaVar"] <- var(spec)
  df.data[i,"BaSkew"] <- skew(spec)
  df.data[i,"BaKurt"] <- kurtosis(spec)
}

#U235 window 55:75, normalize to 90:980
x1 <- grep("Channel55",colnames(df.data))
x2 <- grep("Channel75",colnames(df.data))
y1 <- grep("Channel90",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"U235Peak"] <- sum(df.data[i,x1:x2])
  df.data[i,"U235Window"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"U235Mean"] <- mean(spec)
  df.data[i,"U235Var"] <- var(spec)
  df.data[i,"U235Skew"] <- skew(spec)
  df.data[i,"U235Kurt"] <- kurtosis(spec)
}

#U238 window 320:375, normalize to 405:980
x1 <- grep("Channel320",colnames(df.data))
x2 <- grep("Channel375",colnames(df.data))
y1 <- grep("Channel405",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"U238Peak"] <- sum(df.data[i,x1:x2])
  df.data[i,"U238Window"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"U238Mean"] <- mean(spec)
  df.data[i,"U238Var"] <- var(spec)
  df.data[i,"U238Skew"] <- skew(spec)
  df.data[i,"U238Kurt"] <- kurtosis(spec)
}

#K40 window 440:530, normalize to 560:980
x1 <- grep("Channel440",colnames(df.data))
x2 <- grep("Channel530",colnames(df.data))
y1 <- grep("Channel560",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"K40Peak"] <- sum(df.data[i,x1:x2])
  df.data[i,"K40Window"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"K40Mean"] <- mean(spec)
  df.data[i,"K40Var"] <- var(spec)
  df.data[i,"K40Skew"] <- skew(spec)
  df.data[i,"K40Kurt"] <- kurtosis(spec)
}

#TH232 window 800:890, normalize to 920:980
x1 <- grep("Channel800",colnames(df.data))
x2 <- grep("Channel890",colnames(df.data))
y1 <- grep("Channel920",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"TH232Peak"] <- sum(df.data[i,x1:x2])
  df.data[i,"TH232Window"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"TH232Mean"] <- mean(spec)
  df.data[i,"TH232Var"] <- var(spec)
  df.data[i,"TH232Skew"] <- skew(spec)
  df.data[i,"TH232Kurt"] <- kurtosis(spec)
}

#BI214 window 550:630, normalize to 660:980
#targeting peak at 1764 keV
x1 <- grep("Channel550",colnames(df.data))
x2 <- grep("Channel630",colnames(df.data))
y1 <- grep("Channel660",colnames(df.data))
y2 <- grep("Channel980",colnames(df.data))
for(i in 1:nrow(df.data)){
  df.data[i,"BI214Peak"] <- sum(df.data[i,x1:x2])
  df.data[i,"BI214Window"] <- sum(df.data[i,x1:x2])/sum(df.data[i,y1:y2])
  spec <- as.numeric(df.data[i,x1:x2])
  df.data[i,"BI214Mean"] <- mean(spec)
  df.data[i,"BI214Var"] <- var(spec)
  df.data[i,"BI214Skew"] <- skew(spec)
  df.data[i,"BI214Kurt"] <- kurtosis(spec)
}

#the data frame now has a lot more columns from all the window metrics we calculated
#let's take a look
View(df.data)
#ok, it was pretty big; let's just look at a smaller piece of it
View(df.data[,-c(1,3:4,6:1029)])

# look closely at the first few columns and decide which you can throw out
write.csv(df.data[,-c(1:2)],"Samp_spec_feat.csv")

# DATA VISUALIZATION
# make a bunch of plots to see the value of the features you just engineered


#this load only necessary if you already wrote your file and are loading it later for data vis
#df.data <- read.csv("features.csv") 

#just a spectrum first to make sure it's still sensible
Nspec <- 8000  #which spectrum do you want to look at
x1 <- grep("Channel0", colnames(df.data))
x2 <- grep("Channel1023",colnames(df.data))
counts <- as.numeric(df.data[Nspec,x1:x2])  
ggplot() + geom_line(aes(x=bins, y=counts)) + scale_y_log10()

#some pair-wise multi-plots
pairs(df.data[,c("K40Skew","CoSkew","TH232Skew","BI214Skew","U238Skew")],col=df.data$SourceIDs)
pairs(df.data[,c("CsKurt","CoKurt","BaKurt","U235Kurt","U238Kurt")],col=df.data$SourceIDs)
pairs(df.data[,c("U235Skew","CoMean","BaVar","CsKurt","U238Kurt")],col=df.data$SourceIDs)

#looking closer at feature relationships
ggplot(df.data, aes(x=CoWindow, y=CsWindow)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U235Window, y=BaWindow)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=BI214Window, y=BaWindow)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U238Window, y=BaWindow)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CsMean, y=CsVar)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CsKurt, y=CsSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CsMean, y=CsSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CoMean, y=CoVar)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CoKurt, y=CoSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=CoMean, y=CoSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data[which(df.data$BG=="ABQ"),], aes(x=BaMean, y=BaVar)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=BaKurt, y=BaSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=BaMean, y=BaSkew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U235Mean, y=U235Var)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U235Kurt, y=U235Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U235Mean, y=U235Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U238Mean, y=U238Var)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U238Kurt, y=U238Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U238Mean, y=U238Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=K40Mean, y=K40Var)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=K40Kurt, y=K40Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=K40Mean, y=K40Skew)) +
  geom_point(aes(color=SourceIDs))

ggplot(df.data, aes(x=U238Var, y=TH232Var)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=U235Kurt, y=U235Skew)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=U238Mean, y=U238Var)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=TH232Mean, y=TH232Var)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=TH232Kurt, y=TH232Skew)) +
  geom_point(aes(color=BG))

ggplot(df.data, aes(x=TH232Mean, y=TH232Skew)) +
  geom_point(aes(color=BG))

