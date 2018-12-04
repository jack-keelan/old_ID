# this file takes GADRAS models and creates a new data set
# first, I scale the GADRAS models to a variety of signal strengths
# then I add those to each of the possible BACKGROUNDS
# the new data set is a raster over SCALED SOURCE and BACKGROUND spectra
# I know it looks beastly. Sorry about that.

# clearly you need to reset your working directory
setwd("C:/Users/turk_la/Documents/SSAM/Data/SSAM24April2017-v1.1-20170516/")

# reading in CSVs for the BG and Source data
df.BG <- read.csv("SSAM_Bkgds_24April2017.csv")
df.data <- read.csv("SSAM24April2017.csv")

# create SourceIDs as a useful FACTOR
# unfortunately, you need to figure out which rows correspond to which sources
# this could probably be automated with grep, feel free to improve it
df.BG$SourceIDs <- df.BG$Name   #the BG 'source' I set to the city name
df.data[1:44,"SourceIDs"] <- "Ba133"
df.data[45:88,"SourceIDs"] <- "Co60"
df.data[89:132,"SourceIDs"] <- "Cs137"
df.data[133:176,"SourceIDs"] <- "Ir192"
df.data[177:220,"SourceIDs"] <- "Se75"
df.data$SourceIDs <- as.factor(df.data$SourceIDs)

# instantiate a bunch of data frame to be used
# this are for temporary use as they get manipulated
# if you want to increase the number of scaling factors to raster over, you will need to add more
df.sum1 <- df.sum2 <- df.sum3 <- df.sum4 <- df.sum5 <- df.sum6 <- df.sum7 <- df.sum8 <- df.sum9 <- df.data
df.sum10 <- df.sum11 <- df.sum12 <- df.sum13 <- df.sum14 <- df.sum15 <- df.sum16 <- df.sum17 <- df.sum18 <- df.sum19 <- df.sum20 <- df.data
df.scale1 <- df.scale2 <- df.scale3 <- df.scale4 <- df.scale5 <- df.scale6 <- df.scale7 <- df.scale8 <- df.scale9 <- df.data
df.scale10 <- df.scale11 <- df.scale12 <- df.scale13 <- df.scale14 <- df.scale15 <- df.scale16 <- df.scale17 <- df.scale18 <- df.scale19 <- df.scale20 <- df.data
df.temp0 <- df.temp1 <- df.temp2 <- df.temp3 <- df.temp4 <- df.temp5 <- df.temp6 <- df.temp7 <- df.temp8 <- df.temp9 <- df.temp10 <- df.data
df.temp11 <- df.temp12 <- df.temp13 <- df.temp14 <- df.temp15 <- df.temp16 <- df.temp17 <- df.temp18 <- df.temp19 <- df.temp20 <- df.data

# find where the spectral data is
x1 <- grep("Channel0",colnames(df.data))
x2 <- grep("Channel1023",colnames(df.data))

# down scale the source data
# as you see, I scale over a range from 75% to 2%
# you may find that you need finer scaling at the high end; feel free to adjust
df.scale1[,x1:x2] <- 0.02*df.data[,x1:x2]
df.scale2[,x1:x2] <- 0.04*df.data[,x1:x2]
df.scale3[,x1:x2] <- 0.06*df.data[,x1:x2]
df.scale4[,x1:x2] <- 0.08*df.data[,x1:x2]
df.scale5[,x1:x2] <- 0.10*df.data[,x1:x2]
df.scale6[,x1:x2] <- 0.12*df.data[,x1:x2]
df.scale7[,x1:x2] <- 0.14*df.data[,x1:x2]
df.scale8[,x1:x2] <- 0.16*df.data[,x1:x2]
df.scale9[,x1:x2] <- 0.18*df.data[,x1:x2]
df.scale10[,x1:x2] <- 0.20*df.data[,x1:x2]
df.scale11[,x1:x2] <- 0.22*df.data[,x1:x2]
df.scale12[,x1:x2] <- 0.24*df.data[,x1:x2]
df.scale13[,x1:x2] <- 0.26*df.data[,x1:x2]
df.scale14[,x1:x2] <- 0.28*df.data[,x1:x2]
df.scale15[,x1:x2] <- 0.30*df.data[,x1:x2]
df.scale16[,x1:x2] <- 0.35*df.data[,x1:x2]
df.scale17[,x1:x2] <- 0.40*df.data[,x1:x2]
df.scale18[,x1:x2] <- 0.45*df.data[,x1:x2]
df.scale19[,x1:x2] <- 0.50*df.data[,x1:x2]
df.scale20[,x1:x2] <- 0.75*df.data[,x1:x2]

#create lists of data frames to use in loops

#a list of all the scaled data frames
dflist <- list(df.data, df.scale1, df.scale2,df.scale3, df.scale4,df.scale5, 
               df.scale6,df.scale7, df.scale8, df.scale9, df.scale10,df.scale11, 
               df.scale12,df.scale13, df.scale14, df.scale15, df.scale16,
               df.scale17, df.scale18, df.scale19, df.scale20)
#a list of all the scaling factors
factor <- c(1,.02, .04, .06, .08, .10, .12, .14, .16, .18, .20, .22, .24, .26, 
            .28, .30, .35, .40, .45, .50, .75)

#a list of data frames that will be used as holders along the way
templist <- list(df.temp0,df.temp1, df.temp2, df.temp3, df.temp4, df.temp5,
                 df.temp6, df.temp7, df.temp7, df.temp9, df.temp10, df.temp11, 
                 df.temp12, df.temp13, df.temp14, df.temp15,df.temp16, 
                 df.temp17, df.temp18, df.temp19, df.temp20)

#this is the BIG LOOP for adding source and background data
#my apologies

for(j in 1:length(dflist)){         #looping over the list of dataframes of scaled sources
  df.temp <- as.data.frame(dflist[j])
  df.tempsum <- as.data.frame(templist[j])
  for(i in 1:1024){                 #looping over the spectral channels to sum source & bg
    df.sum1[,i+19] <- df.temp[,i+19] + df.BG[1,i+19]  #I used 9 different BG spectra 
    df.sum2[,i+19] <- df.temp[,i+19] + df.BG[2,i+19]  #if you have a different number of spectra
    df.sum3[,i+19] <- df.temp[,i+19] + df.BG[3,i+19]  #you will have to adjust this loop
    df.sum4[,i+19] <- df.temp[,i+19] + df.BG[4,i+19]
    df.sum5[,i+19] <- df.temp[,i+19] + df.BG[5,i+19]
    df.sum6[,i+19] <- df.temp[,i+19] + df.BG[6,i+19]
    df.sum7[,i+19] <- df.temp[,i+19] + df.BG[7,i+19]
    df.sum8[,i+19] <- df.temp[,i+19] + df.BG[8,i+19]
    df.sum9[,i+19] <- df.temp[,i+19] + df.BG[9,i+19]
  }
  df.sum1[,"Name"] <- paste(as.character(df.data[,"Name"]), 
            as.character(df.BG[1,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum2[,"Name"] <- paste(as.character(df.data[,"Name"]), 
            as.character(df.BG[2,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum3[,"Name"] <- paste(as.character(df.data[,"Name"]), 
            as.character(df.BG[3,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum4[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[4,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum5[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[5,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum6[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[6,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum7[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[7,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum8[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[8,"Name"]),"x",as.character(factor[j]), sep="")
  df.sum9[,"Name"] <- paste(as.character(df.data[,"Name"]),as.character(df.BG[9,"Name"]),"x",as.character(factor[j]), sep="")
  #this is a temporary holder
  df.tempsum <- rbind(df.sum1, df.sum2, df.sum3, df.sum4, df.sum5, df.sum6, 
                      df.sum7, df.sum8, df.sum9)
  templist[[j]] <- df.tempsum
}

#I bind all of that sh!t together into one massive data frame
df.sums <- rbind(templist[[1]], templist[[2]],templist[[3]],templist[[4]],
                 templist[[5]],templist[[6]],templist[[7]],templist[[8]],
                 templist[[9]],templist[[10]],templist[[11]],templist[[12]],
                 templist[[13]],templist[[14]],templist[[15]],templist[[16]],
                 templist[[17]],templist[[18]],templist[[19]],templist[[20]],templist[[21]])

#adding a column to indicate which background is used in the spectrum
bg1 <- grep("ABQ",df.sums$Name)
df.sums[bg1,"BG"] <- "ABQ"
bg2 <- grep("ATLANTA",df.sums$Name)
df.sums[bg2,"BG"] <- "ATL"
bg3 <- grep("INDIANAPOLIS",df.sums$Name)
df.sums[bg3,"BG"] <- "IND"
bg4 <- grep("LASVEGAS",df.sums$Name)
df.sums[bg4,"BG"] <- "LAS"
bg5 <- grep("NEWYORK",df.sums$Name)
df.sums[bg5,"BG"] <- "NYC"
bg6 <- grep("SEATTLE",df.sums$Name)
df.sums[bg6,"BG"] <- "SEA"
bg7 <- grep("KMAX",df.sums$Name)
df.sums[bg7,"BG"] <- "KMAX"
bg8 <- grep("UMAX",df.sums$Name)
df.sums[bg8,"BG"] <- "UMAX"
bg9 <- grep("THMAX",df.sums$Name)
df.sums[bg9,"BG"] <- "THMAX"

#write the output file of all spectra created
write.csv(df.sums,"AllRaster.csv")

#look at some plots to make sure things look sensible
bins <- matrix(1:1024)
Nspec <- 8000  #which spectrum do you want to look at
#x1 & x2 were defined earlier as the column indeces for Channel 0 and Channel 1023
counts <- as.numeric(df.sums[Nspec,x1:x2])  
ggplot() + geom_line(aes(x=bins, y=counts)) + scale_y_log10()

