## sampling the asymptotic data to create new spectra
library(psych)
library(ggplot2)

#you need to reset the working directory
setwd("C:/Users/turk_la/Documents/SSAM/Data/SSAM24April2017-v1.1-20170516/")

#load in the file created by data_generation.R
#or another file that contains asymptotic spectra
df.all <- read.csv("AllRaster.csv")

#create a new data frame to hold the sampled spectra
df.samp <- df.all

# this loop performs two functions:
# downscale the spectra to a range that is more 1-second like
# and poisson sample each channel
n <- as.vector(1:nrow(df.all))
downscale <- 10000    #you can change this number if you want
x1 <- grep("Channel0",colnames(df.all))
x2 <- grep("Channel1023",colnames(df.all))
for(i in 1:1024){
  df.samp[,x1+i-1] <- rpois(n,(df.all[,x1+i-1]/downscale))
}


#let's take a quick look to make sure it's not crazy
bins <- as.vector(1:1024)
Nspec <- 15000  #which spectrum do you want to look at
counts <- as.numeric(df.samp[Nspec,x1:x2])
ggplot() + geom_line(aes(x=bins, y=counts)) +scale_y_log10()
  
#write the output file
write.csv(df.samp,file="AllSampled.csv")



