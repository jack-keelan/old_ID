
library(tidyverse)
library(scatterplot3d)
library(ggplot2)
library(rhdf5)
library(stats)
library(psych)
library(MASS)
library(yaml)

setwd("C:/Users/mooreet_la/Documents/Data Science/data")

##############################################
# Julia's Data

benchtest4 <- read.csv("juliaDataSet/BenchtopLab/EricCoCsBckCf_Ortec_20170414153501.csv")

x<-grep("Ch_01",colnames(benchtest4))
SpecBenchtest <-t(benchtest4[,x[1]:(x[1]+1022)])
SpecBenchtestCs <- subset(SpecBenchtest, select = c(1:3800))
SpecBenchtestCo <- subset(SpecBenchtest, select = c(3900:7350))
SpecBenchtestBkg <- subset(SpecBenchtest, select = c(7400:11000))
SpecBenchtestCf <- subset(SpecBenchtest, select = c(11000:12500))
 
View(SpecBenchtestCs)

column.names <- c("Bin","Energy","AveCs","AveCo","AveBkg","AveCf")
row.names <- c(1:1022)
AveBenchtest <- matrix(NA,nrow=1022,ncol=6,dimnames=list(row.names,column.names))

for(i in 1:1022){
  AveBenchtest[i,"Bin"]<-i
  AveBenchtest[i,"Energy"]<-(1.055+i*2.848)
  AveBenchtest[i,"AveCs"]<-mean(SpecBenchtestCs[i,])
  AveBenchtest[i,"AveCo"]<-mean(SpecBenchtestCo[i,])
  AveBenchtest[i,"AveBkg"]<-mean(SpecBenchtestBkg[i,])
  AveBenchtest[i,"AveCf"]<-mean(SpecBenchtestCf[i,])
#  AveBenchtest[i,"Ave"]<-mean(benchtest4[,x[1]+i-1])
#  AveBenchtest[i,"Sum"]<-sum(benchtest4[,x[1]+i-1])
}
#####################################################################
#########################Just Visualization ##########################
View(AveBenchtest)
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

#########################Just Visualization ##########################
#####################################################################

### Julia's Data
##############################################

############################################################
##  Sarah's yaml
ModelAll <- read.csv("asymptotics/SSAM03Mar2017.csv",header=FALSE)
View(ModelAll)

####################
##  energy resolution

column.names <- c("bin","Energy","FWHM","F/E")
row.names <- c(1:9)
bin <- c(10.5,52,74,225,301,399,454,597,869)  #160
energy <- c(32.5,155,217,661,884,1171,1332,1750,2547) #468
FWHM <- c(6.2,27.4,32,50,55.8,67,67,82,101.5) #27.4   38.2
F_E <- (FWHM/energy)
resolution <- matrix(c(bin,energy,FWHM,F_E),ncol = 4, nrow = 9, 
                     dimnames=list(row.names,column.names))

View(resolution)
x_1 <- resolution[1:9,1]
y_1 <- resolution[1:9,4]
############3
##  the winner
plot(x_1,y_1, ylim = c(.04,0.2), xlim=c(0,1000))

beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")

data_func <- data.frame(y=(y_scale*log(beta/x_1 )), x=x_1 )
points(data_func$x,data_func$y,col="red")

data_rev_func <- data.frame(x=(beta/exp(y_1/y_scale)), y=y_1)
points(data_rev_func$x,data_rev_func$y,col="green")

#######################################
#################33
###########################
## using this formula
column.names <- c("bin","F/E","res diff","rec_bin")
row.names <- c(1:1024)
bin_res <- matrix(ncol = 4, nrow = 1024, 
                     dimnames=list(row.names,column.names))

#data_func <- data.frame(y=(y_scale*log(beta/x_1 )), x=x_1 )
#points(data_func$x,data_func$y,col="red")
#data_rev_func <- data.frame(x=(beta/exp(y_1/y_scale)), y=y_1)
#points(data_rev_func$x,data_rev_func$y,col="green")
for(i in 2:1024){
  bin_res[i,1] <- i
  bin_res[i,2] <- (y_scale*log(beta/i ))
  bin_res[i,4] <- (beta/exp(bin256[i,2]/y_scale))
  if(i == 1){
    bin_res[i,3] <- bin_res[i,2]
  }
  else{
    bin_res[i,3] <- bin_res[i,2]-bin_res[i-1,2]
  }
  if(bin_res[i,3] >= 0) print(i)
}

View(bin_res)

res_delta <- bin_res[6,2]-bin_res[916,2]  # gives nice bin width 6.400e-04
res_bin_width <- res_delta/256




column.names <- c("bin","res_bottom","res_center","1024_bottom", 
                  "bottom","residue","1024_center","Cs")
row.names <- c(1:256)
bin256 <- matrix(ncol = 8, nrow = 256, 
                  dimnames=list(row.names,column.names))

bin256[1,2] <- bin_res[6,2]
#ModelIso40[1:1024,20]
for(i in 1:256){
  bin256[i,1] <- i
  if(i >= 2) bin256[i,2] <- (bin_res[6,2] - i*res_bin_width)
  bin256[i,3] <- (bin256[i,2] - (res_bin_width/2) )
  bin256[i,4] <- (beta/exp(bin256[i,2]/y_scale))
  bin256[i,7] <- (beta/exp(bin256[i,3]/y_scale))
  bin256[i,5] <- trunc(bin256[i,4])
  if(0.99999999999 <= bin256[i,4] - bin256[i,5]) bin256[i,5] <- bin256[i,5]+1
  bin256[i,6] <- bin256[i,4] - bin256[i,5]

}
View(bin256)

for(i in 2:256){
    if(bin256[i,5] == bin256[i-1,5]){
      bin256[i,8] <- AveBenchtest[bin256[i,5],5]*(bin256[i,6] - bin256[i-1,6])
    } else {
      bin256[i,8] <- AveBenchtest[bin256[i,5],5]*(bin256[i,6])
    }
}

plot(bin256[,1],bin256[,8], log="y")

bin_1024_mod <- bin_res[-c(1:5),]

column.names <- c("bin","counts")
row.names <- c(1:1024)
bin_1024_temp <- matrix(ncol = 2, nrow = 1024, 
                 dimnames=list(row.names,column.names))

for(i in 1:1022){
  bin_1024_temp[i,1] <- i
  if(AveBenchtest[i,1] == bin256[100,5]){
    bin_1024_temp[i,2] <- AveBenchtest[i,"AveCo"] * (bin256[i+1,4]-bin256[i,4])
  }
}

AveBenchtest[i,1]


View(bin_1024_temp)

##############################################################33
########################################################3333
#  continuous Cs
column.names <- c("bin","counts")
row.names <- c(1:1022)
renorm_Cs <- matrix(ncol = 2, nrow = 1022, 
                        dimnames=list(row.names,column.names))
renorm_Cs[,1] <- 1:1022
renorm_Cs[,2] <- 1e03*AveBenchtest[,3]

View(renorm_Cs)

Cs_temp <- NA
for(i in 1:1022){
  if(renorm_Cs[i,2] >= 1){
#    print(i)
    Cs_temp[i] <- (i-1)+(0.5/renorm_Cs[i,2])
    for(j in 1:renorm_Cs[i,2]){
#      print("what    ")
#      print(i)
      Cs_temp[i] <- ((i-1)+(0.5/renorm_Cs[i,2]) + j/renorm_Cs[i,2])
#      print(Cs_temp)
    }
  } else {
    Cs_temp[i] <- NA
  }
}

Cs_temp_res[i] <- NA
for(i in 1:sum(na.omit(Cs_temp))){
  Cs_temp_res[i] <- y_scale*log(beta/Cs_temp[i])
}

res_width_Cs <- (max(na.omit(Cs_temp_res))-min(na.omit(Cs_temp_res)))/256
new_Cs <- NA
for(j in 1:256){
  new_Cs[j] <-0
  for(i in 1:sum(na.omit(Cs_temp)) ){
    if(Cs_temp_res[i] < (res_width_Cs*j)) new_Cs[j] <- new_Cs[j]+1
  }
}
#   EXPENSE REPORT
#    SDRD MONTHLKY
#    Strategis SEARCH
#    NEUTRONS
#   OPS PLAN
#   ??????????????????????????????????????????



###############################################################
#################################################################

## using this formula
###############################


############################################3
#######################################3
#    Poisson numbers

set.seed(100)
rpois(100,6)
range(rpois(100,6))

range(rpois(100,6))[1]:range(rpois(100,6))[2]  #break points?
set.seed(100)

cut(rpois(100,6),range(rpois(100,6))[1]:range(rpois(100,6))[2]) # corresponding intervals

cut(rpois(100,6),pretty(rpois(100,6))) # 'pretty' creats the break points for you

pretty(rpois(1000,6))
pretty(rpois(1000,50),15)
## energy resolution



# Training on Sarah's data
############################
##   Sarah's yaml
################



#create new dataframe with just the spectral data
#column 1 will be the bin numbers
#columns 1+i will be individual spectra
column.names <- c("Bin","Energy","Ave","Sum")
row.names <- c(1:1023)

x<-grep("Ch_01",colnames(dfCs10m))
######### why are these stacked? 43 1068 ... and why the change in interval?
SpecCs10m<-t(dfCs10m[,x[1]:(x[1]+1022)])
View(SpecCs10m)
AveCs10m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveCs10m[i,"Bin"]<-i
  AveCs10m[i,"Energy"]<-(1.055+i*2.848)    ### where does this cal come from?
  AveCs10m[i,"Ave"]<-mean(dfCs10m[,x[1]+i-1])
  AveCs10m[i,"Sum"]<-sum(dfCs10m[,x[1]+i-1])
}

