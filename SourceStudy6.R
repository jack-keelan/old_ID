
library(tidyverse)
library(scatterplot3d)
library(ggplot2)
library(rhdf5)
library(stats)
library(psych)
library(MASS)
library(yaml)
#setwd("C:/Users/turk_la/Documents/SSAM/Data")
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
f <- vector(numeric(1:20))
#Working with long dwell Ba133 data at different distances
#step 1: load data files and fill arrays with spectra
dfBa5m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_5m_20150729.csv")
dfBa10m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_10m_20150729.csv")
dfBa15m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_15m_20150729.csv")
dfBa20m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_20m_20150729.csv")
dfBa25m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_25m_20150729.csv")
dfBa30m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_30m_20150729.csv")
dfBa35m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_35m_20150729.csv")
dfBa40m<- read.csv("Ba133/RSLMobile/RSLMobile_Ba133_40m_20150729.csv")
dfBa0m<- read.csv("Ba133/RSLMobile/RSLMobile_Bkgd_AM_20150729.csv")

dfCs10m<- read.csv("Cs137/RSLMobile/RSLMobile_Cs137_10m.csv")

#######################
# Julia's Data
#######################
benchtest <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata1_20170328114019v2.csv")
benchtest2 <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata2_Ortec_20170328141651.csv")
benchtest3 <- read.csv("juliaDataSet/BenchtopLab/Eric2hrdata3_Ortec_20170328162214.csv")
benchtest4 <- read.csv("juliaDataSet/BenchtopLab/EricCoCsBckCf_Ortec_20170414153501.csv")
View(benchtest4)
dim(benchtest4)
#View(dfBaBa5m)

SpecBenchtest <-t(benchtest4[,x[1]:(x[1]+1022)])
SpecBenchtestCs <- subset(SpecBenchtest, select = c(1:3800))
SpecBenchtestCo <- subset(SpecBenchtest, select = c(3900:7350))
SpecBenchtestBkg <- subset(SpecBenchtest, select = c(7400:11000))
SpecBenchtestCf <- subset(SpecBenchtest, select = c(11000:12500))
 
View(SpecBenchtestCs)

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
  AveBenchtest[i,"AveCf"]<-mean(SpecBenchtestCf[i,])
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
plot(x=AveBenchtest[1:1022,1],y=AveBenchtest[1:1022,3],xlab="Enerrgy", ylab="Counts", 
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
#######################
### Julia's Data
#######################

####################
##  Model data
#dfBaBa5m <- data.frame()
#dfBaBa5m <- data.frame(Doubles=double(),
#                 Ints=integer(),
#                 Factors=factor(),
#                 Logicals=logical(),
#                 Characters=character(),
#                 stringsAsFactors=FALSE)
#dfBaBa5m <- yaml.load_file("SSAM/IsotopicsMLSDRD/yaml/BABA05.yaml")
#dfBaBa[,1] <- dfBaBa5m
#dfBaBa10m<- read.csv("SSAM/IsotopicsMLSDRD/yaml/BABA10.yaml")
#dfBaBa20m<- read.csv("SSAM/IsotopicsMLSDRD/yaml/BABA20.yaml")
#dfBaBa40m<- read.csv("SSAM/IsotopicsMLSDRD/yaml/BABA40.yaml")
##############
##  Sarah's yaml
dfCoDU <- dfCoST <- dfCoBA
column.names <- c(1:20)
row.names <- c(1:1042)
testjunk <- data.frame(matrix(ncol = 20, nrow = 1042,
                                dimnames=list(row.names,column.names)))
View(testjunk)
testjunk[1:1042,1] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/BABA40.yaml")
testjunk[1:1042,2] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/BACO40.yaml")
testjunk[1:1042,3] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/BAST40.yaml")
testjunk[1:1042,4] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/BADU40.yaml")
testjunk[1:1042,5] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/COBA40.yaml")
testjunk[1:1042,6] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/COCO40.yaml")
testjunk[1:1042,7] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/COST40.yaml")
testjunk[1:1042,8] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CODU40.yaml")
testjunk[1:1042,9] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CSBA40.yaml")
testjunk[1:1042,9] <- as.double(as.character(k))
k <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CSBA40.yaml")
View(k)
testjunk[1:1042,9] <- as.double(as.character(k))
testjunk[1:1042,10] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CSCO40.yaml")
testjunk[1:1042,11] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CSST40.yaml")
testjunk[1:1042,12] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/CSDU40.yaml")
testjunk[1:1042,13] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/IRBA40.yaml")
testjunk[1:1042,14] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/IRCO40.yaml")
testjunk[1:1042,15] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/IRST40.yaml")
testjunk[1:1042,16] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/IRDU40.yaml")
testjunk[1:1042,17] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/SEBA40.yaml")
testjunk[1:1042,18] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/SECO40.yaml")
testjunk[1:1042,19] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/SEST40.yaml")
testjunk[1:1042,20] <- read.csv("SSAM/IsotopicsMLSDRD/yaml/SEDU40.yaml")
testjunk <- (testjunk[-c(1:20,1042),])

#test yaml
#testjunk <- yaml.load_file('SSAM03Mar2017.yaml')
View(testjunk[,1015:1021])
#setwd("C:/Users/mooreet_la/Documents/Data Science/data/asymptotics")
testyaml<- read.csv("asymptotics/SSAM03Mar2017.yaml")
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
#fit3 <- lm( y~poly(x_1,3) )  ## poly fails
lm.1 <- lm(y_1 ~ x_1 + I(x_1^2) + I(x_1^3) )
lm.1reverse <- lm(x_1 ~ y_1 + I(y_1^2) + I(y_1^3) )
exponential.model <- lm(y_1~ exp(-x_1))
summary(exponential.model)

#exponential.model <- lm(log(Counts)~ Time)
#summary(exponential.model)
#xvalues <- seq(0,0.5, 0.01)
#y.exponential2 <- exp(predict(exponential.model,list(x_1=xvalues)))
#plot(x_1,y_1,pch=16)
#lines(xvalues, y.exponential2,lwd=2, col = "red", xlab = "bins", ylab = "res")


plot(x_1,y_1, xlim=c(0,1000))
beta <- 3500; y_scale <- 0.035; n <- 800
new_func <- data.frame(y=y_scale*log(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="lightblue")
#beta <- 2500; y_scale <- 0.024; n <- 1000
#new_func <- data.frame(y=y_scale*log2((beta/seq(n))), x=seq(n) )
#lines(new_func$x, new_func$y, col="green")
beta <- 3400; y_scale <- 0.022; n <- 1000
new_func <- data.frame(y=y_scale*log2(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="blue")
beta <- 2100; y_scale <- 0.038; n <- 900
new_func <- data.frame(y=y_scale*log1p(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="yellow")
#beta <- 2500; y_scale <- 0.036; n <- 1000
#new_func <- data.frame(y=y_scale*log1p( (beta/seq(n)) -0.75 ), x=seq(n) )
#lines(new_func$x, new_func$y, col="red")
beta <- 3000; y_scale <- 0.085; n <- 1000
new_func <- data.frame(y=y_scale*log10(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")




plot(x_1,y_1)
beta <- 2500; y_scale <- 0.024; n <- 900
new_func <- data.frame(y=y_scale*log2((beta/seq(n))), x=seq(n) )
lines(new_func$x, new_func$y, col="red")
beta <- 3000; y_scale <- 0.024; n <- 900
new_func <- data.frame(y=y_scale*log2(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="blue")
beta <- 3400; y_scale <- 0.023; n <- 900
new_func <- data.frame(y=y_scale*log2((beta/seq(n)) ), x=seq(n) )
lines(new_func$x, new_func$y, col="green")
beta <- 3200; y_scale <- 0.023; n <- 900
new_func <- data.frame(y=y_scale*log2(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="lightblue")
##############3
plot(x_1,y_1)
beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")
beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log((beta/seq(n) +.1) ), x=seq(n) )
lines(new_func$x, new_func$y, col="blue")
beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log((beta/seq(n) +0.25)), x=seq(n) )
lines(new_func$x, new_func$y, col="green")
beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log((beta/seq(n) ) +.5), x=seq(n) )
lines(new_func$x, new_func$y, col="lightblue")
#########################
plot(x_1,y_1)
beta <- 3000; y_scale <- 0.085; n <- 1000
new_func <- data.frame(y=y_scale*log10(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")
beta <- 3000; y_scale <- 0.075; n <- 1000
new_func <- data.frame(y=y_scale*log10(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="blue")
beta <- 3000; y_scale <- 0.080; n <- 1000
new_func <- data.frame(y=y_scale*log10(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="green")
beta <- 3500; y_scale <- 0.085; n <- 1000
new_func <- data.frame(y=y_scale*log10((beta/seq(n) ) ), x=seq(n) )
lines(new_func$x, new_func$y, col="lightblue")
#########################

plot(x_1,y_1)
beta <- 2100; y_scale <- 0.038; n <- 900
new_func <- data.frame(y=y_scale*log1p(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")
beta <- 2500; y_scale <- 0.036; n <- 900
new_func <- data.frame(y=y_scale*log1p( (beta/seq(n)) -0.75 ), x=seq(n) )
lines(new_func$x, new_func$y, col="blue")
beta <- 2500; y_scale <- 0.038; n <- 900
new_func <- data.frame(y=y_scale*log1p(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="green")
beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log1p((beta/seq(n) )  ), x=seq(n) )
lines(new_func$x, new_func$y, col="lightblue")
############3
##  the winner
plot(x_1,y_1, ylim = c(.04,0.2), xlim=c(0,1000))

beta <- 2500; y_scale <- 0.037; n <- 900
new_func <- data.frame(y=y_scale*log(beta/seq(n) ), x=seq(n) )
lines(new_func$x, new_func$y, col="red")

#beta <- 2100; y_scale <- 0.038; n <- 900
#new_func <- data.frame(y=y_scale*log1p(beta/seq(n) ), x=seq(n) )
#lines(new_func$x, new_func$y, col="blue")

data_func <- data.frame(y=(y_scale*log(beta/x_1 )), x=x_1 )
points(data_func$x,data_func$y,col="red")

data_rev_func <- data.frame(x=(beta/exp(y_1/y_scale)), y=y_1)
points(data_rev_func$x,data_rev_func$y,col="green")

###############################
plot(x_1,fitted(exponential.model))
points((1/resolution[1:9,1]), fitted(exponential.model), col='red', pch=20)
#lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")
#######################################
# 1:7 data
y_0 <- 2.354e-02
a <- 4.812e+01
b <- -9.706e+03
c <- 2.599e+05
# 1:7 energy fit data
y_0 <- 2.101e-01
a <- -2.9635e-04
b <- 1.765e-07
c <- -3.397e-11
# 1:9 bin fit data
y_0 <- 2.044e-01
a <- -8.235e-04
b <- 1.422e-06
c <- -7.973e-10
plot(x_1,y_1)
lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")
# reverse fit from resolution to bin
x_0 <- 2428  # false
ax <- -56486
bx <- 430562
cx <- -1055042
plot(y_1,x_1)
lines(x=y_1, y=((ax*y_1) + (bx*(y_1^2)) + (cx*y_1^3) + x_0 ),col="blue")
#######################
#plot(x=1/resolution[1:7,1], y=resolution[1:7,3], xlab="Energy", ylab="FWHM", 
#     main="model data", pch=15, col="black", type="p")
#lines(x=(1/resolution[1:7,1]), y=(150/(resolution[1:7,1])), col="green")


plot(x=resolution[2:7,1], y=resolution[2:7,1], xlab="Energy", ylab="F/E", 
     main="model data", pch=15, col="black", type="p")

library(ggplot2)
fit <- lm(resolution[1:7,3] ~ resolution[1:7,1] + I(resolution[1:7,1]))
#            I(resolution[1:7,1]^3))

#fit <- lm(resolution[1:7,3] ~ I(1/(resolution[1:7,1])))
#            I(resolution[1:7,1]^3))

plot(resolution[2:7,3]~resolution[2:7,1])
#plot(resolution[1:7,1]~(1/resolution[1:7,1]))
points((resolution[1:7,1]), fitted(fit), col='red', pch=20)




#################33
###########################
## using this formula
y_0 <- 2.030e-01
a <- -7.840e-04
b <- 1.277e-06
c <- -6.809e-10

#plot(x=1/resolution[1:7,1], y=resolution[1:7,3], xlab="Energy", ylab="resolution", 
#     main="model data", pch=15, col="black", 
#     type="p") #,xlim=c(0,0.1), ylim=c(0.0,0.51))

plot(x_1,y, ylim = c(0,0.2))
lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")
lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")

# create bin/resolution tranformation vector
# 1:9 bin fit data
y_0 <- 2.044e-01
a <- -8.235e-04
b <- 1.422e-06
c <- -7.973e-10
plot(x_1,y_1)
lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")
# reverse fit from resolution to bin
x_0 <- 2428  # false
ax <- -56486
bx <- 430562
cx <- -1055042
plot(y_1,x_1)
lines(x=y_1, y=((ax*y_1) + (bx*(y_1^2)) + (cx*y_1^3) + x_0 ),col="blue")

column.names <- c("bin","F/E","res diff")
row.names <- c(1:1024)
bin_res <- matrix(ncol = 3, nrow = 1024, 
                     dimnames=list(row.names,column.names))

## y = mx + yl_0
x_start <- 480
x_final <- 869
y_start <- ((a*x_start) + (b*(x_start^2)) + (c*(x_start^3)) + y_0 )
y_final <- ((a*x_final) + (b*(x_final^2)) + (c*(x_final^3)) + y_0 )
rise <- (y_final - y_start)

run <- (x_final - x_start)
m <- (rise/run)
y_intercept <- ((-m*x_start) + ((a*x_start) + (b*(x_start^2)) + 
                  (c*(x_start^3))+ y_0 ))
#y_intercept <- ((-m*x_final) + (a*x_final) + (b*(x_final^2)) + 
#                   (c*(x_final^3)) + y_0 )

plot(x_1,y, xlim=c(475,485), ylim = c(0.043,0.048))
lines(x=x_1, y=((a*x_1) + (b*(x_1^2)) + (c*x_1^3) + y_0 ), col="blue")
abline(h=0.046)
lines(x=x_1[1:9], y=(m*x_1[1:9]+y_intercept), col="red")


data_func <- data.frame(y=(y_scale*log(beta/x_1 )), x=x_1 )
points(data_func$x,data_func$y,col="red")

data_rev_func <- data.frame(x=(beta/exp(y_1/y_scale)), y=y_1)
points(data_rev_func$x,data_rev_func$y,col="green")
for(i in 1:1024){
  bin_res[i,1] <- i
#  bin_res[i,2] <- ((a*i) + (b*(i^2)) + (c*(i^3)) + y_0 )
#  if(i <= 480) bin_res[i,2] <- ((a*i) + (b*(i^2)) + (c*(i^3)) + y_0 )
#  if(i >= 481) bin_res[i,2] <- ((m*i) + y_intercept)
  bin_res[i,2] <- (y_scale*log(beta/i ))
  if(i == 1){
    bin_res[i,3] <- bin_res[i,2]
  }
  else{
    bin_res[i,3] <- bin_res[i,2]-bin_res[i-1,2]
  }
  if(bin_res[i,3] >= 0) print(i)
}

View(bin_res)

res_delta <- bin_res[1,2]-bin_res[1024,2]  # gives nice bin width 6.400e-04
res_bin_width <- res_delta/256


column.names <- c("bin","res_bottom","res_center","1024_bottom","1024_center")
row.names <- c(1:256)
bin256 <- matrix(ncol = 5, nrow = 256, 
                  dimnames=list(row.names,column.names))

bin256[1,2] <- bin_res[1,2]
#ModelIso40[1:1024,20]
for(i in 1:256){
  bin256[i,1] <- i
  if(i >= 2) bin256[i,2] <- (bin_res[1,2] - i*res_bin_width)
  bin256[i,3] <- (bin256[i,2] - (res_bin_width/2) )
  bin256[i,4] <- (beta/exp(bin256[i,2]/y_scale))
  bin256[i,5] <- (beta/exp(bin256[i,3]/y_scale)) 
  
}
View(bin256)


## using this formula
###############################






## energy resolution
################3
# other yaml


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
  ModelIso40[,j+1] <- testjunk[,j]
#  ModelIso40[,j+1] <- testjunk[,3+((j-1)*4)]
#  print(3+((j-1)*4))
}
View(ModelIso40)

plot(x=ModelIso40[1:1024,1], y=testjunk[1:1024,20], xlab="Bin", ylab="Counts", 
     main="Spectrum Bare at 40m", pch=15, col="black", type="l")

lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,6], log="y", col="blue")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,10], log="y", col="red")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,14], log="y", col="green")
lines(x=ModelIso40[1:1024,1], y=ModelIso40[1:1024,18], log="y", col="coral")

legend('bottomleft',c('Ba','Co','Cs','Ir','Se'),
       fill = c('black', 'blue', 'red','green','coral'), bty = 'n',
       border = NA)



#testYaml[,2] <- as.double(as.character(testyaml[c(22:1043),]))
#testYaml[,3] <- as.double(as.character(testyaml[c(1065:2086),]))
#View(testYaml)
#######################
column.names <- c("Bin","Ba","Co","Cs","Ir","Se")
row.names <- c(1:1022)
ModelBA20 <- data.frame(matrix(ncol = 6, nrow = 1022,
                               dimnames=list(row.names,column.names)))
for(i in 1:1022){
  j = i
  ModelBA20[i,1] <- i
}
View(dfCs)

View(dfCo)
ModelBA40[,2] <- as.double(as.character(dfBa[-c(1:19,1042),]))
ModelBA40[,3] <- as.double(as.character(dfCo[-c(1:19,1042),]))
ModelBA40[,4] <- as.double(as.character(dfCs[-c(1:19,1042),]))
ModelBA40[,5] <- as.double(as.character(dfIr[-c(1:19,1042),]))
ModelBA40[,6] <- as.double(as.character(dfSe[-c(1:19,1042),]))
View(ModelBA20)

SumModelBA20Ba <- SumModelBA20Co <- SumModelBA20Cs <- SumModelBA20Ir <- SumModelBA20Se <- 0
SumModelBA20Ba <- sum(ModelBA20[,2])
SumModelBA20Co <- sum(ModelBA20[,3])
SumModelBA20Cs <- sum(ModelBA20[,4])
SumModelBA20Ir <- sum(ModelBA20[,5])
SumModelBA20Se <- sum(ModelBA20[,6])

#AveModelBA20 <- 0
column.names <- c("Bin","Energy", "NormBa","NormCo","NormCs","NormIr","NormSe")
row.names <- c(1:1022)
NormModelBA20 <- matrix(NA,nrow=1022,ncol=7,dimnames=list(row.names,column.names))
for(i in 1:1022){
  NormModelBA20[i,"Bin"] <- i
  NormModelBA20[i,"Energy"] <- (1.055+i*2.848)
  NormModelBA20[i,"NormBa"] <- (ModelBA20[i,2]/sum(ModelBA20[,2]))
  NormModelBA20[i,"NormCo"] <- (ModelBA20[i,3]/sum(ModelBA20[,3]))
  NormModelBA20[i,"NormCs"] <- (ModelBA20[i,4]/sum(ModelBA20[,4])) #*800
  NormModelBA20[i,"NormIr"] <- (ModelBA20[i,5]/sum(ModelBA20[,5]))
  NormModelBA20[i,"NormSe"] <- (ModelBA20[i,6]/sum(ModelBA20[,6]))
}

View(ModelBA20)
ModelBA20[ModelBA20<10e-5]=0
#b <- ModelBA20[,4]
#View(b)
#k <- as.integer(ModelBA20[,4])
#hist(k, ylim=c(0,10e+2),main="spectra", ylab="Frequency",
#     xlab="Total Counts", col='skyblue' , breaks=10, border=F)

#legend('topright',c('Lab'),
#       fill = c('skyblue', 'red','green','coral'), bty = 'n',
#       border = NA)

plot(x=ModelBA20[1:1022,1], y=ModelBA20[1:1022,2], xlab="Bin", ylab="Counts", 
     log="y", main="Spectrum", pch=15, col="black", type="l")

lines(x=ModelBA20[1:1022,1], y=ModelBA20[1:1022,3], log="y", col="blue")
lines(x=ModelBA20[1:1022,1], y=ModelBA20[1:1022,4], log="y", col="red")
lines(x=ModelBA20[1:1022,1], y=ModelBA20[1:1022,5], log="y", col="green")
lines(x=ModelBA20[1:1022,1], y=ModelBA20[1:1022,6], log="y", col="coral")

legend('bottomleft',c('Ba','Co','Cs','Ir','Se'),
       fill = c('black', 'blue', 'red','green','coral'), bty = 'n',
       border = NA)

# normaalized plots
plot(x=NormModelBA20[1:1022,2], y=NormModelBA20[1:1022,3], xlab="Bin", ylab="Counts", 
     log="y", main="Spectrum", pch=15, col="black", type="l")

lines(x=NormModelBA20[1:1022,2], y=NormModelBA20[1:1022,4], log="y", col="blue")
lines(x=NormModelBA20[1:1022,2], y=NormModelBA20[1:1022,5], log="y", col="red")
lines(x=NormModelBA20[1:1022,2], y=NormModelBA20[1:1022,6], log="y", col="green")
lines(x=NormModelBA20[1:1022,2], y=NormModelBA20[1:1022,7], log="y", col="lightblue")

#h <- data.frame(f,g)
#g <- g[-c(1:19,1024),]
#View(g)
#dim(g)
#fBaSpec20m <- data.frame(matrix(ncol = 6, nrow = 1022))
#dfBaSpec20m <- g
#View(dfBaSpec20m)

#plot()
############################
# Training on Sarah's data

library(gbm)
library(caTools)
library(rpart)
library(caret)
library(mda)
#library(kernlab)  # spam is here
#library(ISLR)     # wage is here
library(Hmisc)
library(gridExtra)
library(splines)
library(rattle)
library(rpart)
library(rpart.plot)
library(ElemStatLearn)
library(klaR)




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
View(AveCs10m)
View(dfCs10m)
x<-grep("Ch_01",colnames(dfBa5m))

SpecBa5m<-t(dfBa5m[,x[1]:(x[1]+1022)])
View(SpecBa5m)
AveBa5m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveBa5m[i,"Bin"]<-i
  AveBa5m[i,"Energy"]<-(1.055+i*2.848)
  AveBa5m[i,"Ave"]<-mean(dfBa5m[,x[1]+i-1])
  AveBa5m[i,"Sum"]<-sum(dfBa5m[,x[1]+i-1])
}
View(AveBa5m)

x<-grep("Ch_01",colnames(dfBa10m))
SpecBa10m<-t(dfBa10m[,x[1]:(x[1]+1022)])
View(SpecBa10m)
AveBa10m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveBa10m[i,"Bin"]<-i
  AveBa10m[i,"Energy"]<-(1.055+i*2.848)
  AveBa10m[i,"Ave"]<-mean(dfBa10m[,x[1]+i-1])
  AveBa10m[i,"Sum"]<-sum(dfBa10m[,x[1]+i-1])
}
View(AveBa10m)

x<-grep("Ch_01",colnames(dfBa15m))
SpecBa15m<-t(dfBa15m[,x[1]:(x[1]+1022)])
#View(SpecBa15m)
AveBa15m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveBa15m[i,"Bin"]<-i
  AveBa15m[i,"Energy"]<-(1.055+i*2.848)
  AveBa15m[i,"Ave"]<-mean(dfBa15m[,x[1]+i-1])
  AveBa15m[i,"Sum"]<-sum(dfBa15m[,x[1]+i-1])
}
#View(AveBa15m)

x<-grep("Ch_01",colnames(dfBa20m))
SpecBa20m<-t(dfBa20m[,x[1]:(x[1]+1022)])
View(SpecBa20m)
AveBa20m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveBa20m[i,"Bin"]<-i
  AveBa20m[i,"Energy"]<-(1.055+i*2.848)
  AveBa20m[i,"Ave"]<-mean(dfBa20m[,x[1]+i-1])
  AveBa20m[i,"Sum"]<-sum(dfBa20m[,x[1]+i-1])
}
View(AveBa20m)

#background
x<-grep("Ch_01",colnames(dfBa0m))
SpecBa0m<-t(dfBa0m[,x[1]:(x[1]+1022)])
View(SpecBa0m)
AveBa0m<-matrix(NA,nrow=1023,ncol=4,dimnames=list(row.names,column.names))
for(i in 1:1023){
  AveBa0m[i,"Bin"]<-i
  AveBa0m[i,"Energy"]<-(1.055+i*2.848)
  AveBa0m[i,"Ave"]<-mean(dfBa0m[,x[1]+i-1])
  AveBa0m[i,"Sum"]<-sum(dfBa0m[,x[1]+i-1])
}
View(AveBa0m)


a<-(dfBa5m[,'TotCount.cps.'])
b<-(dfBa10m[,'TotCount.cps.'])
c<-(dfBa15m[,'TotCount.cps.'])
d<-(dfBa20m[,'TotCount.cps.'])
e<-(dfBa25m[,'TotCount.cps.'])
f<-(dfBa30m[,'TotCount.cps.'])
g<-(dfBa35m[,'TotCount.cps.'])
h<-(dfBa40m[,'TotCount.cps.'])
i<-(dfBa0m[,'TotCount.cps.'])
hist(i, xlim=c(1000,1800), ylim=c(0,600),main="Total Counts at Different Distances", ylab="Frequency",
     xlab="Total Counts", col='skyblue',border=F)
hist(b, add=T,col=scales::alpha('red',.5),border=F)
hist(c, add=T,col=scales::alpha('green',.5), border=F)
hist(d, add=T,col=scales::alpha('coral',.5), border=F)
hist(e, add=T,col=scales::alpha('brown',.5), border=F)
hist(f, add=T,col=scales::alpha('blue',.5), border=F)
hist(g, add=T,col=scales::alpha('deeppink',.5), border=F)
hist(h, add=T,col=scales::alpha('gold',.5), border=F)
legend('topright',c('BG','10m','15m','20m'),
       fill = c('skyblue', 'red','green','coral'), bty = 'n',
       border = NA)


askew<-skew(SpecBGm[100:150,2:500])
bskew<-skew(Spec10m[100:150,2:500])
cskew<-skew(Spec15m[100:150,2:500])
dskew<-skew(Spec20m[100:150,2:500])
eskew<-skew(Spec25m[100:150,2:500])
fskew<-skew(Spec30m[100:150,2:500])
gskew<-skew(Spec35m[100:150,2:500])
hskew<-skew(Spec40m[100:150,2:500])
iskew<-skew(Spec5m[100:150,2:500])
jskew<-skew(SpecCs10m[100:150,2:500])

hist(askew, xlim=c(0,2), ylim=c(0,300),main="Skewness in Bins 1:150", ylab="Frequency",
     xlab="Total Counts", col='skyblue',border=F)
hist(bskew, add=T,col=scales::alpha('green',.5),border=F)
hist(cskew, add=T,col=scales::alpha('green',.5), border=F)
hist(dskew, add=T,col=scales::alpha('coral',.5), border=F)
hist(eskew, add=T,col=scales::alpha('brown',.5), border=F)
hist(fskew, add=T,col=scales::alpha('blue',.5), border=F)
hist(gskew, add=T,col=scales::alpha('deeppink',.5), border=F)
hist(hskew, add=T,col=scales::alpha('gold',.5), border=F)
hist(iskew, add=T,col=scales::alpha('black',.5), border=F)
hist(jskew, add=T,col=scales::alpha('coral',.5), border=F)
legend('topright',c('BG','Ba5m','Cs10m'),
       fill = c('skyblue', 'black','coral'), bty = 'n',
       border = NA)

#find skewness targeting Cs
askew<-skew(SpecBGm[100:150,2:700])
bskew<-skew(Spec5m[100:150,2:700])
cskew<-skew(Spec10m[100:150,2:700])
dskew<-skew(SpecCs10m[100:150,2:700])

hist(askew, xlim=c(0,4), ylim=c(0,300),main="Skewness in Bins 100:150", ylab="Frequency",
     xlab="Total Counts", col=alpha('red',.5),border=F)
hist(bskew, add=T,col=scales::alpha('black',.5),border=F)
#hist(cskew, add=T,col=scales::alpha('green',.5), border=F)
hist(dskew, add=T,col=scales::alpha('blue',.5),breaks=15,border=F)
legend('topright',c('BG','Ba5m','Cs10m'),
       fill = c('red', 'black','blue'), bty = 'n',
       border = NA) 

#plot a single spectrum, low energy
plot(x=Spec5m[100:200,1],y=Spec5m[100:200,2],xlab="Bin", ylab="Counts", 
     main="Spectrum", pch=15, col="black", type="l",log="y")
lines(x=SpecCs10m[100:200,1],y=2*SpecCs10m[100:200,2], col="blue")
lines(x=SpecBGm[100:200,1],y=SpecBGm[100:200,2], col="red")
lines(x=Spec30m[1:150,1],y=Spec30m[1:150,2], col="green")
legend('topright',c('BG','Ba5m','Cs10m'),
       fill = c('red', 'black','blue'), bty = 'n',
       border = NA)

#plot average spectrum, low energy
plot(x=AveBa5m[100:400,1],y=6*AveBa5m[100:400,2],xlab="Bin", ylab="Counts", 
     main="Spectra", pch=15, col="black", type="l", log='y')
lines(x=AveBG[100:400,1],y=6*AveBG[100:400,2], col="red")
lines(x=AveCs10m[100:400,1],y=10*AveCs10m[100:400,2], col="blue")
legend('topright',c('BG','Ba5m','Cs10m'),
       fill = c('red', 'black','blue'), bty = 'n',
       border = NA)


hist(skew(SpecCs10m[,2:700]), xlim=c(2,6), ylim=c(0,150),
     main="Skewness bins 1:1023", ylab="Frequency",xlab="Skewness",col=alpha('blue',.5))
hist(skew(Spec5m[,2:700]), add=T, col=alpha('black',.5))
hist(skew(SpecBGm[,2:700]), add=T, col=alpha('red',.5))
legend('topright',c('BG','Ba5m','Cs10m'),
       fill = c('red', 'black','blue'), bty = 'n',
       border = NA)

#need to figure out how to define density function
AveList<-as.list(AveBa5m)
fitdistr(SpecCs10m[,2],"Poisson")

d<- data.frame(BaGroup<-paste("Ba",rep(5*(0:8), each=1),"m", sep=""))
groups<- unique(d$BaGroup)

BaResult <- matrix(NA,nc=length(groups), nr=length(groups))
colnames(BaResult)<-rownames(BaResult)<-groups
#for(g1 in groups){
 # for(g2 in groups){
a<-vector(length=100)
b<-vector(length=100)
c<-vector(length=100)
d<-vector(length=100)
e<-vector(length=100)
f<-vector(length=100)
g<-vector(length=100)
h<-vector(length=100)
k<-vector(length=100)
for(i in 1:10){ 
  for(j in 1:10){
    a[i*j]<-(ks.test(Spec5m[,i],SpecBGm[,j])$p.value)
    b[i*j]<-(ks.test(Spec5m[,i],Spec5m[,j])$p.value)
    c[i*j]<-(ks.test(Spec5m[,i],Spec10m[,j])$p.value)
    d[i*j]<-(ks.test(Spec5m[,i],Spec15m[,j])$p.value)
    e[i*j]<-(ks.test(Spec5m[,i],Spec20m[,j])$p.value)
    f[i*j]<-(ks.test(Spec5m[,i],Spec25m[,j])$p.value)
    g[i*j]<-(ks.test(Spec5m[,i],Spec30m[,j])$p.value)
    h[i*j]<-(ks.test(Spec5m[,i],Spec35m[,j])$p.value)
    k[i*j]<-(ks.test(Spec5m[,i],Spec40m[,j])$p.value)
  }
}
    
BaResult[2,1]<-mean(a)
BaResult[2,2]<-mean(b)
BaResult[2,3]<-mean(c)
BaResult[2,4]<-mean(d)
BaResult[2,5]<-mean(e)
BaResult[2,6]<-mean(f)
BaResult[2,7]<-mean(g)
BaResult[2,8]<-mean(h)
BaResult[2,9]<-mean(k)
# }
#}

#find Kolmogorov-Smirnov test

for(i in 1:100){
  #for(j in 1:100){
    result[1,1]<-mean(ks.test(Spec5m[,1+i],Spec5m[,(1+i)])$p.value)
    result[1,2]<-mean(ks.test(Spec5m[,1+i],sum(Spec10m[,2:100]))$p.value)
    result[1,3]<-mean(ks.test(Spec5m[,1+i],sum(Spec15m[,2:100]))$p.value)
    result[1,4]<-mean(ks.test(Spec5m[,1+i],sum(Spec20m[,2:100]))$p.value)
    #result[1,5]<-mean(ks.test(Spec5m[,1+j],Spec25m[,(1+i)])$p.value)
    #result[1,6]<-mean(ks.test(Spec5m[,1+j],Spec30m[,(1+i)])$p.value)
    #result[1,7]<-mean(ks.test(Spec5m[,1+j],Spec35m[,(1+i)])$p.value)
    #result[1,8]<-mean(ks.test(Spec5m[,1+j],Spec40m[,(1+i)])$p.value)
   # }
}

plot(x=AveCs10m[1:400,1],y=SpecCs10m[1:400,4],xlab="Bin", ylab="Counts", 
     main="Spectrum", pch=15, col="black", type="l")
lines(x=AveCs10m[1:400,1], y=AveCs10m[1:400,3], col="blue")

