#                              Julia's Data
#######################
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
##      read in Lab  data
benchtest4 <- read.csv("juliaDataSet/BenchtopLab/EricCoCsBckCf_Ortec_20170414153501.csv")
x<-grep("Ch_01",colnames(benchtest4)) # finding column # labelled 'ch_01'
SpecBenchtest <-t(benchtest4[,x[1]:(x[1]+1022)])  ### ???  How is this working?
# partition data set
SpecBenchtestCs <- subset(SpecBenchtest, select = c(1:3800))
SpecBenchtestCo <- subset(SpecBenchtest, select = c(3900:7350))
SpecBenchtestBkg <- subset(SpecBenchtest, select = c(7400:11000))
SpecBenchtestCf <- subset(SpecBenchtest, select = c(11000:12500))
####                format real data testing set
##    !!!!!!!!!! Get from below
junk.df <- rbind.data.frame(Model.BaRaster[c(1:3),-c(1:4,6:20)])
colnames(junk.df)[1] <- "iso"
column.names <- colnames(junk.df)
# colnames(testLabCs)[1] <- "iso"
#column.names <- c(1:1024,"iso")
row.names <- (1:3000)
testLabCs <- data.frame(matrix(ncol = 1025, nrow = 3000,
                               dimnames=list(row.names,column.names)))
testjunkCs <- t(SpecBenchtestCs)
testjunkCo <- t(SpecBenchtestCo)
testjunkBkg <- t(SpecBenchtestBkg)

##                setup for machine learning testing
for(i in 1:1000){
  testLabCs[i,2:1024] <- testjunkCs[i,1:1023]
  testLabCs[i,1] <- "Cs137"
  #  testLabCs[i,1] <- 1
  testLabCs[i,1025] <- as.integer(0)
  testLabCs[i+1000,2:1024] <- testjunkCo[i,1:1023]
  testLabCs[i+1000,1] <- "Co60"
  #  testLabCs[i+10,1] <- 5
  testLabCs[i+1000,1025] <- as.integer(0)
  testLabCs[i+2000,2:1024] <- testjunkBkg[i+500,1:1023]
  testLabCs[i+2000,1] <- "Ba133"
  testLabCs[i+2000,1025] <- as.integer(0)
  # testLabCs[i+300,2:1024] <- testjunkCo[i+100,1:1023]
  # testLabCs[i+300,1] <- "Co60"
  # testLabCs[i+300,1025] <- 0
  # testLabCs[i+400,2:1024] <- testjunkCo[i+2500,1:1023]
  # testLabCs[i+400,1] <- "Co60"
  # testLabCs[i+400,1025] <- 0
  # testLabCs[i+500,2:1024] <- testjunkCs[i+100,1:1023]
  # testLabCs[i+500,1] <- "Cs137"
  # testLabCs[i+500,1025] <- 0
  # testLabCs[i+600,2:1024] <- testjunkBkg[i,1:1023]
  # testLabCs[i+600,1] <- "Ba133"
  # testLabCs[i+600,1025] <- 0
  # testLabCs[i+700,2:1024] <- testjunkCs[i+500,1:1023]
  # testLabCs[i+700,1] <- "Cs137"
  # testLabCs[i+700,1025] <- 0
  # testLabCs[i+800,2:1024] <- testjunkCo[i+1000,1:1023]
  # testLabCs[i+800,1] <- "Co60"
  # testLabCs[i+800,1025] <- 0
  # testLabCs[i+90,2:1024] <- testjunkCs[i+2000,1:1023]
  # testLabCs[i+90,1] <- "Cs137"
  # testLabCs[i+90,1025] <- 0
}
testLabCs[2998,1] <- "Se75"
testLabCs[2999,1] <- "Ir192"
