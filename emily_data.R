##  all the forty data files collected by emily & jess in may 

working.dir <- "C:/Users/mooreet_la/Documents/Data Science/data/"

setwd(paste(working.dir, "juliaDataSet/2017_05_03/",sep=""))
file1 <- read.csv("137Cs_112cm_5m/2x20170503131234.csv", header=T,skip=1)
file1$DetID <- "2x";file1$iso <- "Cs137";file1$Dist.cm <- "112-500"
file2 <- read.csv("137Cs_112cm_5m/8d20170503131152.csv", header=T,skip=1)
file2$DetID <- "8d"; file2$iso <- "Cs137"; file2$Dist.cm <- "112-500"
file3 <- read.csv("137Cs_112cm_5m/8d20170503143909.csv", header=T,skip=1)
file3$DetID <- "8d"; file3$iso <- "Cs137"; file3$Dist.cm <- "112-500"
file4 <- read.csv("137Cs_28cm_56cm_112cm/2x20170503114907.csv", header=T,skip=1)
file4$DetID <- "2x"; file4$iso <- "Cs137"; file4$Dist.cm <- "28-56-112"
file5 <- read.csv("137Cs_28cm_56cm_112cm/8d20170503114824.csv", header=T,skip=1)
file5$DetID <- "8d"; file5$iso <- "Cs137"; file5$Dist.cm <- "28-56-112"
file6 <- read.csv("137Cs_28cm_56cm_112cm/8d20170503115148.csv", header=T,skip=1)
file6$DetID <- "8d"; file6$iso <- "Cs137"; file6$Dist.cm <- "28-56-112"

setwd(paste(working.dir, "juliaDataSet/2017_05_04_Morning/",sep=""))
file7 <- read.csv("2x137Cs8m20170504104124.csv", header=T,skip=1)
file7$DetID <- "2x"; file7$iso <- "Cs137"; file7$Dist.cm <- "800"
file8 <- read.csv("2x60Co8m20170504114443.csv", header=T,skip=1)
file8$DetID <- "2x"; file8$iso <- "Co60"; file8$Dist.cm <- "800"
file9 <- read.csv("8d137Cs8m20170504104122.csv", header=T,skip=1)
file9$DetID <- "8d"; file9$iso <- "Cs137"; file9$Dist.cm <- "800"
file10 <- read.csv("8d60Co8m20170504114436.csv", header=T,skip=1)
file10$DetID <- "8d"; file10$iso <- "Co60"; file10$Dist.cm <- "800"

setwd(paste(working.dir, "juliaDataSet/2017_05_04_Afternoon/",sep=""))
file11 <- read.csv("2xCo60_1m_20170504145305.csv", header=T,skip=1)
file11$DetID <- "2x"; file11$iso <- "Co60"; file11$Dist.cm <- "100"
file12 <- read.csv("2xCo60_50cm_20170504160202.csv", header=T,skip=1)
file12$DetID <- "2x"; file12$iso <- "Co60"; file12$Dist.cm <- "50"
file13 <- read.csv("8dCo60_1m_20170504145330.csv", header=T,skip=1)
file13$DetID <- "8d"; file13$iso <- "Co60"; file13$Dist.cm <- "100"
file14 <- read.csv("8dCo60_50cm_20170504160037.csv", header=T,skip=1)
file14$DetID <- "8d"; file14$iso <- "Co60"; file14$Dist.cm <- "50"

setwd(paste(working.dir, "juliaDataSet/2017_05_05/",sep=""))
file15 <- read.csv("2x133Ba1m20170505112652.csv", header=T,skip=1)
file15$DetID <- "2x"; file15$iso <- "Ba133"; file15$Dist.cm <- "100"
file16 <- read.csv("2x133Ba50cm20170505102609.csv", header=T,skip=1)
file16$DetID <- "2x"; file16$iso <- "Ba133"; file16$Dist.cm <- "50"
file17 <- read.csv("8d133Ba1m20170505112522.csv", header=T,skip=1)
file17$DetID <- "8d"; file17$iso <- "Ba133"; file17$Dist.cm <- "100"
file18 <- read.csv("8d133Ba50cm20170505102618.csv", header=T,skip=1)
file18$DetID <- "8d"; file18$iso <- "Ba133"; file18$Dist.cm <- "50"

setwd(paste(working.dir, "juliaDataSet/2017_05_10/",sep=""))
file19 <- read.csv("2x133Ba13ft20170510142709.csv", header=T,skip=1)
file19$DetID <- "2x"; file19$iso <- "Ba133"; file19$Dist.cm <- "396"
file20 <- read.csv("2x133Ba20ft20170510134414.csv", header=T,skip=1)
file20$DetID <- "2x"; file20$iso <- "Ba133"; file20$Dist.cm <- "610"
file21 <- read.csv("2x133Ba25ft20170510140208.csv", header=T,skip=1)
file21$DetID <- "2x"; file21$iso <- "Ba133"; file21$Dist.cm <- "762"
file22 <- read.csv("2x137Cs13ft20170510120649.csv", header=T,skip=1)
file22$DetID <- "2x"; file22$iso <- "Cs137"; file22$Dist.cm <- "396"
file23 <- read.csv("2x137Cs25ft20170510114638.csv", header=T,skip=1)
file23$DetID <- "2x"; file23$iso <- "Cs137"; file23$Dist.cm <- "762"
file24 <- read.csv("2x137Cs38ft20170510111803.csv", header=T,skip=1)
file24$DetID <- "2x"; file24$iso <- "Cs137"; file24$Dist.cm <- "1158"
file25 <- read.csv("2x137Cs50ft20170510105756.csv", header=T,skip=1)
file25$DetID <- "2x"; file25$iso <- "Cs137"; file25$Dist.cm <- "1524"
file26 <- read.csv("2x60Co13ft20170510123041.csv", header=T,skip=1)
file26$DetID <- "2x"; file26$iso <- "Co60"; file26$Dist.cm <- "396"
file27 <- read.csv("2x60Co20ft20170510132238.csv", header=T,skip=1)
file27$DetID <- "2x"; file27$iso <- "Co60"; file27$Dist.cm <- "610"
file28 <- read.csv("2x60Co25ft20170510125927.csv", header=T,skip=1)
file28$DetID <- "2x"; file28$iso <- "Co60"; file28$Dist.cm <- "762"
file29 <- read.csv("2xBgd20170510102528.csv", header=T,skip=1)
file29$DetID <- "2x"; file29$iso <- "BkGD"; file29$Dist.cm <- "0"
file30 <- read.csv("8d133Ba13ft20170510142651.csv", header=T,skip=1)
file30$DetID <- "8d"; file30$iso <- "Ba133"; file30$Dist.cm <- "396"
file31 <- read.csv("8d133Ba20ft20170510134443.csv", header=T,skip=1)
file31$DetID <- "8d"; file31$iso <- "Ba133"; file31$Dist.cm <- "610"
file32 <- read.csv("8d133Ba25ft20170510140215.csv", header=T,skip=1)
file32$DetID <- "8d"; file32$iso <- "Ba133"; file32$Dist.cm <- "762"
file33 <- read.csv("8d137Cs13ft20170510120608.csv", header=T,skip=1)
file33$DetID <- "8d"; file33$iso <- "Cs137"; file33$Dist.cm <- "396"
file34 <- read.csv("8d137Cs25ft20170510114809.csv", header=T,skip=1)
file34$DetID <- "8d"; file34$iso <- "Cs137"; file34$Dist.cm <- "762"
file35 <- read.csv("8d137Cs38ft20170510111906.csv", header=T,skip=1)
file35$DetID <- "8d"; file35$iso <- "Cs137"; file35$Dist.cm <- "1158"
file36 <- read.csv("8d137Cs50ft20170510105801.csv", header=T,skip=1)
file36$DetID <- "8d"; file36$iso <- "Cs137"; file36$Dist.cm <- "1524"
file37 <- read.csv("8d60Co13ft20170510123200.csv", header=T,skip=1)
file37$DetID <- "8d"; file37$iso <- "Co60"; file37$Dist.cm <- "396"
file38 <- read.csv("8d60Co20ft20170510132358.csv", header=T,skip=1)
file38$DetID <- "8d"; file38$iso <- "Co60"; file38$Dist.cm <- "610"
file39 <- read.csv("8d60Co25ft20170510130153.csv", header=T,skip=1)
file39$DetID <- "8d"; file39$iso <- "Co60"; file39$Dist.cm <- "762"
file40 <- read.csv("8dBgd20170510102440.csv", header=T,skip=1)
file40$DetID <- "8d"; file40$iso <- "BkGD"; file40$Dist.cm <- "0"


file1.c<- rbind(file1,file2,file3,file4,file5,file6,file7,file8,file9,file10)
file2.c<- rbind(file11,file12,file13,file14,file15,file16,file17,file18,file19,file20)
file3.c<- rbind(file21,file22,file23,file24,file25,file26,file27,file28,file29,file30)
file4.c<- rbind(file31,file32,file33,file34,file35,file36,file37,file38,file39,file40)

Lab.data <- rbind(file1.c,file2.c,file3.c,file4.c)
