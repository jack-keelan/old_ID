##  just the raw Gadras after being made into a *.csv
##                      Sarah's CSV Examining data

setwd("C:/Users/mooreet_la/Documents/Data Science/data")
##     read in model data
#testyaml<- read.csv("asymptotics/SSAM03Mar2017.yaml")
ModelAll.Gadras <- read.csv("asymptotics/SSAM03Mar2017.csv", header = F)




ModelAll <- ModelAll.Gadras
Model.junk <- t(ModelAll[,-c(1:19)])

##                         format Model data
##   !!!!!!!!  note that there may be a # of channel discrepancy  w/ the real !!!!!!!
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
  ModelIso40[,j+1] <- Model.junk[,4+((j-1)*4)]
  print(4+((j-1)*4))
}
## Examining Sarah's Model Data
#################################
# ###############################################################################
# # ##       OLD junk
# # ModelIso40Ba <- data.frame(matrix(ncol = 1025, nrow = 20,
# #                                   dimnames=list(row.names,column.names)))
# # for(j in 1:20){
# #   ModelIso40Ba[j,] <- Model.junk[3+((j-1)*4),]
# #   #  print(3+((j-1)*4))
# # }
# # for(j in 1:4){
# #   ModelIso40Ba[j,1025] <- "Ba"
# #   ModelIso40Ba[j+4,1025] <- "nonBa"
# #   ModelIso40Ba[j+8,1025] <- "nonBa"
# #   ModelIso40Ba[j+12,1025] <- "nonBa"
# #   ModelIso40Ba[j+16,1025] <- "nonBa"
# # }
# # 
# # #View(ModelIso40Ba[,1000:1025])
# # 
# # ModelIso40Co <- data.frame(matrix(ncol = 1025, nrow = 20,
# #                                   dimnames=list(row.names,column.names)))
# # for(j in 1:20){
# #   ModelIso40Co[j,] <- Model.junk[3+((j-1)*4),]
# #   #  print(3+((j-1)*4))
# # }
# # for(j in 1:4){
# #   ModelIso40Co[j,1025] <- "nonCo"
# #   ModelIso40Co[j+4,1025] <- "Co"
# #   ModelIso40Co[j+8,1025] <- "nonCo"
# #   ModelIso40Co[j+12,1025] <- "nonCo"
# #   ModelIso40Co[j+16,1025] <- "nonCo"
# # }
# # 
# # ModelIso40BigBa <- data.frame(matrix(ncol = 1025, nrow = 100,
# #                                      dimnames=list(row.names,column.names)))
# # for(i in 1:20){
# #   ModelIso40BigBa[i,] <- ModelIso40Ba[i,]
# #   ModelIso40BigBa[i+20,] <- ModelIso40Ba[i,]
# #   ModelIso40BigBa[i+40,] <- ModelIso40Ba[i,]
# #   ModelIso40BigBa[i+60,] <- ModelIso40Ba[i,]
# #   ModelIso40BigBa[i+80,] <- ModelIso40Ba[i,]
# # }
# # #View(ModelIso40BigCs[,1020:1025])
# # ModelIso40BigCo <- data.frame(matrix(ncol = 1025, nrow = 100,
# #                                      dimnames=list(row.names,column.names)))
# # for(i in 1:20){
# #   ModelIso40BigCo[i,] <- ModelIso40Co[i,]
# #   ModelIso40BigCo[i+20,] <- ModelIso40Co[i,]
# #   ModelIso40BigCo[i+40,] <- ModelIso40Co[i,]
# #   ModelIso40BigCo[i+60,] <- ModelIso40Co[i,]
# #   ModelIso40BigCo[i+80,] <- ModelIso40Co[i,]
# # }
