## rastered data, varying mix of background and source
##  'raster' data has a header
setwd("C:/Users/mooreet_la/Documents/Data Science/data")
## unsampled
Model.BaRaster <- read.csv("asymptotics/raster/BaRaster.csv", header = T)
Model.CoRaster <- read.csv("asymptotics/raster/CoRaster.csv", header = T)
Model.CsRaster <- read.csv("asymptotics/raster/CsRaster.csv", header = T)
Model.IrRaster <- read.csv("asymptotics/raster/IrRaster.csv", header = T)
Model.SeRaster <- read.csv("asymptotics/raster/SeRaster.csv", header = T)

##  sampled (poisson) data
Model.sampled <- read.csv("asymptotics/raster/sampled.csv", header = T)
Model.samp.features <- read.csv("asymptotics/raster/samp_features.csv", header = T)


Model.raster <- rbind.data.frame(Model.BaRaster[-c(1:44),-c(1:4,6:20)],
                                 Model.CoRaster[-c(1:44),-c(1:4,6:20)],
                                 Model.CsRaster[-c(1:44),-c(1:4,6:20)],
                                 Model.IrRaster[-c(1:44),-c(1:4,6:20)],
                                 Model.SeRaster[-c(1:44),-c(1:4,6:20)])

row.names <- c(as.character(Model.BaRaster[45:2035,"Name"]),
               as.character(Model.CoRaster[45:2035,"Name"]),
               as.character(Model.CsRaster[45:2035,"Name"]),
               as.character(Model.IrRaster[45:2035,"Name"]),
               as.character(Model.SeRaster[45:2035,"Name"]))
colnames(Model.raster)[1] <- "iso"
rownames(Model.raster) <- row.names

Model.sampled.temp <- Model.sampled[,-c(1:4)]
colnames(Model.sampled.temp)[1] <- "iso"
rownames(Model.sampled.temp) <- row.names
