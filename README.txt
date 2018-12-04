
This set of files are Johanna's R scripts for: combining source & bg data, 
poisson sampling the asymptotic spectra, feature engineering & data vis,
and modeling a random forest to classify data.

Also included are 3 CSV files and 2 random forest models.

data_generation.R
* input: asymptotic GADRAS spectra
* output: a raster of source+background spectra
* file: AllRaster.csv

sampling.R
* input: takes the output of data_generation.R (or another csv of spectra) 
* output: poisson sampled spectra
* file: AllSampled.csv

features.R
* input: takes the output of sampling.R (or another csv of spectra) 
* output: a CSV with the original spectra and new columns filled with features
	right now it is just some window metrics
* this script also contains a lot of DATA VISUALIZATION
	it makes the scatterplots of feature overlap
* file: Samp_spec_feat.csv

model.R
* input: takes the output of features.R (or another csv of spectra, if 
	only using channels for your machine learning)
* output: a random forest model based on features of your choosing
* this script also creates a visualization of the confusion matrix
* files: RF_allchannels_long.rds & RF_features_long.rds