# ---------------------- #
# Preprocessing from DEG #
# ---------------------- #

# set working directory and clear working environment
# setwd("/CARNIVAL")
rm(list=ls());cat("\014");if(length(dev.list())>0){dev.off()}

library(devtools)
load_all() # load CARNIVAL package

df<-read.csv2("inst/SBV_EGF_tvalues.csv", row.names = 'GeneName')  
load("~/DoRothEA/data/TFregulons/Robjects_VIPERformat/consensus/BEST_viperRegulon.rdata")

TF_activities<-run_dorothea(df, regulon=viper_regulon, confidence_level=c('A','B','C'))
#Need to convert Genesymbol to Uniprot 
generate_measfile(measurements=TF_activities, topnumber=50, write2folder="./measurements")
