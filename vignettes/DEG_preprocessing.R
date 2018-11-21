# ---------------------- #
# Preprocessing from DEG #
# ---------------------- #

# set working directory and clear working environment

rm(list=ls());cat("\014");if(length(dev.list())>0){dev.off()}

library(devtools)
load_all() # load CARNIVAL package

#Generate TF input files
df<-read.csv2("inst/SBV_EGF_tvalues.csv", row.names = 'GeneName')  
load("inst/BEST_viperRegulon.rdata")
map<-read.csv("inst/dorothea_TF_mapping.csv")

TF_genesymbol<-run_dorothea(df, regulon=viper_regulon, confidence_level=c('A','B','C'))
TF_uniprot<-genesymbol2uniprot(TF_genesymbol, map, 1, 2)
generate_measfile(measurements=TF_uniprot, topnumber=50, write2folder="inst/measurements")

#Generate PROGENy input files
weight_matrix<-read.csv("inst/model_NatComm+14_human.csv")
df_genenames<-data.frame('gene'=rownames(df),df)

pathway_scores<-runPROGENy(df_genenames,weight_matrix, z_scores = F)

for (cond in colnames(pathway_scores)){
  scores<-rbind(rownames(pathway_scores),pathway_scores[,cond])
  write.table(scores, paste0("inst/measurements/scores_",cond,".txt"),col.names = F, row.names = F, quote = F)
}
