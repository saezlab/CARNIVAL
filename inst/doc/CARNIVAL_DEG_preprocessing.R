## ---- message=FALSE, warning=FALSE---------------------------------------
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("SBV_EGF_tvalues.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
file.copy(from=system.file("dorothea_TF_mapping.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
load(file = system.file("BEST_viperRegulon.rdata",package="CARNIVAL"))

df<-read.csv2("SBV_EGF_tvalues.csv", row.names = 'GeneName')  
map<-read.csv("dorothea_TF_mapping.csv")

TF_genesymbol<-runDoRothEA(df, regulon=viper_regulon, confidence_level=c('A','B','C'))
TF_uniprot<-GeneSymbol2Uniprot(TF_genesymbol, map, 1, 2)
generate_measfile(measurements=TF_uniprot, topnumber=50, write2folder="measurements")


## ---- message=FALSE, warning=FALSE---------------------------------------
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("model_NatComm+14_human.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)

weight_matrix<-read.csv("model_NatComm+14_human.csv")
df_genenames<-data.frame('gene'=rownames(df),df)

pathway_scores<-runPROGENy(df_genenames,weight_matrix, z_scores = F)

for (cond in colnames(pathway_scores)){
  scores<-rbind(rownames(pathway_scores),pathway_scores[,cond])
  write.table(scores, paste0("measurements/scores_",cond,".txt"),col.names = F, row.names = F, quote = F, sep = '\t')
}

