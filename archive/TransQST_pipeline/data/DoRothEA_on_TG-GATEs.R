
# ========= DoRothEA on TG-GATEs dataset ========= #

rm(list=ls()); cat("\014")

# Load functions
source('lib_enrichment_scores.r')

# Load TF regulon genesets
load('CTFRs_v122016.rdata')

# Load expression matrix
load('Expr_TG-GATEs_APAP_in_vitro_human.Rdata') # APAP human in vitro (liver)
# load('Expr_TG-GATEs_PPNL_in_vitro_human.Rdata') # APAP human in vitro (liver)
# load('~/Desktop/RWTH_Aachen/GitHub/QSP/Panuwat/WP4/Causal_Reasoning/Re_implementation_original_framework/5FU_WP8_CR/Expr_AE62322_DiffWell_R_NoTx.Rdata') # NoTx
# load('~/Desktop/RWTH_Aachen/GitHub/QSP/Panuwat/WP4/Causal_Reasoning/Re_implementation_original_framework/5FU_WP8_CR/Expr_AE62322_DiffWell_R_5FU.Rdata') # 5FU

eset <- expr
expr <- exprs(eset)

# install.packages('matrixStats')
library(matrixStats)
m = rowMeans2(as.matrix(expr))
s = rowSds(as.matrix(expr))
# l = lowess(m,s) # Additional step for data transformation to avoid SD=0 (returning NA after scaling)

expr_z_score = t(scale(t(as.matrix(expr)), center = m, scale=s))
# expr_z_score = t(scale(t(as.matrix(expr)), center = m, scale=l$y))

E = expr_z_score

# Estimate TF activities
# TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'GSVA')$NES # Doesn't work -> issue with parallel computing (cluster name/type not defined)

# TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'MEAN')$NES # Worked -> get only positive value (max=1, normalised?)
# save(TF_activities,file = "TF_activities_MEAN_TG-GATEs_APAP_in_vitro_human.Rdata")

# TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'GSEAlm')$NES # Doesn't work -> return an empty matrix

TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'VIPER')$NES # Worked -> get positive and negative values (min/max ~ -3/3, z-score?)
save(TF_activities,file = "TF_activities_VIPER_TG-GATEs_APAP_in_vitro_human.Rdata")
# save(TF_activities,file = "TF_activities_VIPER_TG-GATEs_PPNL_in_vitro_human.Rdata")
# save(TF_activities,file = "TF_activities_VIPER_AE62322_DiffWell_R_NoTx.Rdata")
# save(TF_activities,file = "TF_activities_VIPER_AE62322_DiffWell_R_5FU.Rdata")

# TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'ssGSEA')$NES # Worked -> get very low positive and negative values (min/max ~ -0.3/0.3, z-score/10?)
# save(TF_activities,file = "TF_activities_ssGSEA_TG-GATEs_APAP_in_vitro_human.Rdata")

# TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'ManW')$NES # Doesn't work -> return an empty matrix

# Extract TF activities in CR-pipeline ready format

# ILP_absolute_cutoff <- 1.5 # Choose cut-off value for discretisation
# ILP_absolute_cutoff <- 2.5 # Choose cut-off value for discretisation
ILP_absolute_cutoff <- 2.0 # Choose cut-off value for discretisation


# ===== Manual name assignment ===== #

# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,1:2]))) # 24h Ctrl
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,3:4]))) # 2h Ctrl
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,5:6]))) # 8h Ctrl
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,7:8]))) # 24h High
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,9:10]))) # 2h High
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,11:12]))) # 8h High
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,13:14]))) # 24h Low
Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,15:16]))) # 2h Low
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,17:18]))) # 8h Low
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,19:20]))) # 24h Mid
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,21:22]))) # 2h Mid
# Drug_TF <- as.data.frame(t(rowMeans(TF_activities[,23:24]))) # 8h Mid

# write.table(Drug_TF,file = "TFActs_TGG_APAP_Human_ivt_24h_ctrl_full.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
# write.table(Drug_TF,file = "TFActs_TGG_APAP_Human_ivt_24h_ctrl_full_CutOff2p5.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
# write.table(Drug_TF,file = "TFActs_TGG_PPNL_Human_ivt_24h_ctrl_full.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
Drug_TF_ILP <- Drug_TF
Drug_TF_ILP[Drug_TF >= ILP_absolute_cutoff] <- 1 
Drug_TF_ILP[Drug_TF <= -1*ILP_absolute_cutoff] <- -1 
ToRemoveIdx <- which(Drug_TF < ILP_absolute_cutoff & Drug_TF > -1*ILP_absolute_cutoff)
Drug_TF_ILP <- Drug_TF_ILP[-ToRemoveIdx]
# write.table(Drug_TF_ILP,file = "TFActs_TGG_APAP_Human_ivt_24h_ctrl_UpDown.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
# write.table(Drug_TF_ILP,file = "TFActs_TGG_APAP_Human_ivt_24h_ctrl_UpDown_CutOff2p5.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
write.table(Drug_TF_ILP,file = "TFActs_TGG_APAP_Human_ivt_2h_low_UpDown_CutOff2.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
# write.table(Drug_TF_ILP,file = "TFActs_TGG_PPNL_Human_ivt_24h_ctrl_UpDown.txt",sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)


# ===== Automated name assignment ===== #

# Drug_TF <- as.data.frame(t(TF_activities))
# 
# for (counter in 1:length(rownames(Drug_TF))) {
#   write.table(Drug_TF[counter,],file = paste("TFActs_AE62322_DiffWell_R_full_", rownames(Drug_TF)[counter]  ,".txt",sep=""),sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
#   Drug_TF_ILP <- Drug_TF[counter,]
#   Drug_TF_ILP[Drug_TF[counter,] >= ILP_absolute_cutoff] <- 1 
#   Drug_TF_ILP[Drug_TF[counter,] <= -1*ILP_absolute_cutoff] <- -1 
#   ToRemoveIdx <- which(Drug_TF[counter,] < ILP_absolute_cutoff & Drug_TF[counter,] > -1*ILP_absolute_cutoff)
#   Drug_TF_ILP <- Drug_TF_ILP[-ToRemoveIdx]
#   write.table(Drug_TF_ILP,file = paste("TFActs_AE62322_DiffWell_R_UpDown_", rownames(Drug_TF)[counter]  ,".txt",sep=""),sep="\t",col.names = TRUE,quote=FALSE,row.names = FALSE)
# }

# ===== End of the script ===== #
