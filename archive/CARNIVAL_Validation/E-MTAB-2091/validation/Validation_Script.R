# Validation scripts

rm(list=ls());cat("\014")
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/validation/") # set working directory (relative)

# Select network
ScaffoldNet <- 2 # 1=generic, 2=Omnipath

# Select stimuli's targets
StimuliTarget <- 2 # 1=all, 2=only main

# ================================================== #

# E-MTAB-2091_PP5min.csv & E-MTAB-2091_PP25min.csv
PP5min <- read.table("E-MTAB-2091_PP5min.csv",header=T,sep=",",stringsAsFactors = F)
PP25min <- read.table("E-MTAB-2091_PP25min.csv",header=T,sep=",",stringsAsFactors = F)
MeasuredPP <- colnames(PP5min)[4:ncol(PP5min)] # Same list and order between PP5min and PP25min

# Manual annotation of gene names with HGNC symbol (GeneCards)
#                    "AKT1" "CREB1" "EGFR" "ERK1" "FAK1" "GSK3B" "HSP27" "IKBA" "JNK2" "MEK1" "MKK6" "NFKB" "p38MAPK" "P53" "P70S6K" "RPS6" "SHP2" "WNK1" "RSK1"  
MeasuredPP_HGNC <- c("AKT1","CREB1","EGFR","MAPK3","PTK2","GSK3B","HSPB1","NFKBIA","MAPK9","MAP2K1","MAP2K6","NFKB1","MAPK14","TP53","RPS6KB1","RSPH6A","PTPN11","WNK1","RPS6KA1")

CompoundsPP <- PP5min[,1]
CompoundsPP_All <- NULL
for (counter in 1:length(CompoundsPP)) {
  CompoundsPP_All[[counter]] <- strsplit(CompoundsPP[counter],split = " ",fixed = T)
}
CompoundsPP_Names <- NULL
for (counter in 1:length(CompoundsPP_All)) {
  if (length(CompoundsPP_All[[counter]][[1]])==1) {
    CompoundsPP_Names[[counter]] <- CompoundsPP_All[[counter]][[1]][1]
  } else {
    if (grepl(pattern = "(",x = CompoundsPP_All[[counter]][[1]][2],fixed = T)) {
      CompoundsPP_Names[[counter]] <- CompoundsPP_All[[counter]][[1]][1]
    } else {
      CompoundsPP_Names[[counter]] <- paste0(CompoundsPP_All[[counter]][[1]][1],"_",CompoundsPP_All[[counter]][[1]][2])
    }
  }
}
CompoundsPP_Names <- unlist(CompoundsPP_Names)

if (ScaffoldNet==1) {ScaffoldName <- "generic"} else if (ScaffoldNet==2) {ScaffoldName <- "omnipath"}
if (StimuliTarget==1) {StimTarget <- ""} else if (StimuliTarget==2) {StimTarget <- "_MainTarget"}

# === All combined result === #
Compounds <- t(read.table(file = "~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data/All_Compound_Names.tsv",sep = "\r"))

ValResMat <- as.data.frame(matrix(NA,length(Compounds),length(MeasuredPP)*3))
ValResMatCol <- NULL
for (counter in 1:length(MeasuredPP)) {
  ValResMatCol <- c(ValResMatCol,paste0(MeasuredPP[counter],"_Mod"),paste0(MeasuredPP[counter],"_5m"),paste0(MeasuredPP[counter],"_25m"))
}
colnames(ValResMat) <- ValResMatCol
rownames(ValResMat) <- Compounds
ValResMatSign <- ValResMat

CutOff <- 0.1
ValResMatSignCutOff <- ValResMat

for (counter_compound in 1:length(Compounds)) {
# for (counter_compound in 1:15) {
    
  print(paste0("Now mapping: ",Compounds[counter_compound]))
  
  if (file.exists(paste0("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/results/validation_",Compounds[counter_compound],"_",ScaffoldName,StimTarget,"/nodesActivity_1.txt"))) {
    
    NodeAct_current <- read.delim(paste0("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/results/validation_",Compounds[counter_compound],"_",ScaffoldName,StimTarget,"/nodesActivity_1.txt"),header=T,sep="\t",stringsAsFactors = F)
    
    Overlapped_Proteins <- intersect(MeasuredPP_HGNC,NodeAct_current[,1])
  
    if (length(Overlapped_Proteins)>0) {  
      # Writing results for the overlapped proteins
      Result_Matrix <- matrix(NA,length(Overlapped_Proteins),4)
      colnames(Result_Matrix) <- c("Protein","CARNIVAL","PP5min","PP25min")
      Idx_Condition <- which(Compounds[counter_compound]==CompoundsPP_Names)
      for (counter in 1:length(Overlapped_Proteins)) {
        Current_Overlapped_Protein <- Overlapped_Proteins[counter]
        Current_CARNIVAL_output <- NodeAct_current$Activity[NodeAct_current$Nodes==Current_Overlapped_Protein]
        Current_PP5min_meas <- PP5min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
        Current_PP25min_meas <- PP25min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
        Result_Matrix[counter,] <- c(Current_Overlapped_Protein,Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
        # map to summarised matrix
        Idx_ValResMat <- which(Current_Overlapped_Protein==MeasuredPP_HGNC)
        ValResMat[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- c(Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
        ValResMatSign[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- c(Current_CARNIVAL_output,sign(Current_PP5min_meas),sign(Current_PP25min_meas))
        ValResMatSignCutOff[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- # c(Current_CARNIVAL_output,
                                                                                                       # sign(if (abs(Current_PP5min_meas)<=CutOff) {Current_PP5min_meas=0} else {Current_PP5min_meas}),                                                                                                       
                                                                                                       # sign(if (abs(Current_PP25min_meas)<=CutOff) {Current_PP25min_meas=0} else {Current_PP25min_meas}))
        # if (abs(Current_PP5min_meas)<=CutOff || abs(Current_PP25min_meas)<=CutOff) {c(NA,NA,NA)} else {c(Current_CARNIVAL_output,sign(Current_PP5min_meas),sign(Current_PP25min_meas))}
          c(Current_CARNIVAL_output, 
            if (abs(Current_PP5min_meas)<=CutOff) {NA} else {sign(Current_PP5min_meas)},
            if (abs(Current_PP25min_meas)<=CutOff) {NA} else {sign(Current_PP25min_meas)})
      }
      
      write.table(x = Result_Matrix,file = paste0("Validation_Results_",Compounds[counter_compound],"_",ScaffoldName,StimTarget,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
    }
  }
}

# Analysis on the difference of predicted node activity and measurement (here use ValResMatSignCutOff)
# DiffMat <- matrix(NA,3,length(MeasuredPP_HGNC))
DiffMat <- matrix(NA,4,length(MeasuredPP_HGNC))
colnames(DiffMat) <- MeasuredPP
rownames(DiffMat) <- c("PP5min","NrDat5m","PP25min","NrDat25m")

for (counter_diff in 1:length(MeasuredPP)){
  # ModVal <- ValResMatSignCutOff[,(((counter_diff-1)*3)+1)]; ModVal <- ModVal[!is.na(ModVal)]
  # Meas5min <- ValResMatSignCutOff[,(((counter_diff-1)*3)+2)]; Meas5min <- Meas5min[!is.na(Meas5min)]
  # Meas25min <- ValResMatSignCutOff[,(((counter_diff-1)*3)+3)]; Meas25min <- Meas25min[!is.na(Meas25min)]
  # DiffMat[,counter_diff] <- c(sum(abs(ModVal-Meas5min))/length(Meas5min),sum(abs(ModVal-Meas25min))/length(Meas25min),length(Meas5min))
  ModVal <- ValResMatSignCutOff[,(((counter_diff-1)*3)+1)]; 
  Meas5min <- ValResMatSignCutOff[,(((counter_diff-1)*3)+2)];
  Meas25min <- ValResMatSignCutOff[,(((counter_diff-1)*3)+3)]; 
  Diff5min <- ModVal-Meas5min; Diff5min <- abs(Diff5min[!is.na(Diff5min)])
  Diff25min <- ModVal-Meas25min; Diff25min <- abs(Diff25min[!is.na(Diff25min)])
  DiffMat[,counter_diff] <- c(sum(Diff5min)/length(Diff5min),length(Diff5min),sum(Diff25min)/length(Diff25min),length(Diff25min))
}

# pdf("Average_Difference_generic.pdf")
# # pdf("Average_Difference_omnipath.pdf")
# plot(1:length(MeasuredPP),DiffMat[1,],type = 'p',col='red',cex=DiffMat[3,]/10,
#      xlab = "Measured-PP",ylab="Distance",
#      ylim = c(0,max(c(DiffMat[1,][(!is.na(DiffMat[1,]))],DiffMat[2,][(!is.na(DiffMat[2,]))])+0.5)),
#      main = "Average difference - Generic network",axes = F)
#      # main = "Average difference - Omnipath network",axes = F)
# axis(1, at=seq(1,length(MeasuredPP),by=1),labels=MeasuredPP, las = 2)
# axis(2, at=seq(0,max(c(DiffMat[1,][(!is.na(DiffMat[1,]))],DiffMat[2,][(!is.na(DiffMat[2,]))])+0.5),by=0.2),labels=seq(0,max(c(DiffMat[1,][(!is.na(DiffMat[1,]))],DiffMat[2,][(!is.na(DiffMat[2,]))])+0.5),by=0.2), las = 2)
# points(1:length(MeasuredPP),DiffMat[2,],type = 'p', col='blue',cex=DiffMat[3,]/10)
# lines(1:length(MeasuredPP),rep(1,length(MeasuredPP)),lty=2,col="grey")
# legend("topright", legend=c("PP-5min", "PP-25min"),col=c("red", "blue"), lty=1, cex=0.8,inset = 0.02)
# dev.off()

pdf(paste0("Average_Difference_",ScaffoldName,StimTarget,".pdf"))

plot(1:length(MeasuredPP),DiffMat[1,],type = 'p',col='red',cex=DiffMat[2,]/10,
     xlab = "Measured-PP",ylab="Distance",
     ylim = c(0,2.3),
#      main = "Average difference - Generic network",axes = F)
main = paste0("Average difference - ",ScaffoldName," network"),axes = F)
axis(1, at=seq(1,length(MeasuredPP),by=1),labels=MeasuredPP, las = 2)
axis(2, at=seq(0,2.3,by=0.2),labels=seq(0,2.3,by=0.2), las = 2)
points(1:length(MeasuredPP),DiffMat[3,],type = 'p', col='blue',cex=DiffMat[4,]/10)
lines(1:length(MeasuredPP),rep(1,length(MeasuredPP)),lty=2,col="grey")
legend("topright", legend=c("PP-5min", "PP-25min"),col=c("red", "blue"), lty=1, cex=0.8,inset = 0.02)
dev.off()

write.table(x = ValResMat,file = paste0("Summary_Validation_Results_",ScaffoldName,StimTarget,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
write.table(x = ValResMatSign,file = paste0("Summary_Validation_Sign_Results_",ScaffoldName,StimTarget,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
write.table(x = ValResMatSignCutOff,file = paste0("Summary_Validation_SignCutOff_Results_",ScaffoldName,StimTarget,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
write.table(x = DiffMat,file = paste0("Summary_Validation_AverageDistance_",ScaffoldName,StimTarget,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)

# # === Individual result === #
# 
# # NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol.txt",header=T,sep="\t",stringsAsFactors = F)
# # NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol_PROGENy.txt",header=T,sep="\t",stringsAsFactors = F)
# # NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol_PROGENy_Update_Net1.txt",header=T,sep="\t",stringsAsFactors = F)
# # NodeAct_Betaxolol <- read.delim("nodesActivity_Betaxolol_PROGENy_Update_Net4.txt",header=T,sep="\t",stringsAsFactors = F)
# NodeAct_Betaxolol <- read.delim("formaldehyde/nodesActivity_formaldehyde_Net1.txt",header=T,sep="\t",stringsAsFactors = F)
# Overlapped_Proteins <- intersect(MeasuredPP_HGNC,NodeAct_Betaxolol[,1])
# 
# # Writing results for the overlapped proteins
# Result_Matrix <- matrix(NA,length(Overlapped_Proteins),4)
# colnames(Result_Matrix) <- c("Protein","CARNIVAL","PP5min","PP25min")
# Idx_Condition <- which("betaxolol"==PP5min[,1])
# for (counter in 1:length(Overlapped_Proteins)) {
#   Current_Overlapped_Protein <- Overlapped_Proteins[counter]
#   Current_CARNIVAL_output <- NodeAct_Betaxolol$Activity[NodeAct_Betaxolol$Nodes==Current_Overlapped_Protein]
#   Current_PP5min_meas <- PP5min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
#   Current_PP25min_meas <- PP25min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
#   Result_Matrix[counter,] <- c(Current_Overlapped_Protein,Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
# }
# 
# # write.table(x = Result_Matrix,file = "Validation_Results_Betaxolol.tsv",quote = F,sep = "\t",col.names = T,row.names = F)
# # write.table(x = Result_Matrix,file = "Validation_Results_Betaxolol_Net1.tsv",quote = F,sep = "\t",col.names = T,row.names = F)
# # write.table(x = Result_Matrix,file = "Validation_Results_Betaxolol_Net4.tsv",quote = F,sep = "\t",col.names = T,row.names = F)
# write.table(x = Result_Matrix,file = "formaldehyde/Validation_Results_formaldehyde_Net1.tsv",quote = F,sep = "\t",col.names = T,row.names = F)
# 


# --- End of script --- #
