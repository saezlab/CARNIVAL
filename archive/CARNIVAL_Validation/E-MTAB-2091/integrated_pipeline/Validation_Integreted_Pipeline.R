# Validation scripts - Integrated Pipeline

rm(list=ls());cat("\014")

# Select network
ScaffoldNet <- 3 # 1=omnipath, 2=generic, 3=signor

# Select stimuli's targets
# StimuliTarget <- 2 # 1=all, 2=only main

# Select SD-Cutoff for input loading
Meas_Cutoff <- 2

# Select cutoff measure (better to use absolute value for fold-change?)
# 1=absolute value, 2=mean+/-Cutoff*SD (Gaussian), 3= median+/-Cutoff*mean_abs_diff
DiscretPP <- 2; PP_Cutoff <- 2

# Plot threshold coverage?
PlotThresholdCoverage <- F

# ================================================== #

if (ScaffoldNet==1) {ScaffoldName <- "omnipath"} else if (ScaffoldNet==2) {ScaffoldName <- "generic"} else if (ScaffoldNet==3) {ScaffoldName <- "signor"}
# if (StimuliTarget==1) {StimTarget <- "";ResultDIR <- "Main_and_PROGENy_Targets"} else if (StimuliTarget==2) {StimTarget <- "_MainTarget";ResultDIR <-"Main_Targets_Only"}

# Load source scripts
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL"); source("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/src/CRILPR_Functions.R")
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/") # set working directory (relative)

# Load and discretize pp-datasets: E-MTAB-2091_PP5min.csv & E-MTAB-2091_PP25min.csv
PP5min <- read.table("resources/E-MTAB-2091_PP5min.csv",header=T,sep=",",stringsAsFactors = F)
PP5minZScore <- scale(PP5min[,4:ncol(PP5min)],center=TRUE,scale = TRUE); rownames(PP5minZScore) <- PP5min[,1]
PP5min_Cutoff <- Discretize_CARNIVAL(PP5minZScore,DiscretPP,PP_Cutoff)
# rowSums(abs(PP5min_Cutoff))
PP25min <- read.table("resources/E-MTAB-2091_PP25min.csv",header=T,sep=",",stringsAsFactors = F)
PP25minZScore <- scale(PP25min[,4:ncol(PP25min)],center=TRUE,scale = TRUE); rownames(PP25minZScore) <- PP25min[,1]
PP25min_Cutoff <- Discretize_CARNIVAL(PP25minZScore,DiscretPP,PP_Cutoff)
# rowSums(abs(PP25min_Cutoff))
MeasuredPP <- colnames(PP5min)[4:ncol(PP5min)] # Same list and order between PP5min and PP25min

# Manual annotation of gene names with HGNC symbol (GeneCards)
#                    "AKT1" "CREB1" "EGFR" "ERK1" "FAK1" "GSK3B" "HSP27" "IKBA" "JNK2" "MEK1" "MKK6" "NFKB" "p38MAPK" "P53" "P70S6K" "RPS6" "SHP2" "WNK1" "RSK1"  
MeasuredPP_HGNC <- c("AKT1","CREB1","EGFR","MAPK3","PTK2","GSK3B","HSPB1","NFKBIA","MAPK9","MAP2K1","MAP2K6","NFKB1","MAPK14","TP53","RPS6KB1","RSP6","PTPN11","WNK1","RPS6KA1")

# Extract compound names from pp-datasheet ([!] Not equivalent to the one of input datasets [58 vs 52 compounds])
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


# === All combined result === #
# Load compounds' names from input datasets (n=52)
Compounds <- t(read.table(file = "~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/resources/All_Compound_Names.tsv",sep = "\r"))

# Prepare results matrix
# ValResMat <- as.data.frame(matrix(NA,length(Compounds),length(MeasuredPP)*3))
ValResMat <- as.data.frame(matrix(NA,length(Compounds),length(MeasuredPP)*3))
ValResMatCol <- NULL
for (counter in 1:length(MeasuredPP)) {
  ValResMatCol <- c(ValResMatCol,paste0(MeasuredPP[counter],"_Mod"),paste0(MeasuredPP[counter],"_5m"),paste0(MeasuredPP[counter],"_25m"))
}
colnames(ValResMat) <- ValResMatCol
rownames(ValResMat) <- Compounds
ValResMatSign <- ValResMat 
ValResMatSignCutOff <- ValResMat
ValResMatReal <- ValResMat
ValResMatReal[1:length(ValResMatReal)] <- 0
ValResMatAll <- ValResMat 
ValResMatAll[1:length(ValResMatAll)] <- 0
#ValResMatAll[1:length(ValResMatAll)] <- NA

NoActivityID <- NULL # Manual count OnlyMainTargets n=11
NoNetworkID <- NULL # Check the existence of written dot file

for (counter_compound in 1:length(Compounds)) {
# for (counter_compound in 1:15) {
    
  print(paste0("Now mapping: ",Compounds[counter_compound]))
    
  if (file.exists(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,"/nodesActivity_1.txt"))) {

    if (file.exists(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,"/ActivityNetwork_1.dot"))) {
      
      Idx_Condition <- which(Compounds[counter_compound]==CompoundsPP_Names)
      
      for (counter_all in 4:ncol(PP5min)) { # same for PP25min
        Current_PP5min_meas_all <- PP5min_Cutoff[Idx_Condition,counter_all-3]
        Current_PP25min_meas_all <- PP25min_Cutoff[Idx_Condition,counter_all-3]
        ValResMatAll[counter_compound,(((counter_all-1-3)*3)+2):(((counter_all-1-3)*3)+3)] <- c(Current_PP5min_meas_all,Current_PP25min_meas_all)
        ValResMatReal[counter_compound,(((counter_all-1-3)*3)+2):(((counter_all-1-3)*3)+3)] <- c(PP5min[Idx_Condition,counter_all],PP25min[Idx_Condition,counter_all])
      }
      
      
      NodeAct_current <- read.delim(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,"/nodesActivity_1.txt"),header=T,sep="\t",stringsAsFactors = F)
      Overlapped_Proteins <- intersect(MeasuredPP_HGNC,NodeAct_current[,1])
      
      if (length(Overlapped_Proteins)>0) {  
        # Writing results for the overlapped proteins
        Result_Matrix <- matrix(NA,length(Overlapped_Proteins),4)
        colnames(Result_Matrix) <- c("Protein","CARNIVAL","PP5min","PP25min")
        for (counter in 1:length(Overlapped_Proteins)) {
          Current_Overlapped_Protein <- Overlapped_Proteins[counter]
          Current_CARNIVAL_output <- NodeAct_current$Activity[NodeAct_current$Nodes==Current_Overlapped_Protein]
          Current_PP5min_meas <- PP5min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
          Current_PP25min_meas <- PP25min[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)+3]
          Result_Matrix[counter,] <- c(Current_Overlapped_Protein,Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
          # map to summarised matrix
          Idx_ValResMat <- which(Current_Overlapped_Protein==MeasuredPP_HGNC)
          ValResMatReal[counter_compound,(((Idx_ValResMat-1)*3)+1)] <- Current_CARNIVAL_output
          ValResMat[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- c(Current_CARNIVAL_output,Current_PP5min_meas,Current_PP25min_meas)
          ValResMatSign[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- c(Current_CARNIVAL_output,sign(Current_PP5min_meas),sign(Current_PP25min_meas))
          ValResMatSignCutOff[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <- # c(Current_CARNIVAL_output,
                                                                                                         # sign(if (abs(Current_PP5min_meas)<=CutOff) {Current_PP5min_meas=0} else {Current_PP5min_meas}),                                                                                                       
                                                                                                         # sign(if (abs(Current_PP25min_meas)<=CutOff) {Current_PP25min_meas=0} else {Current_PP25min_meas}))
          # if (abs(Current_PP5min_meas)<=CutOff || abs(Current_PP25min_meas)<=CutOff) {c(NA,NA,NA)} else {c(Current_CARNIVAL_output,sign(Current_PP5min_meas),sign(Current_PP25min_meas))}
            c(Current_CARNIVAL_output, 
              if (PP5min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]==0) {NA} else {PP5min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]},
              if (PP25min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]==0) {NA} else {PP25min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]})
              # if (Current_PP5min_meas < CutOff_PP_Up[1,Idx_ValResMat] & Current_PP5min_meas > CutOff_PP_Down[1,Idx_ValResMat]) {NA} else {sign(Current_PP5min_meas)},
              # if (Current_PP25min_meas < CutOff_PP_Up[2,Idx_ValResMat] & Current_PP25min_meas > CutOff_PP_Down[2,Idx_ValResMat]) {NA} else {sign(Current_PP25min_meas)})
              # if (abs(Current_PP5min_meas)<=CutOff) {NA} else {sign(Current_PP5min_meas)},
              # if (abs(Current_PP25min_meas)<=CutOff) {NA} else {sign(Current_PP25min_meas)})
          ValResMatAll[counter_compound,(((Idx_ValResMat-1)*3)+1):(((Idx_ValResMat-1)*3)+3)] <-
            c(if(is.na(Current_CARNIVAL_output)){0}else(Current_CARNIVAL_output), 
              if (PP5min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]==0) {0} else {PP5min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]},
              if (PP25min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]==0) {0} else {PP25min_Cutoff[Idx_Condition,which(Current_Overlapped_Protein==MeasuredPP_HGNC)]})
        } 
        
        # Write a result file for individual molecules
        # write.table(x = Result_Matrix,file = paste0("Validation_Results_",Compounds[counter_compound],"_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
      }
    } else {
      NoNetworkID <- c(NoNetworkID, counter_compound)
    }
  } else {
    NoActivityID <- c(NoActivityID, counter_compound)
  }
}

# for (counter in 1:nrow(ValResMat)) { # for each compound
#   for (counter2 in 1:(ncol(ValResMatAll)/3)) { # for each readout
#     if ( is.na(ValResMatAll[counter,(((counter2-1)*3)+1)]) & !is.na(ValResMatAll[counter,(((counter2-1)*3)+2)])  ) {
#       ValResMatAll[counter,(((counter2-1)*3)+1)] <- 0
#     }
#   }  
# }


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

# pdf("Average_Difference_generic.pdf")d
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

pdf(paste0("Average_Difference_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
           if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
           else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
           else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
           ,".pdf"))

plot(1:length(MeasuredPP),DiffMat[1,],type = 'p',col='red',cex=DiffMat[2,]/2,
     xlab = "Measured-PP",ylab="Distance",
     ylim = c(0,2.3),
#      main = "Average difference - Generic network",axes = F)
main = paste0("AvgDiff ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff)),axes = F)
axis(1, at=seq(1,length(MeasuredPP),by=1),labels=MeasuredPP, las = 2)
axis(2, at=seq(0,2.3,by=0.2),labels=seq(0,2.3,by=0.2), las = 2)
points(1:length(MeasuredPP),DiffMat[3,],type = 'p', col='blue',cex=DiffMat[4,]/12)
lines(1:length(MeasuredPP),rep(1,length(MeasuredPP)),lty=2,col="grey")
legend("topright", legend=c("PP-5min", "PP-25min"),col=c("red", "blue"), lty=1, cex=0.8,inset = 0.02)
dev.off()


# Calculate accuracy from all datapoints
AccuMat <- matrix(NA,9,length(MeasuredPP_HGNC))
colnames(AccuMat) <- MeasuredPP
rownames(AccuMat) <- c("EmptyNetwork","ZeroMatchPP5min","PerfMatchPP5min","MisMatch5m","InvMatch5m","ZeroMatchPP25min","PerfMatchPP25min","MisMatch25m","InvMatch25m")

for (counter_diff in 1:length(MeasuredPP)){
  ModVal <- ValResMatAll[,(((counter_diff-1)*3)+1)]; 
  Meas5min <- ValResMatAll[,(((counter_diff-1)*3)+2)];
  Meas25min <- ValResMatAll[,(((counter_diff-1)*3)+3)]; 
  Accu5minZero <- sum(abs(ModVal-Meas5min)==0 & ModVal==0) - (length(NoActivityID)+length(NoNetworkID));
  Accu5minPerf <- sum(abs(ModVal-Meas5min)==0 & ModVal!=0);
  Accu5minMM <-sum(abs(ModVal-Meas5min)==1);
  Accu5minInv <-sum(abs(ModVal-Meas5min)==2);
  Accu25minZero <- sum(abs(ModVal-Meas25min)==0 & ModVal==0)- (length(NoActivityID)+length(NoNetworkID));
  Accu25minPerf <- sum(abs(ModVal-Meas25min)==0 & ModVal!=0);
  Accu25minMM <-sum(abs(ModVal-Meas25min)==1);
  Accu25minInv <-sum(abs(ModVal-Meas25min)==2);
  AccuMat[,counter_diff] <- c(length(NoActivityID)+length(NoNetworkID),Accu5minZero,Accu5minPerf,Accu5minMM,Accu5minInv,Accu25minZero,Accu25minPerf,Accu25minMM,Accu25minInv)
}

pdf(paste0("Stastic_Validation_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
           if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
           else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
           else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
           ,".pdf"))

plot(1:length(MeasuredPP),AccuMat[1,],type = 'p', pch=15, col='black',cex=0.8,
     xlab = "Measured-PP",ylab="Counts",
     ylim = c(0,54),
     #      main = "Average difference - Generic network",axes = F)
     main = paste0("Validation ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff)),axes = F)
axis(1, at=seq(1,length(MeasuredPP),by=1),labels=MeasuredPP, las = 2)
axis(2, at=seq(0,length(Compounds),by=5),labels=seq(0,length(Compounds),by=5), las = 2)
points(1:length(MeasuredPP),AccuMat[2,],type = 'p', pch=16,col='blue',cex=1)
points(1:length(MeasuredPP),AccuMat[3,],type = 'p', pch=16,col='darkgreen',cex=1)
points(1:length(MeasuredPP),AccuMat[4,],type = 'p', pch=16,col='goldenrod1',cex=1)
points(1:length(MeasuredPP),AccuMat[5,],type = 'p', pch=16,col='firebrick2',cex=1)

points(1:length(MeasuredPP),AccuMat[6,],type = 'p', pch=2,col='blue',cex=1)
points(1:length(MeasuredPP),AccuMat[7,],type = 'p', pch=2,col='darkgreen',cex=1)
points(1:length(MeasuredPP),AccuMat[8,],type = 'p', pch=2,col='goldenrod1',cex=1)
points(1:length(MeasuredPP),AccuMat[9,],type = 'p', pch=2,col='firebrick2',cex=1)

# lines(1:length(MeasuredPP),rep(1,length(MeasuredPP)),lty=2,col="grey")
legend("topright", legend=c("NoNet","ZM-5m","PM-5m","MM-5m","IM-5m","ZM-25m","PM-25m","MM-25m","IM-25m"),col=c("black","blue", "darkgreen","goldenrod1","firebrick2","blue", "darkgreen","goldenrod1","firebrick2"),pch = c(15,16,16,16,16,2,2,2,2), cex=0.6,inset = 0.02)
dev.off()

write.table(x = AccuMat,file = paste0("Accuracy_Validation_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
                                           if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
                                           else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
                                           else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
                                           ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)


# Plot distribution of node value to real-pp data (from ValResMatReal)
# First remove the empty models
DistModelPP <- ValResMatReal[-c(NoActivityID,NoNetworkID),]

pdf(paste0("FitDistribution_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
                                           if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
                                           else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
                                           else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
                                           ,".pdf"))

# par(mfrow=c(5,4))

require(reshape2)
require(ggplot2)

MeltedDataBoxplot <- NULL

for (counter in 1:(ncol(DistModelPP)/3)) {
  # BoxPlotData <- matrix(NA,max(length(ModelPlusOne5min),length(ModelZero5min),length(ModelMinus5min),length(ModelPlusOne25min),length(ModelZero25min),length(ModelMinus25min)),6)
  BoxPlotData <- matrix(NA,length(Compounds),6)
  ModelPlusOne5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==1),((counter-1)*3)+2];     if (length(ModelPlusOne5min)>0) {BoxPlotData[1:length(ModelPlusOne5min),1] <- ModelPlusOne5min}
  ModelZero5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==0),((counter-1)*3)+2];        if (length(ModelZero5min)>0) {BoxPlotData[1:length(ModelZero5min),2] <- ModelZero5min}
  ModelMinusOne5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==-1),((counter-1)*3)+2];   if (length(ModelMinusOne5min)>0) {BoxPlotData[1:length(ModelMinusOne5min),3] <- ModelMinusOne5min}
  ModelPlusOne25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==1),((counter-1)*3)+3];    if (length(ModelPlusOne25min)>0) {BoxPlotData[1:length(ModelPlusOne25min),4] <- ModelPlusOne25min}
  ModelZero25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==0),((counter-1)*3)+3];       if (length(ModelZero25min)>0) {BoxPlotData[1:length(ModelZero25min),5] <- ModelZero25min}
  ModelMinusOne25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==-1),((counter-1)*3)+3];  if (length(ModelMinusOne25min)>0) {BoxPlotData[1:length(ModelMinusOne25min),6] <- ModelMinusOne25min}
  colnames(BoxPlotData) <-c("1 [5m]","0 [5m]","-1 [5m]","1 [25m]","0 [25m]","-1 [25m]")
  # boxplot(BoxPlotData,ylab="pp activities",las=2,main=paste0("FitDist ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff)," - ",MeasuredPP[counter]),cex.main=1,cex.label=0.8, col=c("gold1","gold1","gold1","darkorange1","darkorange1","darkorange1"))
  # boxplot(BoxPlotData,ylab="pp activities",las=2,main=MeasuredPP[counter],cex.main=1,cex.lab=0.8,cex.axis=0.8, col=c("gold1","gold1","gold1","darkorange1","darkorange1","darkorange1"))
  
  df_current <- melt(BoxPlotData, id.var = "Label")
  df_current[,1] <- MeasuredPP[counter]
  colnames(df_current) <- c("Readout","Condition","ppValue")
  MeltedDataBoxplot <- rbind(MeltedDataBoxplot,df_current)
  
}

colnames(MeltedDataBoxplot) <- c("Readout","Condition","ppValue")

MeltedDataBoxplot <- MeltedDataBoxplot[-which(is.na(MeltedDataBoxplot$ppValue)),]


# p <- ggplot(data = df_current, aes(x=Condition, y=ppValue)) + 
p <- ggplot(data = MeltedDataBoxplot, aes(x=Condition, y=ppValue)) + 
  geom_boxplot(aes(fill=Condition))
p + facet_wrap( ~ Readout, scales="free")+ theme(axis.text.x=element_blank())+ scale_fill_manual(values=c("#AED6F1", "#5DADE2", "#2E86C1","#A9DFBF", "#52BE80", "#229954"))+ ggtitle(paste0("FitDistribution_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
                 if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
                 else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
                 else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}))

dev.off()


# write.table(x = ValResMat,file = paste0("Summary_Validation_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = ValResMatSign,file = paste0("Summary_Validation_Sign_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = ValResMatSignCutOff,file = paste0("Summary_Validation_SignCutOff_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
#                                                   if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
#                                                   else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
#                                                   else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
#                                                   ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = DiffMat,file = paste0("Summary_Validation_AverageDistance_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
#                                       if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff))} 
#                                       else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff))} 
#                                       else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff))}
#                                       ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)

# ================================== #
# ================================== #
# ================================== #

if (PlotThresholdCoverage) {

# Aggregate results from different thresholds (after complete result generations)

  CutOffRange <- c(0.1,0.5,1,1.5,2,2.5)
  AggregatedData <- matrix(NA,9,length(CutOffRange)) 
  
  for (counter in 1:length(CutOffRange)) {
    CurrentSummary <- read.table(paste0("Accuracy_Validation_Results_",ScaffoldName,"_MeasCutOff_2_MeanSDCutOff_",toString(CutOffRange[counter]),".tsv"),sep = "\t", stringsAsFactors = F,header = T)
    AggregatedData[,counter] <- rowMeans(CurrentSummary)
  }
  
  pdf(paste0("Matching_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".pdf"))
  
  par(mar=c(5.1, 4.1, 4.1, 5.1), xpd=TRUE)
  plot(1:length(CutOffRange),AggregatedData[1,],type = 'p', pch=15, col='black',cex=0.8,
       xlab = "Measured-PP",ylab="Counts",
       ylim = c(0,max(AggregatedData)+5),
       #      main = "Average difference - Generic network",axes = F)
       main = paste0("Matching ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)),axes = F)
  axis(1, at=seq(1,length(CutOffRange),by=1),labels=CutOffRange, las = 2)
  axis(2, at=seq(0,max(AggregatedData)+5,by=5),labels=seq(0,max(AggregatedData)+5,by=5), las = 2)
  points(1:length(CutOffRange),AggregatedData[2,],type = 'p', pch=16,col='blue',cex=1)
  points(1:length(CutOffRange),AggregatedData[3,],type = 'p', pch=16,col='darkgreen',cex=1)
  points(1:length(CutOffRange),AggregatedData[4,],type = 'p', pch=16,col='goldenrod1',cex=1)
  points(1:length(CutOffRange),AggregatedData[5,],type = 'p', pch=16,col='firebrick2',cex=1)
  
  points(1:length(CutOffRange),AggregatedData[6,],type = 'p', pch=2,col='blue',cex=1)
  points(1:length(CutOffRange),AggregatedData[7,],type = 'p', pch=2,col='darkgreen',cex=1)
  points(1:length(CutOffRange),AggregatedData[8,],type = 'p', pch=2,col='goldenrod1',cex=1)
  points(1:length(CutOffRange),AggregatedData[9,],type = 'p', pch=2,col='firebrick2',cex=1)
  
  # legend("topright", legend=c("NoNet","ZM-5m","PM-5m","MM-5m","IM-5m","ZM-25m","PM-25m","MM-25m","IM-25m"),col=c("black","blue", "darkgreen","goldenrod1","firebrick2","blue", "darkgreen","goldenrod1","firebrick2"),pch = c(15,16,16,16,16,2,2,2,2), cex=0.6,inset = 0.02)
  legend("right",inset=c(-0.15,0),legend=c("NoNet","ZM-5m","PM-5m","MM-5m","IM-5m","ZM-25m","PM-25m","MM-25m","IM-25m"),col=c("black","blue", "darkgreen","goldenrod1","firebrick2","blue", "darkgreen","goldenrod1","firebrick2"),pch = c(15,16,16,16,16,2,2,2,2), cex=0.6)
  dev.off()

}

# ================================== #
# ================================== #
# ================================== #



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
