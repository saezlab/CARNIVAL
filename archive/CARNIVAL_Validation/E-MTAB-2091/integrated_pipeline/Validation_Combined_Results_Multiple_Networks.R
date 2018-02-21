# Validation scripts - Integrated Pipeline - Combined results

# Clear all variables, screen and figures
rm(list=ls());cat("\014");if (length(dev.list())>0){dev.off()}

# Select network (1=omnipath, 2=generic, 3=signor) # Now select all 3
# ScaffoldNet <- 1 # Single
ScaffoldNetAll <- c(1,2,3) # Multiople
ScaffoldNameAll <- c("omnipath","generic","signor")
  
# Select stimuli's targets (1=only main, 2=main+STITCH)
StimuliTarget <- 1 # Single
# StimuliTarget <- c(1,2) # Multiple

# Select SD-Cutoff for input loading (c(1,1.5,2))
Meas_Cutoff <- 2 # Single
# Meas_Cutoff <- c(1,1.5,2) # Multiple

# Select cutoff measure and value for pp-data
# 1=absolute value, 2=mean+/-Cutoff*SD (Gaussian), 3= median+/-Cutoff*mean_abs_diff
DiscretPP <- 2 
# PP_Cutoff <- 2 # Single
PP_Cutoff <- c(0.1,0.5,1,1.5,2,2.5) # Multiple

# Plot advanced figures?
PlotThresholdCoverage <- T # Combined figure with multiple threshold coverage
PlotCombinedGSEA <- F # Combined GSEA results from multiple MeasCutoffs and inputTypes

# ================================================== #

# ======================================================== #
# ===== STEP 1: READ-IN RESULTS AND GENERATE SUMMARY ===== #
# ======================================================== #

# if (ScaffoldNet==1) {ScaffoldName <- "omnipath"} else if (ScaffoldNet==2) {ScaffoldName <- "generic"} else if (ScaffoldNet==3) {ScaffoldName <- "signor"}
if (StimuliTarget==1) {DIR_STITCH <- "";FOLDER_STITCH <-""; STITCH_Label <- ""} else if (StimuliTarget==2) {DIR_STITCH <- "STITCH_input/"; FOLDER_STITCH <-"_STITCH_input";STITCH_Label <- "_STITCH"}

# Load source scripts
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL"); source("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/src/CRILPR_Functions.R")
setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/") # set working directory (relative)

GlobalROC5m <- list(); GlobalROC25m <- list() # For global ROC curve plot

for (counter_ppCutOff in 1:length(PP_Cutoff)) {
  
  print(paste0("Processing SDcutoff : ",toString(PP_Cutoff[counter_ppCutOff])))
  
  # Load and discretize pp-datasets: E-MTAB-2091_PP5min.csv & E-MTAB-2091_PP25min.csv
  PP5min <- read.table("resources/E-MTAB-2091_PP5min.csv",header=T,sep=",",stringsAsFactors = F)
  PP5minZScore <- scale(PP5min[,4:ncol(PP5min)],center=TRUE,scale = TRUE); rownames(PP5minZScore) <- PP5min[,1]
  PP5min_Cutoff <- Discretize_CARNIVAL(PP5minZScore,DiscretPP,PP_Cutoff[counter_ppCutOff])
  # rowSums(abs(PP5min_Cutoff))
  PP25min <- read.table("resources/E-MTAB-2091_PP25min.csv",header=T,sep=",",stringsAsFactors = F)
  PP25minZScore <- scale(PP25min[,4:ncol(PP25min)],center=TRUE,scale = TRUE); rownames(PP25minZScore) <- PP25min[,1]
  PP25min_Cutoff <- Discretize_CARNIVAL(PP25minZScore,DiscretPP,PP_Cutoff[counter_ppCutOff])
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
  
  # ============================================= #
  # Initialise Collection of all combined results #
  # ============================================= #
  
  ValResMat_Nets <- list()
  ValResMatSign_Nets <- list()
  ValResMatSignCutOff_Nets <- list()
  ValResMatReal_Nets <- list()
  ValResMatAll_Nets <- list()
  ModAct_All_Nets <- list()
  NoActivityID_Nets <- list()
  NoNetworkID_Nets <- list()
  
  for (counter_net in 1:length(ScaffoldNetAll)) {
    
    ScaffoldNet <- ScaffoldNetAll[counter_net]
    ScaffoldName <- ScaffoldNameAll[counter_net] 
  
    # Prepare results matrix
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
    
    ModAct_All <- list()
    ModAct_All[length(Compounds)] <- list(NULL)
    
    for (counter_compound in 1:length(Compounds)) {
    # for (counter_compound in 1:15) {
        
      print(paste0("Now mapping: ",toString(counter_compound),"/",toString(length(Compounds))," - ",Compounds[counter_compound]))
        
      if (file.exists(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/",DIR_STITCH,"ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,FOLDER_STITCH,"/nodesActivity_1.txt"))) {
    
        if (file.exists(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/",DIR_STITCH,"ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,FOLDER_STITCH,"/ActivityNetwork_1.dot"))) {
          
          Idx_Condition <- which(Compounds[counter_compound]==CompoundsPP_Names)
          
          for (counter_all in 4:ncol(PP5min)) { # same for PP25min
            Current_PP5min_meas_all <- PP5min_Cutoff[Idx_Condition,counter_all-3]
            Current_PP25min_meas_all <- PP25min_Cutoff[Idx_Condition,counter_all-3]
            ValResMatAll[counter_compound,(((counter_all-1-3)*3)+2):(((counter_all-1-3)*3)+3)] <- c(Current_PP5min_meas_all,Current_PP25min_meas_all)
            ValResMatReal[counter_compound,(((counter_all-1-3)*3)+2):(((counter_all-1-3)*3)+3)] <- c(PP5min[Idx_Condition,counter_all],PP25min[Idx_Condition,counter_all])
          }
          
          NodeAct_current <- read.delim(paste0("~/Desktop/RWTH_Aachen/GitHub/Modelling_Results/CARNIVAL/E-MTAB-2091/Integrated/",DIR_STITCH,"ThresholdSD",toString(Meas_Cutoff),"/validation_",Compounds[counter_compound],"_",ScaffoldName,FOLDER_STITCH,"/nodesActivity_1.txt"),header=T,sep="\t",stringsAsFactors = F)
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
            ModAct_All[[counter_compound]] <- Result_Matrix
            
            # Write a result file for individual molecules
            # write.table(x = Result_Matrix,file = paste0("Validation_Results_",Compounds[counter_compound],"_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)
          } else {
            # ModAct_All[[counter_compound]] <- NULL
          }
        } else {
          NoNetworkID <- c(NoNetworkID, counter_compound)
        }
      } else {
        NoActivityID <- c(NoActivityID, counter_compound)
      }
    
    }
    
    ValResMat_Nets[[counter_net]] <- ValResMat
    ValResMatSign_Nets[[counter_net]] <- ValResMatSign
    ValResMatSignCutOff_Nets[[counter_net]] <- ValResMatSignCutOff
    ValResMatReal_Nets[[counter_net]] <- ValResMatReal
    ValResMatAll_Nets[[counter_net]] <- ValResMatAll
    ModAct_All_Nets[[counter_net]] <- ModAct_All
    NoActivityID_Nets[[counter_net]] <- NoActivityID
    NoNetworkID_Nets[[counter_net]] <- NoNetworkID
    print(" ")
  }

  # Combine results from certain variables
  # ValResMatAll

  ModelResults <- matrix(NA,nrow(ValResMatAll_Nets[[1]]),ncol(ValResMatAll_Nets[[1]])/3*length(ScaffoldNetAll))
  
  ColText1 <- paste0(rep(MeasuredPP,each=length(ScaffoldNetAll)),"-")
  ColText2 <- rep(ScaffoldNameAll,times=ncol(ValResMatAll_Nets[[1]])/length(ScaffoldNetAll))
  
  colnames(ModelResults) <- paste0(ColText1,ColText2)
    
  for (counter_readout in 1:(ncol(ValResMatAll_Nets[[1]])/length(ScaffoldNetAll))) {
    
    ModelResults[,((counter_readout-1)*3)+1] <- ValResMatAll_Nets[[1]][,((counter_readout-1)*3)+1]
    ModelResults[,((counter_readout-1)*3)+2] <- ValResMatAll_Nets[[2]][,((counter_readout-1)*3)+1]
    ModelResults[,((counter_readout-1)*3)+3] <- ValResMatAll_Nets[[3]][,((counter_readout-1)*3)+1]
    
  }
      
  ModelCombined <- matrix(NA,nrow(ValResMatAll_Nets[[1]]),ncol(ValResMatAll_Nets[[1]])/3)
  colnames(ModelCombined) <- MeasuredPP

  for (counter_combine in 1:(ncol(ValResMatAll_Nets[[1]])/3)) {
    
    ModelCombined[,counter_combine] <- sign(rowSums(ModelResults[,(((counter_combine-1)*3)+1) : (((counter_combine-1)*3)+3)]))
    
  }
  
  
  
  # =============================================== #
  # ===== STEP 2: PLOT REPRESENTATIVE FIGURES ===== #
  # =============================================== #

    
  # =========================================== #
  # ===== FIGURE 1: MODEL vs DIST.PP-DATA ===== #
  # =========================================== #
  
  # Need ValResMat -> Map back combined values
  ValResMatAll <- ValResMatAll_Nets[[1]]
  for (counter_map in 1:(ncol(ValResMatAll)/3)) {
    ValResMatAll[,((counter_map-1)*3)+1] <- ModelCombined[,counter_map]
  }
  
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
  
  write.table(x = AccuMat,file = paste0("Accuracy_Validation_Results_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),
                                        if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
                                        else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
                                        else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
                                        ,STITCH_Label,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
  
  pdf(paste0("Stastic_Validation_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),
             if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
             else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
             else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
             ,STITCH_Label,".pdf"))
  
  plot(1:length(MeasuredPP),AccuMat[1,],type = 'p', pch=15, col='black',cex=0.8,
       # xlab = "Measured-PP",
       ylab="Counts",
       ylim = c(0,54),
       main = paste0("Validation CombinedNets MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff[counter_ppCutOff]),STITCH_Label),axes = F)
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
  
  # ========= #
  # ROC Curve #
  # ========= #
  
  # install.packages("ROCR")
  library(ROCR)
  
  ExtractedROC <- ValResMatAll; colnames(ExtractedROC) <- colnames(ValResMatAll)
  Extracted5m <- matrix(NA,nrow(ExtractedROC),ncol(ExtractedROC)); colnames(Extracted5m) <- colnames(ValResMatAll)
  Extracted25m <- matrix(NA,nrow(ExtractedROC),ncol(ExtractedROC)); colnames(Extracted25m) <- colnames(ValResMatAll)
  
  for (counter_PP in 1:(ncol(ValResMatAll)/3)){
    
    # Map to Extracted5m
    for (counter_ROC in 1:nrow(Extracted5m)) {
      if (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==0) 
         {Extracted5m[counter_ROC,((counter_PP-1)*3)+1] <- 0;Extracted5m[counter_ROC,((counter_PP-1)*3)+2] <- 0} # Zero matched -> change to TN [0,0]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==1) |
              (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==-1)) 
              {Extracted5m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted5m[counter_ROC,((counter_PP-1)*3)+2] <- 1} # Perfect matched -> change to TP [1,1]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==1) |
              (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==-1))
              {Extracted5m[counter_ROC,((counter_PP-1)*3)+1] <- 0;Extracted5m[counter_ROC,((counter_PP-1)*3)+2] <- 1} # Mismatched positive -> change to FN [0,1]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==0) |
              (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==0))
              {Extracted5m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted5m[counter_ROC,((counter_PP-1)*3)+2] <- 0} # Mismatched negative -> change to FP [1,0]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==-1) |
              (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+2]==1))
              {Extracted5m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted5m[counter_ROC,((counter_PP-1)*3)+2] <- 0} # Inversed matched -> change to FP [1,0]
    }
    
    # Map to Extracted25m
    for (counter_ROC in 1:nrow(Extracted25m)) {
      if (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==0) 
      {Extracted25m[counter_ROC,((counter_PP-1)*3)+1] <- 0;Extracted25m[counter_ROC,((counter_PP-1)*3)+3] <- 0} # Zero matched -> change to TN [0,0]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==1) |
               (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==-1)) 
      {Extracted25m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted25m[counter_ROC,((counter_PP-1)*3)+3] <- 1} # Perfect matched -> change to TP [1,1]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==1) |
               (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==0 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==-1))
      {Extracted25m[counter_ROC,((counter_PP-1)*3)+1] <- 0;Extracted25m[counter_ROC,((counter_PP-1)*3)+3] <- 1} # Mismatched positive -> change to FN [0,1]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==0) |
               (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==0))
      {Extracted25m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted25m[counter_ROC,((counter_PP-1)*3)+3] <- 0} # Mismatched negative -> change to FP [1,0]
      else if ((ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==-1) |
               (ExtractedROC[counter_ROC,((counter_PP-1)*3)+1]==-1 & ExtractedROC[counter_ROC,((counter_PP-1)*3)+3]==1))
      {Extracted25m[counter_ROC,((counter_PP-1)*3)+1] <- 1;Extracted25m[counter_ROC,((counter_PP-1)*3)+3] <- 0} # Inversed matched -> change to FP [1,0]
    }
     
  }
  
  pdf(paste0("ROC_Statistics_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),
             if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
             else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
             else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
             ,STITCH_Label,".pdf"),width=11,height=11)
  
  par(mfrow=c(5,4))
  
  for (counter_plot in 1:(ncol(Extracted5m)/3)) {
  
    TestPred5m <- Extracted5m[,((counter_plot-1)*3)+1]
    TestLabel5m <- Extracted5m[,((counter_plot-1)*3)+2]
    
    TestPred25m <- Extracted25m[,((counter_plot-1)*3)+1]
    TestLabel25m <- Extracted25m[,((counter_plot-1)*3)+3]
    
    TestLabel25m[which(is.na(TestLabel25m))] <- 0
    
    if (length(unique(TestLabel5m))>1 | length(unique(TestLabel25m))>1) {
      
      if (length(unique(TestLabel5m))>1 & length(unique(TestLabel25m))>1) {
      
        pred5m <- prediction(TestPred5m,TestLabel5m)
        roc.perf5m = performance(pred5m, measure = "tpr", x.measure = "fpr")
        auc.perf5m = performance(pred5m, measure = "auc")
        plot(roc.perf5m,col=c("blue"),main=gsub("_Mod","",colnames(Extracted5m)[((counter_plot-1)*3)+1]),xlab="",ylab="")

        par(new=TRUE)
        pred25m <- prediction(TestPred25m,TestLabel25m)
        roc.perf25m = performance(pred25m, measure = "tpr", x.measure = "fpr")
        auc.perf25m = performance(pred25m, measure = "auc")
        plot(roc.perf25m,col=c("darkgreen"))
        
        abline(a=0, b= 1,col="grey")
        text(0.8,0.3,paste0("5m : ",toString(round(as.numeric(auc.perf5m@y.values),digits=2))),cex=1)
        text(0.8,0.1,paste0("25m: ",toString(round(as.numeric(auc.perf25m@y.values),digits=2))),cex=1)
        
      } else if (length(unique(TestLabel5m))>1 & length(unique(TestLabel25m))==1) {
        
        pred5m <- prediction(TestPred5m,TestLabel5m)
        roc.perf5m = performance(pred5m, measure = "tpr", x.measure = "fpr")
        auc.perf5m = performance(pred5m, measure = "auc")
        plot(roc.perf5m,col=c("blue"),main=gsub("_Mod","",colnames(Extracted5m)[((counter_plot-1)*3)+1]))
        
        abline(a=0, b= 1,col="grey")
        text(0.8,0.3,paste0("5m : ",toString(round(as.numeric(auc.perf5m@y.values),digits=2))),cex=1)
        
      } else if (length(unique(TestLabel5m))==1 & length(unique(TestLabel25m))>1) {
        
        pred25m <- prediction(TestPred25m,TestLabel25m)
        roc.perf25m = performance(pred25m, measure = "tpr", x.measure = "fpr")
        auc.perf25m = performance(pred25m, measure = "auc")
        plot(roc.perf25m,col=c("darkgreen"),main=gsub("_Mod","",colnames(Extracted5m)[((counter_plot-1)*3)+1]))
        
        abline(a=0, b= 1,col="grey")
        text(0.8,0.1,paste0("25m: ",toString(round(as.numeric(auc.perf25m@y.values),digits=2))),cex=1)
      }
    
    }
    
  }
  
  dev.off()
  
  par(mfrow=c(1,1))
  
  # Collect LocalROC predictions for GlobalROC plot
  
  LocalROC5m <- NULL
  LocalROC25m <- NULL
  
  for (counter_LocalROC in 1:(ncol(ExtractedROC)/3)) {
    LocalROC5m <- rbind(LocalROC5m,cbind(Extracted5m[,((counter_LocalROC-1)*3)+1],Extracted5m[,((counter_LocalROC-1)*3)+2]))
    LocalROC25m <- rbind(LocalROC25m,cbind(Extracted25m[,((counter_LocalROC-1)*3)+1],Extracted25m[,((counter_LocalROC-1)*3)+3]))
  }
  
  GlobalROC5m[[counter_ppCutOff]] <- prediction(LocalROC5m[,1],LocalROC5m[,2])
  GlobalROC25m[[counter_ppCutOff]] <- prediction(LocalROC25m[,1],LocalROC25m[,2])

  
  # =================================== #
  # ===== FIGURE 2: DIFF ANALYSIS ===== #
  # =================================== #
  
  # Need ValResMatSignCutOff -> Map back combined values
  ValResMatSignCutOff <- ValResMatSignCutOff_Nets[[1]]
  for (counter_map in 1:(ncol(ValResMatSignCutOff)/3)) {
    ValResMatSignCutOff[,((counter_map-1)*3)+1] <- ModelCombined[,counter_map]
  }
  
  # Analysis on the difference of predicted node activity and measurement (here use ValResMatSignCutOff)
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

  pdf(paste0("Average_Difference_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),
             if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
             else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
             else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
             ,STITCH_Label,".pdf"))

  plot(1:length(MeasuredPP),DiffMat[1,],type = 'p',col='red',cex=DiffMat[2,]/2,
       xlab = "Measured-PP",ylab="Distance",
       ylim = c(0,2.3),
       main = paste0("AvgDiff CombinedNets MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff[counter_ppCutOff]),STITCH_Label),axes = F)
  axis(1, at=seq(1,length(MeasuredPP),by=1),labels=MeasuredPP, las = 2)
  axis(2, at=seq(0,2.3,by=0.2),labels=seq(0,2.3,by=0.2), las = 2)
  points(1:length(MeasuredPP),DiffMat[3,],type = 'p', col='blue',cex=DiffMat[4,]/12)
  lines(1:length(MeasuredPP),rep(1,length(MeasuredPP)),lty=2,col="grey")
  legend("topright", legend=c("PP-5min", "PP-25min"),col=c("red", "blue"), lty=1, cex=0.8,inset = 0.02)
  dev.off()
  
}

# ======================== #
# === Global ROC-curve === #
# ======================== #

pdf(paste0("Global_ROC_Statistics_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label,".pdf"))

# install.packages("pracma")
library(pracma)

library(RColorBrewer)
ROCcols <- brewer.pal(length(PP_Cutoff), "Spectral")

GlobalROCauc <- NULL
GlobalROCaupr <- NULL

for (counter_ROCplot in 1:length(PP_Cutoff)) {
    
    if (counter_ROCplot!=1) {par(new=TRUE)}
    roc.perf5m = performance(GlobalROC5m[[counter_ROCplot]], measure = "tpr", x.measure = "fpr")
    auc.perf5m = performance(GlobalROC5m[[counter_ROCplot]], measure = "auc")
    # pr.perf5m = performance(pred5m, measure = "prec", x.measure = "rec")
    # IdxPrNA5m <- unique(c(which(is.na(pr.perf5m@x.values[[1]])),which(is.na(pr.perf5m@y.values[[1]]))))
    # aupr.pref5m <- trapz(pr.perf5m@x.values[[1]][-IdxPrNA5m],pr.perf5m@y.values[[1]][-IdxPrNA5m])
    plot(roc.perf5m,col=ROCcols[counter_ROCplot],xlab="",ylab="")
      
    par(new=TRUE)
    roc.perf25m = performance(GlobalROC25m[[counter_ROCplot]], measure = "tpr", x.measure = "fpr")
    auc.perf25m = performance(GlobalROC25m[[counter_ROCplot]], measure = "auc")
    # pr.perf25m = performance(pred25m, measure = "prec", x.measure = "rec")
    # IdxPrNA25m <- unique(c(which(is.na(pr.perf25m@x.values[[1]])),which(is.na(pr.perf25m@y.values[[1]]))))
    # aupr.pref25m <- trapz(pr.perf25m@x.values[[1]][-IdxPrNA25m],pr.perf25m@y.values[[1]][-IdxPrNA25m])
    
    if (counter_ROCplot!=length(PP_Cutoff)) {
      plot(roc.perf25m,col=ROCcols[counter_ROCplot],lty=2,xlab="",ylab="")
    } else {
      plot(roc.perf25m,col=ROCcols[counter_ROCplot],lty=2,main=paste0("Global ROC-curve - CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label))
    }
    abline(a=0, b= 1,col="grey")
    GlobalROCauc <- c(GlobalROCauc,c(round(as.numeric(auc.perf5m@y.values),digits=2),round(as.numeric(auc.perf25m@y.values),digits=2)))
    # GlobalROCaupr <- c(GlobalROCaupr,c(round(as.numeric(aupr.pref5m),digits=3),round(as.numeric(aupr.pref25m),digits=3)))
    
}

GlobalROCLegendText <- NULL
for (counter_ROCLeg in 1:length(PP_Cutoff)) 
  {GlobalROCLegendText <- c(GlobalROCLegendText,toString(PP_Cutoff[counter_ROCLeg]))}

GlobalROCLegendText <- paste0(rep(GlobalROCLegendText,each=2))

GlobalROCLegendTextAll <- paste(rep(c("5m","25m"),times=length(PP_Cutoff)),
                             rep("- SD:",times=length(PP_Cutoff)*2),GlobalROCLegendText,
                             rep("- AUC:",times=length(PP_Cutoff)*2),GlobalROCauc)
                             # rep("- AUPR:",times=length(PP_Cutoff)*2),GlobalROCaupr)

legend("bottomright", legend=GlobalROCLegendTextAll,col=rep(ROCcols,each=2), lty=rep(c(1,2),times=length(PP_Cutoff)), cex=0.7,inset = 0.02)

dev.off()


# =========================================== #
# ===== FIGURE 3: MODEL vs CONT.PP-DATA ===== #
# =========================================== #

# Need ValResMatReal -> Map back combined values
ValResMatReal <- ValResMatReal_Nets[[1]]
for (counter_map in 1:(ncol(ValResMatReal)/3)) {
  ValResMatReal[,((counter_map-1)*3)+1] <- ModelCombined[,counter_map]
}


# Plot distribution of node value to real-pp data (from ValResMatReal)
# First remove the empty models
DistModelPP <- ValResMatReal[-c(NoActivityID,NoNetworkID),]

pdf(paste0("FitDistribution_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label,".pdf"))

require(reshape2)
require(ggplot2)

MeltedDataBoxplot <- NULL

for (counter in 1:(ncol(DistModelPP)/3)) {
  BoxPlotData <- matrix(NA,length(Compounds),6)
  ModelPlusOne5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==1),((counter-1)*3)+2];     if (length(ModelPlusOne5min)>0) {BoxPlotData[1:length(ModelPlusOne5min),1] <- ModelPlusOne5min}
  ModelZero5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==0),((counter-1)*3)+2];        if (length(ModelZero5min)>0) {BoxPlotData[1:length(ModelZero5min),2] <- ModelZero5min}
  ModelMinusOne5min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==-1),((counter-1)*3)+2];   if (length(ModelMinusOne5min)>0) {BoxPlotData[1:length(ModelMinusOne5min),3] <- ModelMinusOne5min}
  ModelPlusOne25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==1),((counter-1)*3)+3];    if (length(ModelPlusOne25min)>0) {BoxPlotData[1:length(ModelPlusOne25min),4] <- ModelPlusOne25min}
  ModelZero25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==0),((counter-1)*3)+3];       if (length(ModelZero25min)>0) {BoxPlotData[1:length(ModelZero25min),5] <- ModelZero25min}
  ModelMinusOne25min <- DistModelPP[which(DistModelPP[,(((counter-1)*3)+1)]==-1),((counter-1)*3)+3];  if (length(ModelMinusOne25min)>0) {BoxPlotData[1:length(ModelMinusOne25min),6] <- ModelMinusOne25min}
  colnames(BoxPlotData) <-c("1 [5m]","0 [5m]","-1 [5m]","1 [25m]","0 [25m]","-1 [25m]")

  df_current <- melt(BoxPlotData, id.var = "Label")
  df_current[,1] <- MeasuredPP[counter]
  colnames(df_current) <- c("Readout","Condition","ppValue")
  MeltedDataBoxplot <- rbind(MeltedDataBoxplot,df_current)
}

colnames(MeltedDataBoxplot) <- c("Readout","Condition","ppValue")
MeltedDataBoxplot <- MeltedDataBoxplot[-which(is.na(MeltedDataBoxplot$ppValue)),]

p <- ggplot(data = MeltedDataBoxplot, aes(x=Condition, y=ppValue)) + 
  # geom_boxplot(aes(fill=Condition)) # Boxplots
  # geom_dotplot(aes(fill=Condition,alpha=0.3),binaxis='y', stackdir='center', dotsize=1) # Dotplots
  # geom_dotplot(aes(fill=Condition,alpha=0.3),binaxis='y', stackdir='up', dotsize=1, stackratio=0) # Dotplots
  geom_violin(aes(fill=Condition),width=1.5) + geom_boxplot(width=0.1) # + theme_minimal()
  # geom_boxplot(aes(fill=Condition)) + geom_dotplot(aes(fill=Condition,alpha=0.3),binaxis='y', stackdir='centerwhole', dotsize=1, stackratio=0) # Boxplots
  # geom_dotplot(aes(fill=Condition,alpha=0.3),binaxis='y', stackdir='up', dotsize=1, stackratio=0) + geom_boxplot(width=0.1) # Dotplots

p + facet_wrap( ~ Readout, scales="free")+ theme(axis.text.x=element_blank())+ scale_fill_manual(values=c("#AED6F1", "#5DADE2", "#2E86C1","#A9DFBF", "#52BE80", "#229954"))+ ggtitle(paste0("FitDistribution_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label))

dev.off()


# =================================== #
# ===== FIGURE 4: GSEA ANALYSIS ===== #
# =================================== #

# Need ModAct_All -> Map back combined values (different approach)
ModAct_All_pool <- ModAct_All_Nets[[1]]
for (counter_map in 1:length(ModAct_All_pool)) {
  ModAct_All_pool[[counter_map]] <- rbind(ModAct_All_Nets[[1]][[counter_map]],ModAct_All_Nets[[2]][[counter_map]],ModAct_All_Nets[[3]][[counter_map]])
}

ModAct_All <- list()
for (counter_map in 1:length(ModAct_All_pool)) {
  ModAct_All[[counter_map]] <- unique(ModAct_All_pool[[counter_map]])
}


# Implement Aurelien's GSEA with piano

library(piano)

nCores <- 2
nPerm <- 10000

gsaResAll <- matrix(NA,length(Compounds),8)
rownames(gsaResAll) <- Compounds
colnames(gsaResAll) <- c("DistSP-5m","pAdjSP-5m","DistSM-5m","pAdjSM-5m","DistSP-25m","pAdjSP-25m","DistSM-25m","pAdjSM-25m")

geneSetStat <- c("fgsea","median")

# for (counter_compound in 1:length(Compounds)) {
for (counter_compound in 1:length(ModAct_All)) {
  # print(paste0("Calculating GSEA for: ",Compounds[[counter_compound]]," - ",toString(counter_compound),"/",toString(length(Compounds))))
  print(paste0("Calculating GSEA for: ",Compounds[[counter_compound]]," - ",toString(counter_compound),"/",toString(length(ModAct_All))))
  Idx_Condition <- which(Compounds[counter_compound]==CompoundsPP_Names)
  PP5min_current <- PP5min[Idx_Condition,4:ncol(PP5min)]
  PP25min_current <- PP25min[Idx_Condition,4:ncol(PP5min)]
  
  if (!is.null(ModAct_All[[counter_compound]])) {
    
    if (nrow(ModAct_All[[counter_compound]])>0) {
      GS_matrix <- matrix(NA,nrow(ModAct_All[[counter_compound]]),2)
      if (nrow(ModAct_All[[counter_compound]])>1) {
        GS_matrix[,1] <- ModAct_All[[counter_compound]][,1]
        GS_matrix[,2] <- ModAct_All[[counter_compound]][,2]
        GS_matrix_plusIdx  <- which(as.numeric(GS_matrix[,2])>0)
        GS_matrix_minusIdx <- which(as.numeric(GS_matrix[,2])<0)
        # GS_matrix[GS_matrix_plusIdx,2]  <- "Up"; GS_matrix[GS_matrix_minusIdx,2] <- "Dn"
      } else if (nrow(ModAct_All[[counter_compound]])==1) {
        GS_matrix[1] <- ModAct_All[[counter_compound]][1]
        GS_matrix[2] <- ModAct_All[[counter_compound]][2]
        GS_matrix_plusIdx  <- which(as.numeric(GS_matrix[2])>0)
        GS_matrix_minusIdx <- which(as.numeric(GS_matrix[2])<0)
      }
      
      # if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=24) {
      # if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=2) {
      if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=2  & counter_compound!=24) {
      # if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0)) {
        geneSet <- loadGSC(GS_matrix)
        gsaRes5min <- NULL; gsaRes25min <- NULL;
        gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
        gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm)
        # gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
        # gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm)
        if (length(gsaRes5min$statDistinctDir)==2) {
          
          OrderDist <- names(gsaRes5min$gsc)
          
          if (OrderDist[1]==1) {
            
            # 5 min
            gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[1]
            if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[1]}
            else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[1]}
            gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[2]
            if (gsaRes5min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[2]}
            else if (gsaRes5min$statDistinctDir[2]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[2]}
            
            # 25 min
            gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[1]
            if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[1]}
            else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[1]}
            gsaResAll[counter_compound,7] <- gsaRes25min$statDistinctDir[2]
            if (gsaRes25min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirUp[2]}
            else if (gsaRes25min$statDistinctDir[2]<0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirDn[2]}
            
          } else if (OrderDist[1]==-1) {
            
            # 5 min
            gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[1]
            if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[1]}
            else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[1]}
            gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[2]
            if (gsaRes5min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[2]}
            else if (gsaRes5min$statDistinctDir[2]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[2]}
            
            # 25 min
            gsaResAll[counter_compound,7] <- gsaRes25min$statDistinctDir[1]
            if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirUp[1]}
            else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirDn[1]}
            gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[2]
            if (gsaRes25min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[2]}
            else if (gsaRes25min$statDistinctDir[2]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[2]}
            
          }
        } else if (length(gsaRes5min$statDistinctDir)==1) {
          OrderDist <- names(gsaRes5min$gsc)
          if (OrderDist[1]==1) {
            
            # 5 min
            gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[1]
            if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[1]}
            else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[1]}
            
            # 25 min
            gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[1]
            if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[1]}
            else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[1]}
            
          } else if (OrderDist[1]==-1) {
            
            # 5 min
            gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[1]
            if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[1]}
            else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[1]}
            
            # 25 min
            gsaResAll[counter_compound,7] <- gsaRes5min$statDistinctDir[1]
            if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,8] <- gsaRes5min$pAdjDistinctDirUp[1]}
            else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,8] <- gsaRes5min$pAdjDistinctDirDn[1]}
            
          }
          
        }
      }    
    }
  }
}

gsaResAll_final <- gsaResAll[which(rowSums(!is.na(gsaResAll))>0),]

gsaResAll_plot <- gsaResAll_final
gsaResAll_plot[,2] <- (-1)*log10(gsaResAll_plot[,2])
gsaResAll_plot[,4] <- (-1)*log10(gsaResAll_plot[,4])
gsaResAll_plot[,6] <- (-1)*log10(gsaResAll_plot[,6])
gsaResAll_plot[,8] <- (-1)*log10(gsaResAll_plot[,8])

pdf(paste0("GSEA_Valcano_Validation_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label,".pdf"))

plot(gsaResAll_plot[,1],gsaResAll_plot[,2],type = 'p',pch=2,col='blue',cex=1,
     xlab = "GSEA score",ylab="-log10(Adj-pVal)",
     ylim = c(0,2),xlim= c(-1,1),
     main = paste0("GSEA/Adj-pVal CombinedNets MeasC/o:",toString(Meas_Cutoff),STITCH_Label),axes = F)
axis(1, at=seq(-1,1,by=0.2),labels=seq(-1,1,by=0.2), las = 2,cex.axis=1)
axis(2, at=seq(0,2,by=0.2),labels=seq(0,2,by=0.2), las = 2)
points(gsaResAll_plot[,3],gsaResAll_plot[,4],type = 'p',pch=2, col='goldenrod1',cex=1)
points(gsaResAll_plot[,5],gsaResAll_plot[,6],type = 'p',pch=16, col='darkgreen',cex=1)
points(gsaResAll_plot[,7],gsaResAll_plot[,8],type = 'p',pch=16, col='firebrick2',cex=1)
lines(rep(0,length(seq(0,2,by=0.2))),seq(0,2,by=0.2),type = "l",lty=2, col='grey',cex=1)
lines(seq(-1,1,by=0.2),rep(1.0,length(seq(-1,1,by=0.2))),type = "l",lty=2, col='grey',cex=1)

legend("top", legend=c("SetUp-5m","SetDn-5m","SetUp-25m","SetDn-25m"),col=c("blue", "goldenrod1","darkgreen","firebrick2"),pch = c(2,2,16,16), cex=0.7,inset = 0)

dev.off()


# Pool all results in one object per SDcutoff and per inputType (for all combined model)

ModAct_All_BkUp <- ModAct_All
# ModAct_All <- ModAct_All_BkUp

ModAct_All_Combined <- NULL
for (counter_map in 1:length(ModAct_All)) {
  if (!is.null(ModAct_All[[counter_map]])) {
    if (nrow(ModAct_All[[counter_map]])==1) {
      ModAct_All[[counter_map]][1] <- paste0(ModAct_All[[counter_map]][1],"_",toString(counter_map),"_",toString(Meas_Cutoff),STITCH_Label)
    } else {
      ModAct_All[[counter_map]][,1] <- paste0(ModAct_All[[counter_map]][,1],"_",toString(counter_map),"_",toString(Meas_Cutoff),STITCH_Label)
    }
  }
  ModAct_All_Combined <- rbind(ModAct_All_Combined,ModAct_All[[counter_map]])
}

write.table(x = ModAct_All_Combined,file = paste0("Pooled_ModelActivity_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label,".tsv"),quote = F,sep = "\t",col.names = T,row.names = F)


# ================================== #
# ================================== #
# ================================== #


# ======================================== #
# ===== FIGURE 5: THRESHOLD ANALYSIS ===== #
# ======================================== #

if (PlotThresholdCoverage) {

# Aggregate results from different thresholds (after complete result generations)

  CutOffRange <- PP_Cutoff
  AggregatedData <- matrix(NA,9,length(CutOffRange)) 
  
  for (counter in 1:length(CutOffRange)) {
    CurrentSummary <- read.table(paste0("Accuracy_Validation_Results_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),"_MeanSDCutOff_",toString(CutOffRange[counter]),STITCH_Label,".tsv"),sep = "\t", stringsAsFactors = F,header = T)
    AggregatedData[,counter] <- rowMeans(CurrentSummary)
  }
  
  pdf(paste0("Matching_CombinedNets_MeasCutOff_",toString(Meas_Cutoff),STITCH_Label,".pdf"))
  
  par(mar=c(5.1, 4.1, 4.1, 5.1), xpdPlotThresholdCoverage=TRUE)
  plot(1:length(CutOffRange),AggregatedData[1,],type = 'p', pch=15, col='black',cex=0.8,
       xlab = "Measured-PP",ylab="Counts",
       ylim = c(0,max(AggregatedData)+5),
       main = paste0("Matching CombinedNets MeasC/o:",toString(Meas_Cutoff),STITCH_Label),axes = F)
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
  
  legend("right",inset=c(-0.15,0),legend=c("NoNet","ZM-5m","PM-5m","MM-5m","IM-5m","ZM-25m","PM-25m","MM-25m","IM-25m"),col=c("black","blue", "darkgreen","goldenrod1","firebrick2","blue", "darkgreen","goldenrod1","firebrick2"),pch = c(15,16,16,16,16,2,2,2,2), cex=0.6)
  dev.off()

}


# ======================================== #
# ===== FIGURE 6: COMBINED GSEA PLOT ===== #
# ======================================== #

if (PlotCombinedGSEA) {

  All_MeasCutOff <- c(1,1.5,2)
  All_InputType <- c(1,2)

  All_InputNames <- c("","_STITCH")
  
  ModAct_PlotCombined <- list()
  
  for (counter_GSEA1 in 1:length(All_MeasCutOff)) {
    for (counter_GSEA2 in 1:length(All_InputType)) {  
      ModAct_PlotCombined[[((counter_GSEA1-1)*2)+counter_GSEA2]] <- read.table(paste0("Pooled_ModelActivity_CombinedNets_MeasCutOff_",toString(All_MeasCutOff[counter_GSEA1]),All_InputNames[All_InputType[counter_GSEA2]],".tsv"),header = T,sep = "\t",stringsAsFactors = F)
    }
  }
  
  
  for (counter_GSEA_combined in 1:length(ModAct_PlotCombined)) {
  
    GS_matrix <- matrix(NA,nrow(ModAct_PlotCombined[[counter_GSEA_combined]]),2)
    GS_meas <- matrix(NA,nrow(ModAct_PlotCombined[[counter_GSEA_combined]]),2)
    if (nrow(ModAct_PlotCombined[[counter_GSEA_combined]])>1) {
      GS_matrix[,1] <- ModAct_PlotCombined[[counter_GSEA_combined]][,1]
      GS_matrix[,2] <- sign(as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][,2]))
      GS_meas[,1] <- as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][,3])
      GS_meas[,2] <- as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][,4])
      GS_matrix_plusIdx  <- which(as.numeric(GS_matrix[,2])>0)
      GS_matrix_minusIdx <- which(as.numeric(GS_matrix[,2])<0)
      # GS_matrix[GS_matrix_plusIdx,2]  <- "Up"; GS_matrix[GS_matrix_minusIdx,2] <- "Dn"
    } else if (nrow(ModAct_All[[counter_GSEA_combined]])==1) {
      GS_matrix[1] <- ModAct_PlotCombined[[counter_GSEA_combined]][1]
      GS_matrix[2] <- sign(as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][2]))
      GS_meas[1] <- as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][3])
      GS_meas[2] <- as.numeric(ModAct_PlotCombined[[counter_GSEA_combined]][4])
      GS_matrix_plusIdx  <- which(as.numeric(GS_matrix[2])>0)
      GS_matrix_minusIdx <- which(as.numeric(GS_matrix[2])<0)
    }
  
  # if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=24) {
  # if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=2  & counter_compound!=24) {
  if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0)) {
    geneSet <- loadGSC(GS_matrix)
    gsaRes5min <- NULL; gsaRes25min <- NULL;
    
    PP5min_current <- GS_meas[,1];names(PP5min_current) <- GS_matrix[,1]
    PP25min_current <- GS_meas[,2];names(PP25min_current) <- GS_matrix[,1]
    
    PP5min_current <- PP5min_current[unique(names(PP5min_current))]
    PP25min_current <- PP25min_current[unique(names(PP25min_current))]
    
    gsaRes5min <- runGSA(PP5min_current, gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
    gsaRes25min <- runGSA(PP25min_current, gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm)
    # gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
    # gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm)
    # # gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
    # gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm)
    if (length(gsaRes5min$statDistinctDir)==2) {
      
      OrderDist <- names(gsaRes5min$gsc)
      
      if (OrderDist[1]==1) {
        
        # 5 min
        gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[1]
        if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[1]}
        else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[1]}
        gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[2]
        if (gsaRes5min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[2]}
        else if (gsaRes5min$statDistinctDir[2]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[2]}
        
        # 25 min
        gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[1]
        if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[1]}
        else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[1]}
        gsaResAll[counter_compound,7] <- gsaRes25min$statDistinctDir[2]
        if (gsaRes25min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirUp[2]}
        else if (gsaRes25min$statDistinctDir[2]<0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirDn[2]}
        
      } else if (OrderDist[1]==-1) {
        
        # 5 min
        gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[1]
        if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[1]}
        else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[1]}
        gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[2]
        if (gsaRes5min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[2]}
        else if (gsaRes5min$statDistinctDir[2]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[2]}
        
        # 25 min
        gsaResAll[counter_compound,7] <- gsaRes25min$statDistinctDir[1]
        if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirUp[1]}
        else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,8] <- gsaRes25min$pAdjDistinctDirDn[1]}
        gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[2]
        if (gsaRes25min$statDistinctDir[2]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[2]}
        else if (gsaRes25min$statDistinctDir[2]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[2]}
        
      }
    } else if (length(gsaRes5min$statDistinctDir)==1) {
      OrderDist <- names(gsaRes5min$gsc)
      if (OrderDist[1]==1) {
        
        # 5 min
        gsaResAll[counter_compound,1] <- gsaRes5min$statDistinctDir[1]
        if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirUp[1]}
        else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[1]}
        
        # 25 min
        gsaResAll[counter_compound,5] <- gsaRes25min$statDistinctDir[1]
        if (gsaRes25min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirUp[1]}
        else if (gsaRes25min$statDistinctDir[1]<0) {gsaResAll[counter_compound,6] <- gsaRes25min$pAdjDistinctDirDn[1]}
        
      } else if (OrderDist[1]==-1) {
        
        # 5 min
        gsaResAll[counter_compound,3] <- gsaRes5min$statDistinctDir[1]
        if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirUp[1]}
        else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,4] <- gsaRes5min$pAdjDistinctDirDn[1]}
        
        # 25 min
        gsaResAll[counter_compound,7] <- gsaRes5min$statDistinctDir[1]
        if (gsaRes5min$statDistinctDir[1]>=0) {gsaResAll[counter_compound,8] <- gsaRes5min$pAdjDistinctDirUp[1]}
        else if (gsaRes5min$statDistinctDir[1]<0) {gsaResAll[counter_compound,8] <- gsaRes5min$pAdjDistinctDirDn[1]}
        
      }
      
    }
  }    
}

gsaResAll_final <- gsaResAll[which(rowSums(!is.na(gsaResAll))>0),]

gsaResAll_plot <- gsaResAll_final
gsaResAll_plot[,2] <- (-1)*log10(gsaResAll_plot[,2])
gsaResAll_plot[,4] <- (-1)*log10(gsaResAll_plot[,4])
gsaResAll_plot[,6] <- (-1)*log10(gsaResAll_plot[,6])
gsaResAll_plot[,8] <- (-1)*log10(gsaResAll_plot[,8])

pdf(paste0("GSEA_Valcano_Validation_CombinedNets_MeasCutOff_CombinedSettings.pdf"))

plot(gsaResAll_plot[,1],gsaResAll_plot[,2],type = 'p',pch=2,col='blue',cex=1,
     xlab = "GSEA score",ylab="-log10(Adj-pVal)",
     ylim = c(0,2),xlim= c(-1,1),
     main = paste0("GSEA/Adj-pVal CombinedNets CombinedSettings"),axes = F)
axis(1, at=seq(-1,1,by=0.2),labels=seq(-1,1,by=0.2), las = 2,cex.axis=1)
axis(2, at=seq(0,2,by=0.2),labels=seq(0,2,by=0.2), las = 2)
points(gsaResAll_plot[,3],gsaResAll_plot[,4],type = 'p',pch=2, col='goldenrod1',cex=1)
points(gsaResAll_plot[,5],gsaResAll_plot[,6],type = 'p',pch=16, col='darkgreen',cex=1)
points(gsaResAll_plot[,7],gsaResAll_plot[,8],type = 'p',pch=16, col='firebrick2',cex=1)
lines(rep(0,length(seq(0,2,by=0.2))),seq(0,2,by=0.2),type = "l",lty=2, col='grey',cex=1)
lines(seq(-1,1,by=0.2),rep(1.0,length(seq(-1,1,by=0.2))),type = "l",lty=2, col='grey',cex=1)

legend("top", legend=c("SetUp-5m","SetDn-5m","SetUp-25m","SetDn-25m"),col=c("blue", "goldenrod1","darkgreen","firebrick2"),pch = c(2,2,16,16), cex=0.7,inset = 0)

dev.off()

  
}


# ================================== #
# ================================== #
# ================================== #

# # === Optional : Save results as files === #

# write.table(x = ValResMat,file = paste0("Summary_Validation_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = ValResMatSign,file = paste0("Summary_Validation_Sign_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = ValResMatSignCutOff,file = paste0("Summary_Validation_SignCutOff_Results_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
#                                                   if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
#                                                   else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
#                                                   else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
#                                                   ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)
# write.table(x = DiffMat,file = paste0("Summary_Validation_AverageDistance_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),
#                                       if (DiscretPP==1) {paste0("_AbsCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
#                                       else if (DiscretPP==2) {paste0("_MeanSDCutOff_",toString(PP_Cutoff[counter_ppCutOff]))} 
#                                       else if (DiscretPP==3) {paste0("_MedianMADCutOff_",toString(PP_Cutoff[counter_ppCutOff]))}
#                                       ,".tsv"),quote = F,sep = "\t",col.names = T,row.names = T)

# # === Optional : Analysis of individual result === #
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
print("Done!")

# --- End of script --- #
