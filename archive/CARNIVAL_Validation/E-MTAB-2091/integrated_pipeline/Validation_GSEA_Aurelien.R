# Validation scripts - Integrated Pipeline

rm(list=ls());cat("\014")

# source("https://bioconductor.org/biocLite.R")
# biocLite("piano")
library(piano)

# Select network
ScaffoldNet <- 2 # 1=omnipath, 2=generic, 3=signor

# Select stimuli's targets
# StimuliTarget <- 2 # 1=all, 2=only main

# Select SD-Cutoff for input loading
Meas_Cutoff <- 1

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

ModAct_All <- list()

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
        
         ModAct_All[[counter_compound]] <- Result_Matrix
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


# Implement Aurelien's GSEA with piano

nCores <- 2
nPerm <- 10000

gsaResAll <- matrix(NA,length(Compounds),8)
rownames(gsaResAll) <- Compounds
colnames(gsaResAll) <- c("DistSP-5m","pAdjSP-5m","DistSM-5m","pAdjSM-5m","DistSP-25m","pAdjSP-25m","DistSM-25m","pAdjSM-25m")

# gsaResAll_statDist <- matrix(NA,length(Compounds),2)
# rownames(gsaResAll_statDist) <- Compounds
# colnames(gsaResAll_statDist) <- c("statDist-5m","statIdst-25m")

geneSetStat <- c("fgsea","median")

for (counter_compound in 1:length(Compounds)) {
  print(paste0("Calculating GSEA for: ",Compounds[[counter_compound]]," - ",toString(counter_compound),"/",toString(length(Compounds))))
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
      if (((length(GS_matrix_plusIdx)>0 & length(GS_matrix_minusIdx))>0) & counter_compound!=2) {
        geneSet <- loadGSC(GS_matrix)
        gsaRes5min <- NULL; gsaRes25min <- NULL;
        gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
        gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "fgsea", ncpus = nCores, nPerm = nPerm)
        # gsaRes5min <- runGSA(unlist(PP5min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm, gsSizeLim = c(1,Inf))
        # gsaRes25min <- runGSA(unlist(PP25min_current), gsc=geneSet, adjMethod = "fdr", geneSetStat = "median", ncpus = nCores, nPerm = nPerm)
        # gsaResAll[counter_compound,1] <- gsaRes5min$pAdjDistinctDirUp[1]
        # gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn[1]
        # gsaResAll[counter_compound,3] <- gsaRes25min$pAdjDistinctDirUp[1]
        # gsaResAll[counter_compound,4] <- gsaRes25min$pAdjDistinctDirDn[1]
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
        
        # gsaResAll[counter_compound,1] <- gsaRes5min$pAdjDistinctDirUp
        # gsaResAll[counter_compound,2] <- gsaRes5min$pAdjDistinctDirDn
        # gsaResAll[counter_compound,3] <- gsaRes25min$pAdjDistinctDirUp
        # gsaResAll[counter_compound,4] <- gsaRes25min$pAdjDistinctDirDn
        # 
        # gsaResAll_statDist[counter_compound,1] <- gsaRes5min$statDistinctDir
        # gsaResAll_statDist[counter_compound,2] <- gsaRes25min$statDistinctDir

      }    
    }
  }
}

gsaResAll_final <- gsaResAll[which(rowSums(!is.na(gsaResAll))>0),]
# gsaResAllstatDist_final <- gsaResAll_statDist[which(rowSums(!is.na(gsaResAll_statDist))>0),]

gsaResAll_plot <- gsaResAll_final
gsaResAll_plot[,2] <- (-1)*log10(gsaResAll_plot[,2])
gsaResAll_plot[,4] <- (-1)*log10(gsaResAll_plot[,4])
gsaResAll_plot[,6] <- (-1)*log10(gsaResAll_plot[,6])
gsaResAll_plot[,8] <- (-1)*log10(gsaResAll_plot[,8])


pdf(paste0("GSEA_Valcano_Validation_",ScaffoldName,"_MeasCutOff_",toString(Meas_Cutoff),".pdf"))

plot(gsaResAll_plot[,1],gsaResAll_plot[,2],type = 'p',pch=2,col='blue',cex=1,
     xlab = "GSEA score",ylab="-log10(Adj-pVal)",
     ylim = c(0,1.2),xlim= c(-1,1),
     main = paste0("GSEA/Adj-pVal ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff)),axes = F)
axis(1, at=seq(-1,1,by=0.2),labels=seq(-1,1,by=0.2), las = 2,cex.axis=1)
axis(2, at=seq(0,1.2,by=0.2),labels=seq(0,1.2,by=0.2), las = 2)
points(gsaResAll_plot[,3],gsaResAll_plot[,4],type = 'p',pch=2, col='goldenrod1',cex=1)
points(gsaResAll_plot[,5],gsaResAll_plot[,6],type = 'p',pch=16, col='darkgreen',cex=1)
points(gsaResAll_plot[,7],gsaResAll_plot[,8],type = 'p',pch=16, col='firebrick2',cex=1)
lines(rep(0,length(seq(0,1.2,by=0.2))),seq(0,1.2,by=0.2),type = "l",lty=2, col='grey',cex=1)
lines(seq(-1,1,by=0.2),rep(0.6,length(seq(-1,1,by=0.2))),type = "l",lty=2, col='grey',cex=1)

legend("topright", legend=c("SetUp-5m","SetDn-5m","SetUp-25m","SetDn-25m"),col=c("blue", "goldenrod1","darkgreen","firebrick2"),pch = c(2,2,16,16), cex=0.7,inset = 0)


dev.off()



# plot(1:nrow(gsaResAll_final),gsaResAll_final[,1],type = 'p',pch=2,col='blue',cex=1,
#      xlab = "",ylab="Adj-pVal",
#      ylim = c(0,1.3),
#      main = paste0("Adj-pVal ",ScaffoldName," MeasC/o:",toString(Meas_Cutoff)," ppC/o:",toString(PP_Cutoff)),axes = F)
# axis(1, at=seq(1,nrow(gsaResAll_final),by=1),labels=rownames(gsaResAll_final), las = 2,cex.axis=0.8)
# axis(2, at=seq(0,1.3,by=0.2),labels=seq(0,1.3,by=0.2), las = 2)
# points(1:nrow(gsaResAll_final),gsaResAll_final[,2],type = 'p',pch=2, col='darkgreen',cex=1)
# points(1:nrow(gsaResAll_final),gsaResAll_final[,3],type = 'p',pch=16, col='goldenrod1',cex=1)
# points(1:nrow(gsaResAll_final),gsaResAll_final[,4],type = 'p',pch=16, col='firebrick2',cex=1)
# legend("topright", legend=c("Up-5m","Dn-5m","Up-25m","Dn-225m"),col=c("blue", "darkgreen","goldenrod1","firebrick2"),pch = c(2,2,16,16), cex=0.7,inset = 0)



# --- End of script --- #
