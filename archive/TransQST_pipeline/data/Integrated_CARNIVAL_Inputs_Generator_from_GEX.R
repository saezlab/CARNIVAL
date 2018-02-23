# =================================================================== #
# Integrated pipeine for CARNIVAL measurement file generator from GEX #
# =================================================================== #

# Steps to proceed
# 1) Process raw array data (.CEL format) -> save to RData for further use
# 2) Calculate DoRothEA scores from processed array data
# 3) Calculate PROGENy scores from processed array data
# 4) Apply cutoff for DoRothEA and PROGENy scores and write measurement file

# Clear variables, screen and figures
rm(list=ls()); cat('\014'); if (length(dev.list()>0)) {dev.off()}

# ========================================== #
# ===== Step 1: Process raw array data ===== #
# ========================================== #

# Requirements: 
# ebits R-package: https://github.com/mschubert/ebits - see also suggested installation script
setwd('~/Desktop/RWTH_Aachen/GitHub/ebits/')
source("https://bioconductor.org/biocLite.R")
source("ebits_installation_script.R")

# Install necessary packages (if needed)
# install.packages('modules')
# biocLite("oligo")

# Load all necessary packages
library(modules)
library(oligo)
library(matrixStats)
library(io)
library(gplots)
library(RColorBrewer)

# Import functions from ebits and return working directory in WP5 folder
b = import('base')
io = import('io')
ma = import('process/microarray')
idmap = import('process/idmap')

setwd('~/Desktop/RWTH_Aachen/GitHub/CARNIVAL_TransQST/data')

# Assign directory of raw dataset
# APAP - Liver
dir = '~/Desktop/RWTH_Aachen/GitHub/Raw_Data/APAP/acetaminophen.Human.in_vitro.Liver/celfiles' # APAP In vitro Human

# List and read all cel files
celfiles = list.files(dir, pattern= '.CEL', full.names = TRUE)
rawData = read.celfiles(celfiles)

# Quality control, log2 normalisation, and probeset annotation
expr = ma$qc(rawData) %>%
  ma$normalize() %>%
  ma$annotate(summarize='hgnc_symbol')
# ma$annotate(summarize='entrezgene')

save(expr,file = "Expr_TG-GATEs_APAP_in_vitro_human.Rdata")

# ============================================= #
# ===== Step 2: Calculate DoRothEA scores ===== #
# ============================================= #

load(file = "Expr_TG-GATEs_APAP_in_vitro_human.Rdata")

# Load functions
source('lib_enrichment_scores.r')

# Load TF regulon genesets
load('CTFRs_v122016.rdata')

# Load expression matrix
load('Expr_TG-GATEs_APAP_in_vitro_human.Rdata') # APAP human in vitro (liver)

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
TF_activities = SLEA(E = E, genesets = CTFRs_genesets, method = 'VIPER')$NES # Worked -> get positive and negative values (min/max ~ -3/3, z-score?)
save(TF_activities,file = "TF_activities_VIPER_TG-GATEs_APAP_in_vitro_human.Rdata")


# ============================================ #
# ===== Step 3: Calculate PROGENy scores ===== #
# ============================================ #

source('calc_progeny_score.R')

# Assign names for mapping on Figure
Compound_Names <- c("APAP","CISP","AZA","CCl4","DEM","DFN","ETO","LPS","TNFa","TUNI")
Compound <- Compound_Names[1]
Rat  <- FALSE
InVivo <- FALSE
Repeat <- FALSE
scaling <- TRUE

load('Progeny_model.RData') # Load progeny model as "model"

if (!InVivo) {
  Time_Points <- c(2,8,24) # hrs
  if (!Rat) {
    PrefixLength <- 16 + nchar(Compound) # DRUG_in_vitro_human_
  } else {
    PrefixLength <- 14 + nchar(Compound) # DRUG_in_vitro_rat_
  }
} else {
  if (!Repeat) {
    Time_Points <- c(3,6,9,24) # hrs
    PrefixLength <- 20 + nchar(Compound) # DRUG_in_vivo_rat_single_
  } else {
    Time_Points <- c(4,8,15,29) # days
    PrefixLength <- 20 + nchar(Compound) # DRUG_in_vivo_rat_repeat_
  }
}

source('calc_progeny_score.R')

All_Progeny_Score <- NULL

for (counter in 1:length(Time_Points)) {
  if (!Repeat) {
    Current_TimePoints <- paste(toString(Time_Points[counter]),'h',sep="")
  } else {
    Current_TimePoints <- paste(toString(Time_Points[counter]),'d',sep="")
  }
  Current_Exps <- which(grepl(Current_TimePoints,colnames(expr)))
  Current_Expr <- expr[,Current_Exps]
  Current_Ctrl <- which(grepl("ctrl",colnames(Current_Expr),fixed=TRUE))
  
  Progeny_Score <- calc_progeny_score(Current_Expr,Current_Ctrl,model,scaling)
  # print(Progeny_Score)
  
  # Calculate the mean from biological relicates
  
  Mean_Progeny <- matrix(NA,4,dim(Progeny_Score)[2]) # 4 <- 1 ctrl + 3 doses (low, mid, high)
  colnames(Mean_Progeny) <- colnames(Progeny_Score)
  
  i <- 1
  j <- 1
  
  RowNamesCollection <- NULL
  while (i < dim(Progeny_Score)[1]+1) {
    NameCheck <- substr(rownames(Progeny_Score[i,]),1,nchar(rownames(Progeny_Score[i,]))-5)
    RepCheck  <- which(grepl(NameCheck,rownames(Progeny_Score),fixed=TRUE))
    MeanScore <- colMeans(Progeny_Score[RepCheck,])
    Mean_Progeny[j,] <- MeanScore
    NamePrint <- substr(NameCheck,PrefixLength+1,nchar(NameCheck)-4)
    RowNamesCollection <- c(RowNamesCollection,NamePrint)
    i <- i+length(RepCheck)
    j <- j+1
  }
  
  rownames(Mean_Progeny) <- RowNamesCollection
  
  Mean_Progeny[abs(Mean_Progeny)<1e-10] <- 0 # Push near zero value to absolute 0
  Mean_Progeny <- Mean_Progeny[-grep("ctrl",rownames(Mean_Progeny),fixed=TRUE),]
  
  All_Progeny_Score <- rbind(All_Progeny_Score,Mean_Progeny)
  # print(All_Progeny_Score)
  
}


# ========================================================= #
# ===== Step 4: Apply cutoff & write measurement file ===== #
# ========================================================= #

# Process data matrix

TF_activities <- t(TF_activities)
TF_act_ILP <- matrix(NA,nrow(TF_activities)/2,ncol(TF_activities))
RowNames_ILP <- NULL

for (counter in 1:(nrow(TF_activities)/2)) {
  TF_act_ILP[counter,] <- colMeans(TF_activities[(((counter-1)*2)+1):(((counter-1)*2)+2),])
  RowNames_ILP <- c(RowNames_ILP,substring(rownames(TF_activities)[((counter-1)*2)+1],21,nchar(rownames(TF_activities)[((counter-1)*2)+1])-9))
}
rownames(TF_act_ILP) <- RowNames_ILP; colnames(TF_act_ILP) <- colnames(TF_activities)

# ===== Z-score transformation ===== #

# DoRothEA scores
m = colMeans2(as.matrix(TF_act_ILP))
s = colSds(as.matrix(TF_act_ILP))
# l = lowess(m,s) # Additional step for data transformation to avoid SD=0 (returning NA after scaling)
TF_act_ILP_z_score = scale(as.matrix(TF_act_ILP), center = m, scale=s)

# PROGENy scores
m = colMeans2(as.matrix(All_Progeny_Score))
s = colSds(as.matrix(All_Progeny_Score))
# l = lowess(m,s) # Additional step for data transformation to avoid SD=0 (returning NA after scaling)
PW_act_ILP_z_score = scale(as.matrix(All_Progeny_Score), center = m, scale=s)


# ===== Select threshold and apply cutoff ===== #

# Extract TF activities in CR-pipeline ready format
ILP_z_score_cutoff <- 2 # Choose cut-off value for discretisation

DRT_Discretised <- TF_act_ILP_z_score
Idx_DRT_UP <- which(DRT_Discretised > ILP_z_score_cutoff,arr.ind = TRUE)
Idx_DRT_DN <- which(DRT_Discretised < (-1)*ILP_z_score_cutoff,arr.ind = TRUE)
Idx_DRT_BS <- which((DRT_Discretised <= ILP_z_score_cutoff) & (DRT_Discretised >= (-1)*ILP_z_score_cutoff),arr.ind = TRUE)
DRT_Discretised[Idx_DRT_UP] <- 1; DRT_Discretised[Idx_DRT_DN] <- -1; DRT_Discretised[Idx_DRT_BS] <- 0; 

PGN_Discretised <- PW_act_ILP_z_score
Idx_PGN_UP <- which(PGN_Discretised > ILP_z_score_cutoff,arr.ind = TRUE)
Idx_PGN_DN <- which(PGN_Discretised < (-1)*ILP_z_score_cutoff,arr.ind = TRUE)
Idx_PGN_BS <- which((PGN_Discretised <= ILP_z_score_cutoff) & (PGN_Discretised >= (-1)*ILP_z_score_cutoff),arr.ind = TRUE)
PGN_Discretised[Idx_PGN_UP] <- 1; PGN_Discretised[Idx_PGN_DN] <- -1; PGN_Discretised[Idx_PGN_BS] <- 0; 
colnames(PGN_Discretised) <- paste0("PRO_",colnames(PGN_Discretised))

# Condition to extract
SelectedCondition <- "low_2h"
# SelectedCondition <- "high_24h"

DRT_MeasIdx <- which(rownames(TF_act_ILP_z_score)==SelectedCondition)
PGN_MeasIdx <- which(rownames(PW_act_ILP_z_score)==SelectedCondition)

# Build progeny catalogue
ProgenyProtein <- read.table("PROGENy_Protein.csv",header = T,sep = ",",stringsAsFactors = F)
ProgenyProtein <- ProgenyProtein[order(ProgenyProtein[,2],ProgenyProtein[,3]),]
ProgenyProtein[,2] <- gsub("-",".",ProgenyProtein[,2],fixed=T)
ProgenyProtein_List <- vector(mode="list",length=length((unique(ProgenyProtein[,2]))))
for (counter in 1:length(unique(ProgenyProtein[,2]))) {
  ProgenyProtein_List[[counter]]$pw <- unique(ProgenyProtein[,2])[counter]
  Idx_pw_proteins <- which(unique(ProgenyProtein[,2])[counter]==ProgenyProtein[,2])
  ProgenyProtein_List[[counter]]$prot <- ProgenyProtein[Idx_pw_proteins,3]
}

DRT_Meas <- DRT_Discretised[DRT_MeasIdx,]; DRT_Meas <- DRT_Meas[-which(DRT_Meas==0)]
PGN_Meas <- PGN_Discretised[PGN_MeasIdx,]; PGN_Meas <- PGN_Meas[-which(PGN_Meas==0)]

PGN_Meas_ToAdd <- NULL
if (length(PGN_Meas)>0) {
  for (counter in 1:length(PGN_Meas)) { 
    Idx_ProgenyList <- which(names(PGN_Meas)[counter]==unique(ProgenyProtein[,2]))
    Matrix_ToAdd <- rep(NA,length(ProgenyProtein_List[[Idx_ProgenyList]]$prot))
    Matrix_ToAdd <- rep(PGN_Meas[counter],length(ProgenyProtein_List[[Idx_ProgenyList]]$prot))
    names(Matrix_ToAdd) <- ProgenyProtein_List[[Idx_ProgenyList]]$prot
    PGN_Meas_ToAdd <- c(PGN_Meas_ToAdd,Matrix_ToAdd)
  }
}

ILP_combined_meas <- t(as.matrix(c(DRT_Meas,PGN_Meas_ToAdd)))

write.table(ILP_combined_meas,file = paste0("ILP_Meas_",SelectedCondition,"_CutOff_",toString(ILP_z_score_cutoff),".tsv"),quote = F,sep = "\t",col.names = T)

# --- End of the script --- #
