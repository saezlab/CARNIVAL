# TG-GATEs data processing with ebits and calculation of Progeny score

# Requirements: 
# 1. ebits R-package: https://github.com/mschubert/ebits - see also suggested installation script
# 2. Progeny model matrix (.Rdata)

# Clear variables, screen and figures
rm(list=ls()); cat('\014'); if (length(dev.list()>0)) {dev.off()}

# Install necessary packages (if needed)
# install.packages('modules')
# source("https://bioconductor.org/biocLite.R")
# biocLite("oligo")

# Load all necessary packages
library(modules)
library(oligo)
library(matrixStats)
library(gplots)
library(RColorBrewer)

# Import functions from ebits and return working directory in WP5 folder
setwd('~/Desktop/RWTH_Aachen/GitHub/ebits/')
b = import('base')
io = import('io')
ma = import('process/microarray')
idmap = import('process/idmap')
setwd('~/Desktop/RWTH_Aachen/GitHub/QSP/Panuwat/WP5/Progeny_on_TG-GATEs/')

# Assign directory of raw dataset

# APAP - Liver
# dir = 'Raw_Data/APAP/acetaminophen.Human.in_vitro.Liver/celfiles/' # APAP In vitro Human
# dir = 'Raw_Data/APAP/acetaminophen.Rat.in_vitro.Liver/celfiles/' # APAP In vitro Rat
# dir = 'Raw_Data/APAP/acetaminophen.Rat.in_vivo.Liver.Single/celfiles/' # APAP In vivo Rat single
# dir = 'Raw_Data/APAP/acetaminophen.Rat.in_vivo.Liver.Repeat/celfiles/' # APAP In vivo Rat repeat

# Cisplatin - Kidney

# dir = 'Raw_Data/Cisplatin/cisplatin.Rat.in_vitro.Liver/celfiles/' # Cisplatin In vitro Rat
# dir = 'Raw_Data/Cisplatin/cisplatin.Rat.in_vivo.Kidney.Single/celfiles/' # Cisplatin In vivo Rat single
# dir = 'Raw_Data/Cisplatin/cisplatin.Rat.in_vivo.Kidney.Repeat/celfiles/' # APAP In vivo Rat repeat

# Additional compounds in Liver (In vitro rat)

# dir = 'Raw_Data/Azathioprine/azathioprine.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/CCl4/carbon_tetrachloride.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/DEM/diethyl_maleate.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/Diclofenac/diclofenac.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/Etoposide/etoposide.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/LPS/LPS.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/TNFa/TNFa.Rat.in_vitro.Liver/celfiles/'
# dir = 'Raw_Data/Tunicamycin/tunicamycin.Rat.in_vitro.Liver/celfiles/'


# List and read all cel files
celfiles = list.files(dir, pattern= '.CEL', full.names = TRUE)
rawData = read.celfiles(celfiles)

# Quality control, log2 normalisation, and probeset annotation
expr = ma$qc(rawData) %>%
  ma$normalize() %>%
  ma$annotate(summarize='hgnc_symbol')
  # ma$annotate(summarize='entrezgene')

# save(expr,file = "Expr_TG-GATEs_APAP_in_vitro_human.Rdata")
# save(expr,file = "Expr_TG-GATEs_APAP_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_APAP_in_vivo_rat_single.Rdata")
# save(expr,file = "Expr_TG-GATEs_APAP_in_vivo_rat_repeat.Rdata")

# save(expr,file = "Expr_TG-GATEs_CISP_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_CISP_in_vivo_rat_single.Rdata")
# save(expr,file = "Expr_TG-GATEs_CISP_in_vivo_rat_repeat.Rdata")

# save(expr,file = "Expr_TG-GATEs_AZA_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_CCl4_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_DEM_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_DFN_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_ETO_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_LPS_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_TNFa_in_vitro_rat.Rdata")
# save(expr,file = "Expr_TG-GATEs_TUNI_in_vitro_rat.Rdata")


# ============================================================= #
# ============================================================= #
# ============================================================= #


# Clear variables, screen and figures
rm(list=ls()); cat('\014'); if (length(dev.list()>0)) {dev.off()}

# setwd('~/Desktop/RWTH_Aachen/GitHub/QSP/Panuwat/WP5/Progeny_on_TG-GATEs/')

# Load the existing expression gene set data

# APAP - Liver
load("Expr_TG-GATEs_APAP_in_vitro_human.Rdata")
# load("Expr_TG-GATEs_APAP_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_APAP_in_vivo_rat_single.Rdata")
# load("Expr_TG-GATEs_APAP_in_vivo_rat_repeat.Rdata")

# Cisplatin - Kidney
# load("Expr_TG-GATEs_CISP_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_CISP_in_vivo_rat_single.Rdata")
# load("Expr_TG-GATEs_CISP_in_vivo_rat_repeat.Rdata")

# Other compounds - in vitro Rat
# load("Expr_TG-GATEs_AZA_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_CCl4_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_DEM_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_DFN_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_ETO_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_LPS_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_TNFa_in_vitro_rat.Rdata")
# load("Expr_TG-GATEs_TUNI_in_vitro_rat.Rdata")

# Assign names for mapping on Figure
Compound_Names <- c("APAP","CISP","AZA","CCl4","DEM","DFN","ETO","LPS","TNFa","TUNI")
Compound <- Compound_Names[1]
Rat  <- FALSE
InVivo <- FALSE
Repeat <- FALSE
scaling <- TRUE

library(matrixStats)
library(gplots)
library(RColorBrewer)

# Assign variable for expression gene set (eset) and expression matrix (expr)
eset <- expr
expr <- exprs(eset)

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


# ====================================== #
# Load script to calculate progeny score
# & Perform calculation for each time-point
# ====================================== #

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


# ====================================== #
# Scaling of PROGENY score
# ====================================== #

# Method 1: Scaling based on the max value either from positive/negative side
for (counter in 1:dim(All_Progeny_Score)[2]) {
  MaxVal <- max(abs(All_Progeny_Score[,counter]))
  All_Progeny_Score[,counter] <- All_Progeny_Score[,counter]/MaxVal
}

# Method 2: Scaling based on z-score
for (counter in 1:dim(All_Progeny_Score)[2]) {
  m_pgn <- mean((All_Progeny_Score[,counter]))
  s_pgn <- sd((All_Progeny_Score[,counter]))
  All_Progeny_Score[,counter] <- scale(All_Progeny_Score[,counter],center = m_pgn,scale = s_pgn)
}

# ====================================== #
# Save results into file
# ====================================== #

SDthresh <- 2

which(All_Progeny_Score<SDthresh, arr.ind = T)



# ============================================================= #
# ============================================================= #
# ============================================================= #


# ===== End of the script ===== #

