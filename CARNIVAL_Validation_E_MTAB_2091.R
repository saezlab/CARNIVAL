# ---------------------- #
# CARNIVAL driver script #
# ---------------------- #

# Systematic pipeline for validation dataset (E-MTAB-2091)

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL")
rm(list=ls()) # clear environment
cat("\014") # clear screen

Compounds <- t(read.table(file = "~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/data/All_Compound_Names.tsv",sep = "\r"))
ScaffoldNet <- c(1,4) # c(1,2,3,4) # Model2 [Signor] has an issue with writeSIF and Model3 [Babur] is too large to write constraints
ScaffoldName <- c("omnipath","signor","babur","generic")
Export_all <- 0 # c(0,1) export all ILP variables or not; if 0, only cplex results, predicted node values and sif file will be written

for (counter_compound in 1:length(Compounds)) {

  for (counter_network in 1:length(ScaffoldNet)) {
    
    Network <- ScaffoldNet[counter_network]
    NetName <- ScaffoldName[ScaffoldNet[counter_network]]    
    
    Result_dir <- paste0(Compounds[counter_compound],"_",NetName) # specify a name for result directory; if NULL, then date and time will be used by default
    
    # ============================== #
    
    # Load necessary packages and functions
    library(readr)
    library(tidyr)
    library(XML)
    source("src/CRILPR_Functions.R")
    
    # Create a directory to store results
    current_dir <- getwd()
    dir.create("results",showWarnings = FALSE)
    setwd(paste(current_dir,"/results",sep=""))
    if (is.null(Result_dir)) {
      dir_name <- paste("results_",Sys.time(),sep="")
    } else {
      dir_name <- paste0("validation_",Result_dir)
    }
    dir.create(dir_name); setwd(current_dir)
    
    # Load ILP inputs
    if (Network==1) {
      network      <- read.table("archive/CARNIVAL_Validation/E-MTAB-2091/network/OmniPathSIF_NoHyphen.tsv", sep = "\t", header = FALSE)
    } else if (Network==2) {
      network      <- read.delim("archive/CARNIVAL_Validation/E-MTAB-2091/network/SignorSIF_NoHyphenNoSlashNoColon.tsv", sep = "\t", header = FALSE)
    } else if (Network==3) {
      network      <- read.table("archive/CARNIVAL_Validation/E-MTAB-2091/network/BaburSIF_NoHyphen.tsv", sep = "\t", header = FALSE)
    } else if (Network==4) {
      network      <- read.table("archive/CARNIVAL_Validation/E-MTAB-2091/network/Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif", sep = "\t", header = FALSE)
    } else {
      stop("Please choose the provided model scaffolds")
    }
    inputs       <- read.table(paste0("archive/CARNIVAL_Validation/E-MTAB-2091/annotation/DrugTarget_",Compounds[counter_compound],"_plusPROGENy_CutOff_50.tsv"), sep="\t", header = TRUE)
    measurements <- read_delim(paste0("archive/CARNIVAL_Validation/E-MTAB-2091/data/Dorothea_",Compounds[counter_compound],"_Cutoff_1.tsv"), "\t", escape_double = FALSE, trim_ws = TRUE)
    
    # Input processing
    data <- measurements
    pknList <- as.data.frame(network)
    colnames(pknList) <- c("Node1", "Sign", "Node2")
    setwd(paste0(current_dir,"/src/")) # temporary shift to src directory
    
    # Write constraints as ILP inputs
    ptm <- proc.time()
    variables <- writeLPFile(data,pknList,inputs,0.1)
    Elapsed_1 <- proc.time() - ptm
    
    # Solve ILP problem with cplex, remove temp files, and return to the main directory
    ptm <- proc.time()
    system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
    Elapsed_2 <- proc.time() - ptm
    # file.remove("testFile.lp")
    file.remove("cplex.log")
    file.copy(from = "results_cplex.txt",to = paste(current_dir,"/results/",dir_name,"/results_cplex.txt",sep=""))
    file.remove("results_cplex.txt")
    setwd(current_dir)
    
    # Write result files
    ptm <- proc.time()
    for(i in 1:length(variables)){
      sif <- readOutResult(cplexSolutionFileName = paste("results/",dir_name,"/results_cplex.txt",sep=""), variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name, Export_all = Export_all)
    }
    Elapsed_3 <- proc.time() - ptm
    
    # Logged computational time
    ElapsedAll <- as.data.frame(matrix(t(c(Elapsed_1[3],Elapsed_2[3],Elapsed_3[3])),3,1))
    rownames(ElapsedAll) <- c("WriteConstraints:","CplexSolving:","ExportResults:")
    write.table(x = ElapsedAll,file = paste("results/",dir_name,"/elapsed_time.txt",sep=""),col.names = F,row.names = T,quote = F)
  }
}

# --- End of script --- #

# [Debug/QC mode] Print variable names
# variables$Condition_1$exp

# --- End of debugging part --- #
