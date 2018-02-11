# ---------------------- #
# CARNIVAL driver script #
# ---------------------- #

# Integrated pipeline for validation dataset (E-MTAB-2091)

setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/")

rm(list=ls()) # clear environment
cat("\014") # clear screen

Compounds <- t(read.table(file = "resources/All_Compound_Names.tsv",sep = "\r"))
ScaffoldNet <- c(1,2,3) # c(1,2,3,4) # Model2 [Signor] has an issue with writeSIF and Model3 [Babur] is too large to write constraints
# ScaffoldNet <- c(1,3) # c(1,2,3,4) # Model2 [Signor] has an issue with writeSIF and Model3 [Babur] is too large to write constraints
# ScaffoldNet <- 2 # c(1,2,3,4) # Model2 [Signor] has an issue with writeSIF and Model3 [Babur] is too large to write constraints
ScaffoldName <- c("omnipath","generic","signor")
InputType <- 2 # [1,2] 1 = Only Direct targets; 2 = Direct targets plus STRING
Export_all <- 0 # c(0,1) export all ILP variables or not; if 0, only cplex results, predicted node values and sif file will be written

for (counter_compound in 1:length(Compounds)) {
# for (counter_compound in 9:length(Compounds)) {
# for (counter_compound in 7:8) {
    
  for (counter_network in 1:length(ScaffoldNet)) {
    
    print("")
    print("=========================================")
    print(paste0("Optimising compound Nr ", toString(counter_compound),"/",toString(length(Compounds))," : " ,Compounds[counter_compound]," - Using network: ",ScaffoldName[ScaffoldNet[counter_network]]))
    print("=========================================")
    print("")
    
    Network <- ScaffoldNet[counter_network]
    NetName <- ScaffoldName[ScaffoldNet[counter_network]]    
    
    Result_dir <- paste0(Compounds[counter_compound],"_",NetName) # specify a name for result directory; if NULL, then date and time will be used by default
    
    # ============================== #
    
    # Load necessary packages and functions
    library(readr)
    library(tidyr)
    library(XML)
    setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/"); source("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/src/CRILPR_Functions.R")
    setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/archive/CARNIVAL_Validation/E-MTAB-2091/integrated_pipeline/")
    
    # Create a directory to store results
    current_dir <- getwd()
    dir.create("results",showWarnings = FALSE)
    setwd(paste(current_dir,"/results",sep=""))
    if (is.null(Result_dir)) {
      dir_name <- paste("results_",Sys.time(),sep="")
    } else {
      if (InputType==1) {
        dir_name <- paste0("validation_",Result_dir)
      } else if (InputType==2) {
        dir_name <- paste0("validation_",Result_dir,"_STITCH_input")
      }
    }
    dir.create(dir_name); setwd(current_dir)
    
    # Load ILP inputs
    if (Network==1) {
      network      <- read.table("resources/OmniPathSIF_NoHyphen.tsv", sep = "\t", header = FALSE)
    } else if (Network==2) {
      network      <- read.table("resources/Network_Generic_FIN_RemovedTFGenes_NoHyphen.sif", sep = "\t", header = FALSE)
    } else if (Network==3) {
      network      <- read.delim("resources/SignorSIF_NoHyphenNoSlashNoColonNoSpace.tsv", sep = "\t", header = FALSE)
    } else {
      stop("Please choose the provided model scaffolds")
    }
    if (InputType==1) {
      inputs       <- try(read.table(paste0("inputs/Inputs_Main_",Compounds[counter_compound],".tsv"), sep="\t", header = TRUE))
      if(inherits(inputs, "try-error")) next
    } else if (InputType==2) {
      inputs       <- try(read.table(paste0("inputs/Inputs_Main_STITCH_",Compounds[counter_compound],"_CutOff_800.tsv"), sep="\t", header = TRUE))
      if(inherits(inputs, "try-error")) next
    }
    
    measurements <- try(read.table(paste0("measurements/DRT_MeanSDCutOff_2_PGN_MeanSDCutOff_2/Meas_DRT_PGN_",Compounds[counter_compound],".tsv"), sep="\t", header=TRUE))
    # measurements <- try(read.table(paste0("measurements/DRT_MeanSDCutOff_1.5_PGN_MeanSDCutOff_1.5/Meas_DRT_PGN_",Compounds[counter_compound],".tsv"), sep="\t", header=TRUE))
    # measurements <- try(read.table(paste0("measurements/DRT_MeanSDCutOff_1_PGN_MeanSDCutOff_1/Meas_DRT_PGN_",Compounds[counter_compound],".tsv"), sep="\t", header=TRUE))
    if(inherits(measurements, "try-error")) next
    
    # Input processing
    data <- measurements
    pknList <- as.data.frame(network)
    colnames(pknList) <- c("Node1", "Sign", "Node2")
    setwd("~/Desktop/RWTH_Aachen/GitHub/CARNIVAL/src/") # temporary shift to src directory
    
    # Write constraints as ILP inputs
    print("Writing constraints...")
    ptm <- proc.time()
    variables <- try(writeLPFile(data,pknList,inputs,0.1))
    if(inherits(variables, "try-error")) next
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
    if (file.exists(paste("results/",dir_name,"/results_cplex.txt",sep=""))) {
      for(i in 1:length(variables)){
        try(sif <- readOutResult(cplexSolutionFileName = paste("results/",dir_name,"/results_cplex.txt",sep=""), variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name, Export_all = Export_all,inputs=inputs,measurements=measurements))
        if(inherits(variables, "try-error")) next
      }
    } else {
      print("No result to be written")
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
