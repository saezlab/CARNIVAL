# ----------------------------------------------------- #
# CARNIVAL driver script - Parallelised version example #
# ----------------------------------------------------- #

# set working directory and clear working environment
# setwd("/CARNIVAL")
rm(list=ls());cat("\014");if(length(dev.list())>0){dev.off()}

library(devtools)
load_all() # load CARNIVAL package

# Choose from our CARNIVAL examples (Ex1 = Toy model; Ex2 = TG-GATEs-APAP; Ex3 = SBVimprover-EGF)
CARNIVAL_example <- 3 # c(1,2,3); if not, set to 'NULL'

# Or assign your input files here
netFile <-  "your_network_file.sif" # required
measFile <- "your_measurement_file.txt" # required
inputFile <- "your_input_target_file.txt" # optional; if not, set to 'NULL'
weightFile <-  "your_node_weight_file.txt" # optional; if not, set to 'NULL'

# Choose CARNIVAL settings
# Result_dir <- NULL # <<< !!! To be set after calling parallised parameters !!! -- See Line 55 >>>
Sys.setenv(TZ="Europe/Berlin") # optional; set time zone to as default results' foldername
parallelCR <- T # running parallelised version?
inverseCR <- F # running inverse causal reasoning version?
nodeID <- "uniprot" # specify whether the nodes are in 'uniprot' or 'gene' ID format

# Plotting-related setting (doesn't affect the optimisation process)
UP2GS <- T # convert UniProtIDs to Gene Symbols in the plotting step?
DOTfig <- T #  write DOT figure? (can be visualised by e.g. GraphViz)
Export_all <- F #  export all ILP variables or not; if F, only predicted node values and sif file will be written

# Set CPLEX stopping criteria
mipGAP        <- 0.05 # in proportion to the best estimated integer solution
poolrelGAP    <- 0.0001 # in relative to the pool of best solution
limitPop      <- 500 # limit the number of populated solutions after identified best solution
poolCap       <- 100 # limit the pool size to store populated solution
poolIntensity <- 4 # (for populating) select search intensity [0 default/ 1 to 4]
poolReplace   <- 2 # select replacement strategy of the pool solution [0 default/ 1 to 2]
alphaWeight   <- 1 # constant coefficient for fitting error in the objective function in case TF activities are not assigned [default 1]
betaWeight    <- 0.2 # relative coefficient of model size to fitting error in the objective function [default 0.2]
timelimit     <- 300 # set time limit for cplex optimisation (in seconds)

# --- NOTE: PLEASE MODIFY YOUR CLUSTER PARAMETERS HERE --- #

# Assign parallelisation parameters
if (parallelCR) {
  library(doParallel)
  argsJob=commandArgs(trailingOnly = TRUE)
  repIndex <- as.numeric(argsJob[1])
  condition <- as.character(argsJob[2]) # can additionally be used to loop over conditions of interest
} else {
  repIndex=1;condition=1
}

betaWeight <- as.numeric(condition)
Result_dir <- paste0("CARNIVAL_results_beta_",betaWeight,"_Rep_",repIndex) # specify a name for result directory; if NULL, then date and time will be used by default

# -------------------------------------------------------- #

# ================================================= #
# ===== Press Run to perform the optimisation ===== #
# ================================================= #

# Crosscheck if at least repIndex (cluster parameter) was assigned
if (is.na(repIndex)) {
  stop(paste0('The cluster parameter [repIndex] was not properly assigned - please check your setting'))
} else {

  # Load necessary packages and functions
  library(readr)
  library(tidyr)
  library(igraph)
  load(file = system.file("progenyMembers.RData",package="CARNIVAL"))

  # Create a directory to store results
  current_dir <- getwd()
  dir.create("results",showWarnings = FALSE)
  setwd(paste(current_dir,"/results",sep=""))
  if (is.null(Result_dir)) {
    dir_name <- paste("results_",Sys.time(),sep="")
  } else {
    dir_name <- Result_dir
  }
  dir.create(dir_name); setwd(current_dir)

  # Load CARNIVAL example files (if defined)
  if (!is.null(CARNIVAL_example)) {
    loaded_CARNIVAL <- load_CARNIVAL_examples(CARNIVAL_example)
    netFile <- loaded_CARNIVAL$netFile
    measFile <- loaded_CARNIVAL$measFile
    inputFile <- loaded_CARNIVAL$inputFile
    weightFile <- loaded_CARNIVAL$weightFile
  }

  # Input processing
  network <- read.table(file = netFile, sep = "\t", header = TRUE)
  measWeights <- as.matrix(read_delim(file = measFile, delim = "\t", escape_double = FALSE, trim_ws = TRUE))
  measWeights <- abs(measWeights) # Weights are all positives
  measurements <- sign(measWeights) # Extracted sign of measurement for ILP fitting
  if (!is.null(inputFile)) {
    inputs <- read.table(file = inputFile, sep = "\t", header = TRUE)
  }
  if (!is.null(weightFile)) {
    edgeWeights <- read_delim(file = weightFile, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    scores <- assignPROGENyScores(progeny = edgeWeights, progenyMembers = progenyMembers, id = nodeID)
  } else {
    scores <- NULL
  }

  # Adding perturbation node for the case of inverse causal reasoning
  if (inverseCR) {
    MappedPertNode <- AddPerturbationNode(network)
    inputs <- MappedPertNode$inputs
    network <- MappedPertNode$network
  }

  data <- measurements
  pknList <- as.data.frame(network)
  colnames(pknList) <- c("Node1", "Sign", "Node2")
  setwd(paste0(current_dir,"/R/")) # temporary shift to src directory

  # Remove intermediate cplex files (if any)
  AllFiles <- list.files()
  CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
  if (length(CloneFiles)>0) {
    for (counter in 1:length(CloneFiles)) {
      file.remove(AllFiles[CloneFiles[counter]])
    }
  }

  # Remove redundant files prior to optimisation
  if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
  if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))}
  if (file.exists("cplex.log")) {file.remove("cplex.log")}
  if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}

  # Write constraints as ILP inputs
  ptm <- proc.time()
  print("Writing constraints...")
  variables <- writeLPFile(data=measurements,pknList=pknList,inputs=inputs,betaWeight=betaWeight,
                           scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,
                           poolCap=poolCap,poolIntensity=poolIntensity,poolReplace=poolReplace,
                           timelimit=timelimit,measWeights=measWeights,
                           repIndex=repIndex,condition = condition)
  Elapsed_1 <- proc.time() - ptm

  # Solve ILP problem with cplex, remove temp files, and return to the main directory
  ptm <- proc.time()
  print("Solving LP problem...")
  system(paste0(getwd(), "/cplex -f cplexCommand_", condition,"_",repIndex,".txt"))
  Elapsed_2 <- proc.time() - ptm

  # Move result files to result folder and remove redundant files after the optimisation
  if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging
  if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.copy(from = paste0("results_cplex_",condition,"_",repIndex,".txt"),to = paste(current_dir,"/results/",dir_name,"/results_cplex.txt",sep="")); file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))}
  if (file.exists("cplex.log")) {file.copy(from = "cplex.log",to = paste0(current_dir,"/results/",dir_name,"/cplex_",condition,"_",repIndex,".log")); file.remove("cplex.log")}
  if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
  setwd(current_dir)

  # Write result files in the results folder
  ptm <- proc.time()
  print("Writing result files...")
  if (file.exists(paste0("results/",dir_name,"/results_cplex.txt"))) {
    for(i in 1:length(variables)){
      res <- exportResult(cplexSolutionFileName = paste0("results/",dir_name,"/results_cplex.txt"),
                          variables = variables, pknList = pknList, conditionIDX = i,
                          dir_name = dir_name, inputs=inputs,measurements=measurements,
                          Export_all = Export_all,writeIndividualResults = T)
      # res <- files2res(counterlist) # retrieve results from previously generated result files
    }
    if (!is.null(res)) {
      if (UP2GS) {res <- Uniprot2GeneSymbol(res)}
      if (DOTfig) {WriteDOTfig(res=res,dir_name=dir_name,
                               inputs=inputs,measurements=measurements,UP2GS=UP2GS)}
      # if (DOTfig) {WriteDOTfig(res=res,idxModel=c(1,2),dir_name=dir_name,
      #                             inputs=inputs,measurements=measurements,UP2GS=UP2GS)}
      save(res,file = paste0("results/",dir_name,"/results_CARNIVAL.Rdata"))
    }
  } else {
    print("No result to be written")
  }
  Elapsed_3 <- proc.time() - ptm

  file.remove(paste0("results/",dir_name,"/results_cplex.txt")) # optional; remove cplex results (to save space)

  # Logged computational time
  ElapsedAll <- as.data.frame(matrix(t(c(Elapsed_1[3],Elapsed_2[3],Elapsed_3[3])),3,1))
  rownames(ElapsedAll) <- c("WriteConstraints:","CplexSolving:","ExportResults:")
  write.table(x = ElapsedAll,file = paste0("results/",dir_name,"/elapsed_time.txt"),col.names = F,row.names = T,quote = F)

}

# --- End of script --- #
