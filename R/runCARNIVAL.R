#'\code{runCARNIVAL}
#'
#'Run CARNIVAL pipeline using to the provided list of inputs (including CARNIVAL built-in examples)
#'
#'@param InputFile The filename of input file for CARNIVAL with a fixed structure (default see: InputFile_CARNIVAL.txt)
#'
#'@return The networks and predicted node activities from the CARNIVAL pipeline in the destined result folder
#'
#'@import doParallel
#'@import readr
#'@import igraph
#'
#'@export

runCARNIVAL <- function(InputFile="InputFile_CARNIVAL.txt")
{
  # Clean working environment
  # rm(list=ls());cat("\014") # clean screen and variables
  # if(length(dev.list())>0){dev.off()} # clean figure

  # library(devtools);load_all()

  # Read-in the InputFile
  if (file.exists(InputFile)) {
    ReadInputs <- readLines(InputFile)
    CplexPath <- strsplit(x = ReadInputs[which(grepl(pattern = "CplexPath",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    netFile <- strsplit(x = ReadInputs[which(grepl(pattern = "netFile",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    measFile <- strsplit(x = ReadInputs[which(grepl(pattern = "measFile",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    inputFile <- strsplit(x = ReadInputs[which(grepl(pattern = "inputFile",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    weightFile <- strsplit(x = ReadInputs[which(grepl(pattern = "weightFile",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    CARNIVAL_example <- strsplit(x = ReadInputs[which(grepl(pattern = "CARNIVAL_example",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    if(!CARNIVAL_example=="NULL"){CARNIVAL_example=as.numeric(CARNIVAL_example)}else{CARNIVAL_example=NULL}
    Result_dir <- strsplit(x = ReadInputs[which(grepl(pattern = "Result_dir",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    if(Result_dir=="NULL"){Result_dir=NULL}
    inverseCR <- as.logical(strsplit(x = ReadInputs[which(grepl(pattern = "inverseCR",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    parallelCR <- as.logical(strsplit(x = ReadInputs[which(grepl(pattern = "parallelCR",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    nodeID <- strsplit(x = ReadInputs[which(grepl(pattern = "nodeID",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2]
    UP2GS <- as.logical(strsplit(x = ReadInputs[which(grepl(pattern = "UP2GS",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    DOTfig <- as.logical(strsplit(x = ReadInputs[which(grepl(pattern = "DOTfig",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    Export_all <- as.logical(strsplit(x = ReadInputs[which(grepl(pattern = "Export_all",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    timelimit <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "timelimit",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    mipGAP <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "mipGAP",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    poolrelGAP <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "poolrelGAP",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    limitPop <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "limitPop",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    poolCap <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "poolCap",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    poolIntensity <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "poolIntensity",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    poolReplace <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "poolReplace",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    alphaWeight <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "alphaWeight",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
    betaWeight <- as.numeric(strsplit(x = ReadInputs[which(grepl(pattern = "betaWeight",x = ReadInputs,fixed = T,ignore.case = F))],split = " = ",fixed = T)[[1]][2])
  } else {
    stop("The input file 'InputFile_CARNIVAL.txt' is missing. Please check the file and your working directory")
  }

  # Load necessary packages and functions
  library(CARNIVAL)
  library(readr)
  load(file = system.file("progenyMembers.RData",package="CARNIVAL"))

  # Assign parallelisation parameters
  if (parallelCR) {
    library(doParallel)
    argsJob=commandArgs(trailingOnly = TRUE)
    repIndex <- as.numeric(argsJob[1])
    condition <- as.character(argsJob[2]) #Can additionally be used to loop over conditions of interest
  } else {
    repIndex=1;condition=1
  }

  # Create a directory to store results
  current_dir <- getwd()
  if (is.null(Result_dir)) {
    dir_name <- paste("results_",Sys.time(),sep="")
  } else {
    dir_name <- Result_dir
  }
  dir.create(dir_name)

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
  measurements <- sign(measWeights) # Extracted sign of measurement for ILP fitting
  measWeights <- abs(measWeights) # Weights are all positives
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

  pknList <<- pknList

  variables <- writeLPFile(data=measurements,pknList=pknList,inputs=inputs,betaWeight=betaWeight,
                           scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,
                           poolCap=poolCap,poolIntensity=poolIntensity,poolReplace=poolReplace,
                           timelimit=timelimit,measWeights=measWeights,
                           repIndex=repIndex,condition = condition)
  Elapsed_1 <- proc.time() - ptm

  # Solve ILP problem with cplex, remove temp files, and return to the main directory
  ptm <- proc.time()
  print("Solving LP problem...")
  # system(paste0(getwd(), "/cplex -f cplexCommand_", condition,"_",repIndex,".txt"))
  system(paste0(CplexPath, "/cplex -f cplexCommand_", condition,"_",repIndex,".txt"))
  Elapsed_2 <- proc.time() - ptm

  # Move result files to result folder and remove redundant files after the optimisation
  if (file.exists(paste0("testFile_",condition,"_",repIndex,".lp"))) {file.remove(paste0("testFile_",condition,"_",repIndex,".lp"))} # might be useful for debugging

  if (file.exists(paste0("results_cplex_",condition, "_",repIndex,".txt"))) {file.copy(from = paste0("results_cplex_",condition,"_",repIndex,".txt"),to = paste0(current_dir,"/",dir_name,"/results_cplex_",condition,"_",repIndex,".txt")); file.remove(paste0("results_cplex_",condition,"_",repIndex,".txt"))}

  if (file.exists("cplex.log")) {file.copy(from = "cplex.log",to = paste0(current_dir,"/",dir_name,"/cplex_",condition,"_",repIndex,".log")); file.remove("cplex.log")}

  if (file.exists(paste0("cplexCommand_", condition,"_",repIndex,".txt"))) {file.remove(paste0("cplexCommand_", condition,"_",repIndex,".txt"))}
  AllFiles <- list.files()

  CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
  if (length(CloneFiles)>0) {
    for (counter in 1:length(CloneFiles)) {
      file.remove(AllFiles[CloneFiles[counter]])
    }
  }

  # Write result files in the results folder
  ptm <- proc.time()
  print("Writing result files...")
  # if (file.exists(paste0("results/",dir_name,"/results_cplex.txt"))) {
  if (file.exists(paste0(dir_name,"/results_cplex_",condition,"_",repIndex,".txt"))) {
    for(i in 1:length(variables)){
      res <- exportResult(cplexSolutionFileName = paste0(dir_name,"/results_cplex_",condition,"_",repIndex,".txt"),
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
      #                             inputs=inputs,measurements=measurements,UP2GS=UP2GS)} # Write figures for individual results
      save(res,file = paste0(dir_name,"/results_CARNIVAL.Rdata"))
    }
  } else {
    print("No result to be written")
  }
  Elapsed_3 <- proc.time() - ptm

  file.remove(paste0(dir_name,"/results_cplex_",condition,"_",repIndex,".txt")) # optional; remove cplex results (to save space)

  # Logged computational time
  ElapsedAll <- as.data.frame(matrix(t(c(Elapsed_1[3],Elapsed_2[3],Elapsed_3[3])),3,1))
  rownames(ElapsedAll) <- c("WriteConstraints:","CplexSolving:","ExportResults:")
  write.table(x = ElapsedAll,file = paste0(dir_name,"/elapsed_time.txt"),col.names = F,row.names = T,quote = F)

}
