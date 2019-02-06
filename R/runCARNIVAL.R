#'\code{runCARNIVAL}
#'
#'Run CARNIVAL pipeline using to the user-provided list of inputs or run CARNIVAL built-in examples
#'Note: The pipeline requires either all required user-defined input variables (netFile and measFile) are set to NULL or CARNIVAL_example is set to NULL to execute
#'
#'@param CplexPath Path to executable cplex file - always required
#'@param netFile Filename of the prior knowledge network - always required or set as NULL to run CARNIVAL built-in example
#'@param measFile Filename of the measurement file (here DoRothEA normalised enrichment scores) - always required or set as NULL to run CARNIVAL built-in example
#'@param inputFile Filename of the list for target of perturbation - optional or set as NULL to run CARNIVAL built-in example
#'@param weightFile Filename of the additional weight (here PROGENy pathway score) - optional or set as NULL to run CARNIVAL built-in example
#'@param CARNIVAL_example Number of built-in CARNIVAL example (1=Toy Model,2=SBVimprove-EGF,3=TG-GATEs-APAP) or set as NULL to use user-defined input files
#'@param Result_dir Specify directory name to store results
#'@param inverseCR Execute the inverse CARNIVAL pipeline (logical T/F)
#'@param parallelCR Execute the parallelised version of CARNIVAL pipeline (logical T/F)
#'@param nodeID Define the input format of nodes in the network (either 'uniprot' or 'gene' symbol)
#'@param UP2GS For plotting: define if Uniprot ID will be converted to gene symbols for better readability (logical T/F)
#'@param DOTfig For plotting: define if DOT figure will be exported in the result folder (logical T/F)
#'@param Export_all Define if all CPLEX variables results will be exported into files (logical T/F) - only recommended for debugging
#'@param timelimit CPLEX parameter: Time limit of CPLEX optimisation (in seconds)
#'@param mipGAP CPLEX parameter: Allowed gap of accepted solution comparing to the best solution (fraction; default: 0.05 = 5 percents)
#'@param poolrelGAP CPLEX parameter: Allowed relative gap of accepted solution comparing within the pool of accepted solution (fraction; default: 0.0001)
#'@param limitPop CPLEX parameter: Allowed number of solutions to be generated (default: 500)
#'@param poolCap CPLEX parameter: Allowed number of solution to be kept in the pool of solution (default: 100)
#'@param poolIntensity CPLEX parameter: Intensity of solution searching (0,1,2,3,4 - default: 4)
#'@param poolReplace CPLEX parameter: Replacement strategy of solutions in the pool (0,1,2 - default: 2 = most diversified solutions)
#'@param alphaWeight Objective function: weight for mismatch penalty (default: 1 - will only be applied once measurement file only contains discrete values)
#'@param betaWeight Objective function: weight for node penalty (defaul: 0.2)
#'
#'@return The networks and predicted node activities from the CARNIVAL pipeline in the destined result folder
#'
#'@import doParallel
#'@import igraph
#'
#'@export

runCARNIVAL <- function(CplexPath=NULL,
                        netFile=NULL,
                        measFile=NULL,
                        inputFile=NULL,
                        weightFile=NULL,
                        CARNIVAL_example=2,
                        Result_dir="Results_CARNIVAL",
                        inverseCR=F,
                        parallelCR=F,
                        nodeID="uniprot",
                        UP2GS=T,
                        DOTfig=T,
                        Export_all=F,
                        timelimit=600,
                        mipGAP=0.05,
                        poolrelGAP=0.0001,
                        limitPop=500,
                        poolCap=100,
                        poolIntensity=4,
                        poolReplace=2,
                        alphaWeight=1,
                        betaWeight=0.2)
{

  # Clean working environment
  # rm(list=ls());cat("\014") # clean screen and variables
  # if(length(dev.list())>0){dev.off()} # clean figure

  # library(devtools);load_all()

  # QC step of provided inputs
  if (!file.exists(CplexPath)) {stop("Please provide a valid path to interactive cplex.")}
  if (!is.null(netFile)) {if (!file.exists(netFile)) {stop("Please provide a valid network filename or leave it as NULL to run CARNIVAL examples.")}}
  if (!is.null(measFile)) {if (!file.exists(measFile)) {stop("Please provide a valid measurement filename or leave it as NULL to run CARNIVAL examples.")}}
  if (!is.null(inputFile)) {if (!file.exists(inputFile)) {stop("Please provide a valid target-input filename or leave it as NULL to run CARNIVAL examples.")}}
  if (!is.null(weightFile)) {if (!file.exists(weightFile)) {stop("Please provide a valid pathway-weight filename or leave it as NULL to run CARNIVAL examples.")}}
  if (!is.null(CARNIVAL_example)) {if (!is.numeric(CARNIVAL_example)) {stop("Please choose a valid CARNIVAL example or provide user-defined CARNIVAL inputs")}}
  if ((!is.null(netFile) | !is.null(measFile)) & is.numeric(CARNIVAL_example)) {stop("Required users-defined input files were provided & a CARNIVAL example was selected: Please either set all required user-defined input filenames to NULL or set CARNIVAL_example to NULL")}
  if (!is.character(Result_dir) & !is.null(Result_dir)) {stop("Please assign a directory name or leave it as NULL to use default name")}
  if (!is.logical(inverseCR)) {stop("Please choose the pipeline (Standard vs Inverse CARNIVAL) with a logical value T/F")}
  if (!is.logical(parallelCR)) {stop("Please choose whether to apply a parallelised pipeline with a logical value T/F")}
  if (!(nodeID %in% c('uniprot','gene'))) {stop("Please define the input format of node either 'uniprot' or 'gene' symbol")}
  if (!is.logical(UP2GS)) {stop("For plotting: please choose whether UniprotID to be converted to gene symbol for a better readability with a logical value T/F")}
  if (!is.logical(DOTfig)) {stop("For plotting: please choose whether to plot DOT figure as an output with a logical value T/F")}
  if (!is.logical(Export_all)) {stop("Please choose whether to export all variables names with a logical value T/F (only recommended for debugging)")}
  if (!is.numeric(timelimit)) {stop("CPLEX parameter: Please set a time limit for CPLEX optimisation in seconds")}
  if (!is.null(mipGAP)) {if (!is.numeric(mipGAP)) {stop("CPLEX parameter: Please set the allowed mipGAP parameter or leave it as NULL for CPLEX default value (1e-04)")}}; if (is.null(mipGAP)) {mipGAP=1e-04}
  if (!is.null(poolrelGAP)) {if (!is.numeric(poolrelGAP)) {stop("CPLEX parameter: Please set the allowed pool relative GAP parameter or leave it as NULL for CPLEX default value (1e75)")}}; if (is.null(poolrelGAP)) {poolrelGAP=1e75}
  if (!is.null(limitPop)) {if (!is.numeric(limitPop)) {stop("CPLEX parameter: Please set the allowed population limit of solution to be generated or leave it as NULL for CPLEX default value (20)")}}; if (is.null(limitPop)) {limitPop=20}
  if (!is.null(poolCap)) {if (!is.numeric(poolCap)) {stop("CPLEX parameter: Please set the allowed number of solutions to be kept or leave it as NULL for CPLEX default value (2.1e9)")}}; if (is.null(poolCap)) {poolCap=2.1e9}
  if (!is.null(poolIntensity)) {if (!(poolIntensity %in% c(0,1,2,3,4))) {stop("CPLEX parameter: Please set the level of intensity for solution searching [0,1,2,3,4] or leave it as NULL for CPLEX default value (0) - to be decided by CPLEX")}}; if (is.null(poolIntensity)) {poolIntensity=0}
  if (!is.null(poolReplace)) {if (!(poolReplace %in% c(0,1,2))) {stop("CPLEX parameter: Please set the replacement strategy of solution [0,1,2] or leave it as NULL for CPLEX default value (0) - First In First Out")}}; if (is.null(poolReplace)) {poolReplace=0}
  if (!is.numeric(alphaWeight)) {stop("Objective Function: Please set a weight for mismatch penalty (will be applied only when the weight of measurement is not defined)")}
  if (!is.numeric(betaWeight)) {stop("Objective Function: Please set a weight for node penalty")}

  # Load necessary packages and functions
  library(CARNIVAL)
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
  if(dir.exists(dir_name)){
    print(paste0(dir_name," already exists and will be replaced."))
    unlink(dir_name, recursive = T)
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
  measWeights <- as.matrix(read.delim(file = measFile, sep = "\t"))
  measurements <- sign(measWeights) # Extracted sign of measurement for ILP fitting
  measWeights <- abs(measWeights) # Weights are all positives
  if (!is.null(inputFile)) {
    inputs <- read.table(file = inputFile, sep = "\t", header = TRUE)
  }
  if (!is.null(weightFile)) {
    edgeWeights <- read.delim(file = weightFile, sep = "\t")
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
  
  if (SysInfo[1]=="Windows") {
    file.copy(from = CplexPath,to = getwd())
    system(paste0("cplex.exe -f cplexCommand_", condition,"_",repIndex,".txt"))
    file.remove("cplex.exe")
  } else {
    system(paste0(CplexPath, " -f cplexCommand_", condition,"_",repIndex,".txt"))
    Elapsed_2 <- proc.time() - ptm
  }

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

  print(" ")
  print("--- End of the CARNIVAL pipeline ---")
  print(" ")

}
