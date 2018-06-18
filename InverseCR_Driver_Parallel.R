# -------------------------------------- #
# Inverse causal reasoning driver script #
# -------------------------------------- #

rm(list=ls()) # clear environment
cat("\014") # clear screen
if (length(dev.list())>0){dev.off()} # clear figure (if any)

# Parallelisation
# install.packages("doParallel")
library(doParallel)
argsJob=commandArgs(trailingOnly = TRUE)
repIndex <- as.numeric(argsJob[1])
# repIndex <- 1

# Select a causal reasoning pipeline and examples
InvCRmethod <- 1 # c(1,2) # 1 = inverse CARNIVAL, 2 = simplified inverse causal reasoning
Example     <- 4 # c(1,2) # 1 = WGCNA/surgical experiments, 2 = Extract WGCNA Model, 3 = simple Toy model

# Additional option for inverse CARNIVAL method (InvCRmethod==1)
AddPertubationNode <- 1 # Add perturbation node (inverse causal reasoning pipeline)

# Set CPLEX stopping criteria for inverse CARNIVAL method (InvCRmethod==1)
# mipGAP      <- 0.001 # (for optimising) in proportion to the best estimated solution
poolrelGAP    <- 0.001 # (for populating) in relative to the best solution 
limitPop      <- 1000 # (for populating) limit the number of populated solutions
poolCap       <- 1000 # (for populating) limit the pool size to store populated solution
poolIntensity <- 4 # (for populating) select search intensity [0 default/ 1 to 4]
alphaWeight   <- 100 # [default 100] coefficient of fitting error in objective function
betaWeight    <- 20 # [default 20] coefficient of model size in objective function
gammaWeight   <- 1 # [default 1] coefficient of weights (from PROGENy) in objective function
timelimit     <- 3600 # set time limit for cplex optimisation

# Choose results exporting options
# Result_dir  <- paste0("Ex",toString(Example),"_InvCRmethod_",InvCRmethod) # specify a name for result directory; if NULL, then date and time will be used by default
Result_dir  <- paste0("Ex",toString(Example),"_InvCRmethod_",InvCRmethod,"_RepBS_",repIndex) # specify a name for result directory; if NULL, then date and time will be used by default
Export_all  <- 0 # c(0,1) export all ILP variables or not; if 0, only predicted node values, sif and dot files will be written

# ============================== #

# Load necessary packages and functions
library(readr)
library(tidyr)
library(XML)
source("src/CARNIVAL_Functions.R")

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

# Assign names for result files
if (Example==1) {
  sif <- "examples/InvCR_Ex1/WGCNA_Full_network.sif"
  meas <- "examples/InvCR_Ex1/WGCNA_Full_meas.tsv"
  ips <- "examples/InvCR_Ex1/WGCNA_Full_inputs.txt"
} else if (Example==2) {
  sif <- "examples/InvCR_Ex2/WGCNA_Extracted_network.sif"
  meas <- "examples/InvCR_Ex2/WGCNA_Extracted_meas.csv"
  ips <- "examples/InvCR_Ex2/WGCNA_Extracted_inputs.txt"
} else if (Example==3) {
  sif <- "examples/InvCR_Ex3/Toy_network.sif"
  meas <- "examples/InvCR_Ex3/Toy_meas.csv"
  ips <- "examples/InvCR_Ex3/Toy_inputs.txt"
} else if (Example==4) {
  sif <- "pkn_reduced_omnipath.sif"
  meas <- paste0("measurements/measurements_symbol_",repIndex,".txt")
  ips <- "inputs_for_pkn_reduced_omnipath.sif"
} else {
  stop("Please select the provided examples or add your own example to the list")
}

if (InvCRmethod==1) {
  
  # Load inputs
  network      <- read.table(sif, sep = "\t", header = TRUE,stringsAsFactors = F)
  inputs       <- read.table(ips, sep="\t", header = TRUE)
  measurements <- read_delim(meas,"\t", escape_double = FALSE, trim_ws = TRUE)

  # Adding perturbation node?
  if (AddPertubationNode==1) {
    NameInput <- names(inputs)
    AddToNet <- data.frame(matrix(NA,length(NameInput)*2,3))
    AddToNet[,1] <- "Perturbation"
    AddToNet[1:length(NameInput),2] <- "1";AddToNet[1:length(NameInput),3] <- NameInput
    AddToNet[(length(NameInput)+1):(length(NameInput)*2),2] <- "-1";AddToNet[(length(NameInput)+1):(length(NameInput)*2),3] <- NameInput
    colnames(AddToNet) <- colnames(network)
    network <- rbind(network,AddToNet)
    inputs <- data.frame("NaN"); colnames(inputs) <- "Perturbation"
  }
  
  # Input processing
  data <- measurements
  pknList <- as.data.frame(network)
  colnames(pknList) <- c("Node1", "Sign", "Node2")
  setwd(paste0(current_dir,"/src/")) # temporary shift to src directory
  
  if (exists("pathwayscore")) {
    if (nrow(pathwayscore)==1) {
      if (length(unique(as.numeric(pathwayscore)))!=1) {
        nodeWeights <- 1 - 2*((abs(pathwayscore) - min(abs(pathwayscore),na.rm=T)) / (max(abs(pathwayscore),na.rm=T) - min(abs(pathwayscore),na.rm=T)))
      } else { 
        nodeWeights <- pathwayscore # if all pathway scores are the same, then use pathwayscore as the nodeWeights directly
      }
      names(nodeWeights) <- colnames(pathwayscore)
      scores <- as.numeric(pathwayscore); names(scores) <- names(nodeWeights)
    } else {
      pathwayscore_current <- pathwayscore[1,]
      nodeWeights <- NULL
      for (counter_pw in 1:ncol(pathwayscoreAll)) {
        ss <- abs(as.numeric(as.matrix(pathwayscoreAll[, counter_pw])))
        nodeWeights <- c(nodeWeights, 1-2*(ss[1]-min(ss, na.rm = TRUE))/(max(ss, na.rm = TRUE)-min(ss, na.rm = TRUE)))
      }
      names(nodeWeights) <- colnames(pathwayscore)
      scores <- as.numeric(pathwayscore_current); names(scores) <- names(nodeWeights)
    }
  } else {
    scores <- NULL
    nodeWeights <- NULL
  }
  
  # Remove intermediate cplex files (if any)
  AllFiles <- list.files()
  CloneFiles <- which(grepl(pattern = "clone",x = AllFiles,fixed = T))
  if (length(CloneFiles)>0) {
    for (counter in 1:length(CloneFiles)) {
      file.remove(AllFiles[CloneFiles[counter]])
    }
  }
  if (file.exists(paste0("testFile_",repIndex,".lp"))) {file.remove(paste0("testFile_",repIndex,".lp"))}
  if (file.exists(paste0("results_cplex_",repIndex,".txt"))) {file.remove(paste0("results_cplex_",repIndex,".txt"))}
  if (file.exists("cplex.log")) {file.remove("cplex.log")}
  if (file.exists("cplexCommand.txt")) {file.remove("cplexCommand.txt")}
  
  # Write constraints as ILP inputs
  ptm <- proc.time()
  print("Writing constraints...")
  # variables <- writeLPFile(data,pknList,inputs,0.1,alphaWeight=100,betaWeight=20,scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,timelimit=timelimit,nodeWeights=nodeWeights,repIndex=repIndex)
  variables <- writeLPFile_BS(data,pknList,inputs,0.1,alphaWeight=alphaWeight,betaWeight=betaWeight,scores=scores,mipGAP=mipGAP,poolrelGAP=poolrelGAP,limitPop=limitPop,poolCap=poolCap,poolIntensity=poolIntensity,timelimit=timelimit,nodeWeights=nodeWeights,repIndex=repIndex)
  Elapsed_1 <- proc.time() - ptm
  
  # Solve ILP problem with cplex, remove temp files, and return to the main directory
  ptm <- proc.time()
  print("Solving LP problem...")
  tryCatch({
  system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
  }, error = function(err1) {
    tryCatch ({  
    system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
    }, error = function(err2) {
      system(paste0(getwd(), "/cplex -f cplexCommand.txt"))
    })
  })
  Elapsed_2 <- proc.time() - ptm
  
  if (file.exists(paste0("testFile_",repIndex,".lp"))) {file.remove(paste0("testFile_",repIndex,".lp"))} # might be useful for debugging 
  if (file.exists(paste0("results_cplex_",repIndex,".txt"))) {file.copy(from = paste0("results_cplex_",repIndex,".txt"),to = paste(current_dir,"/results/",dir_name,"/results_cplex.txt",sep="")); file.remove(paste0("results_cplex_",repIndex,".txt"))}
  if (file.exists("cplex.log")) {file.copy(from = "cplex.log",to = paste(current_dir,"/results/",dir_name,"/cplex.log",sep="")); file.remove("cplex.log")}
  if (file.exists("cplexCommand.txt")) {file.remove("cplexCommand.txt")}
  setwd(current_dir)
  
  # Write result files
  ptm <- proc.time()
  print("Writing result files...")
  if (file.exists(paste("results/",dir_name,"/results_cplex.txt",sep=""))) {
    for(i in 1:length(variables)){
      sif <- readOutResult(cplexSolutionFileName = paste0("results/",dir_name,"/results_cplex.txt"), variables = variables, pknList = pknList, conditionIDX = i,dir_name = dir_name, Export_all = Export_all,inputs=inputs,measurements=measurements)
    }
  } else {
    print("No result to be written")
  }
  Elapsed_3 <- proc.time() - ptm
  
  # Logged computational time
  ElapsedAll <- as.data.frame(matrix(t(c(Elapsed_1[3],Elapsed_2[3],Elapsed_3[3])),3,1))
  rownames(ElapsedAll) <- c("WriteConstraints:","CplexSolving:","ExportResults:")
  write.table(x = ElapsedAll,file = paste("results/",dir_name,"/elapsed_time.txt",sep=""),col.names = F,row.names = T,quote = F)

  # =========================== #   
  # =========================== #   
  # =========================== #   
  
  
} else if (InvCRmethod==2) {

  network <- read.csv(sif,header=TRUE,sep="\t",stringsAsFactors = F)
  measurement <- read.table(meas,header = T,sep="\t",stringsAsFactors = F)
  actMat <- matrix(NA,length(measurement),2)
  colnames(actMat) <- c("Node","Activity")
  actMat[,1] <- names(as.vector(measurement))
  actMat[,2] <- as.integer(as.vector(measurement))
  measurement <- actMat
  
  act <- paste0("results/",dir_name,"/CARNIVAL_ML_Inferred_NodeAct_Case",Example,".tsv")
  ReduceNet <- paste0("results/",dir_name,"/CARNIVAL_ML_Inferred_Network_Case",Example,".sif")
  
  # Load DOT writing script and write starting network condition  
  # source("SIF2DOT_v2.R")
  SIF2DOT_v2(sif=sif,meas=meas,DOTfilename = paste0("results/",dir_name,"/Starting_Network_Case",Example,".dot"))
  
  # === Multi-level causal reasoning === #
  # 0) Initialise nodes' activities with NA and map the known activities for measured nodes
  # 1) For each node not defined by measurement -> retrieve the interaction(s) that has the respective node as the source of interaction(s)
  # 2) For each interaction from 1) -> multiply the sign of interaction with the activity of the target node & collect results
  #    Note: negative sign will filp the value of node activity wrt. measurement data, 
  #          positive sign will take the same sign of node activity wrt. measurement data,
  #          neutral sign (e.g. PPI or both activation/inhibition) or zero measurement will return node activity as zero
  # 3) Call the rule of majority of results by making the sum of results from 2) to determine final nodes' activities [**Need discussions!**]
  # 4) Update inferred nodes' activities as a batch in each round and re-do steps 1,2,3 until all nodes' activities are inferred (no NA)
  
  # Initialise a matrix to store nodes' activities + map measurement data
  AllNodes <- sort((unique(c(as.character(network[,1]),as.character(network[,3])))))
  AllNodesAct <- cbind(AllNodes,rep(NA,length(AllNodes)))
  for (counter in 1:nrow(measurement)) {
    AllNodesAct[which(measurement[counter,1]==AllNodesAct[,1]),2] <- measurement[counter,2]
  }
  
  Round=1 # round of causal reasoning process
  NodeUpdate <- 1
  # while (sum(is.na(AllNodesAct[,2]))>0) { # if not all node values are determined
  while (!is.null(NodeUpdate)) { # if not all node values are determined
    
    NodeNA <- AllNodesAct[which(is.na(AllNodesAct[,2])),1]
    if (length(NodeNA)==0) {break}
    
    NodeUpdate <- NULL
    for (counter in 1:length(NodeNA)) {
      NodeIntAct <- which(NodeNA[counter]==network[,1])
      NodeCurrent <- rep(NA,length(NodeIntAct)) 
      for (counter2 in 1:length(NodeIntAct)) {
        TargetCurrent <- as.character(network[NodeIntAct[counter2],3])
        if (length(TargetCurrent)>0) {
          if (!is.na(TargetCurrent)) {
            SignCurrent <- network[intersect(which(NodeNA[counter]==network[,1]),which(TargetCurrent==network[,3])),2]
            if (!is.na(AllNodesAct[which(TargetCurrent==AllNodesAct[,1]),2])) {
              NodeCurrent[counter2] <- as.numeric(AllNodesAct[which(TargetCurrent==AllNodesAct[,1]),2])*SignCurrent
            }
          }
        }
      }
      
      # Version 1: Take only the nodes which can "all" be explained by downstream data
      if (!is.na(sum(NodeCurrent))) {
        NodeActFinal <- sign(sum(NodeCurrent))
        NodeUpdate <- rbind(NodeUpdate,c(NodeNA[counter],NodeActFinal))
      }
      
      # Version 2: Take nodes which have at least one explanable edge into account
      # NodeCurrent <- NodeCurrent[!is.na(NodeCurrent)] # remove NA first
      # if (length(NodeCurrent)>0) { # if there is still at least one edge remained
      #   NodeActFinal <- sign(sum(NodeCurrent))
      #   NodeUpdate <- rbind(NodeUpdate,c(NodeNA[counter],NodeActFinal))
      # }
      
    }
    if (!is.null(NodeUpdate)) {
      print(paste0("Inferred node activity Round: ",Round))
      print(NodeUpdate)
      for (counter3 in 1:nrow(NodeUpdate)) {
        AllNodesAct[which(NodeUpdate[counter3,1]==AllNodesAct[,1]),2] <- NodeUpdate[counter3,2]
      }
    } else {
      break()
    }
    Round <- Round+1
    # print(Round)
  }
  
  # Write all node activities as a file
  colnames(AllNodesAct) <- c("Node","Activity")
  naIDX <- which(is.na(AllNodesAct[,2]))
  if (length(naIDX)>0) {
    AllNodesAct <- AllNodesAct[-naIDX,]
  }
  
  write.table(AllNodesAct,act,quote = F,col.names = T,row.names = F,sep = "\t")
  
  # Remove interactions where nodes do not have any activity or inferred activities are zero from the network to generate a reduced network
  
  # First get the indices to remove the nodes which were not inferred activities
  AllInferredNodes <- AllNodesAct[,1]
  AllNonInferredNodes <- setdiff(sort(unique(c(network[,1],network[,3]))),AllInferredNodes)
  IntActNonInferredIDX <- NULL
  for (counter in 1:length(AllNonInferredNodes)) {
    IntActNonInferredIDX <- c(IntActNonInferredIDX,which(AllNonInferredNodes[counter]==network[,1]),which(AllNonInferredNodes[counter]==network[,3]))
  }
  IntActNonInferredIDX <- sort(unique(IntActNonInferredIDX))
  
  # Then get the indices to remove the nodes which have inferred zero activities
  
  NodeZeroAct <- AllNodesAct[which(AllNodesAct[,2]==0),1]
  
  if (length(NodeZeroAct)>0) {
    IntAct2Rem <- NULL
    for (counter in 1:length(NodeZeroAct)) {
      IntAct2Rem <- c(IntAct2Rem,unique(c(which(NodeZeroAct[counter]==network[,1]),which(NodeZeroAct[counter]==network[,2]))))
    }
    
    # Now remove the interactions unwanted interactions
    
    if (length(IntActNonInferredIDX)>0) {
      ReducedNetAllSign <- network[-sort(unique(c(IntAct2Rem,IntActNonInferredIDX))),]
    } else {
      ReducedNetAllSign <- network[-IntAct2Rem,]
    }
    
  } else {
    if (length(IntActNonInferredIDX)>0) {
      ReducedNetAllSign <- network[-sort(IntActNonInferredIDX),]
    } else {
      ReducedNetAllSign <- network
    }
  }
  
  if (length(which(ReducedNetAllSign[,2]==0))>0) {
    ReducedNetFinal <- ReducedNetAllSign[-which(ReducedNetAllSign[,2]==0),]
    write.table(ReducedNetFinal,ReduceNet,quote = F,col.names = T,row.names = F,sep = "\t")
  } else {
    write.table(ReducedNetAllSign,ReduceNet,quote = F,col.names = T,row.names = F,sep = "\t")
  }
  
  # Write final network which displays only the nodes with activities
  # SIF2DOT(sif=ReduceNet,meas=meas,DOTfilename = paste0("Inferred_Network_Case",Example,".dot"),act = act)
  SIF2DOT_v2(sif=ReduceNet,meas=meas,DOTfilename = paste0("results/",dir_name,"/Inferred_Network_Case",Example,".dot"),act = act)
  
}

# --- End of script --- #
