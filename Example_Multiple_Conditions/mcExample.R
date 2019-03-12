source("sourceAllFiles.R")
library(CARNIVAL)

CplexPath="~/Documents/cplex"
netFile="Ex1_network_Toy.sif"
measFile="Ex1_measurements_Toy.txt"
inputFile="Ex1_inputs_Toy.txt"
weightFile=NULL
CARNIVAL_example=NULL
Result_dir="Results_CARNIVAL"
inverseCR=F
parallelCR=F
nodeID="uniprot"
UP2GS=F
DOTfig=T
Export_all=F
timelimit=600
mipGAP=0.05
poolrelGAP=0.0001
limitPop=500
poolCap=100
poolIntensity=4
poolReplace=2
alphaWeight=1
betaWeight=0.2
experimental_conditions = c(1, 2)

#
betaWeight=0.2
mipGAP=0.1
poolrelGAP=0.01
limitPop=100
poolCap=100
poolIntensity=0
poolReplace=2
timelimit=1800
measWeights=NULL
condition=""
deltaWeight=0.01

CARNIVAL_Result <- runCARNIVAL(CplexPath=CplexPath,
                               Result_dir=Result_dir,
                               netFile = netFile,
                               measFile = measFile,
                               inputFile = inputFile, 
                               experimental_conditions = experimental_conditions, 
                               inverseCR = FALSE, 
                               CARNIVAL_example = CARNIVAL_example, 
                               DOTfig = FALSE,
                               UP2GS=F)
