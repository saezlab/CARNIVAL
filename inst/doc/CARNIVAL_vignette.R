## ---- message=FALSE, warning=FALSE---------------------------------------
# rm(list=ls());cat("\014");if(length(dev.list())>0){dev.off()} # clean workspace
library(CARNIVAL) # load CARNIVAL library

runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1271/cplex/bin/x86-64_osx/cplex",
            netFile=NULL,measFile=NULL,
            inputFile=NULL,weightFile=NULL,
            Result_dir="Results_CARNIVAL_Ex1",
            CARNIVAL_example=1,
            UP2GS=F)

## ---- message=FALSE, warning=FALSE---------------------------------------
# rm(list=ls());cat("\014");if(length(dev.list())>0){dev.off()} # clean workspace
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("Ex2_network_SBV_Omnipath.sif",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
file.copy(from=system.file("Ex2_measurements_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
file.copy(from=system.file("Ex2_inputs_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
file.copy(from=system.file("Ex2_weights_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE)

runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1271/cplex/bin/x86-64_osx/cplex",
            netFile="Ex2_network_SBV_Omnipath.sif",
            measFile="Ex2_measurements_SBV_EGF.txt",
            inputFile="Ex2_inputs_SBV_EGF.txt",
            weightFile="Ex2_weights_SBV_EGF.txt",
            Result_dir="Results_CARNIVAL_Ex2",
            CARNIVAL_example=NULL)

