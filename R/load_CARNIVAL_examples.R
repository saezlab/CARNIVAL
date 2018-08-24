load_CARNIVAL_examples <- function(CARNIVAL_example) {

  if (CARNIVAL_example==1) {
    netFile <- system.file("Ex1_network_Toy.sif",package="CARNIVAL")
    measFile <- system.file("Ex1_measurements_Toy.txt",package="CARNIVAL")
    inputFile <- system.file("Ex1_inputs_Toy.txt",package="CARNIVAL") # optional; if not, set to 'NULL'
    weightFile <- NULL # optional; if not, set to 'NULL'
  } else if (CARNIVAL_example==2) {
    netFile <- system.file("Ex2_network_TGG_IMIM_Human_Liver.sif",package="CARNIVAL")
    measFile <- system.file("Ex2_measurements_TGG_APAP.txt",package="CARNIVAL")
    inputFile <- system.file("Ex2_inputs_TGG_APAP.txt",package="CARNIVAL")
    weightFile <- NULL # optional; if not, set to 'NULL'
  } else if (CARNIVAL_example==3) {
    netFile <- system.file("Ex3_network_SBV_Omnipath.sif",package="CARNIVAL")
    measFile <- system.file("Ex3_measurements_SBV_EGF.txt",package="CARNIVAL")
    inputFile <- system.file("Ex3_inputs_SBV_EGF.txt",package="CARNIVAL")
    weightFile <- system.file("Ex3_weights_SBV_EGF.txt",package="CARNIVAL")
  } else {
    stop("Please select the provided examples or add your own example to the list")
  }

  loaded_CARNIVAL <- list(netFile=netFile,measFile=measFile,inputFile=inputFile,weightFile=weightFile)

  return(loaded_CARNIVAL)

}
