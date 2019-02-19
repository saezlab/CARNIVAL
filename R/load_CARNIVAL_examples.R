#'\code{load_CARNIVAL_examples}
#'
#'@param CARNIVAL_example The number of CARNIVAL example to be loaded; 1 = Toy model, 2 = EGF-SBVimprover,3 = APAP-TGGs
#'
#'@return The input variables for CARNIVAL including network, measurement, input target +/- pathways scores
#'
#'@export


load_CARNIVAL_examples <- function(CARNIVAL_example) {

  if (CARNIVAL_example==1) {
    netFile <- system.file("Ex1_network_Toy.sif",package="CARNIVAL")
    measFile <- system.file("Ex1_measurements_Toy.txt",package="CARNIVAL")
    inputFile <- system.file("Ex1_inputs_Toy.txt",package="CARNIVAL") # optional; if not, set to 'NULL'
    weightFile <- NULL # optional; if not, set to 'NULL'
  } else if (CARNIVAL_example==2) {
    netFile <- system.file("Ex2_network_SBV_Omnipath.sif",package="CARNIVAL")
    measFile <- system.file("measurements/meas_EGF_50.txt",package="CARNIVAL")
    inputFile <- system.file("Ex2_inputs_SBV_EGF.txt",package="CARNIVAL") # optional; if not, set to 'NULL'
    weightFile <- system.file("measurements/scores_EGF.txt",package="CARNIVAL") # optional; if not, set to 'NULL'
  } else if (CARNIVAL_example==3) {
    netFile <- system.file("Ex3_network_APAP_TGG_Omnipath.sif",package="CARNIVAL")
    measFile <- system.file("Ex3_measurement_APAP_TGG_24hrHighDose.txt",package="CARNIVAL")
    inputFile <- NULL # optional; if not, set to 'NULL'
    weightFile <- system.file("Ex3_weights_APAP_TGG_24hrHighDose.txt",package="CARNIVAL") # optional; if not, set to 'NULL'
  } else {
    stop("Please select the provided examples or add your own example to the list")
  }

  loaded_CARNIVAL <- list(netFile=netFile,measFile=measFile,inputFile=inputFile,weightFile=weightFile)

  return(loaded_CARNIVAL)

}
