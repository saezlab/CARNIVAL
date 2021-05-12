#TODO 
#library(CARNIVAL)

files.sources = list.files("/Users/olgaivanova/GoogleDrive/_PhD_Heidelberg/playground/carnival_style/carnival/R", full.names=T)
sapply(files.sources, function(x) {
  print(x)
  source(x)
  })

test_networks_folder <- "../test_networks"
#TODO 
test_networks_folder <- "/Users/olgaivanova/GoogleDrive/_PhD_Heidelberg/playground/carnival_style/carnival/tests/test_networks"

simplest_network_file <- file.path(test_networks_folder, "direct_network.sif")
simplest_network_measurements_files <- file.path(test_networks_folder, "direct_network_measurements.csv")
simplest_network_perturbations_file <- file.path(test_networks_folder, "direct_network_perturbations.csv")

simplest_network <- read.table(simplest_network_file, header=T)
simplest_network_measurements <- read.table(simplest_network_measurements_file, header=T)
simplest_network_perturbations <- read.table(simplest_network_perturbations_file, header=T)

# # get input data
# load(file = system.file("toy_inputs_ex1.RData",
#                         package="CARNIVAL"))
# load(file = system.file("toy_measurements_ex1.RData",
#                         package="CARNIVAL"))
# load(file = system.file("toy_network_ex1.RData",
#                         package="CARNIVAL"))

# get expected result
load(file = system.file("result_expected.RData",
                        package="CARNIVAL"))

# obtain actual reesult
result_actual = runCARNIVAL(inputObj = simplest_network, 
                            measObj = simplest_network_measurements, 
                            netObj = simplest_network_perturbations)

#testing
test_that("Comparison of the results", {
  expect_equal(result_actual, result_expected)
})

#TODO  for future checkNames tests 

#test_grepl <- c("hel-lo", "hel*lo","hel_lo","hel lo", "hel>lo", "hel<lo", "hel+lo", "hel=lo", "hel/lo", "hello")
gregexpr(pattern=prepared_pattern, text = test_grepl)
