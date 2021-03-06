% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_run_carnival.R
\name{generateLpFileCarnival}
\alias{generateLpFileCarnival}
\title{\code{generateLpFileCarnival}}
\usage{
generateLpFileCarnival(
  perturbations = NULL,
  measurements,
  priorKnowledgeNetwork,
  weights = NULL,
  carnivalOptions = defaultLpSolveCarnivalOptions()
)
}
\arguments{
\item{perturbations}{(optional, if inverse CARNIVAL flavour is used further) vector of targets of perturbations.}

\item{measurements}{vector of the measurements (i.e. DoRothEA/VIPER normalised
enrichment scores)}

\item{priorKnowledgeNetwork}{data frame of the prior knowledge network}

\item{weights}{(optional) vector of the additional weights: e.g. PROGENy pathway
scores or measured protein activities.}

\item{carnivalOptions}{the list of options for the run. See defaultLpSolveCarnivalOptions(),
defaultCplexCarnivalOptions, defaultCbcCarnivalOptions.}
}
\value{
paths to .lp file and .RData file that can be used for runFromLpCarnival()
}
\description{
\code{generateLpFileCarnival}
}
\details{
Prepares the input data for the run: tranforms data into lp file and .Rdata file.
These files can be reused to run CARNIVAL without preprocessing step using runCarnivalFromLp(..)
}
\examples{
load(file = system.file("toy_perturbations_ex1.RData",
                        package="CARNIVAL"))
load(file = system.file("toy_measurements_ex1.RData",
                        package="CARNIVAL"))
load(file = system.file("toy_network_ex1.RData",
                        package="CARNIVAL"))

## lpSolve
#res1 = generateLpFileCarnival(perturbations = toy_perturbations_ex1,
#                             measurements = toy_measurements_ex1,
#                             priorKnowledgeNetwork = toy_network_ex1,
#                             carnivalOptions = defaultLpSolveCarnivalOptions())

#res1["lpFile"] ##path to generated lp file
#res1["parsedDataFile"] ##path to data file used during generation

## Examples for cbc and cplex are commented out because these solvers are not part of R environment
## and need to be installed separately
##
## cbc
## res2 = generateLpFileCarnival(perturbations = toy_perturbations_ex1,
##                               measurements = toy_measurements_ex1,
##                               priorKnowledgeNetwork = toy_network_ex1,
##                               carnivalOptions = defaultCbcCarnivalOptions())
##
## res2["lpFile"] ##path to generated lp file
## res2["parsedDataFile"] ##path to data file used during generation
##
## cplex
## res3 = generateLpFileCarnival(perturbations = toy_perturbations_ex1,
##                              measurements = toy_measurements_ex1,
##                              priorKnowledgeNetwork = toy_network_ex1,
##                              carnivalOptions = defaultCplexCarnivalOptions())
##
## res3["lpFile"] ##path to generated lp file
## res3["parsedDataFile"] ##path to data file used during generation
}
