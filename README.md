# CARNIVAL

CARNIVAL is an R-based tool based on [Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) which performs causal reasoning to infer a subset of signalling network from transcriptomics data.
Transcription factors’ (TFs) activities and pathway scores from gene expressions can be inferred with our in-house tools [DoRothEA](https://github.com/saezlab/DoRothEA) & [PROGENy](https://github.com/saezlab/progeny).
TFs’ activities and signed directed protein-protein interaction networks +/- drug targets and pathway scores are then used to derive a series of linear constraints to generate integer linear programming (ILP) problems. 
An ILP solver (CPLEX) is subsequently applied to identify the sub-network topology with minimised discrepancies on fitting error and model size.

## Getting Started

A driver script of CARNIVAL is provided in the 'vignette' folder which could either be used as a Driver script to run the pipeline step-by-step. A condensed set of optmisation settings are listed at the beginning of the driver script for users' convenience. The template script of ther cluster-based variant was also supplied.

### Prerequisites

CARNIVAL requires the interactive version of IBM Cplex solver as the optimisation algorithm. The academic version of IBM Cplex is available [here](https://www.ibm.com/developerworks/community/blogs/jfp/entry/CPLEX_Is_Free_For_Students?lang=en) 

### Installing

CARNIVAL is currently available for the installation as an R-package from our GitHub page

```R
# Install CNORprob from Github (or load library for development version)
library(devtools)
install_github('saezlab/CARNIVAL', force=TRUE)
# --- OR --- #
library(devtools)
load_all()
```

## Running the tests

Several examples are available as the test case for CARNIVAL. Users can select the examples by assigning the example number to the "CARNIVAL_example" variable or use your own model inputs. Current examples include: 
1) Toy Model of two crosstalk pathways 
2) TG-GATEs dataset with paracetamol (APAP) as the perturbator
3) SBVimprover species translational dataset with EGF as the perturbator

```R
# Choose from our CARNIVAL examples (Ex1 = Toy model; Ex2 = TG-GATEs-APAP; Ex3 = SBVimprover-EGF)
CARNIVAL_example <- 3 # c(1,2,3); if not, set to 'NULL'

# Or assign your input files here
netFile <-  "your_network_file.sif" # required
measFile <- "your_measurement_file.txt" # required
inputFile <- "your_input_target_file.txt" # optional; if not, set to 'NULL'
weightFile <-  "your_node_weight_file.txt" # optional; if not, set to 'NULL'

```

### Setting and options for optimisation and subsequent analyses 

A set of optimisation parameters and settings can be assigned at the beginning of the driver script.

```R
# Choose CARNIVAL settings
Result_dir <- NULL # specify a name for result directory; if NULL, then date and time will be used by default
Sys.setenv(TZ="Europe/Berlin") # optional; set time zone to as default results' foldername
parallelCR <- F # running parallelised version?
inverseCR <- F # running inverse causal reasoning version?
nodeID <- "uniprot" # choose between 'uniprot' or 'gene'
UP2GS <- T # Convert UniProtIDs to Gene Symbols?
DOTfig <- T #  write DOT figure?
Export_all<- F #  export all ILP variables or not; if 0, only predicted node values and sif file will be written

# Set CPLEX stopping criteria
mipGAP        <- 0.05 # in proportion to the best estimated integer solution
poolrelGAP    <- 0.0001 # in relative to the pool of best solution
limitPop      <- 500 # limit the number of populated solutions after identified best solution
poolCap       <- 100 # limit the pool size to store populated solution
poolIntensity <- 4 # (for populating) select search intensity [0 default/ 1 to 4]
poolReplace   <- 2 # select replacement strategy of the pool solution [0 default/ 1 to 2]
betaWeight    <- 0.2 # relative coefficient of model size to fitting error in the objective function [default 0.2]
timelimit     <- 120 # set time limit for cplex optimisation (in seconds)
```

The results from the optimisation and subsequent analyses will be saved in the sub-folder "Results".

## Authors

Panuwat Trairatphisan (panuwat.trairatphisan -at- gmail.com)
Enio Gjerga (enio.gjerga -at- gmail.com)
Anika Liu (anikaliu94 -at- gmail.com)

See also the list of [contributors](https://github.com/saezlab/CARNIVAL/contributors) who participated in this project.

## License

Distributed under the GNU GPLv3 License. See accompanying file [LICENSE.txt](https://github.com/saezlab/CARNIVAL/blob/master/LICENSE.txt) or copy at [http://www.gnu.org/licenses/gpl-3.0.html](http://www.gnu.org/licenses/gpl-3.0.html).

## References

[Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f):

> Melas IN, Sakellaropoulos T, Iorio F, Alexopoulos L, Loh WY, Lauffenburger DA, Saez-Rodriguez J, Bai JPF. (2015). Identification of drug-specific pathways based on gene expression data: application to drug induced lung injury. *Integrative Biology*, Issue 7, Pages 904-920, https://doi.org/10.1039/C4IB00294F.

## Acknowledgement

CARNIVAL have been developed for the modelling projects within the [TransQST Consortium](https://transqst.org)

"This project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 116030. The Joint Undertaking receives support from the European Union's Horizon 2020 research and innovation programme and EFPIA."
