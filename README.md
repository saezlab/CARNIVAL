# CARNIVAL

CARNIVAL is an R-based tool based on [Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) which performs causal reasoning to infer a subset of signalling network from transcriptomics data.
Transcription factors’ (TFs) activities and pathway scores from gene expressions can be inferred with our in-house tools [DoRothEA](https://github.com/saezlab/DoRothEA) & PROGENy.
TFs’ activities and signed directed protein-protein interaction networks +/- drug targets and pathway scores are then used to derive a series of linear constraints to generate integer linear programming (ILP) problems. 
An ILP solver (CPLEX) is subsequently applied to identify the sub-network topology with minimised discrepancies on fitting error and model size.

## Getting Started

A vignette of CARNIVAL is provided in the 'vignette' folder which could either be used as a Driver script to run the pipeline step-by-step or to simply generate the whole results with Knit(r). A condensed set of optmisation settings are listed at the beginning of the vignette for users' convenience.

### Prerequisites

CARNIVAL requires the interactive version of IBM Cplex solver as the optimisation algorithm. The academic version of IBM Cplex is available [here](https://www.ibm.com/developerworks/community/blogs/jfp/entry/CPLEX_Is_Free_For_Students?lang=en) 

Important Note: The respective cplex version based on the operating systems has to be put on the "R" directory in the package.

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

Several examples are available as the test case for CNORprob. Users can select the examples by assigning the example number to the "Selected_Model" variable. Current examples include: 
1) CNOToy (the running example of the CellNOpt package)
2) FALCON pipeline example, see [paper](https://academic.oup.com/bioinformatics/article/33/21/3431/3897376)
3) PDGF (dissecting PDGF signalling in Gastrointestinal Stromal Tumour - GIST), see [paper](http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0156223)
4) L-plastin (investigating L-plastin activating signalling pathways in breast cancer cell lines), see [paper](http://www.fasebj.org/content/30/3/1218.long)
5) (Under investigation) Stress-Response (SR) crosstalk between oxidative stress and DNA damage response pathways
6) (Under investigation) CCl4-induced liver injury in mice (in vivo)
7) (Under investigation) APAP-induced Jnk-Jun crosstalk in hepatocyte (in vitro)
8) An example of how to model network motif with XOR gate 

```R
# Select model
# 1 = CNOToy, 2 = FALCON Ex, 3 = PDGF, 4 = L-plastin
# 5 = SR-crosstalk, 6 = CCl4 IFADO, 7 = APAP-JNK, 8 = pp-Raf XOR
Selected_Model <- 1
```

### Setting and options for optimisation and subsequent analyses 

A set of optimisation parameters can be assigned at the beginning of the vignette. Several options can be chosen for the printing of intermedate results and the extraction of the final results. In the post optimisation analyses section, users can define which types of analysis, how many rounds (as well as the number of parameters' increment for LPSA) to perform.

```R
# Assign optimisation and parameter settings
optRound_optim    <- 1        # rounds of optimisation
L1Reg             <- 0.01     # assign weight for L1-regularisation
MaxTime           <- 180      # time for each round of optimisation [seconds]
HLbound           <- 0.5      # cut-off for high and low weights
SSthresh          <- 2e-16    # cut-off for states' difference at steady-state
printCost         <- 0        # print or not print intermediate fitting cost [0,1]
PlotIterations    <- 1        # rounds of optimisation to generate plots
SaveOptResults    <- TRUE     # [TRUE,FALSE] generate reports from optimisation

# Optimiser (rsolnp) options/control list (see rsolnp vignette: https://cran.r-project.org/web/packages/Rsolnp/Rsolnp.pdf)
rho               <- 1        # penalty weighting scaler / default = 1
outer.iter        <- 30       # maximum major iterations / default = 400
inner.iter        <- 30       # maximum minor iterations / default = 800
delta             <- 1e-7     # relative step size eval. / default = 1e-7
tol               <- 1e-8     # relative tol. for optim. / default = 1e-8
trace             <- 1        # print objfunc every iter / default = 1

# Post-optimisation analysis
Analyses          <- c(T,T,T) # [F,T] edge knockout, node knockout, sensitivity analysis
optRound_analysis <- 1        # rounds of optmisation in each analysis
LPSA_Increments   <- 2        # number of increments in LPSA analysis
```

Subsequent to these assignments, users can further run the rest of the script (vignette) on R-console or simply use Knit to generate a HTML report from the pipeline. Note that the results from the optimisation and subsequent analyses will be saved in the sub-folder "Results" within the "vignette" directory

### Additonal/Special assignments in CNORprob

Apart from the core setting options above, users can also assign additional features e.g. the preprocessing of prior knowledge network (please refer to the documentation of CellNOpt for more detail) as well as the network constraint.

"CNORprob_buildModel" provide 4 additional variables: 
1) "expandOR" refers to the expansion of the OR gate which it was not assigned from the initial list. 
2) "ORlist" refers to the introduction of specific OR relationship for specific interaction e.g. 'Grb2SoS_OR_GabSOS=GGSOS' for PDGF model. 
3) "HardConstraint" refers to the assignment that the sum of all weights/probabilities for all incoming activation reactions to be 1 (might be too strict for some cases). 
4) "Force" refers to the assignment of weight/probability for a single activated to always be 1 (might also be too strict for several cases).

```R
# Preprocessing (please see CellNopt documentation)
model <- preprocessing(CNOlist, pknmodel, expansion=FALSE,compression=TRUE, cutNONC=TRUE, verbose=FALSE)

# Model building with specified OR gate interaction
estim <- CNORprob_buildModel(CNOlist,model,expandOR=FALSE,ORlist=c("Grb2SOS_OR_GabSOS=GGSOS"),HardConstraint=TRUE,Force=TRUE,L1Reg=L1Reg,HLbound=HLbound,SSthresh=SSthresh,PlotIterations=PlotIterations,rsolnp_options=rsolnp_options)
```

## Authors

Panuwat Trairatphisan (panuwat.trairatphisan -at- gmail.com)
Enio Gjerga (enio.gjerga -at- gmail.com)
Anika Liu (anikaliu -at- yahoo.de)


See also the list of [contributors](https://github.com/saezlab/CARNIVAL/contributors) who participated in this project.

## License

Distributed under the GNU GPLv3 License. See accompanying file [LICENSE.txt](https://github.com/saezlab/CARNIVAL/blob/master/LICENSE.txt) or copy at [http://www.gnu.org/licenses/gpl-3.0.html](http://www.gnu.org/licenses/gpl-3.0.html).

## References

[Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f):

> Melas IN, Sakellaropoulos T, Iorio F, Alexopoulos L, Loh WY, Lauffenburger DA, Saez-Rodriguez J, Bai JPF. (2015). Identification of drug-specific pathways based on gene expression data: application to drug induced lung injury. *Integrative Biology*, Issue 7, Pages 904-920, https://doi.org/10.1039/C4IB00294F.

## Acknowledgement

CARNIVAL have been developed for the modelling projects within the [TransQST Consortium](https://transqst.org)

"This project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 116030. The Joint Undertaking receives support from the European Union's Horizon 2020 research and innovation programme and EFPIA."
