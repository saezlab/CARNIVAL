# CARNIVAL

CARNIVAL is an R-package providing a framework to perform causal reasoning to infer a subset of signalling network from transcriptomics data. This work was originally based on [Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) with a number improved functionalities comparing to the original version.
Transcription factors’ (TFs) activities and pathway scores from gene expressions can be inferred with our in-house tools [DoRothEA](https://github.com/saezlab/DoRothEA) & [PROGENy](https://github.com/saezlab/progeny), respectively.
TFs’ activities and signed directed protein-protein interaction networks +/- drug targets and pathway scores are then used to derive a series of linear constraints to generate integer linear programming (ILP) problems. 
An ILP solver (CPLEX) is subsequently applied to identify the sub-network topology with minimised discrepancies on fitting error and model size.

## Getting Started

A tutorial for running CARNIVAL is provided as a vignette in R-Markdown and HTML formats. The wrapper script "runCARNIVAL" was introduced to take input arguments, pre-process input descriptions, run optimisation and export results as network files and figures. Three built-in CARNIVAL examples are also supplied as case studies for users.

### Prerequisites

CARNIVAL requires the interactive version of IBM Cplex solver as the network optimiser. The IBM ILOG Cplex is freely available through Academic Initiative [here](https://www.ibm.com/products/ilog-cplex-optimization-studio?S_PKG=CoG&cm_mmc=Search_Google-_-Data+Science_Data+Science-_-WW_IDA-_-+IBM++CPLEX_Broad_CoG&cm_mmca1=000000RE&cm_mmca2=10000668&cm_mmca7=9041989&cm_mmca8=kwd-412296208719&cm_mmca9=_k_Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB_k_&cm_mmca10=267798126431&cm_mmca11=b&mkwid=_k_Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB_k_|470|135655&cvosrc=ppc.google.%2Bibm%20%2Bcplex&cvo_campaign=000000RE&cvo_crid=267798126431&Matchtype=b&gclid=Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB) 

### Installing

CARNIVAL is currently available for the installation as an R-package from our GitHub page

```R
# Install CARNIVAL from Github using devtools
# install.packages('devtools') # in case devtools hasn't been installed
library(devtools)
install_github('saezlab/CARNIVAL', build_vignettes = TRUE)
# Or download the source file from GitHub and install from source
install.packages(path_to_file, repos = NULL, type="source")
```

## Running the tests

First, user has to define the path to interactive CPLEX on the variable 'CplexPath' in the runCARNIVAL function. The path to interactive version of CPLEX is differed based on the operating system. The default installation path for each OS is as follows:
- For Mac OS: the path is usually "~/Applications/IBM/ILOG/CPLEX_Studio1271/cplex/bin/x86-64_osx" where the version of CPLEX (CPLEX_Studio1271) has to be changed accordingly
- For Windows: --- to be tested ---
- For Linux: --- to be tested ---

Three examples are available as the test cases for CARNIVAL. Users can select the examples by assigning the example number to the "CARNIVAL_example" variable or use user-defined own model inputs. Current examples include: 
1) Toy Model of two crosstalk pathways 
2) SBVimprover species translational dataset with EGF as the perturbator
3) TG-GATEs dataset with paracetamol (APAP) as the perturbator

To run the built-in CARNIVAL examples, users could run the following code:

```R
library(CARNIVAL) # load CARNIVAL library

runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1271/cplex/bin/x86-64_osx/",
            netFile=NULL,measFile=NULL,
            inputFile=NULL,weightFile=NULL,
            Result_dir="Results_CARNIVAL_Ex1",
            CARNIVAL_example=1)
```

To run the user defined optimisation problem, users should have already prepared the network file and the measurement input (here TF's enrichment scores from DoRothEA) +/- additional information i.e. target of perturbation and additional node penalty weight (here pathway scores from PROGENy).
Once the input files are prepared in the correct format, the user could run the following code: (Note: we present here the input files for Example 2 as a case study. We advise users to check the format of input files from the ones for Example 2 to ensure the compatibility with the CARNIVAL pipeline.)

```R
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("Ex2_network_SBV_Omnipath.sif",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve network file
file.copy(from=system.file("Ex2_measurements_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve measurement file
file.copy(from=system.file("Ex2_inputs_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve target of perturbation file
file.copy(from=system.file("Ex2_weights_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve additional/pathway weight file

runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1271/cplex/bin/x86-64_osx/",
            netFile="Ex2_network_SBV_Omnipath.sif",
            measFile="Ex2_measurements_SBV_EGF.txt",
            inputFile="Ex2_inputs_SBV_EGF.txt",
            weightFile="Ex2_weights_SBV_EGF.txt",
            Result_dir="Results_CARNIVAL_Ex2",
            CARNIVAL_example=NULL)
```

The results from the optimisation i.e. optimised network description and figures will be saved in the result folder.

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

[DoRothEA v2 - Garcia-Alonso et al.](https://www.biorxiv.org/content/early/2018/06/03/337915):

> Garcia-Alonso L, Ibrahim MM, Turei D, Saez-Rodriguez J. (2018). Benchmark and integration of resources for the estimation of human transcription factor activities. *bioRXiv*, https://doi.org/10.1101/337915.

[PROGENy - Schubert et al.](https://www.nature.com/articles/s41467-017-02391-6):

> Schubert M, Klinger B, Klünemann M, Sieber A, Uhlitz F, Sauer S, Garnett MJ, Blüthgen N, Saez-Rodriguez J. (2018). Perturbation-response genes reveal signaling footprints in cancer gene expression. *Nature Communication*, Issue 9, Nr. 20. https://doi.org/10.1038/s41467-017-02391-6.


## Acknowledgement

CARNIVAL has been developed as a computational tool to analyse -omics data within the [TransQST Consortium](https://transqst.org)

"This project has received funding from the Innovative Medicines Initiative 2 Joint Undertaking under grant agreement No 116030. The Joint Undertaking receives support from the European Union's Horizon 2020 research and innovation programme and EFPIA."
