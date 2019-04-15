# CARNIVAL

CARNIVAL is an R-package providing a framework to perform causal reasoning to infer a subset of signalling network from transcriptomics data. This work was originally based on [Melas et al.](https://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) with a number improved functionalities comparing to the original version.
Transcription factors’ (TFs) activities and pathway scores from gene expressions can be inferred with our in-house tools [DoRothEA](https://github.com/saezlab/DoRothEA) & [PROGENy](https://github.com/saezlab/progeny), respectively.
TFs’ activities and signed directed protein-protein interaction networks +/- drug targets and pathway scores are then used to derive a series of linear constraints to generate integer linear programming (ILP) problems. 
An ILP solver (CPLEX) is subsequently applied to identify the sub-network topology with minimised discrepancies on fitting error and model size.

More detailed descriptions of CARNIVAL, benchmarking and applicational studies can be found in [Liu, Trairatphisan, Gjerga et al.](https://www.biorxiv.org/content/10.1101/541888v1):

> Liu A*, Trairatphisan P*, Gjerga E*, Didangelos A, Barratt J, Saez-Rodriguez J. (2019). From expression footprints to causal pathways: contextualizing large signaling networks with CARNIVAL. *bioRxiv*, https://doi.org/10.1101/541888 (*equal contributions).


## Getting Started

A tutorial for preparing CARNIVAL input files starting from differentially gene expression (DEG) and for running the CARNIVAL pipeline are provided as vignettes in R-Markdown, R-script and HTML formats. The wrapper script "runCARNIVAL" was introduced to take input arguments, pre-process input descriptions, run optimisation and export results as network files and figures. Three built-in CARNIVAL examples are also supplied as case studies for users.

### Prerequisites

CARNIVAL requires the interactive version of IBM Cplex solver as the network optimiser. The IBM ILOG Cplex is freely available through Academic Initiative [here](https://www.ibm.com/products/ilog-cplex-optimization-studio?S_PKG=CoG&cm_mmc=Search_Google-_-Data+Science_Data+Science-_-WW_IDA-_-+IBM++CPLEX_Broad_CoG&cm_mmca1=000000RE&cm_mmca2=10000668&cm_mmca7=9041989&cm_mmca8=kwd-412296208719&cm_mmca9=_k_Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB_k_&cm_mmca10=267798126431&cm_mmca11=b&mkwid=_k_Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB_k_|470|135655&cvosrc=ppc.google.%2Bibm%20%2Bcplex&cvo_campaign=000000RE&cvo_crid=267798126431&Matchtype=b&gclid=Cj0KCQiAr93gBRDSARIsADvHiOpDUEHgUuzu8fJvf3vmO5rI0axgtaleqdmwk6JRPIDeNcIjgIHMhZIaAiwWEALw_wcB) 

### Installing

CARNIVAL is currently available for the installation as an R-package from our GitHub page

```R
# Install CARNIVAL from Github using devtools
# install.packages('devtools') # in case devtools hasn't been installed
library(devtools)
install_github('saezlab/CARNIVAL')
# or download the source file from GitHub and install from source
install.packages('path_to_extracted_CARNIVAL_directory', repos = NULL, type="source")
```

## Running CARNIVAL

To obtain the list of tutorials/vignettes of the CARNIVAL package, user can start with typing the following commmand on R-console:

```R
browseVignettes(package='CARNIVAL')
```

In the CARNIVAL package, 3 built-in examples are available as the test cases as follows:
1) Toy Model of two crosstalk pathways 
2) SBVimprover species translational dataset with EGF as the perturbator
3) TG-GATEs dataset with paracetamol (APAP) as the perturbator

### DEG Pre-processing

Users can generate the measurement file which contains calculated transcription factor (TF) normalised enrichment scores by applying the function runDoRothEA. An example of the pipeline to generate a measurement file for Example 2 (SBVimprover-EGF) is shown as follows:

```R
library(CARNIVAL) # load CARNIVAL library

#Load files
file.copy(from=system.file("SBV_EGF_tvalues.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
file.copy(from=system.file("dorothea_TF_mapping.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)
load(file = system.file("BEST_viperRegulon.rdata",package="CARNIVAL"))

df<-read.csv2("SBV_EGF_tvalues.csv", row.names = 'GeneName')  
map<-read.csv("dorothea_TF_mapping.csv")

#Run DoRothEA and convert from gene symbol to uniprot ID
TF_genesymbol<-runDoRothEA(df, regulon=viper_regulon, confidence_level=c('A','B','C'))
TF_uniprot<-GeneSymbol2Uniprot(TF_genesymbol, map, 1, 2)

#Generate measurement files in CARNIVAL input format
generate_measfile(measurements=TF_uniprot, topnumber=50, write2folder="measurements")

```

Also, users can generate the additional weight input file which contains calculated pathway scores by applying the function runPROGENy. An example of the pipeline to generate a measurement file for Example 2 (SBVimprover-EGF) is shown as follows:

```R
library(CARNIVAL) # load CARNIVAL library

#Load files
file.copy(from=system.file("model_NatComm+14_human.csv",package="CARNIVAL"),to=getwd(),overwrite=TRUE)

weight_matrix<-read.csv("model_NatComm+14_human.csv")
df_genenames<-data.frame('gene'=rownames(df),df)

#Run PROGENy
pathway_scores<-runPROGENy(df_genenames,weight_matrix, z_scores = F)

#Generate CARNIVAL input files
for (cond in colnames(pathway_scores)){
  scores<-rbind(rownames(pathway_scores),pathway_scores[,cond])
  write.table(scores, paste0("measurements/scores_",cond,".txt"),col.names = F, row.names = F, quote = F, sep = '\t')
}
```

The results from DEG-preprocessing pipeline are saved in the directory "measurements" in the current working directory.

### CARNIVAL pipeline

To run the CARNIVAL pipeline, users fist have to define the path to interactive CPLEX on the variable 'CplexPath' in the runCARNIVAL function. The path to interactive version of CPLEX is differed based on the operating system. The default installation path for each OS is as follows:
- For Mac OS: "~/Applications/IBM/ILOG/CPLEX_Studio129/cplex/bin/x86-64_osx/cplex" -- Note that the version of CPLEX has to be changed accordingly (the latest version is "CPLEX_Studio129")
- For Linux: "/opt/ibm/ILOG/CPLEX_Studio129/cplex/bin/x86-64_linux/cplex"
- For Windows: "C:/Program Files/IBM/ILOG/CPLEX_Studio129/cplex/bin/x64_win64/cplex.exe"

Next, users can select the examples by assigning the example number to the "CARNIVAL_example" variable or use user-defined own model inputs. To run the built-in CARNIVAL examples, users could run the following code:

```R
library(CARNIVAL) # load CARNIVAL library

CARNIVAL_Result <- runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1281/cplex/bin/x86-64_osx/cplex",
            Result_dir="Results_CARNIVAL_Ex1",
            CARNIVAL_example=1,
            UP2GS=F)
```

To run the user defined optimisation problem, users should have already prepared the network file and the measurement input (here TF's enrichment scores from DoRothEA) +/- additional information i.e. target of perturbation and additional node penalty weight (here pathway scores from PROGENy).
Once the input files are prepared in the correct format, the user could run the following code: (Note: we present here the input files for Example 2 as a case study. We advise users to check the format of input files from the ones for Example 2 to ensure the compatibility with the CARNIVAL pipeline.)

```R
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("Ex2_network_SBV_Omnipath.sif",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve network file
file.copy(from=system.file("Ex2_measurements_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve measurement file
file.copy(from=system.file("Ex2_inputs_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve target of perturbation file
file.copy(from=system.file("Ex2_weights_SBV_EGF.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve additional/pathway weight file

CARNIVAL_Result <- runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1281/cplex/bin/x86-64_osx/cplex",
            netFile="Ex2_network_SBV_Omnipath.sif",
            measFile="Ex2_measurements_SBV_EGF.txt",
            inputFile="Ex2_inputs_SBV_EGF.txt",
            weightFile="Ex2_weights_SBV_EGF.txt",
            Result_dir="Results_CARNIVAL_Ex2",
            CARNIVAL_example=NULL)
```

In case the target(s) of perturbation is unknown (e.g. in the case of multi-factorial diseases or broad-spectrum drugs), users can choose the Inverse CARNIVAL pipeline to exclusively build a sub-network without input's target(s) information (i.e. inputFile = NULL, inverseCR=T). Note that the pathway scores from PROGENy is highly recommended to be included once running the Inverse CARNIVAL pipeline.
Here we present Example 3 as a case study for the Inverse CARNIVAL pipeline (to investigate downstream toxicity effects of paracetamol/APAP on cellular processes). An example of R-code is provided below:


```R
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("Ex3_network_APAP_TGG_Omnipath.sif",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve network file
file.copy(from=system.file("Ex3_measurement_APAP_TGG_24hrHighDose.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve measurement file
file.copy(from=system.file("Ex3_weights_APAP_TGG_24hrHighDose.txt",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve additional/pathway weight file

CARNIVAL_Result <- runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1281/cplex/bin/x86-64_osx/cplex",
            netFile="Ex3_network_APAP_TGG_Omnipath.sif",
            measFile="Ex3_measurement_APAP_TGG_24hrHighDose.txt",
            weightFile="Ex3_weights_APAP_TGG_24hrHighDose.txt",
            Result_dir="Results_CARNIVAL_Ex3",
            CARNIVAL_example=NULL,
            inverseCR=T)
```


### Additional settings on 'runCARNIVAL'

Note that the pipeline as presented is the Standard CARNIVAL pipeline with known perturbation targets (as listed in the variable inputFile). Additional CARNIVAL settings can be directly assigned onto the runCARNIVAL function e.g.

- To the optimisation time in CPLEX: set the 'timelimit' variable to a desired value (e.g. timelimit=600 [in seconds])
- To export and write all CPLEX variables into files: set the 'Export_all' variable to TRUE (Export_all=T; recommended for debugging)
- In addition, additional CPLEX parameters and alpha and beta weights in the objective function can also be manually assigned. More details can be obtained in the help section: ?runCARNIVAL

### Exported CARNIVAL results

The results from the runCARNIVAL function i.e. the optimised network descriptions can be saved as a variable as they together with figures will be saved in the result folder. In case multiple solutions were found, individual model structures and nodes activities will be written to the file "interactions_(i)_ model(j).tsv" and "nodesActivity_(i)_ model(j).txt", respectively, where (i) refers to the experimental condition and (j) refers to the numbering of solutions.

The aggregation of the results from multiple solutions (as well as from a single solution) are exported as into the file "weightedModel_(i).txt" and "nodeAttributes_(i).txt" for network structure and nodes' attributes, respectively. The exported DOT figure combines the results from multiple (and single) solutions can be open with e.g. [GraphViz](https://graphviz.gitlab.io/download) (an executable package on Mac OS is also available [here](https://download.cnet.com/Graphviz/3000-2054_4-50791.html)) and the results are also stored as an RData file for further analyses together with a log file. 

Additonal details on CARNIVAL results and troubleshooting sections can be found on the [Wiki page](https://github.com/saezlab/CARNIVAL/wiki).

### Enrichment analyses

CARNIVAL offers a pipeline to run a quick enrichment (over-representation) analysis using the curated gene set from MSigDB (C2) branch as presented in the article  (the GMT file from the other branches can also be used). The results from the CARNIVAL example 3 can be performed using the following code:

```R
library(CARNIVAL) # load CARNIVAL library

file.copy(from=system.file("Ex3_network_APAP_TGG_Omnipath.sif",package="CARNIVAL"),to=getwd(),overwrite=TRUE) # retrieve network file

# In case the network is in UniprotID format (e.g. from Omnipath), the node names need to be converted into gene symbol first for enrichment
universe <- mapUniprotPKN(netFile = "Ex3_network_APAP_TGG_Omnipath.sif",organism = 'human')

# Note: The mapped node names are also saved into the file nodes_PKN_uniprot_genesymbol.tsv. This file can be used as the 'universe' for over-representation analyses
enrichCARNIVAL(Result_dir="Results_CARNIVAL_Ex3",
				universeFile="nodes_PKN_uniprot_genesymbol.tsv",
				datasource='kegg',
				directionalORA=T,
				undirectionalORA=T,
				plot=T,
				pathwayfilter = T,
				pValSig = 0.05)

```

The enrichment results will be saved in the designated 'Result_dir' folder. All pathways with p-value < 0.5 will be included in a csv file and enrichment figures, both combined up- and down-regulated as well as the separated version can be selected for plotting (see options in the enrichCARNIVAL function).

### Running CARNIVAL on a parallelised cluster

To facilitate the running of parallelised jobs on a clustered computer, CARNIVAL also offers an option to set the tag-number for each individual run (currently up to two numbers, onto the variables "parallelIdx1" and "parallelIdx2" - set to 1 and 1 by default). This assighment prevents the overwriting of the linear integer (LP) constraints file as well as of the exported result file from CPLEX with the same filename. In this case, users has to prepare a separated (driver) R-script to load necessary libraries and assign inputs of the runCARNIVAL function which can then be called from bash/shell/command prompt. An example for setting up the pipeline using CARNIVAL Ex2 as an example is provided below.

```R
# --- Save the code below into an R-script e.g. runParallelCARNIVAL.R --- #

library(CARNIVAL) # load CARNIVAL library
library(doParallel) # load parallel job library
argsJob=commandArgs(trailingOnly = TRUE)
CARNIVAL_Result <- runCARNIVAL(CplexPath="~/Applications/IBM/ILOG/CPLEX_Studio1281/cplex/bin/x86-64_osx/cplex",
            Result_dir="Results_CARNIVAL_Ex2_Parallel",
            CARNIVAL_example=2,
	    parallelIdx1 = as.numeric(argsJob[1]),
            parallelIdx2 = as.numeric(argsJob[2])
	    )
```

After saving the code into an R-script, users can launch the pipeline with the command 'Rscript' on batch/shell/command prompt followed by the filename of the R-script and the two tag-numbers (here e.g. '2' and '3'). These numbers should be changed accordingly upon running the jobs in parallel on a cluster. Please refer to the documentation on how to run a parallelised job on the cluster that you are working with as each cluster has their own command/argument to submit and initiate the job.

```Console
>>> Rscript runParallelCARNIVAL.R 2 3
```

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
