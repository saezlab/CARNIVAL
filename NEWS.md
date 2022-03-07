# CARNIVAL 2.3.0
* bug fix: parsing the output of ILP solvers were incorrect, especially 
building the nodesAttributes. The bug resulted in inconsistencies between the 
reported nodes and interactions
* bug fix: empty workdir and outputFolder resulted in unhandled errors. From now on,
current working directory is used for temporary and result files if not specified otherwise in the options
* bug fix: the function defaultCbcSolveCarnivalOptions() had a undefined variable. 
* bug fix: lpSolve has a non-negativity constraint which was ignored so far. Now this issue is handled. 
lpSolve can be used for small networks and in tests. 
* tests added: added many small-medium, general tests for lpSolve, CPLEX and CbC

# CARNIVAL 2.2.0
* Changed API: introduced separate functions for different flavours of CARNIVAL.
* A more convenient way to work with CARNIVAL parameters. Parameters setup is possible through jsons and function calls.
* Better file naming to prevent concurrent file writing when running several instances of CARNIVAL.
* An easy way to add more solvers.
* A possibility to tune CARNIVAL setups for cplex: manual addition of parameters is possible through jsons.
* General improvements and refactoring of the code, removed multiple duplications.
* Removed multiple experimental conditions in a matrix form (was not used).
* Inputs are transformed to vectors (except prior knowledge network).
* MetaInfo is saved: runId, parsed data (internal data representation), start time, CARNIVAL flavour.
* Reading from preparsed data and/or lp file is possible.

# CARNIVAL 1.0.0
* Official release with Bioconductor 3.11

# CARNIVAL 0.99.44
* Deployment of GitHub page

# CARNIVAL 0.99.0
* Initial submission to Bioconductor
