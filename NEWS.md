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
