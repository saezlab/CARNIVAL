# Installation script for ebits

# install.packages('devtools')
library(devtools)
devtools::install_github('klmr/modules')

detach("package:oligo",unload=TRUE)

install.packages('modules')
library(modules)

# Now clone ebits from GitHub: either use the command below at the terminal or from GitHub Desktop
# cd your_local_directory
# git clone https://github.com/EBI-predocs/ebits.git

# Install additional packages from biocLite
BiocInstaller::biocLite('narray')
BiocInstaller::biocLite('rhdf5')

library(narray)
library(rhdf5)
# Now set your working directory in the ebits folder and test if you can load the module !!!with relative paths!!! 
ma = import('process/microarray')

# If it runs fine then you're ready to go ;)

