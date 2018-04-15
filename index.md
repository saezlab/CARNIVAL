---
layout: default
title: Home
---


# PHONEMeS

## Overview

PHONEMeS (**PHO**sphorylation **NE**tworks for **M**ass **S**pectrometry) is a method to model signaling networks based on untargeted phosphoproteomics mass spectrometry data and kinase/phosphatase-substrate interactions.

This package contains the R package and accompanying scripts that implement the method as well as several examples of how to run a PHONEMeS analysis.

The input for PHONEMeS consists of phosphoproteomic data after treatment with kinase inhibitors. Gaussian mixture modeling is then used to find phosphosites that exhibit a naturally Boolean behaviour with two populations, representing a control and a perturbed state. The data are mapped unto a kinase/phosphatase-substrate network taken from several dedicated databases. PHONEMeS then optimizes the network and extracts possible paths connecting inhibited kinases and perturbed phosphosites by iteratively sampling edges from the background network, simulating the logic model, and finally evaluating the network by comparison to the data. In each iteration, sampling weights are corrected based on the edge frequencies in the best models.

<img src="/PHONEMeS/public/network.png" alt="Example network">

<center><i>Example output of PHONEMeS analysis after MTOR inhibition</i></center>

## References

Please use this reference to cite PHONEMeS:

> Terfve, C. D. A., Wilkes, E. H., Casado, P., Cutillas, P. R., and Saez-Rodriguez, J. (2015). [Large-scale models of signal propagation in human cells derived from discovery phosphoproteomic data.](http://www.nature.com/articles/ncomms9033) _Nature Communications_, **6**:8033.

```
@article{Terfve2015,
         author = {Terfve, Camille D A and Wilkes, Edmund H and 
                   Casado, Pedro and Cutillas, Pedro R and 
                   Saez-Rodriguez, Julio},
         doi = {10.1038/ncomms9033},
         issn = {2041-1723},
         journal = {Nature communications},
         month = {jan},
         pages = {8033},
         pmid = {26354681},
         title = {{Large-scale models of signal propagation in human cells derived from discovery phosphoproteomic data.}},
         url = {http://www.ncbi.nlm.nih.gov/pubmed/26354681},
         volume = {6},
         year = {2015}
         }
```

### Other References

+ Description of parts of the data:

  > Wilkes, E. H., Terfve, C., Gribben, J. G., Saez-Rodriguez, J., and Cutillas, P. R. (2015). [Empirical inference of circuitry and plasticity in a kinase signaling network.](http://www.pnas.org/content/112/25/7719.abstract) _Proceedings of the National Academy of Sciences of the United States of America_, **112**(25):7719–24.
