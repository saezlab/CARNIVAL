---
layout: default
title: Home
---


# CARNIVAL

## Overview

CARNIVAL (**CA**usal **R**easoning for **N**etwork identification using **I**nteger **VAL**ue programming) is a method for the identification of drug-specific activated pathways form Gene Expression (GEX) Data or inferred Transcription Factor (TF) Activities.

This is a tool currently being developed by the [SaezLab](http://saezlab.org/) members and is an extension of the previously implemented Causal Reasoning([*Melas et al.*](http://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f)) method.

The input for CARNIVAL consists of:

 * Discretized TF activities which can be inferred from GEX data through the [DoRothEA](https://github.com/saezlab/DoRothEA) method.
 
 * A list of perturbation targets for a specific drug/drug combination and their effects on each of them.
 
 * A Prior Knowledge Network (as a SIF file) containing a complete and comprehensive list of signed and directed interactions between proteins.
 
 * [PROGENy](https://github.com/saezlab/progeny) scores representing magnitudes of inferred pathway activities from GEX data.
 
<img src="/CARNIVAL/public/workflow2.png" alt="CARNIVAL workflow">

<center><i>Inputs and Outputs of CARNIVAL</i></center>


The main idea behind CARNIVAL consist on identifying a subset of interactions from a Prior Knowledge Network that represent possible regulated pathways linking drug perturbation targets to active Transcription Factors. An Integer Linear Programming (ILP) approach (based on a formulation from [*Melas et al.*](http://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1003204)), is being implemented to model the rules of signal trunsduction from one element of the network to the other. Applications include the identification of drug’s modes of action and deregulated processes in a disease by confirming alterations of main signalling pathways and off-taget effects.

## References

> Melas I.N. et al. [Identification of drug-specific pathways based on gene expression data: application to drug induced lung injury](http://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) _Integrative Biology, 2015, 7, 904_.

> Garcia-Alonso L. et al. [Transcription Factor Activities Enhance Markers of Drug Sensitivity in Cancer.](http://cancerres.aacrjournals.org/content/78/3/769.full) _Cancer Res. 2018 Feb 1;78(3):769-780. doi: 10.1158/0008-5472.CAN-17-1679. Epub 2017 Dec 11._

> Schubert M. et al. [Perturbation-response genes reveal signaling footprints in cancer gene expression](https://www.nature.com/articles/s41467-017-02391-6) _Nature Communicationsvolume 9, Article number: 20 (2018) doi:10.1038/s41467-017-02391-6_
