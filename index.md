---
layout: default
title: Home
---


# CARNIVAL

## Overview

CARNIVAL (**CA**usal **R**easoning for **N**etwork identification using **I**nteger **VAL**ue programming) is a method for the identification of upstream reguatory signalling pathways from downstream gene expression (GEX).

This is a tool currently being developed by the [SaezLab](http://saezlab.org/) members and is an extension of the previously implemented Causal Reasoning([*Melas et al.*](http://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f)) method. 

The aim of the CARNIVAL pipeline is to identify a subset of interactions from a prior knowledge network that represent potential regulated pathways linking known or potential targets of perturbation towards active transcription factors derived from GEX data. The pipeline includes a number improved functionalities comparing to the original version: Transcription factors’ (TFs) activities and pathway scores from gene expressions can be inferred with our in-house tools DoRothEA & PROGENy, respectively. TFs’ activities and signed directed protein-protein interaction networks with or without the provided target of perturbations and pathway scores are then used to derive a series of linear constraints to generate integer linear programming (ILP) problems. An ILP solver (IBM ILOG CPLEX) is subsequently applied to identify the sub-network topology with minimised fitting error and model size.

Applications of CARNIVAL include the identification of drug’s modes of action and of deregulated processes in diseases (even if the molecular targets remain unknown) by deciphering the alterations of main signalling pathways as well as alternative pathways and off-target effects.

<img src="/CARNIVAL/public/CARNIVAL_Workflow_Dec2018.png" alt="CARNIVAL workflow">

<center><i>Inputs and Outputs of CARNIVAL</i></center>
<br>

The input for CARNIVAL consists of:

 * A prior knowledge network (PKN) comprises a list of signed and directed interactions between signalling proteins. (Required)

 * Inferred transcription factor activities which can be inferred from GEX data using [DoRothEA](https://github.com/saezlab/DoRothEA). (Required)
 
 * A list of target of perturbations (drugs, diseases, etc.) with or without their effects on signalling proteins. (Optional) 
 
 * Inferred pathway scores representing signalling pathway activities from GEX data using[PROGENy](https://github.com/saezlab/progeny) (Optional)

The outcome of CARNIVAL includes the list of identified networks that fitted to the provided experimental data as well as the predicted activities of signalling proteins in the networks whether they are up- or down-regulated.

## References

> Melas I.N. et al. [Identification of drug-specific pathways based on gene expression data: application to drug induced lung injury](http://pubs.rsc.org/en/content/articlehtml/2015/ib/c4ib00294f) _Integrative Biology, 2015, 7, 904_.

> Garcia-Alonso L. et al. [Transcription Factor Activities Enhance Markers of Drug Sensitivity in Cancer.](http://cancerres.aacrjournals.org/content/78/3/769.full) _Cancer Res. 2018 Feb 1;78(3):769-780. doi: 10.1158/0008-5472.CAN-17-1679. Epub 2017 Dec 11._

> Schubert M. et al. [Perturbation-response genes reveal signaling footprints in cancer gene expression](https://www.nature.com/articles/s41467-017-02391-6) _Nature Communicationsvolume 9, Article number: 20 (2018) doi:10.1038/s41467-017-02391-6_
