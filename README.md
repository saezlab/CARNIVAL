# CARNIVAL (CAusal Reasoning for (signalling) Network identification with Integer VALue programming)

A re-implementation of the causal reasoning pipeline published by Melas et al. 2015 

## Requirement
- R 
- Cplex (already included in the tool)

## Input
1) Network topology - in SIF format (source [tab] 1/-1 [tab] target)
2) Measured downstream targets - first line: list of measured gene expressions separated by tab, from second lines: measured expression value (1/-1) 
3) Perturbed upstream targets - first line: list of perturbed signalling molecules separated by tab, from second lines: effect of perturbation (1/-1/NA)

## Output
1) Network topology (SIF format [+activity flow network, to be implemented])
2) List of predicted protein activities
