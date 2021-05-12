Below you will find a detailed description of parameters for each of the solver
{ 
    "solverPath": "solverPath", #cplex solver (binary file) path
    "lpFilename": "", #the path to LP file if you already have it
    "cplexCommandFilename": "", 
    "timelimit": 3600, #parameter of cplex
    "betaWeight": 0.2, #parameter of CARNIVAL: betaweight is used to penalise for a network size
    "threads": 0, #cplex parameter: how many threads to use 
	"clonelog": 1, #cplex paramter: if clonelog files should be created
	"workdir": "",
	"cplexMemoryLimit": 8192,
    "cleanTmpFiles": "TRUE",
    "keepLPFiles": "TRUE",
    "dirName": "NULL",
    "mipGap": 0.05,
    "poolrelGap": 0.0001,
    "limitPop": 500,
    "poolCap": 100,
    "poolIntensity": 4,
    "poolReplace": 2
}