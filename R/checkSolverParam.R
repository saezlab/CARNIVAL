## Checking solver parameters
##
## Enio Gjerga, 2020

checkSolverParam <- function(timelimit=timelimit,
                             mipGAP=mipGAP,
                             poolrelGAP=poolrelGAP,
                             limitPop=limitPop,
                             poolCap=poolCap,
                             poolIntensity=poolIntensity,
                             poolReplace=poolReplace,
                             threads=threads,
                             alphaWeight=alphaWeight,
                             betaWeight=betaWeight){
  
  returnList = NULL
  parallelIdx1=1
  parallelIdx2=1
  if(!is.numeric(parallelIdx1) | !is.numeric(parallelIdx2)){
    stop("Please set numbers on the parameters 'parallelIdx1' and 'parallelIdx2' 
         for running CARNIVAL in parallelisation ")
  } else {
    if(parallelIdx1==1 & parallelIdx2==1) { # default case
      repIndex=1;condition=1
    } else {
      condition=parallelIdx1;repIndex=parallelIdx2
    }
    
    returnList = list()
    returnList[[length(returnList)+1]] = condition
    returnList[[length(returnList)+1]] = repIndex
    names(returnList) = c("condition", "repIndex")
  }
  
  if(!is.numeric(timelimit)){
    stop("CPLEX parameter: Please set a time limit for CPLEX optimisation in 
         seconds")
  }
  
  if (!is.null(mipGAP)){
    if(!is.numeric(mipGAP)){stop("CPLEX parameter: Please set the allowed 
                                  mipGAP parameter or leave it as NULL for CPLEX 
                                  default value (1e-04)")
    }
  }
  
  if (is.null(mipGAP)){
    mipGAP=1e-04
  }
  
  if(!is.null(poolrelGAP)){
    if(!is.numeric(poolrelGAP)){
      stop("CPLEX parameter: Please set the allowed pool relative GAP parameter 
           or leave it as NULL for CPLEX default value (1e75)")
    }
  }
  
  if(is.null(poolrelGAP)){
    poolrelGAP=1e75
  }
  
  if(!is.null(limitPop)){
    if(!is.numeric(limitPop)){
      stop("CPLEX parameter: Please set the allowed population limit of solution 
           to be generated or leave it as NULL for CPLEX default value (20)")
    }
  }
  
  if(is.null(limitPop)){
    limitPop=20
  }
  
  if(!is.null(poolCap)){
    if(!is.numeric(poolCap)){
      stop("CPLEX parameter: Please set the allowed number of solutions to be 
           kept or leave it as NULL for CPLEX default value (2.1e9)")
    }
  }
  
  if(is.null(poolCap)){
    poolCap=2.1e9
  }
  
  if (!is.null(poolIntensity)){
    if(!(poolIntensity %in% c(0,1,2,3,4))){
      stop("CPLEX parameter: Please set the level of intensity for solution 
           searching [0,1,2,3,4] or leave it as NULL for CPLEX default value (0)
           - to be decided by CPLEX")
    }
  }
  
  if(is.null(poolIntensity)){
    poolIntensity=0
  }
  
  if(!is.null(poolReplace)){
    if(!(poolReplace %in% c(0,1,2))){
      stop("CPLEX parameter: Please set the replacement strategy of solution 
           [0,1,2] or leave it as NULL for CPLEX default value (0) - First In 
           First Out")
    }
  }

  if(!is.null(threads)){
    if(!(threads >= 0)){
      stop("CPLEX parameter: Please set the number of threads to 0 for automatic
           detection or any value > 0 for a specific number of threads.")
    }
  }

  if(is.null(poolReplace)){
    poolReplace=0
  }
  
  if(!is.numeric(alphaWeight)){
    stop("Objective Function: Please set a weight for mismatch penalty (will be 
         applied only when the weight of measurement is not defined)")
  }
  
  if(!is.numeric(betaWeight)){
    stop("Objective Function: Please set a weight for node penalty")
  }

  return(returnList)
}