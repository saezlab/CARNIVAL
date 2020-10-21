#' default_CARNIVAL_options
#' 
#' generates default CARNIVAL options.  
#' 
#' @return returns a list with all possible options implemented in CARNIVAL.
#' see the documentation on \code{\link{CARNIVAL::runCARNIVAL}}.
#' @export
#' 

#TODO add Attila as authors

default_carnival_options = function(){
    
    opts <- list(solver_path=NULL,
         solver=c("lpSolve"), 
         timelimit=3600, 
         mip_gap=0.05,
         poolrel_gap=0.0001,
         limit_pop=500, 
         pool_cap=100,
         pool_intensity=4,
         pool_replace=2,
         alpha_weight=1, 
         beta_weight=0.2,
         threads = 1,
         max_memory_usage="8GB",
         dir_name=NULL
    )
    return(opts)
}



#' check_CARNIVAL_options
#' 
#' checks options provided for CARNIVAL
#' 
check_carnival_options <- function(opts){
    
    if(!is.list(opts)) stop("CARNIVAL options should be a list")
    req_names <- c(
        "solver_path",
        "solver", 
        "timelimit",
        "mip_gap",
        "poolrel_gap",
        "limit_pop", 
        "pool_cap",
        "pool_intensity",
        "pool_replace",
        "alpha_weight", 
        "beta_weight",
        "threads",
        "dir_name")
    
    if(!all(req_names %in% names(opts))){
        stop("CARNIVAL options should contain all options. 
             Start by calling default_carnival_options() and replace entries. ")
    }
    
    
    if(is.null(opts$solver_path)) stop("path to ILP solver must be provided")
    
}
