#' parse carnival logs
#' 
#' parses the CPLEX log file and reads some basic information
#' @param log path of log file resulted from a carnival run OR the content
#' of this file read by \link{\code[readr]{read_lines}}.
#' @return list variable with following fields:
#' - `convergence` a table that contains information on the convergence of CPLEX 
#' - `n_solutions` number of solutions found 
#' - `objective` objective function value
#' - `termination_reason`: reason of termination.
#' @importFrom readr read_lines

parse_CPLEX_log <- function(log){
    
    if(length(log) == 1 && file.exists(log)){
        lines <- readr::read_lines(log)    
    }else(
        lines = log
    )
    
    # There are possible multiple phases, each one with convergence tables. 
    
    parse_convergence = TRUE
    interrupted = FALSE
    
    n_phases = length(grep("Populate: phase ",lines))
    
    if(n_phases==0) {
        # for some very small cases there is no convergence reported. 
        parse_convergence = FALSE
    } else{
        parse_convergence = TRUE
    }
    
    
    if(parse_convergence){
        convergence_text <- lines
        
        # detect table headers: 
        table_headers = grep("^ +Nodes +Cuts",convergence_text)
        empty_lines = which(nchar(convergence_text)==0)
        
        parsed_table = list()
        
        for(i_table in 1:length(table_headers)){
            # select text from the phase
            convergence_text <- lines
            
            # table starts after second row of header and an empty line
            tbl_start = table_headers[i_table] +3
            
            # the table ends at the first empty line after it starts, unless 
            # CPLEX was interrupted:
            
            # we should check if CPLEX finished
            # if CPLEX was interrupted, then there is no empty space after 
            # the header. 
            
            if(!any(empty_lines > tbl_start)) {
                interrupted = TRUE
                tbl_end = length(convergence_text)
            }else{
                interrupted = FALSE
                tbl_end <- empty_lines[empty_lines > tbl_start][[1]]-1
            }
            
            convergence_text <- convergence_text[tbl_start:tbl_end]
            
            parsed_table[[i_table]] <- convergence_text_to_table(text = convergence_text)
            
        }
        
        phase_table <-  bind_rows(parsed_table)
        
        
    }else{
        phase_table = empty_convergence()
    }
    ## process final part:
    
    if(!interrupted){
        # number of solution
        solution_text <- grep("^Solution pool:",lines,value = TRUE)
        n_solutions =  regmatches(solution_text,regexpr("[0-9]+", solution_text)) %>%
            as.numeric()
        
        
        
        # termination
        termination_text <- grep("Populate - ",lines,value = TRUE)
        termination_reason <- 
            regmatches(termination_text,regexpr("-.*:", termination_text)) %>%
            substr(.,start = 3, stop = nchar(.)-1)
        
        objective = regmatches(termination_text,regexpr("=.*$", termination_text)) %>%
            substr(.,start = 2, stop = nchar(.)) %>% as.numeric()
    }else{
        n_solutions = 0
        objective = as.numeric(NaN)
        termination_reason = "CPLEX was interrupted"
    }
    
    diagnostics <- list(convergence = phase_table,
                        n_solutions = n_solutions,
                        objective = objective,
                        termination_reason = termination_reason)
}





parse_CPLEX_log_old <- function(log){
    
    if(length(log) == 1 && file.exists(log)){
        lines <- readr::read_lines(log)    
    }else(
        lines = log
    )
    
    ## Convergence table
    # Starts with Populate: phase I or II
    # then discard  and find first Nodes
    # lasts until Clique cuts
    
    parse_convergence = TRUE
    interrupted = FALSE
    
    n_phases = length(grep("Populate: phase ",lines))
    
    if(n_phases==0) {
        # for some very small cases there is no convergence reported. 
        parse_convergence = FALSE
    }
    if(n_phases>2) warning("parser assumes max. 2 phases. Convergence report is incomplete. Submit a Github issue if this is ever detected.")
    
    if(parse_convergence){
        convergence_text <- lines
        # Structure of a phase: 
        # - Phase #number is stated
        # - lines describing optimisation steps
        # - Convergence table
        # the convergence table starts and ends with an empty line
        # the header is also separated by an empty line
        ph_start <- grep("Populate: phase I ",convergence_text)
        
        # detect table headers: 
        table_headers = grep("^ +Nodes +Cuts",convergence_text)
        # table starts after second row of header and an empty line
        tbl_start = table_headers[table_headers>ph_start][[1]] +3
        
        # the end is the third empty line after the start of Phase: 
        empty_lines = which(nchar(convergence_text)==0)
        tbl_end <- empty_lines[empty_lines > tbl_start][[1]]-1
        
        convergence_text <- convergence_text[tbl_start:tbl_end]
        
        phase_table <- convergence_text_to_table(text = convergence_text)
        
        if(n_phases > 1){
            
            # sometimes more phases but only 1 table, then we skip processing 
            if(length(table_headers)  > 1 ){
                convergence_text <- lines
                ph_start <- grep("Populate: phase II ",convergence_text)
                
                table_headers = grep("^ +Nodes +Cuts",convergence_text)
                # table starts after second row of header and an empty line
                tbl_start = table_headers[table_headers>ph_start][[1]] +3
                
                empty_lines = which(nchar(convergence_text)==0)
                
                # we should check if CPLEX finished or reached memory limit. 
                # if CPLEX was interrupted, then there is no empty space after 
                # the header. 
                
                if(!any(empty_lines > tbl_start)) {
                    interrupted = TRUE
                    tbl_end = length(convergence_text)
                }else{
                    interrupted = FALSE
                    tbl_end <- empty_lines[empty_lines > tbl_start][[1]]-1
                }
                
                convergence_text <- convergence_text[tbl_start:tbl_end]
                phaseII_table <- convergence_text_to_table(text = convergence_text)
                
                phase_table <-  bind_rows(phase_table,phaseII_table)
                
            } 
            
            
        }
    }else{
        phase_table = empty_convergence()
    }
    ## process final part:
    
    if(!interrupted){
        # number of solution
        solution_text <- grep("^Solution pool:",lines,value = TRUE)
        n_solutions =  regmatches(solution_text,regexpr("[0-9]+", solution_text)) %>%
            as.numeric()
        
        
        
        # termination
        termination_text <- grep("Populate - ",lines,value = TRUE)
        termination_reason <- 
            regmatches(termination_text,regexpr("-.*:", termination_text)) %>%
            substr(.,start = 3, stop = nchar(.)-1)
        
        objective = regmatches(termination_text,regexpr("=.*$", termination_text)) %>%
            substr(.,start = 2, stop = nchar(.)) %>% as.numeric()
    }else{
        n_solutions = 0
        objective = as.numeric(NaN)
        termination_reason = "CPLEX was interrupted"
    }
    
    diagnostics <- list(convergence = phase_table,
                        n_solutions = n_solutions,
                        objective = objective,
                        termination_reason = termination_reason)
}


# converts the lines containing convergence description to table
convergence_text_to_table <- function(text){
    
    # atm we work with hard coded headers: 
    header <- c("Node", "Nodes Left", "Objective",  "IInf",
                "Best Integer", "Best Bound", "ItCnt", "Gap" )
    
    # remove header:
    text = text[-1:-2]
    
    # remove cuts
    cuts <- grep("Cuts",text)
    if(length(cuts>0)) text <- text[-cuts]
    # remove stars and plus sign
    marked <- grep("[+*]",text)
    if(length(marked)>0) text <- text[-marked]
    
    # remove node file size reports:
    node_file_index <-grep("Nodefile size",text)
    if(length(node_file_index)>0) text <- text[-node_file_index]
    
    # save the lines where elapsed time is reported: 
    time_line_index <- grep("Elapsed time",text)
    time_text = text[time_line_index]
    if(length(time_line_index)>0)   text <- text[-time_line_index]
    
    
    if(length(text)==0) return(empty_convergence())
    
    # split lines to values
    line_list <- strsplit(text," +")
    
    # remove lines with less length
    llength <- lapply(line_list, length) %>% unlist()
    
    
    phaseI_table <- line_list[llength==9] %>%
        do.call(rbind,.) %>% 
        as_tibble(.,.name_repair ="minimal")
    
    phaseI_table <- phaseI_table[,-1]
    names(phaseI_table) <- header
    phaseI_table <- phaseI_table %>% 
        mutate(Gap = gsub("%","",Gap)) %>%
        rename(`Gap [%]` = Gap) %>%
        mutate(across(everything(),as.numeric)) %>%
        mutate(Gap = Objective - `Best Bound`)
    
    time_table <- time_string_to_table(time_text)
    # time table is joint to the convergence table based on the position
    
    time_table$index = time_line_index - (1:length(time_line_index))+1
    
    result <- phaseI_table %>%
        mutate(index = 1:n()) %>%
        left_join(time_table,by = "index") %>%
        dplyr::select(-index)
    
}


empty_convergence <- function(){
    data.frame(Node= numeric(),
           `Nodes Left`= numeric(),
           Objective = numeric(),
           IInf = numeric(),
           `Best Integer` = numeric(),
           `Best Bound`= numeric(),
           ItCnt = numeric(),
           Gap = numeric() )
    
}
# add elapsed time info
time_string_to_table <- function(time_text){
    
    if(length(time_text)>0){
        raw_split <- time_text %>% str_split(.," ")
        time_table = lapply(raw_split, function(x)x[c(4,6,10,14)]) %>% 
            do.call(rbind,.) %>%
            as_tibble(,.name_repair ="minimal")
        names(time_table) = c("Elapsed time [s]", "ticks","Tree size [MB]","Solutions")
        
        result <- time_table %>% mutate(ticks = gsub("(","",ticks,fixed = TRUE),
                                        Solutions = gsub(")","",Solutions,fixed = TRUE)) %>%
            mutate(across(everything(),as.numeric))    
    }else{
        
        result <- tibble(`Elapsed time [s]`=numeric(),
                         ticks = numeric(),
                         `Tree size [MB]`=numeric(),
                         `Solutions`= numeric())
    }
    return(result)
    
}


### Test
# res = parse_CPLEX_log("./carnival_logs/carnival_logs/case_study_1/107.out")
# res = parse_CPLEX_log(log_file = "./carnival_logs/carnival_logs/case_study_1/1.out")
# 
# files = list.files("./carnival_logs/carnival_logs/case_study_1/",pattern = "*.out",full.names = TRUE)
