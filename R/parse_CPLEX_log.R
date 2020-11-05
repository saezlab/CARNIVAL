#' parse carnival logs
#' 
#' parses the CPLEX log file and reads some basic information
#' @param log_file path of log file resulted from a carnival run
#' @return list variable with following fields:
#' - `convergence` a table that contains information on the convergence of CPLEX 
#' - `n_solutions` number of solutions found 
#' - `objective` objective function value
#' - `termination_reason`: reason of termination.
#' @importFrom readr read_lines
#' @importFrom stringr str_detect

parse_CPLEX_log <- function(log_file){
    
    lines <- readr::read_lines(log_file)
    
    ## Convergence table
    # Starts with Populate: phase I or II
    # then discard  and find first Nodes
    # lasts until Clique cuts
    
    n_phases = length(which(stringr::str_detect(convergence_text,"Clique cuts ")))
    if(n_phases>2) warning("parser assumes max. 2 phases. Convergence report is incomplete.")
    
    convergence_text <- lines
    start <- which(stringr::str_detect(convergence_text,"Populate: phase I "))
    end <-  which(stringr::str_detect(convergence_text,"Clique cuts "))[[1]]-1
    convergence_text <- convergence_text[start:end]
    start <- which(nchar(convergence_text)==0)[[1]] + 2 
    convergence_text <- convergence_text[start: length(convergence_text) ]
    
    phase_table <- convergence_text_to_table(text = convergence_text)
    
    if(n_phases > 1){
        convergence_text <- lines
        start <- which(stringr::str_detect(convergence_text,"Populate: phase II "))
        end <-  which(stringr::str_detect(convergence_text,"Clique cuts "))[[2]]-1
        convergence_text <- convergence_text[start:end]
        start <- which(nchar(convergence_text)==0)[[1]] + 2 
        convergence_text <- convergence_text[start: length(convergence_text) ]
        
        phaseII_table <- convergence_text_to_table(text = convergence_text)
        
        phase_table <-  bind_rows(phase_table,phaseII_table)
    }
    ## process final part:
    
    # number of solution
    solution_text <- grep("^Solution pool:",lines,value = TRUE)
    n_solutions = str_extract(solution_text,"[0-9]+") %>% as.numeric()
    
    # termination
    termination_text <- grep("Populate - ",lines,value = TRUE)
    termination_reason <- str_extract(termination_text,"-.*:") %>%
        str_sub(.,start = 3, end = nchar(.)-1)
    
    objective = str_extract(termination_text,"=.*$") %>%
        str_sub(.,start = 2, end = nchar(.))%>% as.numeric()
    
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
    cuts <- which(stringr::str_detect(text,"Cuts"))
    if(length(cuts>0)) text <- text[-cuts]
    # remove stars and plus sign
    marked <- which(stringr::str_detect(text,"[+*]"))
    if(length(marked)>0) text <- text[-marked]
    
    # remove node file size reports:
    node_file_index <- which(stringr::str_detect(text,"Nodefile size"))
    if(length(node_file_index)>0) text <- text[-node_file_index]
    
    # save the lines where elapsed time is reported: 
    time_line_index <- which(stringr::str_detect(text,"Elapsed time"))
    time_text = text[time_line_index]
    if(length(time_line_index)>0)   text <- text[-time_line_index]
    
    # split lines to values
    line_list <- strsplit(text," +")
    
    # remove lines with less length
    llength <- lapply(line_list, length) %>% unlist()
    
    
    phaseI_table <- line_list[llength==9] %>%
        do.call(rbind,.) %>% 
        as_tibble(,.name_repair ="minimal")
    
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
        select(-index)
    
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
