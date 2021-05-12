#' Parses the cplex log file and reads some basic information.
#' 
#' @param log path of log file resulted from a carnival run OR the content
#' of this file read by \code{\link[readr]{read_lines}}.
#' 
#' @return list variable with following fields:
#' - `convergence` a table that contains information on the convergence of CPLEX 
#' - `n_solutions` number of solutions found 
#' - `objective` objective function value
#' - `termination_reason`: reason of termination
#' 
#' @author Attila Gabor, 2021
#' @importFrom readr read_lines
parseCplexLog <- function(log) {
    
    if (length(log) == 1 && file.exists(log)) {
        lines <- readr::read_lines(log)    
    } else (
        lines <- log
    )
    
    # There are possible multiple phases, each one with convergence tables. 
    parse_convergence <- TRUE
    interrupted <- FALSE
    
    n_phases <- length(grep("Populate: phase ", lines))
    
    if (n_phases == 0) {
        # for some very small cases there is no convergence reported. 
        parse_convergence <- FALSE
    } else {
        parse_convergence <- TRUE
    }
    
    if (parse_convergence) {
        convergence_text <- lines
        
        # detect table headers: 
        table_headers <- grep("^ +Nodes +Cuts",convergence_text)
        empty_lines <- which(nchar(convergence_text) == 0)
        
        parsed_table <- list()
        
        for (i_table in 1:length(table_headers)) {
            # select text from the phase
            convergence_text <- lines
            
            # table starts after second row of header and an empty line
            tbl_start <- table_headers[i_table] + 3
            
            # the table ends at the first empty line after it starts, unless 
            # cplex was interrupted:
            
            # we should check if cplex finished
            # if cplex was interrupted, then there is no empty space after 
            # the header. 
            if (!any(empty_lines > tbl_start)) {
                interrupted <- TRUE
                tbl_end <- length(convergence_text)
            } else {
                interrupted <- FALSE
                tbl_end <- empty_lines[empty_lines > tbl_start][[1]] - 1
            }
            
            convergence_text <- convergence_text[tbl_start:tbl_end]
            parsed_table[[i_table]] <- convergenceTextToTable(text = convergence_text)
            
        }
        
        phase_table <- dplyr::bind_rows(parsed_table)
        
    } else {
        phase_table <- emptyConvergence()
    }
    
    ## process the final part:
    if (!interrupted) {
        # number of solution
        solution_text <- grep("^Solution pool:", lines, value = TRUE)
        n_solutions <-  regmatches(solution_text,regexpr("[0-9]+", solution_text)) %>%
            as.numeric()
        
        # termination
        termination_text <- grep("Populate - ", lines, value = TRUE)
        termination_reason <- 
            regmatches(termination_text,regexpr("-.*:", termination_text)) %>%
            substr(., start = 3, stop = nchar(.) - 1)
        
        objective <- regmatches(termination_text,regexpr("=.*$", termination_text)) %>%
            substr(., start = 2, stop = nchar(.)) %>% as.numeric()
    } else {
        n_solutions <- 0
        objective <- as.numeric(NaN)
        termination_reason <- "CPLEX was interrupted"
    }
    
    diagnostics <- list(convergence = phase_table,
                        n_solutions = n_solutions,
                        objective = objective,
                        termination_reason = termination_reason)
    return(diagnostics)
}

# converts the lines containing convergence description to a table
convergenceTextToTable <- function(text) {
    
    # atm we work with hard coded headers: 
    header <- c("Node", "Nodes Left", "Objective",  "IInf",
                "Best Integer", "Best Bound", "ItCnt", "Gap" )
    
    # remove header:
    text <- text[-1:-2]
    
    # remove cuts
    cuts <- grep("Cuts", text)
    if (length(cuts > 0)) text <- text[-cuts]
    # remove stars and plus sign
    marked <- grep("[+*]",text)
    if (length(marked) > 0) text <- text[-marked]
    
    # remove node file size reports:
    node_file_index <- grep("Nodefile size",text)
    if (length(node_file_index) > 0) text <- text[-node_file_index]
    
    # save the lines where elapsed time is reported: 
    time_line_index <- grep("Elapsed time",text)
    time_text <- text[time_line_index]
    if (length(time_line_index) > 0) text <- text[-time_line_index]
    
    if (length(text) == 0) return(emptyConvergence())
    
    # split lines to values
    line_list <- strsplit(text," +")
    
    # remove lines with less length
    llength <- lapply(line_list, length) %>% unlist()
    
    phaseI_table <- line_list[llength == 9] %>%
        do.call(rbind,.) %>% 
        dplyr::as_tibble(., .name_repair = "minimal")
    
    phaseI_table <- phaseI_table[,-1]
    names(phaseI_table) <- header
    phaseI_table <- phaseI_table %>% 
        dplyr::mutate(Gap = gsub("%","", Gap)) %>%
        dplyr::rename(`Gap [%]` = Gap) %>%
        dplyr::mutate(across(everything(),as.numeric)) %>%
        dplyr::mutate(Gap = Objective - `Best Bound`)
    
    time_table <- timeStringToTable(time_text)
    # time table is joint to the convergence table based on the position
    
    time_table$index <- time_line_index - (1:length(time_line_index)) + 1
    
    result <- phaseI_table %>%
        dplyr::mutate(index = 1:n()) %>%
        dplyr::left_join(time_table,by = "index") %>%
        dplyr::select(-index)
    
    return(result)
}

emptyConvergence <- function(){
    data.frame(Node = numeric(),
              `Nodes Left` = numeric(),
              Objective = numeric(),
              IInf = numeric(),
              `Best Integer` = numeric(),
              `Best Bound` = numeric(),
              ItCnt = numeric(),
              Gap = numeric())
}

# add elapsed time info
timeStringToTable <- function(time_text){
    if (length(time_text) > 0) {
        raw_split <- time_text %>% stringr::str_split(., " ")
        time_table <- lapply(raw_split, function(x) x[c(4,6,10,14)] ) %>% 
                                do.call(rbind,.) %>%
                                dplyr::as_tibble(, .name_repair = "minimal")
        names(time_table) <- c("Elapsed time [s]", "ticks","Tree size [MB]","Solutions")
        
        result <- time_table %>% 
                        dplyr::mutate(ticks = gsub("(","", ticks, fixed = TRUE),
                                      Solutions = gsub(")","", Solutions, fixed = TRUE)) %>%
                        dplyr::mutate(across(everything(),as.numeric))    
    } else {
        result <- dplyr::tibble(`Elapsed time [s]` = numeric(),
                                ticks = numeric(),
                                `Tree size [MB]` = numeric(),
                                `Solutions` = numeric())
    }
    
    return(result)
}
