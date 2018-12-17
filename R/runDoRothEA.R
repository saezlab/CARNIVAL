#'\code{runDoRothEA}
#'
#' This function infers TF activities based on the DoRothEA regulon and the viper function.
#' The regulon can be filtered by confidence level (A-E).
#'
#' @param confidence_level A vector describing which confidence levels of the DoRothEA regulon to use
#' @param regulon DoRothEA regulon list
#' @param df A n*m dataframe describing the contrast t-statistics, where n is the number of genes and m the number of conditions.
#' @param write2file Path to outputfile for all TF_activities if desired
#'
#' @import tidyverse
#' @import viper
#'
#' @return A n*m dataframe describing the inferred TF activities, where n is the number of Tfs and m the number of conditions.
#'
#' @export

runDoRothEA<-function(df, regulon, confidence_level=c('A','B','C'), write2file = NULL){
  library(tidyverse)
  names(regulon) <- sapply(strsplit(names(viper_regulon), split = ' - '), head, 1)
  filtered_regulon <- regulon %>%
    map_df(.f = function(i) {
      tf_target = i$tfmode %>%
        enframe(name = "target", value="mor") %>%
        mutate(likelihood = i$likelihood)
    },.id = "tf")  %>%
    separate(tf, into=c("tf", "conf"), sep="_") %>%
    filter(conf %in% confidence_level) %>%
    arrange(tf)%>%
    split(.$tf) %>%
    map(function(dat) {
      tf = dat %>% distinct(tf) %>% pull()
      targets = setNames(dat$mor, dat$target)
      likelihood = dat$likelihood
      list(tfmode =targets, likelihood = likelihood)})

  TF_activities = as.data.frame(viper::viper(eset = df, regulon = filtered_regulon, nes = T, method = 'none', minsize = 4, eset.filter = F))
  if(!is.null(write2file)){write.csv2(TF_activities, file = write2file)}

    return(TF_activities)
}


