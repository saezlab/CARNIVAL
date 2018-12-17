#'\code{GeneSymbol2Uniprot}
#'
#' This function converts the gene symbol rownames of a dataframe to uniprot rownames. In case one gene symbol maps to two uniprot IDs, the row will be duplicated.
#'
#' @param df A vector of genes.
#' @param map A dataframe with a uniprot ID column and a genesymbol column.
#' @param geneID Column index of the gene symbol column in map.
#' @param uniprotID Column index of the uniprot column in map.
#'
#' @import tidyverse
#'
#' @return A vector of genes or uniprot IDs.
#'
#' @export


GeneSymbol2Uniprot=function(df, map, geneID=1, uniprotID=2){

  df$genesymbols_for_mapping<-rownames(df)
  colnames(map)[geneID]<-'genesymbols_for_mapping'
  colnames(map)[uniprotID]<-'uniprotids_for_mapping'
  df<-dplyr::left_join(df, map)
  rownames(df)<-df$uniprotids_for_mapping
  df<-df%>%dplyr::select(-uniprotids_for_mapping,-genesymbols_for_mapping)
}

