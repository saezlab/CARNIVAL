## This function converts the gene symbol rownames of a dataframe to uniprot 
## rownames. In case one gene symbol maps to two uniprot IDs, the row will be 
## duplicated.
## 
## Panuwat Trairatphisan, 2020

GeneSymbol2Uniprot=function(df, map, geneID=1, uniprotID=2){
  
  df$genesymbols_for_mapping<-rownames(df)
  colnames(map)[geneID]<-'genesymbols_for_mapping'
  colnames(map)[uniprotID]<-'uniprotids_for_mapping'
  df<-dplyr::left_join(df, map, by ="genesymbols_for_mapping")
  rownames(df)<-df$uniprotids_for_mapping
  df<-df%>%dplyr::select(-uniprotids_for_mapping,-genesymbols_for_mapping)
  
  return(df)
  
}