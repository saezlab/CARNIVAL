#'\code{mapUniprotPKN}
#'
#' This function generate a file containing all nodes in the prior knowledge network with UniprotID to be assigned as universe for enrichment analysis
#'
#' @param netFile Prior knowledge network file in Uniprot format
#' @param organism organism that the Uniprot IDs refer to ('human','mouse','rat'; default='human')
#'
#' @import UniProt.ws
#' @import dplyr
#' @import tidyr
#'
#' @return An object containing all PKN nodes (universe) in Gene Symbol format written to a file for enrichment analyses
#'
#' @export

mapUniprotPKN<-function(netFile=NULL, organism='human'){
  
  require(UniProt.ws)
  require(dplyr)
  require(tidyr)

  if (is.null(netFile)) {stop("Please provide the prior knowledge network file in the Uniprot ID format")}
  if (organism=="human") {taxId <- 9606
  } else if (organism=="mouse") {taxId <- 10090 
  } else if (organism=="rat") {taxId <- 10116}
  
  # --- Will take awhile to update (10-15 minutes) --- #
  PKN <- read.table(file = netFile,header = T,sep = "\t",stringsAsFactors = F)
  # Check the length of node names (UniprotIDs have asscension pattern with 6 or 10 characters: https://www.uniprot.org/help/accession_numbers)
  if (unique(nchar(PKN[which(nchar(PKN[,1])==min(nchar(PKN[,1]))),1]))<6 | unique(nchar(PKN[which(nchar(PKN[,1])==max(nchar(PKN[,1]))),1]))>10) {warning("Please ensure that the node name in PKN is in the UniprotID format")}
  network <- data.frame(uniprot=sort(unique(c(PKN[,1],PKN[,3]))),stringsAsFactors = F)
  
  print(paste0("Retrieving Uniprot database for : ",organism," - could take 5-15 minutes depending on the internet connection..."))
  up <- UniProt.ws(taxId = taxId)
  
  print(paste0("Mapping Uniprot to gene symbol (taking only the first official one)"))
  up_xref <- UniProt.ws::select(up, keys = network$uniprot , columns = c("GENES"), keytype = "UNIPROTKB") #Map
  up_xref <- up_xref %>% separate(GENES, into=c("GeneSymbol", "AltGeneSymbols"), sep=" ", remove=FALSE, extra="merge", fill="right") #Keep only first Gene
  network$genesymbol<-up_xref$GeneSymbol
  
  write.table(network, "nodes_PKN_uniprot_genesymbol.tsv", row.names = F,sep="\t",quote = F,col.names = T)
  
  print('The mapping of UniprotIDs to Gene Symbols is complete')
  print('The mapped results are saved into the file nodes_PKN_uniprot_genesymbol.tsv and will be compatible with the function enrichCARNIVAL()')
  
  return(network)
}
