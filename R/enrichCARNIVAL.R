#'\code{enrichCARNIVAL}
#'
#' This function performs the enrichment of CARNIVAL nodes with over-representation analysis using the curated gene set from MSigDB (C2 branch)
#' The enrichment can be performed using the KEGG, Biocarta or Reactome subsets or using the whole curated C2 geneset.
#' Note: The universe of the geneset should be all the nodes in the prior knowledge network. Here the nodes from Omnipath are used by default.
#'
#' @param Result_dir The Result CARNIVAL folder to perform enrichment analyses
#' @param universeFile The file containing the list of nodes/genes (with the header(s) 'genesymbol' +/- 'uniprot') to be considered as universe in enrichment analyses
#' @param networkID Index of the column with identifiers used in the network in the universeFile
#' @param universeID Index of the column with the genesymbol identifiers in the universeFile
#' @param datasource The data source on MSigDB C2 branch: c("kegg":default,"biocarta","reactome","allc2")
#' @param datapath Path to GMT file if not specified by datasource 
#' @param directionalORA If TRUE directional over-representation analysis is performed
#' @param undirectionalORA If TRUE undirectional over-representation analysis is performed
#' @param plot If TRUE plots are generated
#' @param topEnrichAll Number of top enrichmed pathways to be plotted (default: 40)
#' @param pathwayfilter Filtering pathways which are related to cancers, infections and diseases to highlight cellular processes
#' @param pValSig Significant p-value to be labelled on the plotted figures
#'
#' @import GSEABase
#' @import dplyr
#' @import Category
#' @import ggplot2
#'
#' @return Enriched pathways with p-value written into files and figures showing top enriched pathways for combined Up/Down direction and separated
#'
#' @export

 enrichCARNIVAL<-function(Result_dir="Results_CARNIVAL",universeFile=NULL, networkID=1, universeID= 2, datapath=NULL, datasource=NULL,directionalORA=T, undirectionalORA=T, plot=F, topEnrichAll=40,pathwayfilter=T,pValSig=0.05){
  
  # Load required libraries
  require(GSEABase)
  require(dplyr)
  require(Category)
  require(ggplot2)
  
  #### Input check ####
  if (is.null(universeFile)) {stop("Please provide a set of genes to be used as the universe for enrichment analyses")}
  map<-read.table(universeFile, header = T,stringsAsFactors = F)
  universe<-map[, universeID]
  datasourcefile<-NULL
  if(is.null(universe)){stop("Please provide the set of genes to be used as universe for enrichment analyses with 'genesymbol' as the header")}
  if (sum(is.na(universe))>0) {universe <- universe[-which(is.na(universe),arr.ind = T)]}
  
  if (!is.null(datasource)){
    if (datasource %in% c("biocarta","reactome", "kegg")){
      datasourcefile<-paste0("c2.cp.",datasource,".v6.2.symbols.gmt")
    }else if (datasource=="allc2"){
      datasourcefile<-"source_enrichment/c2.cp.v6.2.symbols.gmt"
    }else {
      stop("Please provide a valid datasource, or leave it as NULL and provide a datapath instead.")
    } 
  }
  
  if(is.null(datasourcefile)){
    if(!is.null(datapath)){
      datasourcefile<-datapath
    }else{stop("Please provide a datasource or datapath.")}
 }
  
  file.copy(from=system.file(datasourcefile,package="CARNIVAL"), to=getwd() ,overwrite=TRUE) # retrieve network file
test<-getGmt(datasourcefile,
collectionType=BroadCollection(category="c2"),
geneIdType=SymbolIdentifier())
file.remove(datasourcefile)



set<-read.delim(paste0(Result_dir,"/nodesAttributes_1.txt"))
if (undirectionalORA ==T){
  #Is the colname in the nodesAttributes file fixed to uniprot? Then just changing the header of the mapping file might be easier
  if (length(map[,networkID])>0) {
    #    colnames(set)[which(colnames(set)=='Node')]<-'uniprot'
    colnames(set)[which(colnames(set)=='Node')]<-colnames(map)[networkID]
    set_init <- set
    #    set<-set_init%>%left_join(map, by='uniprot')%>%
    #      filter(AvgAct!=0)%>%
    #      filter(NodeType!='S')  #Comment this out
    set<-set_init%>%left_join(map, by=colnames(map)[networkID])%>%
      filter(AvgAct!=0)%>%
      filter(NodeType!='S')  #Comment this out
    genes<-set$gene
    
  } else {
    
    set_init <- set
    set<-set_init %>%
      filter(AvgAct!=0)%>%
      filter(NodeType!='S')  #Comment this out
    IDmap <- read.table(file = system.file("HUMAN_9606_idmapping_onlyGeneName.dat",package="CARNIVAL"),header = F,sep = "\t",stringsAsFactors = F)
    genes <- rep(NA,length(set$Node))
    for (counter in 1:length(genes)) {
      genes[counter] <- IDmap[which(IDmap[,1] == set$Node[counter]),3][1]
    }
  }
  if(sum(is.na(genes))>0){genes<-genes[-which(is.na(genes))]}
  
  
  #RunOPA
  kparams <- GSEAKEGGHyperGParams(name<-"My Custom GSEA based annot Params",geneSetCollection=test,
                                  geneIds = as.character(genes),universeGeneIds = as.character(universe),
                                  pvalueCutoff = 0.5,testDirection = "over")
  kOver <- hyperGTest(kparams)
  
  #Extract results
  pval<-data.frame('pval'=kOver@pvalues)
  pval$annot<-rownames(pval)
  res <- pval
  write.csv2(res,paste0(Result_dir,"/CARNIVAL_enrichment_",Result_dir,"_",datasource,".csv"),row.names = F)
  if (pathwayfilter==T) {
    disease<-res[grepl('CANCER',res$annot)|grepl('LEUKEMIA',res$annot)|grepl('OMA',res$annot)|grepl('INFECTION', res$annot)|grepl('DIABETES', res$annot)|grepl('DISEASES', res$annot),]
    nodisease<-res%>%filter(!(annot %in% disease$annot))
    poi<-res%>%filter(annot %in% nodisease$annot)
  } else {poi<-res}
  
  ifelse(test = nrow(poi)>topEnrichAll, yes = res_top <- poi[1:topEnrichAll,],no = res_top <- poi)
  
  res_top$annot <- factor(res_top$annot,levels=res_top$annot) 
  
  if (plot==T){
    g_bar<-ggplot(res_top, aes(x = reorder(annot,-log10(pval)),y=-log10(pval)))+
      # geom_bar(stat="identity",aes(fill=annot)) + 
      geom_point(aes(color=annot,size=1)) + 
      coord_flip() +
      xlab("") +
      ylab(paste0("-log10(pval) : significant=",pValSig)) +
      theme_bw() +  
      theme(axis.text.x = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            axis.text.y = element_text(color = "grey20", size = 10, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
            axis.title.x = element_text(color = "grey20", size = 10, angle = 0, hjust = .5, vjust = 0, face = "plain"),
            axis.title.y = element_text(color = "grey20", size = 10, angle = 90, hjust = .5, vjust = .5, face = "plain"),
            legend.title=element_text(size=7), 
            legend.text=element_text(size=7),
            legend.position="none") +
      geom_hline(yintercept = -log10(pValSig), linetype='dotted')
    
    ggsave(filename = paste0(Result_dir,"/CARNIVAL_enrichment_",Result_dir,"_PW_All_",datasource,".pdf"),plot = g_bar, width=10, height=10)
  }
}



# Separate directional
if (directionalORA==T){
  if (length(map[,networkID])>0) {
    colnames(set)[which(colnames(set)=='Node')]<-colnames(map)[networkID]
    set_init <- set
    setup<-set_init%>%left_join(map, by=colnames(map)[networkID])%>%
      filter(AvgAct>0)%>%
      filter(NodeType!='S')  #Comment this out
    genesup<-setup$gene
    
    setdn<-set_init%>%left_join(map, by=colnames(map)[networkID])%>%
      filter(AvgAct<0)%>%
      filter(NodeType!='S')  #Comment this out
    genesdn<-setdn$gene
    
  
    } else {
    
    setup<-set_init %>%
      filter(AvgAct>0)%>%
      filter(NodeType!='S')  #Comment this out
    # genesup<-setup$gene
    
    IDmap <- read.table(file = system.file("HUMAN_9606_idmapping_onlyGeneName.dat",package="CARNIVAL"),header = F,sep = "\t",stringsAsFactors = F)
    
    genesup <- rep(NA,length(setup$Node))
    for (counter in 1:length(genesup)) {
      genesup[counter] <- IDmap[which(IDmap[,1] == setup$Node[counter]),3][1]
    }
    
    setdn<-set_init %>%
      filter(AvgAct<0)%>%
      filter(NodeType!='S')  #Comment this out
    # genesdn<-setdn$gene
    genesdn <- rep(NA,length(setdn$Node))
    for (counter in 1:length(genesdn)) {
      genesdn[counter] <- IDmap[which(IDmap[,1] == setdn$Node[counter]),3][1]
    }
    
  } 
  
  if(sum(is.na(genesup))>0){genesup<-genesup[-which(is.na(genesup))]}
  if(sum(is.na(genesdn))>0){genesdn<-genesdn[-which(is.na(genesdn))]}
  
  
  #RunOPA
  kparams_up <- GSEAKEGGHyperGParams(name<-"My Custom GSEA based annot Params",geneSetCollection=test,
                                     geneIds = as.character(genesup),universeGeneIds = as.character(universe),
                                     pvalueCutoff = 0.5,testDirection = "over")
  kOver_up <- hyperGTest(kparams_up)
  
  kparams_dn <- GSEAKEGGHyperGParams(name<-"My Custom GSEA based annot Params",geneSetCollection=test,
                                     geneIds = as.character(genesdn),universeGeneIds = as.character(universe),
                                     pvalueCutoff = 0.5,testDirection = "over")
  kOver_dn <- hyperGTest(kparams_dn)
  
  #Extract results
  up_all<-NULL
  pval_up<-data.frame('pval'=kOver_up@pvalues)
  if(nrow(pval_up)>0){
    pval_up$annot<-rownames(pval_up)
    up_all<-pval_up
    up_all$direct<-'up-regulated'
  }
  
  dn_all<-NULL
  pval_dn<-data.frame('pval'=kOver_dn@pvalues)
  if(nrow(pval_dn)>0){
    pval_dn$annot<-rownames(pval_dn)
    dn_all<-pval_dn
    dn_all$direct<-'down-regulated'
  }
 
  df<-bind_rows(up_all,dn_all)
  write.csv2(df,paste0(Result_dir,"/CARNIVAL_enrichment_directional_",Result_dir,"_",datasource,".csv"),row.names = F)
  
  if (plot==T & nrow(df)>0){
    if (pathwayfilter==T) {
      sig<-df%>%group_by(annot, direct)%>%summarise('med'=median(pval))%>%filter(med<0.05)
      disease<-sig[grepl('CANCER',sig$annot)|grepl('LEUKEMIA',sig$annot)|grepl('OMA',sig$annot)|grepl('INFECTION', sig$annot)|grepl('DIABETES', sig$annot)|grepl('DISEASES', sig$annot),]
      nodisease<-sig%>%filter(!(annot %in% disease$annot))
      nodisease<-nodisease[order(nodisease$med, decreasing = F),]
      poi<-df%>%filter(annot %in% nodisease$annot)
    } else {poi<-sig}
    
    poi$annot<-factor(poi$annot,levels = rev(unique(nodisease$annot)))
    g3<- ggplot((poi), aes(annot, -log10(pval)))+
      # geom_boxplot(aes(color=annot))+
      geom_point(aes(color=annot))+
      facet_wrap(~direct, nrow=1)+
      coord_flip()+
      geom_hline(yintercept = -log10(pValSig), linetype='dotted')+
      theme_bw()+
      theme(legend.title = element_blank(),axis.title.y = element_blank(),legend.position = 'none')+
      guides(color=FALSE)+
      xlab("") +
      ylab(paste0("-log10(pval) : significant pval=",pValSig))
    ggsave(g3, filename = paste0(Result_dir,"/CARNIVAL_enrichment_",Result_dir,"_PW_UpDn_",datasource,".pdf"), width=(max(nchar(as.character(poi$annot)))/10)+2, height=(length(unique(poi$annot))/10)+1)
    
  }
  
  print("Enrichment analysis completed: Please check enrichment results in the result folder")
  
}
}


