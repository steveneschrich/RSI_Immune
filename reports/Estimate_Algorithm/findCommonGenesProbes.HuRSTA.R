findCommonGenesProbes.HuRSTA <-function() {
  
  library(estimate)
  #
  # HuRSTA mapping of ESTIMATE "common" probes.
  #
  # First, take the common_genes.txt file and filter merck probesets
  # by these genes (using EntrezID).
  #
  # Take the mapping in merck_CIBERSORT_probesets.txt and pick all probesets
  # for a single gene. Calculate median for all probesets, and pick the probeset
  # with the highest median 
  #

  # First, take the common_genes.txt file and filter merck probesets
  # by these genes (using EntrezID).
  common.genes<-read.table(file="estimate_common_genes.txt",
                           header=T,
                           row.names=1,
                           sep="\t",
                           comment.char="",
                           quote="",
                           as.is=T,
                           check.names=F)
  hursta.annotation<-read.table(file="../../data.raw/hursta_annotation.txt",header=T,row.names=1,sep="\t", comment.char="",quote="",as.is=T,check.names=F)
  hursta.annotation.common<-hursta.annotation[which(hursta.annotation$GeneID %in% rownames(common.genes)),]

  ps<-hursta.annotation.common

  tcc.primary.expression<-readRDS(file="../../data.derived/tcc-primary-expression.RDS")
  medians<-apply(tcc.primary.expression,1,median)
  names(medians)<-rownames(tcc.primary.expression)


  ESTIMATE.mapping<-data.frame(row.names=unique(ps$"GeneID"))

  for (p in ps$"GeneID") {
    potential.probesets<-rownames(ps)[which(ps$"GeneID"==p)]
    chosen.probeset<-potential.probesets[which(max(medians[potential.probesets])==medians[potential.probesets])]
    ESTIMATE.mapping[p,"Probeset"]<-chosen.probeset
    ESTIMATE.mapping[p,"Gene Symbol"]<-common_genes[which(common_genes$EntrezID==p),"GeneSymbol"]
  }

  write.table(file="ESTIMATE-HuRSTA-mapping.txt",ESTIMATE.mapping,quote=F,sep="\t")

}
