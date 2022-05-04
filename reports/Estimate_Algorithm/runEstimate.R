runEstimate<-function() {
  
  library(estimate)
  
  ESTIMATE.mapping<-read.table(file="ESTIMATE-HuRSTA-mapping.txt",
                               sep="\t", header=T,row.names=NULL,
                               check.names=FALSE,
                               stringsAsFactors=FALSE)
  tcc.primary.expression<-readRDS("../../data.derived/tcc-primary-expression.RDS")
  
  # Create a subset
  tcc.ESTIMATE<-tcc.primary.expression[ESTIMATE.mapping[,"Probeset"],]
  tcc.ESTIMATE.probesets<-rownames(tcc.ESTIMATE)
  rownames(tcc.ESTIMATE)<-ESTIMATE.mapping[,"Gene Symbol"]
  write.table(file="tcc-primary-ESTIMATE.txt",tcc.ESTIMATE,quote=F,sep="\t")
  write.table(file="tcc-primary-ESTIMATE-probesets.txt",tcc.ESTIMATE.probesets,quote=F,sep="\t")

  outputGCT(as.data.frame(tcc.ESTIMATE),"tcc-primary-ESTIMATE.gct")
  
  estimateScore(input.ds="tcc-primary-ESTIMATE.gct",
                output.ds="tcc-primary-ESTIMATE-predictions.gct",
                platform="affymetrix")
}