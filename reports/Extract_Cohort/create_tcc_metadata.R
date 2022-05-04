create_tcc_metadata<-function() {
  # create a merged file of sample annotation on primary with subtype annotation
  
  tcc.primary.metadata<-read.table(file="../../data.raw/tcc_10469_barcodes_vetted.txt",
                                   header=T,row.names=1,sep="\t",as.is=T,
                                   comment.char='',quote="")
  
  tcc.primary.subcategories<-read.table(file="../../data.raw/tcc_subcategories_merged_barcodes.txt",
                                        header=T,row.names=1,sep="\t",as.is=T,
                                        comment.char='',quote="")
  
  # Merge in SubCategory
  tcc.primary.metadata$SOO_SubCategory<-NULL
  tcc.primary.metadata[rownames(tcc.primary.subcategories),"SOO_SubCategory"]<-tcc.primary.subcategories$SOO_SubCategory
  
  # Create a field consisting of either the Merged field or the general field
  tcc.primary.metadata$SOO_Merged<-apply(tcc.primary.metadata,1,function(y){
    if (!is.na(y["SOO_SubCategory"])) {
      y["SOO_SubCategory"]
    } else {
      y["SOO_Conformed"]
    }
  })
  
  # Create some "NOS" field types.
  tcc.primary.metadata.subset<-tcc.primary.metadata[which(tcc.primary.metadata$SOO_Merged=="Breast"),]
  tcc.primary.metadata[rownames(tcc.primary.metadata.subset),"SOO_Merged"]<-NA
  tcc.primary.metadata.subset<-tcc.primary.metadata[which(tcc.primary.metadata$SOO_Merged=="Lung"),]
  tcc.primary.metadata[rownames(tcc.primary.metadata.subset),"SOO_Merged"]<-"Lung NOS"
  tcc.primary.metadata.subset<-tcc.primary.metadata[which(tcc.primary.metadata$SOO_Merged=="Skin"),]
  tcc.primary.metadata[rownames(tcc.primary.metadata.subset),"SOO_Merged"]<-"NMSC"
  
  return(tcc.primary.metadata)
}