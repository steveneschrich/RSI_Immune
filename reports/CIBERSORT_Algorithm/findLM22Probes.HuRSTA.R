#
# HuRSTA mapping of CIBERSORT probes.
# Take the mapping in merck_CIBERSORT_probesets.txt and pick all probesets
# for a single gene. Calculate median for all probesets, and pick the probeset
# with the highest median 
#

#load("tcc-primary.rdata")
tcc.primary.expression<-readRDS(file="../../data.derived/tcc-primary-expression.RDS")
medians<-apply(tcc.primary.expression,1,median)
names(medians)<-rownames(tcc.primary.expression)

ps<-read.table(file="tcc-primary-CIBERSORT-probesets.txt",header=T,row.names=1,sep="\t",as.is=T,check.names=F)

CIBERSORT.mapping<-data.frame(row.names=unique(ps$"Gene Symbol"))

for (p in ps$"Gene Symbol") {
   potential.probesets<-rownames(ps)[which(ps$"Gene Symbol"==p)]
   chosen.probeset<-potential.probesets[which(max(medians[potential.probesets])==medians[potential.probesets])]
   CIBERSORT.mapping[p,"Probeset"]<-chosen.probeset
}

write.table(file="CIBERSORT-HuRSTA-mapping.txt",CIBERSORT.mapping,quote=F,sep="\t")

# Create a subset
tcc.CIBERSORT<-tcc.primary.expression[CIBERSORT.mapping[,"Probeset"],]
tcc.CIBERSORT.probesets<-rownames(tcc.CIBERSORT)
rownames(tcc.CIBERSORT)<-rownames(CIBERSORT.mapping)
write.table(file="tcc-primary-CIBERSORT.txt",tcc.CIBERSORT,quote=F,sep="\t")
write.table(file="tcc-primary-CIBERSORT-probesets.txt",tcc.CIBERSORT.probesets,quote=F,sep="\t")

