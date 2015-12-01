rm (list=ls())
setwd("~/Documents/NCSU/RNAseq_CMM_BL_Unique/Paper_Fig.5/")

library(plyr)
library(ggplot2)
library(bear)

### Barplot for Fig. 5b first 
tgc <- read.table("Fig5c_avg.txt",header=TRUE,sep="\t")
#tgc = tgc[,-1]
max_min = read.table("Fig5_MaxMin.txt",header=TRUE,sep="\t")
max_min = max_min[,-1]

celltype = rep(c("YFP_NEG","YFP_POS","NO_SORT","ALL_SORT"),5)
genes = c(rep("ARF2",4),rep("ARF3",4),rep("ARF4",4),rep("TAS3",4),rep("TAS3b",4))

celltype = as.factor(celltype)
celltype = factor(celltype,levels=c("YFP_NEG","YFP_POS","NO_SORT","ALL_SORT"))

dat = data.frame(celltype=celltype,genes=genes)

fpkm = c()
for(i in 1:nrow(tgc)){
        tmp = c(tgc[i,2],tgc[i,3],tgc[i,4],tgc[i,5],tgc[i,6])
        fpkm = c(fpkm,tmp)
}

dat$fpkm = fpkm

##
max = c()
min = c()
for(i in 1:nrow(max_min)){
        tmpm = c(max_min[i,1],max_min[i,3],max_min[i,5],max_min[i,7])
        tmpmin = c(max_min[i,2],max_min[i,4],max_min[i,6],max_min[i,8])
        max = c(max,tmpm)
        min = c(min,tmpmin)
}

##
ggplot(data=dat, aes(x=celltype, y=fpkm, group=genes, fill=genes)) +
        geom_bar(stat="identity", position=position_dodge()) +
        scale_fill_brewer(palette="Paired") +
       geom_errorbar(aes(ymin= min,ymax= max),size=.2,
       width=.2,
       position=position_dodge(0.9)) + theme_minimal()

###
#line 

ggplot(data=dat, aes(x=celltype, y=fpkm, group=genes, colour=genes)) +
        geom_line() + geom_point()+
        geom_errorbar(aes(ymin= min,ymax= max),size=.5,
                      width=.2,
                      position=position_dodge(0)) + theme_minimal()