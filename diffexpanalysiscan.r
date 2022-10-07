#diffrential gene expression analysis
setwd("C:/Users/admin/Documents")
#To identify genes which are differential in tumor vs control samples
mat=matrix(NA,ncol=4,nrow = nrow(log1))
rownames(mat)=rownames(log1)
colnames(mat)=c('meanTumor','meanControl','pvalue','log2FC')
df<-c('cpm1')
dy<-replace(df,cpm1,1)
dy

for(i in 1:nrow(log1)){
  vector1 = as.numeric(log1[i, 1:3])
  
  vector2 = as.numeric(log1[i, 4:6])
  
  
  if(length(unique(vector1))==1){
    next
  }
  if(length(unique(vector2))==1){
    next
  }
  
  res=t.test(vector1, vector2, paired = F, alternative = "two.sided")
  mat[i,1]=res$estimate[[1]]
  mat[i,2]=res$estimate[[2]]
  mat[i,3]=res$p.value
  mat[i,4]=mat[i,1]-mat[i,2]
}  
View(mat)  



mat=as.data.frame(mat)
num=which(is.nan(mat$pvalue))
mat[num,'pvalue']=1

#if (!requireNamespace("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("EnhancedVolcano")
library(EnhancedVolcano)
EnhancedVolcano(mat,lab = rownames(mat),x = 'log2FC' ,y ='pvalue')

