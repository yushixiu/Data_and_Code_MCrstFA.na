library(gclus)
library(mclust)
library(EMMIXmfa)
library(cpca)
require(MASS)
source(paste(WD.PATH, 'function/initial.R', sep=''))
source(paste(WD.PATH, 'function/MCFA.na.R', sep=''))
source(paste(WD.PATH, 'function/MCtFA.na.R', sep=''))
source(paste(WD.PATH, 'function/MCrstFA.na.R', sep=''))
data=read.table(paste(WD.PATH, 'data/imports-85.data', sep=''),sep=',')
names(data)=c("symboling","normalized-losses","make","fuel-type","aspiration","num-of-doors","body-style","drive-wheels","engine-location","wheel-base","length","width","height","curb-weight","engine-type","num-of-cylinders","engine-size","fuel-system","bore","stroke","compression-ratio","horsepower","peak-rpm","city-mpg","highway-mpg","price")
suppressWarnings({
  Y.na=scale(as.data.frame(lapply(data[,c(2,10:14,17,19:26)],as.numeric),na.rm=T))
})
#missing rate 1.85%
na.posi=is.na(Y.na)
n=nrow(Y.na)
p=ncol(Y.na)
sum(na.posi)/(n*p)
#initial values
ymean=colMeans(Y.na,na.rm=T)
Y=Y.na
for(i in 1:ncol(Y))
{
  mis.i=which(is.na(Y.na[,i]))
  Y[mis.i,i]=ymean[i]
}
dim(Y)
#clustering
set.seed(17)
clus1=kmeans(Y,1)$cluster
clus2=kmeans(Y,2)$cluster
clus3=kmeans(Y,3)$cluster
clus4=kmeans(Y,4)$cluster
clus5=kmeans(Y,5)$cluster
#initial values (g=1-5, q=1-7)
initial1=initial2=initial3=initial4=initial5=c()
for(i in 1:7)
{
  initial1[[i]]=init.para(Y=Y,g=1,q=i,true.clus=clus1,init="CPC",init.clus=T,random.start=F)
  initial2[[i]]=init.para(Y=Y,g=2,q=i,true.clus=clus2,init="CPC",init.clus=T,random.start=F)
  initial3[[i]]=init.para(Y=Y,g=3,q=i,true.clus=clus3,init="CPC",init.clus=T,random.start=F)
  initial4[[i]]=init.para(Y=Y,g=4,q=i,true.clus=clus4,init="CPC",init.clus=T,random.start=F)
  initial5[[i]]=init.para(Y=Y,g=5,q=i,true.clus=clus5,init="CPC",init.clus=T,random.start=F)
}
#model fitting
fit.n.na1=fit.n.na2=fit.n.na3=fit.n.na4=fit.n.na5=list()
fit.t.na1=fit.t.na2=fit.t.na3=fit.t.na4=fit.t.na5=list()
fit.rst.na1=fit.rst.na2=fit.rst.na3=fit.rst.na4=fit.rst.na5=list()
for(i in 1:7)
{
  fit.n.na1[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial1[[i]],true.clus=clus1,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.n.na2[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial2[[i]],true.clus=clus2,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.n.na3[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial3[[i]],true.clus=clus3,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.n.na4[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial4[[i]],true.clus=clus4,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.n.na5[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial5[[i]],true.clus=clus5,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.t.na1[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial1[[i]],true.clus=clus1,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.t.na2[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial2[[i]],true.clus=clus2,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.t.na3[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial3[[i]],true.clus=clus3,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.t.na4[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial4[[i]],true.clus=clus4,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.t.na5[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial5[[i]],true.clus=clus5,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na1[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial1[[i]],true.clus=clus1,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na2[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial2[[i]],true.clus=clus2,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na3[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial3[[i]],true.clus=clus3,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na4[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial4[[i]],true.clus=clus4,eqnu=F,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na5[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial5[[i]],true.clus=clus5,eqnu=F,tol=1e-6,max.iter=2000,per=100)
}

#BIC&ICL
bic_n=rbind(c(fi.n.na1[[1]]$BIC,fit.n.na1[[2]]$BIC,fit.n.na1[[3]]$BIC,fit.n.na1[[4]]$BIC,fit.n.na1[[5]]$BIC,fit.n.na1[[6]]$BIC,fit.n.na1[[7]]$BIC),
            c(fi.n.na2[[1]]$BIC,fit.n.na2[[2]]$BIC,fit.n.na2[[3]]$BIC,fit.n.na2[[4]]$BIC,fit.n.na2[[5]]$BIC,fit.n.na2[[6]]$BIC,fit.n.na2[[7]]$BIC),
            c(fi.n.na3[[1]]$BIC,fit.n.na3[[2]]$BIC,fit.n.na3[[3]]$BIC,fit.n.na3[[4]]$BIC,fit.n.na3[[5]]$BIC,fit.n.na3[[6]]$BIC,fit.n.na3[[7]]$BIC),
            c(fi.n.na4[[1]]$BIC,fit.n.na4[[2]]$BIC,fit.n.na4[[3]]$BIC,fit.n.na4[[4]]$BIC,fit.n.na4[[5]]$BIC,fit.n.na4[[6]]$BIC,fit.n.na4[[7]]$BIC),
            c(fi.n.na5[[1]]$BIC,fit.n.na5[[2]]$BIC,fit.n.na5[[3]]$BIC,fit.n.na5[[4]]$BIC,fit.n.na5[[5]]$BIC,fit.n.na5[[6]]$BIC,fit.n.na5[[7]]$BIC))
bic_t=rbind(c(fit.t.na1[[1]]$BIC,fit.t.na1[[2]]$BIC,fit.t.na1[[3]]$BIC,fit.t.na1[[4]]$BIC,fit.t.na1[[5]]$BIC,fit.t.na1[[6]]$BIC,fit.t.na1[[7]]$BIC),
            c(fit.t.na2[[1]]$BIC,fit.t.na2[[2]]$BIC,fit.t.na2[[3]]$BIC,fit.t.na2[[4]]$BIC,fit.t.na2[[5]]$BIC,fit.t.na2[[6]]$BIC,fit.t.na2[[7]]$BIC),
            c(fit.t.na3[[1]]$BIC,fit.t.na3[[2]]$BIC,fit.t.na3[[3]]$BIC,fit.t.na3[[4]]$BIC,fit.t.na3[[5]]$BIC,fit.t.na3[[6]]$BIC,fit.t.na3[[7]]$BIC),
            c(fit.t.na4[[1]]$BIC,fit.t.na4[[2]]$BIC,fit.t.na4[[3]]$BIC,fit.t.na4[[4]]$BIC,fit.t.na4[[5]]$BIC,fit.t.na4[[6]]$BIC,fit.t.na4[[7]]$BIC),
            c(fit.t.na5[[1]]$BIC,fit.t.na5[[2]]$BIC,fit.t.na5[[3]]$BIC,fit.t.na5[[4]]$BIC,fit.t.na5[[5]]$BIC,fit.t.na5[[6]]$BIC,fit.t.na5[[7]]$BIC))
bic_rst=rbind(c(fit.rst.na1[[1]]$BIC,fit.rst.na1[[2]]$BIC,fit.rst.na1[[3]]$BIC,fit.rst.na1[[4]]$BIC,fit.rst.na1[[5]]$BIC,fit.rst.na1[[6]]$BIC,fit.rst.na1[[7]]$BIC),
              c(fit.rst.na2[[1]]$BIC,fit.rst.na2[[2]]$BIC,fit.rst.na2[[3]]$BIC,fit.rst.na2[[4]]$BIC,fit.rst.na2[[5]]$BIC,fit.rst.na2[[6]]$BIC,fit.rst.na2[[7]]$BIC),
              c(fit.rst.na3[[1]]$BIC,fit.rst.na3[[2]]$BIC,fit.rst.na3[[3]]$BIC,fit.rst.na3[[4]]$BIC,fit.rst.na3[[5]]$BIC,fit.rst.na3[[6]]$BIC,fit.rst.na3[[7]]$BIC),
              c(fit.rst.na4[[1]]$BIC,fit.rst.na4[[2]]$BIC,fit.rst.na4[[3]]$BIC,fit.rst.na4[[4]]$BIC,fit.rst.na4[[5]]$BIC,fit.rst.na4[[6]]$BIC,fit.rst.na4[[7]]$BIC),
              c(fit.rst.na5[[1]]$BIC,fit.rst.na5[[2]]$BIC,fit.rst.na5[[3]]$BIC,fit.rst.na5[[4]]$BIC,fit.rst.na5[[5]]$BIC,fit.rst.na5[[6]]$BIC,fit.rst.na5[[7]]$BIC))
icl_n=rbind(c(fit.n.na1[[1]]$ICL,fit.n.na1[[2]]$ICL,fit.n.na1[[3]]$ICL,fit.n.na1[[4]]$ICL,fit.n.na1[[5]]$ICL,fit.n.na1[[6]]$ICL,fit.n.na1[[7]]$ICL),
            c(fit.n.na2[[1]]$ICL,fit.n.na2[[2]]$ICL,fit.n.na2[[3]]$ICL,fit.n.na2[[4]]$ICL,fit.n.na2[[5]]$ICL,fit.n.na2[[6]]$ICL,fit.n.na2[[7]]$ICL),
            c(fit.n.na3[[1]]$ICL,fit.n.na3[[2]]$ICL,fit.n.na3[[3]]$ICL,fit.n.na3[[4]]$ICL,fit.n.na3[[5]]$ICL,fit.n.na3[[6]]$ICL,fit.n.na3[[7]]$ICL),
            c(fit.n.na4[[1]]$ICL,fit.n.na4[[2]]$ICL,fit.n.na4[[3]]$ICL,fit.n.na4[[4]]$ICL,fit.n.na4[[5]]$ICL,fit.n.na4[[6]]$ICL,fit.n.na4[[7]]$ICL),
            c(fit.n.na5[[1]]$ICL,fit.n.na5[[2]]$ICL,fit.n.na5[[3]]$ICL,fit.n.na5[[4]]$ICL,fit.n.na5[[5]]$ICL,fit.n.na5[[6]]$ICL,fit.n.na5[[7]]$ICL))
icl_t=rbind(c(fit.t.na1[[1]]$ICL,fit.t.na1[[2]]$ICL,fit.t.na1[[3]]$ICL,fit.t.na1[[4]]$ICL,fit.t.na1[[5]]$ICL,fit.t.na1[[6]]$ICL,fit.t.na1[[7]]$ICL),
            c(fit.t.na2[[1]]$ICL,fit.t.na2[[2]]$ICL,fit.t.na2[[3]]$ICL,fit.t.na2[[4]]$ICL,fit.t.na2[[5]]$ICL,fit.t.na2[[6]]$ICL,fit.t.na2[[7]]$ICL),
            c(fit.t.na3[[1]]$ICL,fit.t.na3[[2]]$ICL,fit.t.na3[[3]]$ICL,fit.t.na3[[4]]$ICL,fit.t.na3[[5]]$ICL,fit.t.na3[[6]]$ICL,fit.t.na3[[7]]$ICL),
            c(fit.t.na4[[1]]$ICL,fit.t.na4[[2]]$ICL,fit.t.na4[[3]]$ICL,fit.t.na4[[4]]$ICL,fit.t.na4[[5]]$ICL,fit.t.na4[[6]]$ICL,fit.t.na4[[7]]$ICL),
            c(fit.t.na5[[1]]$ICL,fit.t.na5[[2]]$ICL,fit.t.na5[[3]]$ICL,fit.t.na5[[4]]$ICL,fit.t.na5[[5]]$ICL,fit.t.na5[[6]]$ICL,fit.t.na5[[7]]$ICL))
icl_rst=rbind(c(fit.rst.na1[[1]]$ICL,fit.rst.na1[[2]]$ICL,fit.rst.na1[[3]]$ICL,fit.rst.na1[[4]]$ICL,fit.rst.na1[[5]]$ICL,fit.rst.na1[[6]]$ICL,fit.rst.na1[[7]]$ICL),
              c(fit.rst.na2[[1]]$ICL,fit.rst.na2[[2]]$ICL,fit.rst.na2[[3]]$ICL,fit.rst.na2[[4]]$ICL,fit.rst.na2[[5]]$ICL,fit.rst.na2[[6]]$ICL,fit.rst.na2[[7]]$ICL),
              c(fit.rst.na3[[1]]$ICL,fit.rst.na3[[2]]$ICL,fit.rst.na3[[3]]$ICL,fit.rst.na3[[4]]$ICL,fit.rst.na3[[5]]$ICL,fit.rst.na3[[6]]$ICL,fit.rst.na3[[7]]$ICL),
              c(fit.rst.na4[[1]]$ICL,fit.rst.na4[[2]]$ICL,fit.rst.na4[[3]]$ICL,fit.rst.na4[[4]]$ICL,fit.rst.na4[[5]]$ICL,fit.rst.na4[[6]]$ICL,fit.rst.na4[[7]]$ICL),
              c(fit.rst.na5[[1]]$ICL,fit.rst.na5[[2]]$ICL,fit.rst.na5[[3]]$ICL,fit.rst.na5[[4]]$ICL,fit.rst.na5[[5]]$ICL,fit.rst.na5[[6]]$ICL,fit.rst.na5[[7]]$ICL))

p.clus=fit.rst.na5[[6]]$post.clus
