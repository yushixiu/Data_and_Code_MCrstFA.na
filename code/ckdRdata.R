library(cpca)
require(MASS)
source(paste(WD.PATH, 'function/initial.R', sep=''))
source(paste(WD.PATH, 'function/MCFA.na.R', sep=''))
source(paste(WD.PATH, 'function/MCtFA.na.R', sep=''))
source(paste(WD.PATH, 'function/MCrstFA.na.R', sep=''))
da=read.table(paste(WD.PATH, 'data/ckd.txt', sep=''))
colnames(da)=c("Age","Blood Pressure","Blood Glucose Random","Blood Urea","Serum Creatinine","Sodium","Potassium","Hemoglobin","Packed Cell Volume","White Blood Cell Count","Red Blood Cell Count","Class")
#true.clus
da[,12]=da[,12]+1
true.clus=as.numeric(da[,12])
Y.na=scale(da[,1:11])
#missing rate 14.45%
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
initial=c()
for(i in 1:5)
{
  initial[[i]]=init.para(Y=Y,g=2,q=i,true.clus=true.clus,init="CPC",init.clus=T,random.start=F)
}
#model fitting
fit.n.na=fit.t.na=fit.rst.na=list()
for(i in 1:5)
{
  fit.n.na[[i]]=MCFA.na.ECME(Y.na=Y.na,initial=initial[[i]],true.clus=true.clus,eqnu=T,tol=1e-6,max.iter=2000,per=100)
  fit.t.na[[i]]=MCtFA.na.ECME(Y.na=Y.na,initial=initial[[i]],true.clus=true.clus,eqnu=T,tol=1e-6,max.iter=2000,per=100)
  fit.rst.na[[i]]=MCrstFA.na.ECME(Y.na=Y.na,initial=initial[[i]],true.clus=true.clus,eqnu=T,tol=1e-6,max.iter=2000,per=100)
}


