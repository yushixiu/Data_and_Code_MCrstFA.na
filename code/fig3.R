library(gclus)
library(mclust)
library(EMMIXmfa)
library(cpca)
require(MASS)
load(paste(WD.PATH, 'data/automobile.RData', sep=''))
suppressWarnings({
  Y.na=scale(as.data.frame(lapply(data[,c(2,10:14,17,19:26)],as.numeric),na.rm=T))
})
ymean=colMeans(Y.na,na.rm=T)
Y=Y.na
for(i in 1:ncol(Y))
{
  mis.i=which(is.na(Y.na[,i]))
  Y[mis.i,i]=ymean[i]
}
dim(Y)
Y5=Y[,c(6,7,11,14,15)]
sY5=round(cor(Y5),3)
p.clus=fit.rst.na5[[6]]$post.clus
da5=data[,c(14,17,22,25,26)]
names(da5)=c('Curb Weight','Engine Size','Horsepower','Highway MPG',' ')
n=nrow(Y5)
Data=cbind(Y5,p.clus)
G1=Data[which(Data[,ncol(Data)]==1),]
G2=Data[which(Data[,ncol(Data)]==2),]
G3=Data[which(Data[,ncol(Data)]==3),]
G4=Data[which(Data[,ncol(Data)]==4),]
G5=Data[which(Data[,ncol(Data)]==5),]
mymatrix=function(n)
{
  A=diag(1:n)
  A[lower.tri(A)]=(1:(n*(n-1)/2))+n
  A[upper.tri(A)]=(1:(n*(n-1)/2))+n*3
  B=rbind(A,c(26,26,26,26,26))
  return(B)
}
postscript(paste(WD.PATH, 'results/scatter_plot.eps', sep=''),width = 800,height = 800)
layout(mymatrix(5), heights = c(rep(1,5),0.2))
par(mar=c(0,0,0,0))
for(i in 1:5)
{
  varg=list(g1=G1[,i],g2=G2[,i],g3=G3[,i],g4=G4[,i],g5=G5[,i])
  h1=lapply(varg,hist,breaks=seq(min(Data[,i],na.rm=T),max(Data[,i],na.rm=T),length=25),plot=F)
  t1=rbind(h1[[1]]$density,h1[[2]]$density,h1[[3]]$density,h1[[4]]$density,h1[[5]]$density)
  rownames(t1)=names(h1)
  colnames(t1)=h1[[1]]$mids
  plot(0:24,0:24, ylim=c(0.07,max(t1)), type='n', xlab='', ylab='', xaxt='n', yaxt='n', las=1)
  barplot(t1[1,]*0.75,ylim=c(0,max(t1)),col='darkolivegreen3',border='forestgreen',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  barplot(t1[2,]*0.75,ylim=c(0,max(t1)),col='dodgerblue2',border='dodgerblue4',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  barplot(t1[3,]*0.75,ylim=c(0,max(t1)),col='mediumpurple2',border='mediumpurple4',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  barplot(t1[4,]*0.75,ylim=c(0,max(t1)),col='gold1',border='darkgoldenrod1',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  barplot(t1[5,]*0.75,ylim=c(0,max(t1)),col='lightcoral',border='red2',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  legend('topright',legend=names(da5)[(i)],bty='n',cex=1.5)
}
legend('topright',legend=bquote(bold('Price')),bty='n',cex=1.5)
for(j in 1:4)
{
  for(i in (j+1):5)
  {
    xlim=c(min(Data[,j],na.rm=T),max(Data[,j],na.rm=T))
    ylim=c(min(Data[,i],na.rm=T),max(Data[,i],na.rm=T))
    plot(G1[,j],G1[,i],col='darkolivegreen3',pch=1,cex=0.8,xlab="",ylab="",xaxt='n',yaxt='n',ylim=ylim,xlim=xlim)
    points(G2[,j],G2[,i],pch=2,col='dodgerblue2',cex=0.8)
    points(G3[,j],G3[,i],pch=3,col='mediumpurple2',cex=0.8)
    points(G4[,j],G4[,i],pch=4,col='gold1',cex=0.8)
    points(G5[,j],G5[,i],pch=5,col='lightcoral',cex=0.8)
  }
}

for(j in 2:5)
{
  for(i in 1:(j-1))
  {
    xlim=c(min(Data[,j],na.rm=T),max(Data[,j],na.rm=T))
    ylim=c(min(Data[,i],na.rm=T),max(Data[,i],na.rm=T))
    plot(G2[,j],G2[,i],col='0',xlab='',ylab='',xaxt='n',yaxt='n',ylim=ylim,xlim=xlim)
    legend('center',legend=sY5[j,i],bty="n",cex=1.5)
  }
}
plot(1, type = 'n', axes=FALSE, xlab='', ylab='')
legend('left', legend = c('Cluster: ', '1','2','3','4','5'), horiz=TRUE,
       col = c('0','dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),pch=c(0,2,1,3,4,5),bty='n',cex=1.3)
dev.off()


