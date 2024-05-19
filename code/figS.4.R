load(paste(WD.PATH, 'data/CKD.RData', sep=''))

ymean=colMeans(Y.na,na.rm=T)
Y=Y.na
for(i in 1:ncol(Y))
{
  mis.i=which(is.na(Y.na[,i]))
  Y[mis.i,i]=ymean[i]
}
complete_data=Y.na
na.ind=which(as.vector(t(is.na(Y.na))==T))
Y.imp=fit.rst.na[[4]]$Y.mis
complete_data=t(complete_data)
complete_data[na.ind]=Y.imp
complete_data=t(complete_data)
complete_data_g1=complete_data[true.clus==1,]
complete_data_g2=complete_data[true.clus==2,]
y.na_g1=Y.na[true.clus==1,]
y.na_g2=Y.na[true.clus==2,]

#scatter plot
n=nrow(Y)
da_c=da[,-12]
Data=cbind(Y,true.clus)
G1=Data[which(Data[,ncol(Data)]==1),]
G2=Data[which(Data[,ncol(Data)]==2),]
mymatrix=function(n)
{
  A=diag(1:n)
  A[lower.tri(A)]=(1:(n*(n-1)/2))+n
  A[upper.tri(A)]=(1:(n*(n-1)/2))+n*6
  return(A)
}

postscript(paste(WD.PATH, 'results/ckd_scatter.eps', sep=''), width = 7, height = 7,paper = 'special',horizontal = FALSE)
nf=layout(mymatrix(11))
par(mar=c(0,0,0,0),pty='s')
for(i in 1:11)
{
  varg=list(g1=G1[,i],g2=G2[,i])
  h1=lapply(varg,hist,breaks=seq(min(Data[,i],na.rm=T),max(Data[,i],na.rm=T),length=25),plot=F)
  t1=rbind(h1[[1]]$density,h1[[2]]$density)
  rownames(t1)=names(h1)
  colnames(t1)=h1[[1]]$mids
  plot(0:24,0:24, ylim=c(0.03,max(t1)), type='n', xlab='', ylab='', xaxt='n', yaxt='n', las=1)
  barplot(t1[1,]*0.75,ylim=c(0,max(t1)),col='thistle',border='violet',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  barplot(t1[2,]*0.75,ylim=c(0,max(t1)),col='paleturquoise3',border='turquoise4',axes=FALSE,xaxt='n',yaxt='n',las=1,space=0,add=T)
  legend('topright',legend=bquote(bolditalic(x[.(i)])), bty='n', cex=1.5, y.intersp = 0.05)
}
for(j in 1:10)
{
  for(i in (j+1):11)
  {
    xlim=c(1.2*min(Data[,j],na.rm=T),1.2*max(Data[,j],na.rm=T))
    ylim=c(1.2*min(Data[,i],na.rm=T),1.2*max(Data[,i],na.rm=T))
    plot(G1[,j],G1[,i],col='thistle',pch=1,cex=0.8,xlab='',ylab='',xaxt='n',yaxt='n',ylim=ylim,xlim=xlim)
    a=matrix(complete_data_g1[is.na(y.na_g1[,j])==T&is.na(y.na_g1[,i])==T,c(j,i)],ncol=2)
    points(a[,1],a[,2], pch=4, col='red3', cex=0.6,ylim=ylim, xlim=xlim)
    abline(lm(complete_data_g1[,i]~complete_data_g1[,j]),lty=2)
  }
}
for(j in 2:11)
{
  for(i in 1:(j-1))
  {
    xlim=c(min(Data[,j],na.rm=T),max(Data[,j],na.rm=T))
    ylim=c(min(Data[,i],na.rm=T),max(Data[,i],na.rm=T))
    plot(G2[,j],G2[,i],col='paleturquoise3',pch=2,cex=0.8,xlab='',ylab='',xaxt='n',yaxt='n',ylim=ylim,xlim=xlim)
    b=matrix(complete_data_g2[is.na(y.na_g2[,j])==T&is.na(y.na_g2[,i])==T,c(j,i)],ncol=2)
    points(b[,1],b[,2], pch=4, col='red3', cex=0.6,ylim=ylim, xlim=xlim)
    abline(lm(complete_data_g2[,i]~complete_data_g2[,j]),lty=2)
  }
}
dev.off()
