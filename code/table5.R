#Observed pairwise correlations
load(paste(WD.PATH, 'data/CKD.RData', sep=''))
Y.na=scale(da[,1:11])
#missing rate 14.45%
na.posi=is.na(Y.na)
n=nrow(Y.na)
p=ncol(Y.na)
head(Y.na)
c1 = Y.na[which(da[,ncol(da)]==1),]
c2 = Y.na[which(da[,ncol(da)]==2),]
y_pair1=yy1=y_pair2=yy2=list()
corr1=corr2=c()
rr1=rr2=matrix(NA,10,10)
for(j in 1:10)
{
  for(i in (j+1):11)
  {
    y_pair1[[(i-j)]]=na.omit(c1[,c(j,i)])
    corr1[(i-j)]=cor(na.omit(c1[,c(j,i)]))[1,2]
    y_pair2[[(i-j)]]=na.omit(c2[,c(j,i)])
    corr2[(i-j)]=cor(na.omit(c2[,c(j,i)]))[1,2]
  }
  yy1[[j]]=y_pair1
  yy1[[j]]=yy1[[j]][1:(11-j)]
  rr1[,j]=corr1
  yy2[[j]]=y_pair2
  yy2[[j]]=yy2[[j]][1:(11-j)]
  rr2[,j]=corr2
}

cc=matrix(NA,11,11)
for(j in 1:10)
{
  for(i in (j+1):11)
  {
    cc[i,j]=rr1[(i-j),j]
  }
}

for(j in 2:11)
{
  for(i in 1:(j-1))
  {
    cc[i,j]=rr2[(j-i),i]
  }
}
write.csv(round(cc,4), paste(WD.PATH, 'results/Table5.csv', sep=''), row.names = TRUE)

