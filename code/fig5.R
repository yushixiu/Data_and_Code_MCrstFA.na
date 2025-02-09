library(mvtnorm)
load(paste(WD.PATH, 'data/CKD.RData', sep=''))
source(paste(WD.PATH, 'function/rMST.R', sep=''))

profile.nu = function(nu, Y.na, fit)
{
  n=nrow(Y.na)
  p=ncol(Y.na)
  na.posi=is.na(Y.na)
  po=p-rowSums(na.posi)
  ind.na=colSums(t(na.posi)*2^(1:p-1))
  num.class.na=length(unique(ind.na))
  row.posi=O.list=as.list(numeric(num.class.na))
  uni.ind=unique(ind.na)
  for(i in 1:num.class.na)
  {
    row.posi[[i]]=which(ind.na==uni.ind[i])
    O.list[[i]]=matrix(diag(p)[!na.posi[row.posi[[i]][1],],],ncol=p)
  }
  #ML estimates
  w=fit$para$w
  mu = fit$para$mu
  S=fit$para$Sig
  la=fit$para$alpha  
  Y=Y.na
  Y[is.na(Y)]=99999
  wden=matrix(NA,n,g)
  for(i in 1:g)
  {
    for(j in 1:num.class.na)
    {
      O=O.list[[j]]
      ind=row.posi[[j]]
      Y.pat=matrix(Y[ind,],ncol=p)
      Y.o=Y.pat%*%t(O)
      OSO=O%*%S[,,i]%*%t(O)
      mu.o=as.vector(O%*%mu[,i])
      alpha.o=O%*%la[,i]
      wden[ind,i]=w[i]*drMST(Y.o,mu.o,OSO,alpha.o,nu)
    }
  }
  indv.den=rowSums(wden)
  return(sum(log(indv.den)))
}

nu = seq(0.1, 50, 0.05)
pf.nu = rep(NA, length(nu))
fit.rst=fit.rst.na[[4]]
# profile log-likelihood for equal DOFs
for(i in 1:length(nu)) pf.nu[i] = profile.nu(nu =nu[i], Y.na = Y.na, fit = fit.rst)

postscript(paste(WD.PATH, 'results/fig5.eps', sep=''), width = 7, height = 7,paper = 'special',horizontal = FALSE)
plot(nu, pf.nu, type='l', xlab = expression(nu), ylab = "Profile log-likelihood", cex.lab=1.2, cex.axis=1.2, xaxs = "i", yaxs = "i", xlim=c(-0.5, 50), ylim=c(-3900,max(pf.nu)+50))
points(nu[which.max(pf.nu)], max(pf.nu), col=2, pch=4, cex=2)
segments(nu[which.max(pf.nu)], -3900, nu[which.max(pf.nu)], max(pf.nu), lty=2, col=2)
text(nu[which.max(pf.nu)]+8, max(pf.nu)+10, '(5.02, -3087.924)', col=2, cex=1.2)
#title('MCrstFA.na with equal DOFs')
dev.off()
