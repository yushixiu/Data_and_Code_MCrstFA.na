sqrt.mt=function(S)
{
  p=ncol(S)
  if(p==1) S.sqrt=as.matrix(sqrt(S))
  else
  {
    eig.S=eigen(S)
    S.sqrt=eig.S$ve%*%diag(sqrt(eig.S$va))%*%t(eig.S$ve)
  }
}

rMSt=function(n,xi,S,alpha,nu)
{
  library(tmvtnorm)
  library(mvtnorm)
  p = nrow(S) 
  gamma = rtmvnorm(n=n, mean=rep(0,1), sigma=diag(1), lower=rep(0,1), upper=rep(Inf,1))
  U = t(rmvnorm(n, mean=rep(0, p), sigma=S))
  x =  t(alpha %*% t(gamma) + U)
  tau = rgamma(n, shape = nu/2, rate = nu/2)
  y =  t( c(xi) + t(x/sqrt(tau)) )
  return(y)
}

dMST=function(Y,mu,S,la,nu,log=F)
{
  require(mvtnorm)
  if(is.vector(Y)==T) Y=as.matrix(Y)
  if(is.matrix(S)==F) S=as.matrix(S)
  p=length(la)
  cent=t(Y)-mu
  om=S+la%*%t(la)
  xi=as.vector(t(la)%*%solve(om)%*%cent)
  si2=as.vector(1/(1+t(la)%*%solve(S)%*%la))
  u=solve(sqrt.mt(om))%*%cent
  Delta=colSums(u^2)
  r0=(nu+p)/(nu+Delta)
  A=xi/sqrt(si2)
  c0=A*sqrt(r0)
  Tc0=pt(c0,df=nu+p)
  logtp=lgamma((nu+p)/2)-lgamma(nu/2)-log(det(om))/2-p/2*log(pi*nu)-(nu+p)/2*log(1+Delta/nu)
  if(log==F)
  {
    den=2*exp(logtp)*Tc0
  }
  else{
    den=log(2)+logtp+log(Tc0)
  }
  return(den)
}