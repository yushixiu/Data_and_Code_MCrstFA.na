init.para=function(Y,g,q,true.clus,init=c("RD","CPC"),init.clus=T,random.start=F)
{
  library(moments)
  n=nrow(Y)
  p=ncol(Y)
  nu=rep(10,g)
  ybar=mu=matrix(NA,p,g)
  la=matrix(NA,q,g)
  D=matrix(NA,p,p)
  Om=array(NA,dim=c(q,q,g))
  Om.nsq=array(NA,dim=c(q,q,g))
  Sig.g=array(NA,dim=c(p,p,g))
  Sp=matrix(0,p,p)
  if(init.clus==T)
  {
    clus=true.clus
    cat("Initialization with true class labels.","\n")
  }
  else if(random.start==T)
  {
    clus=apply(rmultinom(n,1,rep(1/g,g)),2,order)[g,]
    cat("Initialization with random starts.","\n")
  }
  else
  {
    repeat
    {
      cat("Initialization with K means.","\n")
      clus=kmeans(Y,g,nstart=25)$cluster
      if(sum(table(clus)<=1)==0) break
    }
  }
  w=as.numeric(table(clus)/n)
  for(i in 1:g)
  {
    ind.Y=Y[which(clus==i),]
    ybar[,i]=colMeans(ind.Y)
    Sig.g[,,i]=var(ind.Y)
    Sp=Sp+(nrow(ind.Y)-1)/(n-g)*Sig.g[,,i]
    #U=factanal(ind.Y,q,scores="regression")$scores
    #la[,i]=skewness(U)
    la[,i]=runif(q, -1, 1)
  }
  D=diag(diag(Sp))
  if(init=="CPC")
  {
    Q=cpc(Sig.g)$CPC
    A=as.matrix(Q[,1:q])
    Ch=chol(t(A)%*%A)
    A.ch=A%*%solve(Ch)
  }
  if(init=="RD")
  {
    A=matrix(rnorm(p*q),p,q)
    Ch=chol(t(A)%*%A)
    A.ch=A%*%solve(Ch)
  }
  for(i in 1:g)
  {
    DSD=sqrt(solve(D))%*%Sig.g[,,i]%*%sqrt(solve(D))
    H=eigen(DSD)$ve[,1:q]
    if(q==1)
    {
      DSD.eig.va=as.matrix(eigen(DSD)$va[1:q])
    }
    else
    {
      DSD.eig.va=diag(eigen(DSD)$va[1:q])
    }
    if(q==p)
    {
      tilde.sig2=eigen(DSD)$va[p]
    }
    else
    {
      tilde.sig2=sum(eigen(DSD)$va[(q+1):p])/(p-q)
    }
    Om[,,i]=t(A.ch)%*%sqrt(D)%*%H%*%(DSD.eig.va-tilde.sig2*diag(q))%*%t(H)%*%sqrt(D)%*%A.ch
  }
  xi=t(A.ch)%*%ybar
  list(w=w,xi=xi,A=A.ch,D=D,Om=Om,la=la,nu=nu,clus=clus)
}

