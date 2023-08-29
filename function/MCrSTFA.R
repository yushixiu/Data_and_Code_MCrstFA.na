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

rMSN=function(n,mu,S,la)
{
  if(is.matrix(S)==F) S=as.matrix(S)
  p=length(la)
  sqrt.S=sqrt.mt(S)
  ga=matrix(abs(rnorm(n)),1,n)
  U=matrix(rnorm(p*n),p,n)
  Y=mu+la%*%ga+sqrt.S%*%U
  return(t(Y))
}

rMST=function(n,mu,S,la,nu)
{
  if(is.matrix(S)==F) S=as.matrix(S)
  p=length(la)
  Y=t(mu+t(rMSN(n,mu=rep(0,p),S,la)/sqrt(rgamma(n,nu/2,nu/2))))
  return(Y)
}

mcstfa.loglik=function(Y,g,A,xi,Om,D,w,nu,la)
{
  n=nrow(Y)
  w.den=matrix(NA,n,g)
  mu=A%*%xi
  alpha=A%*%la
  for(i in 1:g)
  {
    Sig=A%*%Om[,,i]%*%t(A)+D[,,i]
    w.den[,i]=w[i]*dMST(Y,mu[,i],Sig,alpha[,i],nu[i],log=F)
  }
  loglik=sum(log(rowSums(w.den)))
  return(list(loglik=loglik,w.den=w.den))
}

MCrSTFA.ecme=function(Y,g,q,initial,eqD=T,eqnu=F,tol=1e-6,true.clus,max.iter,per=100)
{
  method="ECME"
  n=nrow(Y)
  p=ncol(Y)
  beta=array(NA,dim=c(p,q,g))
  Sig.g=Sig.inv=array(NA,dim=c(p,p,g))
  delta=tau=kappa=matrix(NA,n,g)
  s1j=s2j=ga=matrix(NA,n,g)
  u=eta=zeta=array(NA,dim=c(q,n,g))
  D=array(NA,dim=c(p,p,g))
  w=initial$w
  A=initial$A
  xi=initial$xi
  Om=initial$Om
  for(i in 1:g) D[,,i]=initial$D
  la=initial$la
  nu=initial$nu
  CMQ.nu.fn=function(Y,z,mu,Sig,la,nu)
  {
    -sum(z*dMST(Y,mu,Sig,la,nu,log=T))
  }
  CML.nu.fn=function(Y,w,mu,Sig,la,nu)
  {
    n=nrow(Y)
    wden=matrix(NA,n,g)
    for(i in 1:g)
    {
      wden[,i]=w[i]*dMST(Y,mu[,i],Sig[,,i],la[,i],nu)
    }
    indv.den=rowSums(wden)
    return(-sum(log(indv.den)))
  }
  mu=A%*%xi
  begin=proc.time()[1]
  ell=mcstfa.loglik(Y,g,A,xi,Om,D,w,nu,la)
  loglik.old=ell$loglik
  iter=0
  cat(paste(rep("-",20),sep="",collapse=""),method,":\t MCSTFA: (g=",g,"; p=",p,"; q=",q,")","\n")
  cat("iter=",iter,",\t init.log.like=",loglik.old,sep="","\n")
  iter.loglik=loglik.old
  repeat
  {
    iter=iter+1
    if(eqD==T)
    {
      D.g=matrix(0,p,p)
    }
    A1=array(0,dim=c(p,q))
    A2=array(0,dim=c(q,q))
    z=ell$w.den/rowSums(ell$w.den)
    ng=colSums(z)
    w=ng/n
    mu=A%*%xi
    alpha=A%*%la
    for(i in 1:g)
    {
      cent=t(Y)-mu[,i]
      Sig.g[,,i]=A%*%Om[,,i]%*%t(A)+D[,,i]
      DiA=A/diag(D[,,i])
      Sig.inv[,,i]=diag(1/diag(D[,,i]))-DiA%*%solve(solve(Om[,,i])+t(A)%*%DiA)%*%t(DiA)
      beta[,,i]=Sig.inv[,,i]%*%A%*%Om[,,i]
      
      V=Sig.g[,,i]+alpha[,i]%*%t(alpha[,i])
      eigV=eigen(V)
      Vh.inv=eigV$ve%*%diag(1/sqrt(eigV$va),p)%*%t(eigV$ve)
      v.cent=Vh.inv%*%cent
      delta[,i]=colSums(v.cent^2)
      tmp=c(Vh.inv%*%alpha[,i])
      h=colSums(tmp*v.cent)
      si2=1-sum(tmp^2)
      Aj=h/sqrt(si2)
      rn2=(nu[i]+p-2)/(nu[i]+delta[,i])
      cn2=Aj*sqrt(rn2)
      r0=(nu[i]+p)/(nu[i]+delta[,i])
      c0=Aj*sqrt(r0)
      r2=(nu[i]+p+2)/(nu[i]+delta[,i])
      c2=Aj*sqrt(r2)
      
      Tc0=pt(c0,df=nu[i]+p)
      Tc2=pt(c2,df=nu[i]+p+2)
      tc0=dt(c0,df=nu[i]+p)
      tcn2=dt(cn2,df=nu[i]+p-2)
      
      tau[,i]=r0*(Tc2/Tc0)
      s1j[,i]=h*tau[,i]+sqrt(si2)*sqrt(r0)*tc0/Tc0
      s2j[,i]=h*s1j[,i]+si2
      ga[,i]=h+sqrt(si2)*tcn2/(sqrt(rn2)*Tc0)
      u[,,i]=xi[,i]+t(beta[,,i])%*%cent+(la[,i]-t(beta[,,i])%*%alpha[,i])%*%t(ga[,i])
      eta[,,i]=t(t(xi[,i]+t(beta[,,i])%*%cent)*tau[,i])+(la[,i]-t(beta[,,i])%*%alpha[,i])%*%t(s1j[,i])
      zeta[,,i]=t(t(xi[,i]+t(beta[,,i])%*%cent)*s1j[,i])+(la[,i]-t(beta[,,i])%*%alpha[,i])%*%t(s2j[,i])
      
      Ztau=colSums(z*tau)
      ZK=colSums(z*kappa)
      Zs1j=sum(z[,i]*s1j[,i])
      Zs2j=sum(z[,i]*s2j[,i])
      if(q==1)
      {
        a=matrix(eta[,,i],1,length(eta[,,i]))
        b=matrix(zeta[,,i],1,length(zeta[,,i]))
        Zeta=colSums(z[,i]*t(a))
        Zzeta=colSums(z[,i]*t(b))
      }
      else
      {
        Zeta=colSums(z[,i]*t(eta[,,i]))
        Zzeta=colSums(z[,i]*t(zeta[,,i]))
      }
      Ztauy=colSums(z[,i]*tau[,i]*Y)
      Zs1jy=colSums(z[,i]*s1j[,i]*Y)
      Y2=t(z[,i]*tau[,i]*Y)%*%Y
      etaY=t(z[,i]*Y)%*%t(matrix(eta[,,i],q,n))
      centeta=t(z[,i]*t(matrix(eta[,,i],q,n)))%*%t(cent)
      
      ZPsi=Zzeta%*%t(la[,i]-t(beta[,,i])%*%alpha[,i])+Zeta%*%t(xi[,i])+centeta%*%beta[,,i]+ng[i]*(diag(1,q)-t(beta[,,i])%*%A)%*%Om[,,i]
      xi[,i]=(Zs2j*Zeta-Zs1j*Zzeta)/(Zs2j*Ztau[i]-Zs1j^2)
      la[,i]=(Ztau[i]*Zzeta-Zs1j*Zeta)/(Zs2j*Ztau[i]-Zs1j^2)
      
      A1=A1+etaY
      A2=A2+ZPsi
      Om1=ZPsi-Zeta%*%t(xi[,i])-Zzeta%*%t(la[,i])-xi[,i]%*%(t(Zeta)-Ztau[i]*t(xi[,i])-Zs1j*t(la[,i]))-la[,i]%*%(t(Zzeta)-Zs1j*t(xi[,i])-Zs2j*t(la[,i]))
      Om[,,i]=Om1/ng[i]
      if(eqD==T)
      {
        D1=Y2-A%*%t(etaY)-etaY%*%t(A)+A%*%ZPsi%*%t(A)
        D.g=D.g+diag(diag(D1/n),p)
      }
      else
      {
        D1=Y2-A%*%t(etaY)-etaY%*%t(A)+A%*%ZPsi%*%t(A)
        D[,,i]=diag(diag(D1/ng[i]),p)
      }
    }
    if(eqD==T)
    {
      for(i in 1:g)
      {
        D[,,i]=D.g
      }
    }
    A=A1%*%solve(A2)
    if(eqnu==F)
    {
      for(i in 1:g)
      {
        nu[i]=optim(par=nu[i],fn=CMQ.nu.fn,method="L-BFGS-B",lower=2,upper=1000,Y=Y,z=z[,i],mu=mu[,i],Sig=Sig.g[,,i],la=alpha[,i])$par
      }
    }
    else
    {
      nu=rep(optim(par=nu[1],fn=CML.nu.fn,method="L-BFGS-B",lower=2,upper=1000,Y=Y,w=w,mu=mu,Sig=Sig.g,la=alpha)$par,g)
    }
    ell=mcstfa.loglik(Y,g,A,xi,Om,D,w,nu,la)
    loglik.new=ell$loglik
    iter.loglik=c(iter.loglik,loglik.new)
    diff=loglik.new-loglik.old
    if(iter%%per==0) cat("iter=",iter,",\t obs.logli=",loglik.new,"\t diff=",diff,"\n")
    if(diff<tol|iter==max.iter) break
    loglik.old=loglik.new
  }
  z=ell$w.den/rowSums(ell$w.den)
  post.clus=matrix(apply(z,1,order),nrow=g)[g,]
  if(length(unique(post.clus))==length(unique(true.clus)))
  {
    CCR=1-classError(true.clus,post.clus)$errorRate
  }
  else
  {
    CCR=sum(apply(table(post.clus,true.clus),1,max))/n
  }
  ARI=adjustedRandIndex(true.clus,post.clus)
  
  if(eqnu==T & eqD==T) m=(g-1)+p+q*(p+g)+1/2*g*q*(q+1)-q^2+1+g*q
  if(eqnu==F & eqD==T) m=(g-1)+p+q*(p+g)+1/2*g*q*(q+1)-q^2+g+g*q
  if(eqnu==T & eqD==F) m=(g-1)+p*g+q*(p+g)+1/2*g*q*(q+1)-q^2+1+g*q
  if(eqnu==F & eqD==F) m=(g-1)+p*g+q*(p+g)+1/2*g*q*(q+1)-q^2+g+g*q
  
  AIC=2*m-2*loglik.new
  BIC=m*log(n)-2*loglik.new
  ICL=BIC-2*sum(z*log(z+(1e-300)))
  AWE=ICL+3*m+m*log(n)
  mds=c(loglik=loglik.new,no.para=m,AIC=AIC,BIC=BIC,ICL=ICL,ARI=ARI,CCR=CCR)
  
  Ch=chol(t(A)%*%A)
  A=A%*%solve(Ch)
  xi=Ch%*%xi
  la=Ch%*%la
  mu=A%*%xi
  alpha=A%*%la
  Sig=Sig.inv=as.list(g)
  for(i in 1:g)
  {
    DiA=A/diag(D[,,i])
    Om[,,i]=Ch%*%Om[,,i]%*%t(Ch)
    Sig[[i]]=A%*%Om[,,i]%*%t(A)+D[,,i]
    Sig.inv[[i]]=diag(1/diag(D[,,i]))-DiA%*%solve(solve(Om[,,i])+t(A)%*%DiA)%*%t(DiA)
    beta[,,i]=Sig.inv[[i]]%*%A%*%Om[,,i]
    cent=t(Y)-mu[,i]
    V=Sig[[i]]+alpha[,i]%*%t(alpha[,i])
    eigV=eigen(V)
    Vh.inv=eigV$ve%*%diag(1/sqrt(eigV$va),p)%*%t(eigV$ve)
    v.cent=Vh.inv%*%cent
    delta[,i]=colSums(v.cent^2)
    tmp=c(Vh.inv%*%alpha[,i])
    h=colSums(tmp*v.cent)
    si2=1-sum(tmp^2)
    Aj=h/sqrt(si2)
    rn2=(nu[i]+p-2)/(nu[i]+delta[,i])
    cn2=Aj*sqrt(rn2)
    r0=(nu[i]+p)/(nu[i]+delta[,i])
    c0=Aj*sqrt(r0)
    Tc0=pt(c0,df=nu[i]+p)
    tcn2=dt(cn2,df=nu[i]+p-2)
    ga[,i]=h+sqrt(si2)*tcn2/(sqrt(rn2)*Tc0)
  }
  dd=diag(D[,,1])
  if(g>=2)
  {
    for(i in 2:g)
    {
      dd=rbind(dd,diag(D[,,i]))
    }
  }
  para=list(w=w,A=A,xi=xi,Om=Om,D=dd,la=la,mu=mu,Sig=Sig,nu=nu,alpha=alpha)
  out=as.vector(mu)
  z.new=unmap(post.clus)
  zuij=array(NA,dim=c(n,q,g))
  zuij.new=array(NA,dim=c(n,q,g))
  for(i in 1:g)
  {
    for(j in 1:n)
    {
      zuij[j,,i]=z[j,i]*(xi[,i]+t(beta[,,i])%*%(Y[j,]-mu[,i])+(la[,i]-t(beta[,,i])%*%alpha[,i])*ga[j,i])
      zuij.new[j,,i]=z.new[j,i]*(xi[,i]+t(beta[,,i])%*%(Y[j,]-mu[,i])+(la[,i]-t(beta[,,i])%*%alpha[,i])*ga[j,i])
    }
  }
  uhat=apply(zuij,1:2,sum)
  uhat.comp=apply(zuij.new,1:2,sum)
  yhat=t(A%*%t(uhat))
  yhat.comp=t(A%*%t(uhat.comp))
  mse=mean((Y-yhat)^2,na.rm=T)
  mse.comp=mean((Y-yhat.comp)^2,na.rm=T)
  end=proc.time()[1]
  Time=end-begin
  cat("Method=",method,":\t iter=",iter,",\t obs.loglik=",loglik.new,",\t nu=",nu,",\t diff=",diff,"\n")
  cat("no.para=",m,"AIC=",AIC,"BIC=",BIC,"ICL=",ICL,"AWE=",AWE,"ARI=",ARI,"CCR=",CCR,"MSE=",mse,"MSE.comp=",mse.comp,"\n","emcstfa.ecme takes",Time,"seconds\n\n")
  cat(paste(rep("-",60),sep="",collapse=""),"\n")
  return(list(method=method,iter=iter,Time=Time,g=g,q=q,iter.InL=iter.loglik,loglik=loglik.new,mds=mds,para=para,post.clus=post.clus,uhat=uhat,uhat.comp=uhat.comp,yhat=yhat,yhat.comp=yhat.comp,MSE=mse,MSE.comp=mse.comp))
}

















