#MCFA.na

#mcfa-ECME
MCFA.na.ECME=function(Y.na,initial,true.clus,eqnu=F,tol=1e-6,max.iter=100,per=10)
{
  method="ECME"
  begin=proc.time()[1]
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
  Y=Y.na
  Y[is.na(Y)]=99999
  Ip=diag(p)
  w=initial$w
  A=initial$A
  xi=initial$xi
  Om=initial$Om
  D=initial$D
  q=ncol(A)
  g=length(w)
  nu=rep(1000,g)
  la=matrix(0,q,g)
  
  wden=matrix(NA,n,g)
  Sig.g=array(NA,dim=c(p,p,g))
  Y.hat=array(NA,dim=c(n,p,g))
  mu=A%*%xi
  alpha=A%*%la
  for(i in 1:g)
  {
    Sig.g[,,i]=A%*%Om[,,i]%*%t(A)+D
    Y.hat[,,i]=Y
    for(j in 1:num.class.na)
    {
      O=O.list[[j]]
      ind=row.posi[[j]]
      Y.pat=matrix(Y[ind,],ncol=p)
      Y.o=Y.pat%*%t(O)
      OSO=O%*%Sig.g[,,i]%*%t(O)
      mu.o=as.vector(O%*%mu[,i])
      alpha.o=O%*%alpha[,i]
      wden[ind,i]=w[i]*drMST(Y.o,mu.o,OSO,alpha.o,nu[i])
    }
  }
  indv.den=rowSums(wden)
  loglik.old=sum(log(indv.den))
  iter=0
  cat(paste(rep("-",20),sep="",collapse=""),method,":\t MCFA.na: (g=",g,"; p=",p,"; q=",q,")","\n")
  cat("iter=",iter,",\t init.loglik=",loglik.old,sep="","\n")
  iter.loglik=loglik.old
  repeat
  {
    iter=iter+1
    A1=array(0,dim=c(p,q))
    A2=array(0,dim=c(q,q))
    D.g=matrix(0,p,p)
    u=array(NA,dim=c(q,n,g))
    #E-step
    z=wden/indv.den
    #M-step
    ng=colSums(z)
    w=ng/n
    mu=A%*%xi
    alpha=A%*%la
    
    for(i in 1:g)
    {
      tau=s1j=s2j=ga=si2=matrix(NA,1,n)
      eta=zeta=matrix(NA,q,n)
      
      Zeta.sum=Zzeta.sum=rep(0,q)
      ZPsi.sum=matrix(0,q,q)
      ZH.sum=matrix(0,p,q)
      ZUpsilon.sum=matrix(0,p,p)
      
      Sig.g[,,i]=A%*%Om[,,i]%*%t(A)+D
      
      V=Sig.g[,,i]+alpha[,i]%*%t(alpha[,i])
      for(j in 1:num.class.na)
      {
        O=O.list[[j]]
        ind=row.posi[[j]]
        po=nrow(O)
        num=length(ind)
        OSO=O%*%Sig.g[,,i]%*%t(O)
        OVO=O%*%V%*%t(O)
        ODO=O%*%D%*%t(O)
        Y.pat=matrix(Y[ind,],ncol=p)
        Y.o=Y.pat%*%t(O)
        mu.o=as.vector(O%*%mu[,i])
        alpha.o=O%*%alpha[,i]
        
        SOO=t(O)%*%solve(OSO)%*%O
        COO=t(O)%*%solve(ODO)%*%O
        DCOO=D%*%COO
        IDOO=(diag(p)-DCOO)%*%D
        IAOO=(diag(p)-DCOO)%*%A
        ISOO=diag(p)-Sig.g[,,i]%*%SOO
        
        cent.o=t(Y.o)-mu.o
        cent=t(Y.pat)-mu[,i]
        
        betaoo=SOO%*%A%*%Om[,,i]
        
        a=mu[,i]+Sig.g[,,i]%*%SOO%*%cent
        b=xi[,i]+t(betaoo)%*%cent
        d=(diag(q)-t(betaoo)%*%A)%*%la[,i]
        
        eigV=eigen(OVO)
        V.sq.inv=eigV$ve%*%diag(1/sqrt(eigV$va),po)%*%t(eigV$ve)
        V.cent=V.sq.inv%*%cent.o
        delta=colSums(V.cent^2)
        tmp=c(V.sq.inv%*%alpha.o)
        h=colSums(tmp*V.cent)
        si2[,ind]=1-sum(tmp^2)
        Aj=h/sqrt(si2[,ind]) #M
        
        rn2=(nu[i]+po-2)/(nu[i]+delta)
        cn2=Aj*sqrt(rn2)
        r0=(nu[i]+po)/(nu[i]+delta)
        c0=Aj*sqrt(r0)
        r2=(nu[i]+po+2)/(nu[i]+delta)
        c2=Aj*sqrt(r2)
        Tc0=pt(c0,df=nu[i]+po)
        Tc2=pt(c2,df=nu[i]+po+2)
        tc0=dt(c0,df=nu[i]+po)
        tcn2=dt(cn2,df=nu[i]+po-2)
        
        tau[,ind]=r0*(Tc2/Tc0)
        s1j[,ind]=h*tau[,ind]+sqrt(si2[,ind])*sqrt(r0)*tc0/Tc0
        s2j[,ind]=h*s1j[,ind]+si2[,ind]
        ga[,ind]=h+sqrt(si2[,ind])*tcn2/(sqrt(rn2)*Tc0)
        
        
        u[,ind,i]=b+d%*%t(ga[,ind])
        eta[,ind]=t(t(b)*tau[,ind])+d%*%t(s1j[,ind])
        zeta[,ind]=t(t(b)*s1j[,ind])+d%*%t(s2j[,ind])
        Y.hat[ind,,i]=t(a+ISOO%*%alpha[,i]%*%t(ga[,ind]))
        
        if(q==1)
        {
          Zeta=sum(z[ind,i]*eta[,ind])
          Zzeta=sum(z[ind,i]*zeta[,ind])
        }
        else{
          Zeta=colSums(z[ind,i]*t(eta[,ind]))
          Zzeta=colSums(z[ind,i]*t(zeta[,ind]))
        }
        Zeta.sum=Zeta.sum+Zeta
        Zzeta.sum=Zzeta.sum+Zzeta
        ZPsi=Zzeta%*%t(d)+eta[,ind]%*%(z[ind,i]*t(b))+sum(z[ind,i])*(diag(q)-t(betaoo)%*%A)%*%Om[,,i]
        ZPsi.sum=ZPsi.sum+ZPsi
        if(q==1)
        {
          ZH=IAOO%*%ZPsi+DCOO%*%t(Y.pat)%*%matrix((z[ind,i]*eta[,ind]),ncol=1)
        }
        else{
          ZH=IAOO%*%ZPsi+DCOO%*%t(Y.pat)%*%(z[ind,i]*t(eta[,ind]))
        }
        ZH.sum=ZH.sum+ZH
        ZPhi=sum(z[ind,i])*ISOO%*%Sig.g[,,i]+a%*%((z[ind,i]*tau[,ind])*t(a))+matrix(rep(ISOO%*%alpha[,i],num),ncol=num)%*%(2*z[ind,i]*s1j[,ind]*t(a))+sum(z[ind,i]*s2j[,ind])*ISOO%*%alpha[,i]%*%t(ISOO%*%alpha[,i])
        ZUpsilon=ZPhi-ZH%*%t(A)-A%*%t(ZH)+A%*%ZPsi%*%t(A)
        ZUpsilon.sum=ZUpsilon.sum+ZUpsilon
      }
      Ztau.sum=sum(z[,i]*tau)
      Zs1j.sum=sum(z[,i]*s1j)
      Zs2j.sum=sum(z[,i]*s2j)
      
      #CM-step
      xi[,i]=(Zs2j.sum*Zeta.sum-Zs1j.sum*Zzeta.sum)/(Zs2j.sum*Ztau.sum-Zs1j.sum^2)
      #la[,i]=(Ztau.sum*Zzeta.sum-Zs1j.sum*Zeta.sum)/(Zs2j.sum*Ztau.sum-Zs1j.sum^2)
      A1=A1+ZH.sum
      A2=A2+ZPsi.sum
      Om1=ZPsi.sum-Zeta.sum%*%t(xi[,i])-Zzeta.sum%*%t(la[,i])-xi[,i]%*%(t(Zeta.sum)-Ztau.sum*t(xi[,i])-Zs1j.sum*t(la[,i]))-la[,i]%*%(t(Zzeta.sum)-Zs1j.sum*t(xi[,i])-Zs2j.sum*t(la[,i]))
      Om[,,i]=Om1/ng[i]
      D1=ZUpsilon.sum
      D.g=D.g+diag(diag(D1/n),p)
    }
    A=A1%*%solve(A2)
    D=D.g
    
    mu=A%*%xi
    alpha=A%*%la
    for(i in 1:g)
    {
      Sig.g[,,i]=A%*%Om[,,i]%*%t(A)+D
      for(j in 1:num.class.na)
      {
        O=O.list[[j]]
        ind=row.posi[[j]]
        Y.pat=matrix(Y[ind,],ncol=p)
        Y.o=Y.pat%*%t(O)
        OSO=O%*%Sig.g[,,i]%*%t(O)
        mu.o=as.vector(O%*%mu[,i])
        alpha.o=O%*%alpha[,i]
        wden[ind,i]=w[i]*drMST(Y.o,mu.o,OSO,alpha.o,nu[i])
      }
    }
    indv.den=rowSums(wden)
    loglik.new=sum(log(indv.den))
    
    iter.loglik=c(iter.loglik,loglik.new)
    diff=loglik.new-loglik.old
    if(iter%%per==0) cat("iter=",iter,",\t obs.logli=",loglik.new,"\t diff=",diff,"\n")
    if(diff<tol|iter==max.iter) break
    loglik.old=loglik.new
  }
  z=wden/indv.den
  post.clus=matrix(apply(z,1,order),nrow=g)[g,]
  if(length(unique(post.clus))==length(unique(true.clus)))
  {
    CCR=1-classError(true.clus,post.clus)$errorRate
  }
  else {
    CCR=sum(apply(table(post.clus,true.clus),1,max))/n
  }
  ARI=adjustedRandIndex(true.clus,post.clus)
  m=(g-1)+q*g+(p*q-q^2)+p+1/2*q*(q+1)*g
  
  AIC=2*m-2*loglik.new
  BIC=m*log(n)-2*loglik.new
  ICL=BIC-2*sum(z*log(z+(1e-300)))
  AWE=ICL+3*m+m*log(n)
  
  Ch=chol(t(A)%*%A)
  A=A%*%solve(Ch)
  xi=Ch%*%xi
  la=Ch%*%la
  mu=A%*%xi
  alpha=A%*%la
  Sig=Sig.inv=as.list(g)
  DiA=A/diag(D)
  for(i in 1:g)
  {
    Om[,,i]=Ch%*%Om[,,i]%*%t(Ch)
    Sig[[i]]=A%*%Om[,,i]%*%t(A)+D
    Sig.inv[[i]]=diag(1/diag(D))-DiA%*%solve(solve(Om[,,i])+t(A)%*%DiA)%*%t(DiA)
    
    V=Sig[[i]]+alpha[,i]%*%t(alpha[,i])
    for(j in 1:num.class.na)
    {
      O=O.list[[j]]
      ind=row.posi[[j]]
      po=nrow(O)
      OSO=O%*%Sig.g[,,i]%*%t(O)
      OVO=O%*%V%*%t(O)
      Y.pat=matrix(Y[ind,],ncol=p)
      Y.o=Y.pat%*%t(O)
      mu.o=as.vector(O%*%mu[,i])
      alpha.o=O%*%alpha[,i]
      
      SOO=t(O)%*%solve(OSO)%*%O
      cent.o=t(Y.o)-mu.o
      cent=t(Y.pat)-mu[,i]
      betaoo=SOO%*%A%*%Om[,,i]
      b=xi[,i]+t(betaoo)%*%cent
      d=(diag(q)-t(betaoo)%*%A)%*%la[,i]
      
      eigV=eigen(OVO)
      V.sq.inv=eigV$ve%*%diag(1/sqrt(eigV$va),po)%*%t(eigV$ve)
      V.cent=V.sq.inv%*%cent.o
      delta=colSums(V.cent^2)
      tmp=c(V.sq.inv%*%alpha.o)
      h=colSums(tmp*V.cent)
      si2[,ind]=1-sum(tmp^2)
      Aj=h/sqrt(si2[,ind])
      
      rn2=(nu[i]+po-2)/(nu[i]+delta)
      cn2=Aj*sqrt(rn2)
      r0=(nu[i]+po)/(nu[i]+delta)
      c0=Aj*sqrt(r0)
      Tc0=pt(c0,df=nu[i]+po)
      tcn2=dt(cn2,df=nu[i]+po-2)
      
      ga[,ind]=h+sqrt(si2[,ind])*tcn2/(sqrt(rn2)*Tc0)
      u[,ind,i]=b+d%*%t(ga[,ind])
    }
  }
  est.para=list(w=w,A=A,xi=xi,la=la,Om=Om,D=D,mu=mu,Sig=Sig.g,alpha=alpha,nu=nu)
  
  #out=as.vector(mu)
  z.new=matrix(0,n,g)
  for(i in 1:g)
  {
    if(length(which(post.clus==i))==0) z.new[,i]=0
    else{
      z.new[which(post.clus==i),i]=1
    }
  }
  zuij=array(NA,dim=c(n,q,g))
  zuij.new=array(NA,dim=c(n,q,g))
  for(i in 1:g)
  {
    zuij[,,i]=z[,i]*t(u[,,i])
    zuij.new[,,i]=z.new[,i]*t(u[,,i])
  }
  uhat=apply(zuij,1:2,sum)
  uhat.new=apply(zuij.new,1:2,sum)
  yhat=t(A%*%t(uhat))
  yhat.new=t(A%*%t(uhat.new))
  nona.ind=which(as.vector(t(na.posi))==F)
  Y.obs=as.vector(t(Y))[nona.ind]
  yhat.obs=as.vector(t(yhat))[nona.ind]
  yhat.obs.new=as.vector(t(yhat.new))[nona.ind]
  
  mse=mean((Y.obs-yhat.obs)^2,na.rm=T)
  mse.new=mean((Y.obs-yhat.obs.new)^2,na.rm=T)
  
  ZY=array(NA,dim=c(n,p,g))
  for(i in 1:g)
  {
    #ZY[,,i]=z[,i]*Y.hat[,,i]
    ZY[,,i]=z.new[,i]*Y.hat[,,i]
  }
  Y.comp=apply(ZY,1:2,sum)
  na.ind=which(as.vector(t(na.posi))==T)
  Y.mis=as.vector(t(Y.comp))[na.ind]
  
  cat("iter=",iter,",\t obs.loglik=",loglik.new,sep="","\n")
  end=proc.time()[1]
  sec.time=end-begin
  cat("It took",sec.time,"seconds. \n")
  cat(paste(rep("-",50),sep="",collapse=""),"\n")
  return(list(iter=iter,iter.InL=iter.loglik,sec.time=sec.time,g=g,q=q,loglik=loglik.new,m=m,AIC=AIC,BIC=BIC,ICL=ICL,AWE=AWE,CCR=CCR,ARI=ARI,para=est.para,post.clus=post.clus,uhat=uhat,uhat.new=uhat.new,Y.comp=Y.comp,Y.mis=Y.mis,yhat=yhat,yhat.new=yhat.new,MSE=mse,MSE.new=mse.new,Y.obs=Y.obs,yhat.obs.new=yhat.obs.new))
}

