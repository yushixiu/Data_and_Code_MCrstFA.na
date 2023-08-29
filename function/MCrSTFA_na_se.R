#MCrSTFA.na.se
SE.MCrSTFA.na = function(Y.na, w, xi, A, D, Om, la, nu, true.clus, eqnu=T)
{  
  n = nrow(Y.na)
  p = ncol(Y.na)
  q = ncol(A)
  g = length(w)
  
  # O matrices 
  na.posi = is.na(Y.na)                            
  na.class = colSums( t(na.posi)*2^(1:(p-1)) )
  uni.na.class = unique(na.class) 
  num.na.class = length(uni.na.class)            
  Ip = diag(p)
  O.list = ind.list = as.list(num.na.class)                                                                      
  for(i in 1:num.na.class)
  {
    ind.list[[i]]= which(na.class == uni.na.class[i])  
    O.list[[i]] = matrix(diag(p)[!na.posi[ind.list[[i]][1],],], ncol = p) 
  }
  
  Y=Y.na
  Y[is.na(Y)]=99999
  Sig=array(NA,dim=c(p,p,g))
  alpha=array(NA,dim=c(p,1,g))
  w.den=matrix(NA,n,g)
  Y.hat=array(NA,dim=c(n,p,g))
  mu=A%*%xi
  
  for(i in 1:g){
    Sig[,,i] = A %*% Om[,,i] %*% t(A) + D
    alpha[,,i] = A %*% la[,i]
    Y.hat[,,i]=Y
    for(k in 1:num.na.class)
    {   
      O = O.list[[k]]
      ind = ind.list[[k]] 
      Y.pat.hat = matrix(Y[ind,] , ncol = p) 
      Y.o = Y.pat.hat %*% t(O)  
      OSO = O %*% Sig[,,i] %*% t(O)  #Sigma.oo
      mu.o = as.vector(O%*%mu[,i])  #mu.o
      alpha.o = O %*% alpha[,,i]   #alpha.o
      w.den[ind,i] = w[i] * drMST(Y.o, mu.o, OSO, alpha.o, nu=nu[i], log=F) 
    }
  }
  
  tau = log.tau = matrix(NA,n,g)
  ka2 = array(NA,dim=c(1,1,n))  
  hg1 = matrix(NA,1,n)  
  hg2 = array(NA,dim=c(1,1,n))    
  gamma1 = array(NA, dim=c(n,1,g))
  gamma2 = array(NA,dim=c(1,1,n)) 
  Beta = array(NA,dim=c(p,q,g)) 
  eta =  array(NA,dim=c(n,q,g)) 
  zeta =  array(NA,dim=c(q,1,n)) 
  Psi = array(NA,dim=c(q,q,n)) 
  Phi = array(NA,dim=c(p,p,n))
  H = array(NA,dim=c(p,q,n))
  SSw = matrix(NA,n,g-1)
  SSxi = SSla = SSom = SS = NULL
  SSdr = SSa = 0
  if (eqnu == T){
    SSnu = 0
  } else { 
    SSnu = NULL
  }
  
  vech.A = outer(1:p,1:q) >= 0
  vec.xl = outer(1:q,1:g) >= 0
  
  
  Z = w.den / rowSums(w.den)
  if (g > 1){
    for (i in 1:(g-1)){
      SSw[,i] = Z[,i]/w[i] - Z[,g]/w[g]
    }
  }
  
  for (i in 1:g)
  {
    ss = NULL
    sxi = sla = matrix(NA,n,q)
    som = matrix(NA,n,length(Om[,,i][!upper.tri(Om[,,i])]))
    sa = matrix(NA,n,length(A[vech.A]))
    sdr = matrix(NA,n,p)
    snu = rep(NA,n)
    
    V = Sig[,,i] + alpha[,,i] %*% t(alpha[,,i])
    
    for(k in 1:num.na.class){
      ind = ind.list[[k]]
      O = O.list[[k]]
      po = nrow(O)  
      num = length(ind)
      
      #
      OSO = O %*% Sig[,,i] %*% t(O)
      OVO = O %*% V %*% t(O)
      OVO.inv = solve(OVO)	
      ODO = O %*% D %*% t(O)
      Y.pat.hat = matrix(Y[ind,], ncol = p) 
      Y.o = Y.pat.hat %*% t(O)
      mu.o = c(O %*% mu[,i]) #dim=pjo*1
      alpha.o = O %*% alpha[,,i] #dim=pjo*q
      
      cent.o = t(Y.o) - mu.o  #dim=pjo*num
      cent = t(Y.pat.hat) - mu[,i]  #dim=p*num																																 
      
      Soo = t(O) %*% solve(OSO) %*% O 
      Coo = t(O) %*% solve(ODO) %*% O
      DCoo = D %*% Coo
      IAoo = (Ip-DCoo) %*% A  
      ISoo = Ip - Sig[,,i] %*% Soo 
      
      
      Delta_Sigma = round(diag(1) - t(alpha.o) %*% OVO.inv %*% alpha.o, digits=8) #Gammai
      h = t(alpha.o) %*% OVO.inv %*% cent.o 
      M = colSums((OVO.inv %*% cent.o)*cent.o) 
      A1 = sqrt((nu[i]+po+2)/(nu[i]+M)) * t(h) 
      A2 = sqrt((nu[i]+po)/(nu[i]+M)) * t(h) 
      NU = round(nu[i],0)
      cdf1 = apply(t(A1), 2, pmvt, lower = rep(-Inf, 1), delta = rep(0, 1), sigma = Delta_Sigma, df = NU+po+2) 
      cdf2 = apply(t(A2), 2, pmvt, lower = rep(-Inf, 1), delta = rep(0, 1), sigma = Delta_Sigma, df = NU+po)  
      tau[ind,i] = ((nu[i]+po)/(nu[i]+M))*(cdf1/cdf2)  #num
      
      #E(log_tau|yjo,zij=1)
      f = NULL;
      for (j in 1:num){
        inx = ind[j]
        con = digamma( (nu[i] + po + 1)/2 ) - digamma((nu[i] + po)/2) - 1/(nu[i] + M[j]) #f_nu() function
        
        #f[j] = adaptIntegrate( function(x) dmvt( x, delta = rep(0,1), sigma = (nu[i]+M[j])/(nu[i]+po)*Delta_Sigma, df = nu[i]+po, log = FALSE)* (con-log(1+t(x)%*%solve(Delta_Sigma)%*%x/(M[j]+nu[i]))+(nu[i]+po+1)*t(x)%*%solve(Delta_Sigma)%*%x/((M[j]+nu[i]+t(x)%*%solve(Delta_Sigma)%*%x)*(M[j]+nu[i])) ), rep(-100000,q), c(h[,j]), maxEval=2000000,absError=10e-5,tol=1e-5 )$integral
        f[j]=adaptIntegrate(function(x) dmvt(x,delta=rep(0,1),sigma=matrix((nu[i]+M[j])/(nu[i]+po)*Delta_Sigma,1,1),df=nu[i]+po,log=FALSE)*(con-log(1+t(x)%*%solve(Delta_Sigma)%*%x/(M[j]+nu[i]))+(nu[i]+po+1)*t(x)%*%solve(Delta_Sigma)%*%x/((M[j]+nu[i]+t(x)%*%solve(Delta_Sigma)%*%x)*(M[j]+nu[i]))),rep(-100000,1),c(h[,j]),maxEval = 2000000,absError = 10e-5,tol = 1e-5)$integral
        log.tau[inx,i] = tau[inx,i] - log((nu[i] + M[j]) / 2) - ((nu[i]+po)/(nu[i]+M[j])) + digamma((nu[i] + po) / 2) + 1 / cdf2[j] * f[j]
        
        ka2[,,inx] = (nu[i]+M[j])/(nu[i]+po+2) * Delta_Sigma
        
        tmt.mom = meanvarTMD(lower = rep(0 ,1), upper = rep(1e12, 1), mu=h[,j], Sigma=ka2[,,inx], nu=nu[i]+po+2, dist="t")
        hg1[,inx] = tmt.mom$mean  
        hg2[,,inx] = tmt.mom$EYY 
        gamma2[,,inx] = tau[inx,i] * hg2[,,inx]  #dim=1*1*n 
      }
      gamma1[ind,,i] = tau[ind,i] * t(hg1[,ind]) #dim=n*1    
      
      #U|(y,gamma,tau,Zij=1)~Nq()
      Beta[,,i] = Soo %*% A %*% Om[,,i]  #dim=p*q
      mSc = mu[,i] + Sig[,,i] %*% Soo %*% cent  #dim=p*num 
      b = xi[,i] + t(Beta[,,i]) %*% cent    #q*1 
      d = la[,i] - t(Beta[,,i]) %*% alpha[,,i]  #q*1
      
      for (j in 1:num){
        inx = ind[j]
        eta[inx,,i] = t(b[,j]*tau[inx,i] + d %*% gamma1[inx,,i]) #dim=1*q
        zeta[,,inx] = b[,j] %*% t(gamma1[inx,,i]) + d %*% gamma2[,,inx] #q*1
        Psi[,,inx] = b[,j] %*% t(eta[inx,,i]) + zeta[,,inx] %*% t(d) + ( diag(1,q) - t(Beta[,,i]) %*% A ) %*% Om[,,i]
        Phi1 = mSc[,j] %*% t(gamma1[inx,,i]) %*% t(alpha[,,i]) %*% t(ISoo)
        Phi[,,inx] = tau[inx,i]*mSc[,j] %*% t(mSc[,j]) + Phi1 + t(Phi1) + ISoo %*% alpha[,,i] %*% gamma2[,,inx] %*% t(alpha[,,i]) %*% t(ISoo) + ISoo %*% Sig[,,i]
        H[,,inx] = IAoo %*% Psi[,,inx] + DCoo %*% Y.pat.hat[j,] %*% t(eta[inx,,i])
        
        
        sxi[inx,] = Z[inx,i] * solve(Om[,,i]) %*% t(eta[inx,,i]-tau[inx,i]*t(xi[,i])-gamma1[inx,,i]%*%t(la[,i]))
        sla[inx,] = Z[inx,i]*solve(Om[,,i])%*%(zeta[,,inx]-xi[,i]%*%t(gamma1[inx,,i])-t(gamma2[,,inx]%*%la[,i]))
        
        tmp.Om = Psi[,,inx] - eta[inx,,i] %*% t(xi[,i]) - xi[,i] %*% ( t(eta[inx,,i]) - tau[inx,i] * t(xi[,i]) - gamma1[inx,,i] %*% t(la[,i]) ) + la[,i] %*% ( gamma1[inx,,i] %*% t(xi[,i]) - t(zeta[,,inx]) + gamma2[,,inx] %*% la[,i] ) - zeta[,,inx] %*% t(la[,i])
        Omj = -1/2 * Z[inx,i] * ( solve(Om[,,i]) - solve(Om[,,i]) %*% tmp.Om %*% solve(Om[,,i]) )
        som[inx,] = c(Omj[!upper.tri(Omj)])
        
        Aj = Z[inx,i] * solve(D) %*% ( H[,,inx] - A %*% Psi[,,inx] )
        sa[inx,] = c(Aj[vech.A]) 
        
        tmp.D = Phi[,,inx] - H[,,inx] %*% t(A) - A %*% t(H[,,inx]) + A %*% Psi[,,inx] %*% t(A)
        sdr[inx,] = diag( -1/2 * Z[inx,i] * (solve(D) - solve(D) %*% tmp.D %*% solve(D) ))
        
        snu[inx] = 1/2 * Z[inx,i] * ( log.tau[inx,i] - tau[inx,i] + log(nu[i]/2) - digamma(nu[i]/2) + 1 )
      }
    }
    SSxi = cbind(SSxi, sxi)
    SSla = cbind(SSla, sla)
    SSom = cbind(SSom, som)
    SSa = SSa + sa
    SSdr = SSdr + sdr
    if (eqnu == T){
      SSnu = SSnu + snu} else {
        SSnu = cbind(SSnu, snu)
      }
  }
  
  est.om=para.name.om=NULL
  for(i in 1:g)
  {
    est.om=c(est.om,Om[,,i][!upper.tri(Om[,,i])])
    para.name.om=c(para.name.om,paste("om",outer(1:q,1:q,paste,sep="")[!upper.tri(Om[,,i])],",",i,sep=""))
  }
  if(g>=3)
  {
    SE.w=sqrt(diag(solve(t(SSw)%*%SSw)))
    SE.xi=sqrt(diag(solve(t(SSxi)%*%SSxi)))
    SE.la=sqrt(diag(solve(t(SSla)%*%SSla)))
    SE.om=sqrt(diag(solve(t(SSom)%*%SSom)))
    SE.dr=sqrt(diag(solve(t(SSdr)%*%SSdr)))
    SE.nu=sqrt(diag(solve(t(SSnu)%*%SSnu)))
    SE.a=sqrt(diag(solve(t(SSa)%*%SSa)))
    
    SE.all=c(SE.w,SE.xi,SE.la,SE.om,SE.dr,SE.nu,SE.a)
    
    if(eqnu==T)
    {
      para=c(w[1:g-1],c(xi),c(la),est.om,diag(D),nu[1],A[vech.A])
      para.name.nu=c(paste("nu",1,sep=""))
    }
    else{
      para=c(w[1:g-1],c(xi),c(la),est.om,diag(D),nu,A[vech.A])
      para.name.nu=c(paste("nu",1:g,sep=""))
    }
    para.SE=cbind(est=para,se=SE.all)
    para.name=c(paste("w",1:(g-1),sep=""),paste("xi",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),paste("la",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),para.name.om,paste("d",diag(outer(1:p,1:p,paste,sep="")),sep=""),para.name.nu,paste("a",outer(1:p,1:q,paste,sep="")[vech.A],sep=""))
    rownames(para.SE)=para.name
  }
  else if(g==2)
  {
    SS=cbind(SS,SSw,SSxi,SSla,SSom,SSdr,SSnu)
    SE=sqrt(diag(solve(t(SS)%*%SS)))
    SE.a=sqrt(diag(solve(t(SSa)%*%SSa)))
    SE.all=c(SE,SE.a)
    
    if(eqnu==T)
    {
      para=c(w[1:g-1],c(xi),c(la),est.om,diag(D),nu[1],A[vech.A])
      para.name.nu=c(paste("nu",1,sep=""))
    }
    else{
      para=c(w[1:g-1],c(xi),c(la),est.om,diag(D),nu,A[vech.A])
      para.name.nu=c(paste("nu",1:g,sep=""))
    }
    para.SE=cbind(est=para,se=SE.all)
    para.name=c(paste("w",1:(g-1),sep=""),paste("xi",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),paste("la",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),para.name.om,paste("d",diag(outer(1:p,1:p,paste,sep="")),sep=""),para.name.nu,paste("a",outer(1:p,1:q,paste,sep="")[vech.A],sep=""))
    rownames(para.SE)=para.name
  }
  else if(g==1)
  {
    SS=cbind(SS,SSxi,SSla,SSom,SSdr,SSnu)
    SE=sqrt(diag(solve(t(SS)%*%SS)))
    SE.a=sqrt(diag(solve(t(SSa)%*%SSa)))
    SE.all=c(SE,SE.a)
    para=c(c(xi),c(la),est.om,diag(D),nu,A[vech.A])
    para.SE=cbind(est=para,se=SE.all)
    para.name=c(paste("xi",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),paste("la",outer(1:q,1:g,paste,sep=",")[vec.xl],sep=""),para.name.om,paste("d",diag(outer(1:p,1:p,paste,sep="")),sep=""),paste("nu",g,sep=""),paste("a",outer(1:p,1:q,paste,sep="")[vech.A],sep=""))
    rownames(para.SE)=para.name
  }
  return(para.SE)
}




