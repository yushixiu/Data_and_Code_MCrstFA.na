#### simulation2 ####
p=6
g=3
q=2
initial=NULL
initial$nu = c(6,10,4)
initial$la = matrix(c(3,-3,-7,7,5,-5),q,g)
initial$w = c(1/3,1/3,1/3)
initial$xi = matrix(c(5.5, 5.5, -7.5, -7.5, 1.5, -1.5),q,g)
initial$A=matrix(c(0.05,0.37,0.12,0.69,0.01,0.67,0.46,0.07,0.22,0.59,0.66,0.84),p,q)
initial$D=diag(c(0.25,0.05,0.35,0.15,0.25,0.55))/20
initial$Om = array(0, dim=c(q,q,g))
initial$Om[,,1] = matrix(c(0.2,0.15,0.15,0.35),q,q)*3
initial$Om[,,2] = matrix(c(0.55,0.2,0.2,0.4),q,q)*3
initial$Om[,,3] = matrix(c(0.35,0.1,0.1,0.35),q,q)*3
initial$mu=initial$A%*%initial$xi
initial$alpha=initial$A%*%initial$la
initial$Sig=array(NA,dim=c(p,p,g))
for(i in 1:g)
{
  initial$Sig[,,i]=initial$A%*%initial$Om[,,i]%*%t(initial$A)+initial$D
}
n=c(300,900,1800)
true.clus1 = c(rep(1,n[1]),rep(2,n[1]),rep(3,n[1]))
true.clus2 = c(rep(1,n[2]),rep(2,n[2]),rep(3,n[2]))
true.clus3 = c(rep(1,n[3]),rep(2,n[3]),rep(3,n[3]))
max.iter=2000
per=100
se1=se2=se3=list()
for(j in 1:m)
{
  Y1=Y2=Y3=NULL
  for(i in 1:g)
  {
    Y1=rbind(Y1, rMST(n[1], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
    Y2=rbind(Y2, rMST(n[2], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
    Y3=rbind(Y3, rMST(n[3], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
  }
  Y.na1=gener.na(Y1,miss.rate)
  Y.na2=gener.na(Y2,miss.rate)
  Y.na3=gener.na(Y3,miss.rate)
  fit1=MCrstFA.na.ECME(Y.na=Y.na1,initial=initial,true.clus=true.clus1,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  if('try-error' %in% class(fit1))
  {
    for(i in 1:g)
    {
      Y1=rbind(Y1, rMST(n[1], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
    }
    Y.na1=gener.na(Y1,miss.rate)
    fit1=MCrstFA.na.ECME(Y.na=Y.na1,initial=initial,true.clus=true.clus1,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  }
  else{fit1=MCrstFA.na.ECME(Y.na=Y.na1,initial=initial,true.clus=true.clus1,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)}
  
  fit2=MCrstFA.na.ECME(Y.na=Y.na2,initial=initial,true.clus=true.clus2,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  if('try-error' %in% class(fit2))
  {
    for(i in 1:g)
    {
      Y2=rbind(Y2, rMST(n[2], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
    }
    Y.na2=gener.na(Y2,miss.rate)
    fit2=MCrstFA.na.ECME(Y.na=Y.na2,initial=initial,true.clus=true.clus2,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  }
  else{fit2=MCrstFA.na.ECME(Y.na=Y.na2,initial=initial,true.clus=true.clus2,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)}
  
  fit3=MCrstFA.na.ECME(Y.na=Y.na3,initial=initial,true.clus=true.clus3,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  if('try-error' %in% class(fit3))
  {
    for(i in 1:g)
    {
      Y3=rbind(Y3, rMST(n[3], initial$mu[,i], initial$Sig[,,i], initial$alpha[,i], initial$nu[i]))
    }
    Y.na3=gener.na(Y3,miss.rate)
    fit3=MCrstFA.na.ECME(Y.na=Y.na3,initial=initial,true.clus=true.clus3,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)
  }
  else{fit3=MCrstFA.na.ECME(Y.na=Y.na3,initial=initial,true.clus=true.clus3,eqnu=F,tol=1e-6,max.iter=max.iter,per=per)}
  #se
  se1[[j]]=SE.MCrSTFA.na(Y.na=Y.na1,w=fit1$para$w,xi=fit1$para$xi,A=fit1$para$A,D=fit1$para$D,Om=fit1$para$Om,la=fit1$para$la,nu=fit1$para$nu,true.clus=true.clus1,eqnu=F)
  se2[[j]]=SE.MCrSTFA.na(Y.na=Y.na2,w=fit2$para$w,xi=fit2$para$xi,A=fit2$para$A,D=fit2$para$D,Om=fit2$para$Om,la=fit2$para$la,nu=fit2$para$nu,true.clus=true.clus2,eqnu=F)
  se3[[j]]=SE.MCrSTFA.na(Y.na=Y.na3,w=fit3$para$w,xi=fit3$para$xi,A=fit3$para$A,D=fit3$para$D,Om=fit3$para$Om,la=fit3$para$la,nu=fit3$para$nu,true.clus=true.clus3,eqnu=F)
  write(c(j,fit1$para$w), paste(PATH,'para.w_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$A), paste(PATH,'para.A_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$xi), paste(PATH,'para.xi_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$la), paste(PATH,'para.la_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$Om), paste(PATH,'para.Om_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$D), paste(PATH,'para.D_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit1$para$nu), paste(PATH,'para.nu_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$w), paste(PATH,'para.w_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$A), paste(PATH,'para.A_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$xi), paste(PATH,'para.xi_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$la), paste(PATH,'para.la_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$Om), paste(PATH,'para.Om_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$D), paste(PATH,'para.D_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit2$para$nu), paste(PATH,'para.nu_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$w), paste(PATH,'para.w_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$A), paste(PATH,'para.A_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$xi), paste(PATH,'para.xi_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$la), paste(PATH,'para.la_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$Om), paste(PATH,'para.Om_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$D), paste(PATH,'para.D_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,fit3$para$nu), paste(PATH,'para.nu_n3.txt',sep=''), ncol=10000, append=T)
  write(c(j,se1[[j]][,2]), paste(PATH,'se_n1.txt',sep=''), ncol=10000, append=T)
  write(c(j,se2[[j]][,2]), paste(PATH,'se_n2.txt',sep=''), ncol=10000, append=T)
  write(c(j,se3[[j]][,2]), paste(PATH,'se_n3.txt',sep=''), ncol=10000, append=T)
}


