####### RMSE #######
pred.error=function(para.true, para.est)
{
  mse=(para.true-para.est)^2
  rmse=sqrt(mse)
  return(list(MSE=mse, RMSE=rmse))
}
rmse_xi_n1=rmse_xi_n2=rmse_xi_n3=matrix(NA,q,g)
rmse_la_n1=rmse_la_n2=rmse_la_n3=matrix(NA,q,g)
rmse_Om_n1=rmse_Om_n2=rmse_Om_n3=array(0, dim=c(q,q,g))
rmse_w_n1=rmse_w_n2=rmse_w_n3=c()
rmse_nu_n1=rmse_nu_n2=rmse_nu_n3=c()
rmse_A_n1=rmse_A_n2=rmse_A_n3=matrix(NA,p,q)
rmse_D_n1=rmse_D_n2=rmse_D_n3=matrix(NA,p,p)

sum_xi_n1=sum_xi_n2=sum_xi_n3=matrix(0,q,g)
sum_la_n1=sum_la_n2=sum_la_n3=matrix(0,q,g)
sum_Om_n1=sum_Om_n2=sum_Om_n3=array(0, dim=c(q,q,g))
sum_w_n1=sum_w_n2=sum_w_n3=c(0)
sum_nu_n1=sum_nu_n2=sum_nu_n3=c(0)
sum_A_n1=sum_A_n2=sum_A_n3=matrix(0,p,q)
sum_D_n1=sum_D_n2=sum_D_n3=matrix(0,p,p)

for(j in 1:m)
{
  for(i in 1:g)
  {
    rmse_xi_n1[,i]=pred.error(initial$xi[,i], para_xi_n1[[j]][,i])$RMSE
    rmse_xi_n2[,i]=pred.error(initial$xi[,i], para_xi_n2[[j]][,i])$RMSE
    rmse_xi_n3[,i]=pred.error(initial$xi[,i], para_xi_n3[[j]][,i])$RMSE
    rmse_la_n1[,i]=pred.error(initial$la[,i], para_la_n1[[j]][,i])$RMSE
    rmse_la_n2[,i]=pred.error(initial$la[,i], para_la_n2[[j]][,i])$RMSE
    rmse_la_n3[,i]=pred.error(initial$la[,i], para_la_n3[[j]][,i])$RMSE
    rmse_Om_n1[,,i]=pred.error(initial$Om[,,i], para_Om_n1[[j]][,,i])$RMSE
    rmse_Om_n2[,,i]=pred.error(initial$Om[,,i], para_Om_n2[[j]][,,i])$RMSE
    rmse_Om_n3[,,i]=pred.error(initial$Om[,,i], para_Om_n3[[j]][,,i])$RMSE
  }
  rmse_w_n1=pred.error(initial$w, w_n1[j,])$RMSE
  rmse_w_n2=pred.error(initial$w, w_n2[j,])$RMSE
  rmse_w_n3=pred.error(initial$w, w_n3[j,])$RMSE
  rmse_nu_n1=pred.error(initial$nu, nu_n1[j,])$RMSE
  rmse_nu_n2=pred.error(initial$nu, nu_n2[j,])$RMSE
  rmse_nu_n3=pred.error(initial$nu, nu_n3[j,])$RMSE
  rmse_A_n1=pred.error(initial$A, para_A_n1[[j]])$RMSE
  rmse_A_n2=pred.error(initial$A, para_A_n2[[j]])$RMSE
  rmse_A_n3=pred.error(initial$A, para_A_n3[[j]])$RMSE
  rmse_D_n1=pred.error(initial$D, para_D_n1[[j]])$RMSE
  rmse_D_n2=pred.error(initial$D, para_D_n2[[j]])$RMSE
  rmse_D_n3=pred.error(initial$D, para_D_n3[[j]])$RMSE
  
  sum_xi_n1=sum_xi_n1+rmse_xi_n1
  sum_xi_n2=sum_xi_n2+rmse_xi_n2
  sum_xi_n3=sum_xi_n3+rmse_xi_n3
  sum_la_n1=sum_la_n1+rmse_la_n1
  sum_la_n2=sum_la_n2+rmse_la_n2
  sum_la_n3=sum_la_n3+rmse_la_n3
  sum_Om_n1=sum_Om_n1+rmse_Om_n1
  sum_Om_n2=sum_Om_n2+rmse_Om_n2
  sum_Om_n3=sum_Om_n3+rmse_Om_n3
  sum_w_n1=sum_w_n1+rmse_w_n1
  sum_w_n2=sum_w_n2+rmse_w_n2
  sum_w_n3=sum_w_n3+rmse_w_n3
  sum_nu_n1=sum_nu_n1+rmse_nu_n1
  sum_nu_n2=sum_nu_n2+rmse_nu_n2
  sum_nu_n3=sum_nu_n3+rmse_nu_n3
  sum_A_n1=sum_A_n1+rmse_A_n1
  sum_A_n2=sum_A_n2+rmse_A_n2
  sum_A_n3=sum_A_n3+rmse_A_n3
  sum_D_n1=sum_D_n1+rmse_D_n1
  sum_D_n2=sum_D_n2+rmse_D_n2
  sum_D_n3=sum_D_n3+rmse_D_n3
}
xi_rmse_n1=sum_xi_n1/m
xi_rmse_n2=sum_xi_n2/m
xi_rmse_n3=sum_xi_n3/m
la_rmse_n1=sum_la_n1/m
la_rmse_n2=sum_la_n2/m
la_rmse_n3=sum_la_n3/m
Om_rmse_n1=sum_Om_n1/m
Om_rmse_n2=sum_Om_n2/m
Om_rmse_n3=sum_Om_n3/m
w_rmse_n1=sum_w_n1/m
w_rmse_n2=sum_w_n2/m
w_rmse_n3=sum_w_n3/m
nu_rmse_n1=sum_nu_n1/m
nu_rmse_n2=sum_nu_n2/m
nu_rmse_n3=sum_nu_n3/m
A_rmse_n1=sum_A_n1/m
A_rmse_n2=sum_A_n2/m
A_rmse_n3=sum_A_n3/m
D_rmse_n1=sum_D_n1/m
D_rmse_n2=sum_D_n2/m
D_rmse_n3=sum_D_n3/m



