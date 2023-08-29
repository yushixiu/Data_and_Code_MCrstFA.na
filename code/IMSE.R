imse_n1=imse_n2=imse_n3=matrix(0,44,1)
for(j in 1:m)
{
  imse_n1=imse_n1+se1[[j]]
  imse_n2=imse_n2+se2[[j]]
  imse_n3=imse_n3+se3[[j]]
}
imse_n1_mean=imse_n1/m
imse_n2_mean=imse_n2/m
imse_n3_mean=imse_n3/m

xi_imse_n1=imse_n1_mean[3:8,1]
xi_imse_n1=matrix(xi_imse_n1,q,g)
xi_imse_n2=imse_n2_mean[3:8,1]
xi_imse_n2=matrix(xi_imse_n2,q,g)
xi_imse_n3=imse_n3_mean[3:8,1]
xi_imse_n3=matrix(xi_imse_n3,q,g)
la_imse_n1=imse_n1_mean[9:14,1]
la_imse_n1=matrix(la_imse_n1,q,g)
la_imse_n2=imse_n2_mean[9:14,1]
la_imse_n2=matrix(la_imse_n2,q,g)
la_imse_n3=imse_n3_mean[9:14,1]
la_imse_n3=matrix(la_imse_n3,q,g)
Om_se1=imse_n1_mean[15:23,1]
Om_g11=matrix(c(Om_se1[1],Om_se1[2],Om_se1[2],Om_se1[3]),q,q)
Om_g21=matrix(c(Om_se1[4],Om_se1[5],Om_se1[5],Om_se1[6]),q,q)
Om_g31=matrix(c(Om_se1[7],Om_se1[8],Om_se1[8],Om_se1[9]),q,q)
Om_imse_n1=array(c(Om_g11,Om_g21,Om_g31), dim=c(q,q,g))
Om_se2=imse_n2_mean[15:23,1]
Om_g12=matrix(c(Om_se2[1],Om_se2[2],Om_se2[2],Om_se2[3]),q,q)
Om_g22=matrix(c(Om_se2[4],Om_se2[5],Om_se2[5],Om_se2[6]),q,q)
Om_g32=matrix(c(Om_se2[7],Om_se2[8],Om_se2[8],Om_se2[9]),q,q)
Om_imse_n2=array(c(Om_g12,Om_g22,Om_g32), dim=c(q,q,g))
Om_se3=imse_n3_mean[15:23,1]
Om_g13=matrix(c(Om_se3[1],Om_se3[2],Om_se3[2],Om_se3[3]),q,q)
Om_g23=matrix(c(Om_se3[4],Om_se3[5],Om_se3[5],Om_se3[6]),q,q)
Om_g33=matrix(c(Om_se3[7],Om_se3[8],Om_se3[8],Om_se3[9]),q,q)
Om_imse_n3=array(c(Om_g13,Om_g23,Om_g33), dim=c(q,q,g))
w_imse_n1=c(imse_n1_mean[1:2,1])
w_imse_n2=c(imse_n2_mean[1:2,1])
w_imse_n3=c(imse_n3_mean[1:2,1])
nu_imse_n1=c(imse_n1_mean[30:32,1])
nu_imse_n2=c(imse_n2_mean[30:32,1])
nu_imse_n3=c(imse_n3_mean[30:32,1])
A_imse_n1=imse_n1_mean[33:44,1]
A_imse_n1=matrix(A_imse_n1,p,q)
A_imse_n2=imse_n2_mean[33:44,1]
A_imse_n2=matrix(A_imse_n2,p,q)
A_imse_n3=imse_n3_mean[33:44,1]
A_imse_n3=matrix(A_imse_n3,p,q)
D_imse_n1=c(imse_n1_mean[24:29,1])
D_imse_n2=c(imse_n2_mean[24:29,1])
D_imse_n3=c(imse_n3_mean[24:29,1])


