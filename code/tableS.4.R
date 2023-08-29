#missing rate r=30%
library(matrixcalc)
library(mvtnorm)
library(gclus)
library(mclust)
library(EMMIXmfa)
library(cpca)
require(MASS)
source(paste(WD.PATH, 'function/MCrstFA.R', sep=''))
source(paste(WD.PATH, 'function/MCrstFA.na.R', sep=''))
source(paste(WD.PATH, 'function/MCrSTFA_na_se.R', sep=''))
source(paste(WD.PATH, 'function/gener_na.R', sep=''))
PATH=paste(WD.PATH, 'results/Sim2_TableS.4(r=30)/', sep='')
miss.rate=0.3
source(paste(WD.PATH, 'code/simulation2.R', sep=''))

#TableS.4 (r=30%)
w_n1=as.matrix(read.table(paste(PATH,'para.w_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
nu_n1=as.matrix(read.table(paste(PATH,'para.nu_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
A_n1=as.matrix(read.table(paste(PATH,'para.A_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
D_n1=as.matrix(read.table(paste(PATH,'para.D_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
la_n1=as.matrix(read.table(paste(PATH,'para.la_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
xi_n1=as.matrix(read.table(paste(PATH,'para.xi_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
Om_n1=as.matrix(read.table(paste(PATH,'para.Om_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
w_n2=as.matrix(read.table(paste(PATH,'para.w_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
nu_n2=as.matrix(read.table(paste(PATH,'para.nu_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
A_n2=as.matrix(read.table(paste(PATH,'para.A_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
D_n2=as.matrix(read.table(paste(PATH,'para.D_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
la_n2=as.matrix(read.table(paste(PATH,'para.la_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
xi_n2=as.matrix(read.table(paste(PATH,'para.xi_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
Om_n2=as.matrix(read.table(paste(PATH,'para.Om_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
w_n3=as.matrix(read.table(paste(PATH,'para.w_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
nu_n3=as.matrix(read.table(paste(PATH,'para.nu_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
A_n3=as.matrix(read.table(paste(PATH,'para.A_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
D_n3=as.matrix(read.table(paste(PATH,'para.D_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
la_n3=as.matrix(read.table(paste(PATH,'para.la_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
xi_n3=as.matrix(read.table(paste(PATH,'para.xi_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
Om_n3=as.matrix(read.table(paste(PATH,'para.Om_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
se_n1=as.matrix(read.table(paste(PATH,'se_n1.txt',sep=''),na.strings='NA',sep=''))[,-1]
se_n2=as.matrix(read.table(paste(PATH,'se_n2.txt',sep=''),na.strings='NA',sep=''))[,-1]
se_n3=as.matrix(read.table(paste(PATH,'se_n3.txt',sep=''),na.strings='NA',sep=''))[,-1]
para_A_n1=para_D_n1=para_la_n1=para_xi_n1=para_Om_n1=list()
para_A_n2=para_D_n2=para_la_n2=para_xi_n2=para_Om_n2=list()
para_A_n3=para_D_n3=para_la_n3=para_xi_n3=para_Om_n3=list()
se1=se2=se3=list()
for(j in 1:m)
{
  para_A_n1[[j]]=matrix(A_n1[j,],p,q)
  para_D_n1[[j]]=matrix(D_n1[j,],p,p)
  para_la_n1[[j]]=matrix(la_n1[j,],q,g)
  para_xi_n1[[j]]=matrix(xi_n1[j,],q,g)
  para_Om_n1[[j]]=array(Om_n1[j,], dim=c(q,q,g))
  para_A_n2[[j]]=matrix(A_n2[j,],p,q)
  para_D_n2[[j]]=matrix(D_n2[j,],p,p)
  para_la_n2[[j]]=matrix(la_n2[j,],q,g)
  para_xi_n2[[j]]=matrix(xi_n2[j,],q,g)
  para_Om_n2[[j]]=array(Om_n2[j,], dim=c(q,q,g))
  para_A_n3[[j]]=matrix(A_n3[j,],p,q)
  para_D_n3[[j]]=matrix(D_n3[j,],p,p)
  para_la_n3[[j]]=matrix(la_n3[j,],q,g)
  para_xi_n3[[j]]=matrix(xi_n3[j,],q,g)
  para_Om_n3[[j]]=array(Om_n3[j,], dim=c(q,q,g))
  se1[[j]]=matrix(se_n1[j,],44,1)
  se2[[j]]=matrix(se_n2[j,],44,1)
  se3[[j]]=matrix(se_n3[j,],44,1)
}
source(paste(WD.PATH, 'code/RMSE.R', sep=''))
source(paste(WD.PATH, 'code/STD.R', sep=''))
source(paste(WD.PATH, 'code/IMSE.R', sep=''))
tab1_n1=cbind(rbind(t(xi_rmse_n1),t(la_rmse_n1),Om_rmse_n1[,,1],Om_rmse_n1[,,2],Om_rmse_n1[,,3],A_rmse_n1),
              rbind(t(xi_sd_n1),t(la_sd_n1),Om_sd_n1[,,1],Om_sd_n1[,,2],Om_sd_n1[,,3],A_sd_n1),
              rbind(t(xi_imse_n1),t(la_imse_n1),Om_imse_n1[,,1],Om_imse_n1[,,2],Om_imse_n1[,,3],A_imse_n1))
tab1_n2=cbind(rbind(t(xi_rmse_n2),t(la_rmse_n2),Om_rmse_n2[,,1],Om_rmse_n2[,,2],Om_rmse_n2[,,3],A_rmse_n2),
              rbind(t(xi_sd_n2),t(la_sd_n2),Om_sd_n2[,,1],Om_sd_n2[,,2],Om_sd_n2[,,3],A_sd_n2),
              rbind(t(xi_imse_n2),t(la_imse_n2),Om_imse_n2[,,1],Om_imse_n2[,,2],Om_imse_n2[,,3],A_imse_n2))
tab1_n3=cbind(rbind(t(xi_rmse_n3),t(la_rmse_n3),Om_rmse_n3[,,1],Om_rmse_n3[,,2],Om_rmse_n3[,,3],A_rmse_n3),
              rbind(t(xi_sd_n3),t(la_sd_n3),Om_sd_n3[,,1],Om_sd_n3[,,2],Om_sd_n3[,,3],A_sd_n3),
              rbind(t(xi_imse_n3),t(la_imse_n3),Om_imse_n3[,,1],Om_imse_n3[,,2],Om_imse_n3[,,3],A_imse_n3))
row.names(tab1_n1)=row.names(tab1_n2)=row.names(tab1_n3)=c('xi_g1','xi_g2','xi_g3','la_g1','la_g2','la_g3',
                                                           'Om_q1_g1','Om_q2_g1','Om_q1_g2','Om_q2_g2','Om_q1_g3','Om_q2_g3',
                                                           'A_p1','A_p2','A_p3','A_p4','A_p5','A_p6')
colnames(tab1_n1)=colnames(tab1_n2)=colnames(tab1_n3)=c('RMSE(q1)','RMSE(q2)','STD(q1)','STD(q2)','IMSE(q1)','IMSE(q2)')
tab1=list(round(tab1_n1,5),round(tab1_n2,5),round(tab1_n3,5))
names(tab1)=c('n=300','n=900','n=1800')

tab2_n1=cbind(matrix(c(w_rmse_n1[1:2],nu_rmse_n1,diag(D_rmse_n1)),11,1),
              matrix(c(w_sd_n1[1:2],nu_sd_n1,diag(D_sd_n1)),11,1),
              matrix(c(w_imse_n1,nu_imse_n1,D_imse_n1),11,1))
tab2_n2=cbind(matrix(c(w_rmse_n2[1:2],nu_rmse_n2,diag(D_rmse_n2)),11,1),
              matrix(c(w_sd_n2[1:2],nu_sd_n2,diag(D_sd_n2)),11,1),
              matrix(c(w_imse_n2,nu_imse_n2,D_imse_n2),11,1))
tab2_n3=cbind(matrix(c(w_rmse_n3[1:2],nu_rmse_n3,diag(D_rmse_n3)),11,1),
              matrix(c(w_sd_n3[1:2],nu_sd_n3,diag(D_sd_n3)),11,1),
              matrix(c(w_imse_n3,nu_imse_n3,D_imse_n3),11,1))
row.names(tab2_n1)=row.names(tab2_n2)=row.names(tab2_n3)=c('pi_g1','pi_g2','nu_g1','nu_g2','nu_g3',
                                                           'D_p1','D_p2','D_p3','D_p4','D_p5','D_p6')
colnames(tab2_n1)=colnames(tab2_n2)=colnames(tab2_n3)=c('RMSE','STD','IMSE')
tab2=list(round(tab2_n1,5),round(tab2_n2,5),round(tab2_n3,5))
names(tab2)=c('n=300','n=900','n=1800')
table.s4=list(tab1,tab2)
file_path=paste(WD.PATH, 'results/TableS.4.csv', sep='')
capture.output(table.s4, file=file_path)


