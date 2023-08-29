library(plot3D)
#### yhat obs #### 
yhat.obs_n=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mcfa.yhat.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
yhat.obs_t=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mctfa.yhat.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
yhat.obs_rst=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mcrstfa.yhat.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
pclus_n=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mcfa.clus.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
pclus_t=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mctfa.clus.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
pclus_rst=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mcrstfa.clus.obs.txt', sep=''),na.strings='NA',sep=''))[1,-1]
yna=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/yna.txt', sep=''),na.strings='NA',sep=''))[1,-1]
Y.na=matrix(yna,1000,p)
Y.comp=Y.na
na.posi=is.na(Y.comp)
ty=t(Y.comp)
tna=is.na(ty)
na.posi1=is.na(Y.comp[,1])
na.posi2=is.na(Y.comp[,2])
na.posi3=is.na(Y.comp[,3])
Y.obs=as.matrix(Y.comp[(na.posi1==F & na.posi2==F & na.posi3==F),])
y1=y2=y3=ty
y1[tna==F]=yhat.obs_n
y2[tna==F]=yhat.obs_t
y3[tna==F]=yhat.obs_rst
y1=t(y1)
y2=t(y2)
y3=t(y3)
miss=c()
for(i in 1:1000)
{
  if(na.posi1[i]==F & na.posi2[i]==F & na.posi3[i]==F) {miss[i]=1}
  else {miss[i]=0}
}
Y.comp=cbind(Y.comp,miss)
n=500
true.clus = c(rep(1,n),rep(2,n))
true.clus=true.clus[Y.comp[,4]==1]

#### input fitted data ####
mcfa.clus.obs = pclus_n[Y.comp[,4]==1]
mctfa.clus.obs = pclus_t[Y.comp[,4]==1]
mcstfa.clus.obs = pclus_rst[Y.comp[,4]==1]

mcfa.yhat.obs = as.matrix(y1[(na.posi1==F & na.posi2==F & na.posi3==F),])
mctfa.yhat.obs = as.matrix(y2[(na.posi1==F & na.posi2==F & na.posi3==F),])
mcstfa.yhat.obs = as.matrix(y3[(na.posi1==F & na.posi2==F & na.posi3==F),])

#### plot yhat obs #### 
n=length(which(true.clus==1)==T)
xlim = c(-30,20)
ylim = c(-30,20) 
zlim = c(-40,40)
#-------- norm --------
color.n = c()
color.n[which(true.clus==1)] = 'lightskyblue'
color.n[which(true.clus==2)] = 'lightpink'
color.n[which(mcfa.clus.obs==1)+n*g] = 'mediumblue'
color.n[which(mcfa.clus.obs==2)+n*g] = 'maroon1'
sign.n = c()
sign.n[which(true.clus==1)] = 0
sign.n[which(true.clus==2)] = 1
sign.n[which(mcfa.clus.obs==1)+n*g] = 3
sign.n[which(mcfa.clus.obs==2)+n*g] = 8
#--------- t ----------
color.t = c()
color.t[which(true.clus==1)] ='lightskyblue'
color.t[which(true.clus==2)] = 'lightpink'
color.t[which(mctfa.clus.obs==1)+n*g] = 'mediumblue'
color.t[which(mctfa.clus.obs==2)+n*g] = 'maroon1'
sign.t = c()
sign.t[which(true.clus==1)] = 0
sign.t[which(true.clus==2)] = 1
sign.t[which(mctfa.clus.obs==1)+n*g] = 3
sign.t[which(mctfa.clus.obs==2)+n*g] = 8
#--------- st ---------
color.st = c()
color.st[which(true.clus==1)] = 'lightskyblue'
color.st[which(true.clus==2)] = 'lightpink'
color.st[which(mcstfa.clus.obs==1)+n*g] = 'mediumblue'
color.st[which(mcstfa.clus.obs==2)+n*g] = 'maroon1'
sign.st = c()
sign.st[which(true.clus==1)] = 0
sign.st[which(true.clus==2)] = 1
sign.st[which(mcstfa.clus.obs==1)+n*g] = 3
sign.st[which(mcstfa.clus.obs==2)+n*g] = 8

ari_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ari2.txt', sep=''), sep='')[1,-1])
ccr_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ccr2.txt', sep=''), sep='')[1,-1])

postscript(paste(WD.PATH, 'results/sim1_scatter.eps', sep=''), width=800, height=800)
par(pty='s', cex=0.5, lwd=0.3, mai=c(0.1,0.1,0.1,0.1))
par(fig=c(0.25,0.75,0.5,0.9))
#mcfa
scatter3D(x = Y.obs[1:n,1], y = Y.obs[1:n,2], z = Y.obs[1:n,3],
          col = color.n[1:n], pch=sign.n[1:n],  
          xlim = xlim, ylim = ylim, zlim = zlim, 
          xlab = '', ylab = '', zlab = '', 
          colkey = FALSE, theta=380, phi=25, bty = 'b2', ticktype = 'detailed')
scatter3D(x = Y.obs[(n+1):length(true.clus),1], y = Y.obs[(n+1):length(true.clus),2], z = Y.obs[(n+1):length(true.clus),3], 
          colkey = FALSE, col = color.n[(n+1):length(true.clus)], pch=sign.n[(n+1):length(true.clus)], add = TRUE)
scatter3D(x = mcfa.yhat.obs[which(mcfa.clus.obs==1),1], y = mcfa.yhat.obs[which(mcfa.clus.obs==1),2], z = mcfa.yhat.obs[which(mcfa.clus.obs==1),3], 
          colkey = FALSE, col = color.n[which(color.n=='mediumblue')], pch=sign.n[which(sign.n==3)], add = TRUE)
scatter3D(x = mcfa.yhat.obs[which(mcfa.clus.obs==2),1], y = mcfa.yhat.obs[which(mcfa.clus.obs==2),2], z = mcfa.yhat.obs[which(mcfa.clus.obs==2),3], 
          colkey = FALSE, col = color.n[which(color.n=='maroon1')], pch=sign.n[which(sign.n==8)], add = TRUE)
text(0.15,-0.48,expression(italic(x)[1]), cex=1.5)
text(-0.3,-0.3,expression(italic(x)[2]), cex=1.5)
text(-0.36,0.05,expression(italic(x)[3]), cex=1.5)
legend('bottom',legend=c(paste('ARI=', round(ari_2[1],2)),paste('CCR=', round(ccr_2[1],2))),cex=1.5,inset=c(0.1,0.15),bty='n')
mtext('MCFA', font=2)
#mctfa
par(fig=c(0.15,0.55,0.1,0.5), new=TRUE)
scatter3D(x = Y.obs[1:n,1], y = Y.obs[1:n,2], z = Y.obs[1:n,3],
          col = color.t[1:n], pch=sign.t[1:n],  
          xlim = xlim, ylim = ylim, zlim = zlim, 
          xlab = '', ylab = '', zlab = '', 
          colkey = FALSE, theta=380, phi=25, bty = 'b2', ticktype = 'detailed')
scatter3D(x = Y.obs[(n+1):length(true.clus),1], y = Y.obs[(n+1):length(true.clus),2], z = Y.obs[(n+1):length(true.clus),3], 
          colkey = FALSE, col = color.t[(n+1):length(true.clus)], pch=sign.t[(n+1):length(true.clus)], add = TRUE)
scatter3D(x = mctfa.yhat.obs[which(mctfa.clus.obs==1),1], y = mctfa.yhat.obs[which(mctfa.clus.obs==1),2], z = mctfa.yhat.obs[which(mctfa.clus.obs==1),3], 
          colkey = FALSE, col = color.t[which(color.t=='mediumblue')], pch=sign.t[which(sign.t==3)], add = TRUE)
scatter3D(x = mctfa.yhat.obs[which(mctfa.clus.obs==2),1], y = mctfa.yhat.obs[which(mctfa.clus.obs==2),2], z = mctfa.yhat.obs[which(mctfa.clus.obs==2),3], 
          colkey = FALSE, col = color.t[which(color.t=='maroon1')], pch=sign.t[which(sign.t==8)], add = TRUE)
text(0.15,-0.48,expression(italic(x)[1]), cex=1.5)
text(-0.3,-0.3,expression(italic(x)[2]), cex=1.5)
text(-0.36,0.05,expression(italic(x)[3]), cex=1.5)
legend('bottom',legend=c(paste('ARI=', round(ari_2[2],2)),paste('CCR=', round(ccr_2[2],2))),cex=1.5,inset=c(0.1,0.15),bty='n')
mtext('MCtFA', font=2)
#mcrstfa
par(fig=c(0.45,0.85,0.1,0.5), new=TRUE)
scatter3D(x = Y.obs[1:n,1], y = Y.obs[1:n,2], z = Y.obs[1:n,3],
          col = color.st[1:n], pch=sign.st[1:n],  
          xlim = xlim, ylim = ylim, zlim = zlim, 
          xlab = '', ylab = '', zlab = '', 
          colkey = FALSE, theta=380, phi=25, bty = 'b2', ticktype = 'detailed')
scatter3D(x = Y.obs[(n+1):length(true.clus),1], y = Y.obs[(n+1):length(true.clus),2], z = Y.obs[(n+1):length(true.clus),3], 
          colkey = FALSE, col = color.st[(n+1):length(true.clus)], pch=sign.st[(n+1):length(true.clus)], add = TRUE)
scatter3D(x = mcstfa.yhat.obs[which(mcstfa.clus.obs==1),1], y = mcstfa.yhat.obs[which(mcstfa.clus.obs==1),2], z = mcstfa.yhat.obs[which(mcstfa.clus.obs==1),3], 
          colkey = FALSE, col = color.st[which(color.st=='mediumblue')], pch=sign.st[which(sign.st==3)], add = TRUE)
scatter3D(x = mcstfa.yhat.obs[which(mcstfa.clus.obs==2),1], y = mcstfa.yhat.obs[which(mcstfa.clus.obs==2),2], z = mcstfa.yhat.obs[which(mcstfa.clus.obs==2),3], 
          colkey = FALSE, col = color.st[which(color.st=='maroon1')], pch=sign.st[which(sign.st==8)], add = TRUE)
text(0.15,-0.48,expression(italic(x)[1]), cex=1.5)
text(-0.3,-0.3,expression(italic(x)[2]), cex=1.5)
text(-0.36,0.05,expression(italic(x)[3]), cex=1.5)
legend('bottom',legend=c(paste('ARI=', round(ari_2[3],2)),paste('CCR=', round(ccr_2[3],2))),cex=1.5,inset=c(0.1,0.15),text.font=2,bty='n')
mtext('MCrstFA', font=2)
par(mar=c(0,0,0,0),pty="m", cex=0.8)
par(fig=c(0,1,0,0.1), new=TRUE)
plot(1, type='n', axes=F, xlab="", ylab="")
legend('center', c('Class 1 (true)','Class 2 (true)','Class 1 (predicted)', 'Class 2 (predicted)'), 
       col = c('lightskyblue','lightpink','mediumblue','maroon1'), pch=c(0,1,3,8), bty='n',horiz = T)
title('Missing rate: 20%',line=-2,cex.main = 1.5,outer=T)
dev.off()

