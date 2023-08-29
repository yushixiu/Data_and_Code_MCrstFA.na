library(rgl)
library(misc3d)
library(moments)
library(mvtnorm)
library(MomTrunc)
library(tmvtnorm)
source(paste(WD.PATH, 'function/rMST.R', sep=''))

#True parameters
set.seed(17)
n=700
p=3
g=1
nu = 3
xi = matrix(c(0,0,0),p,g)
la1 = matrix(c(0,5,5),p,g)
la2 = matrix(c(0,5,-5),p,g)
la3 = matrix(c(0,-5,5),p,g)
la4 = matrix(c(0,-5,-5),p,g)
sig = array(NA, dim=c(p,p,g))
sig[,,1] = rbind(c(6, 0, 0), c(0, 7, 0), c(0, 0, 6))
Y1=rMSt(n, xi[,1], sig[,,1], la1[,1], nu)
Y2=rMSt(n, xi[,1], sig[,,1], la2[,1], nu)
Y3=rMSt(n, xi[,1], sig[,,1], la3[,1], nu)
Y4=rMSt(n, xi[,1], sig[,,1], la4[,1], nu)

sym = c(9,2,1,5)
colors = c('#00FFFF','#00FF00','#FFFF00','#FF00FF') 
x=seq(-500, 500, length = 200)
y=seq(-500, 500, length = 200)
z=seq(-500, 500, length = 200)

#open3d()
mfrow3d(nr = 2, nc = 2, sharedMouse = TRUE)
par3d(windowRect = 50+c( 0, 0, 700, 700),zoom=6)

next3d()
con=plot3d(Y1,box=F,size=0,type='n',xlim=c(min(Y1[,1]),max(Y1[,1])), ylim=c(min(Y1[,2]),max(Y1[,2])), zlim=c(min(Y1[,3]),max(Y1[,3])),xlab='', ylab='', zlab='')
le = rep(5e-9,5)
rMST.contour = function(x, y, z){
  dMST(Y=cbind(x, y, z),mu=xi[,1],S=sig[,,1],la=la1[,1],nu=nu) }
contour3d(rMST.contour,level=le[1],x,y,z,add=T,fill = T,alpha = 0.3, color=colors[1])
rgl.viewpoint(theta = 75, phi = 20)
bgplot3d({
  plot.new()
  title(main = expression(paste('(a) ', bolditalic(lambda)[1] , '=(0,5,5)')), line = 1, cex.main = 1.7, font.main= 4)
})

next3d()
con=plot3d(Y2,box=F,size=0,type='n',xlim=c(min(Y2[,1]),max(Y2[,1])), ylim=c(min(Y2[,2]),max(Y2[,2])), zlim=c(min(Y2[,3]),max(Y2[,3])),xlab='', ylab='', zlab='')
le = rep(5e-9,5)
rMST.contour = function(x, y, z){
  dMST(Y=cbind(x, y, z),mu=xi[,1],S=sig[,,1],la=la2[,1],nu=nu) }
contour3d(rMST.contour,level=le[1],x,y,z,add=T,fill = T,alpha = 0.3, color=colors[2])
rgl.viewpoint(theta = 75, phi = 20)
bgplot3d({
  plot.new()
  title(main = expression(paste('(b) ', bolditalic(lambda)[2] , '=(0,5,-5)')), line = 1, cex.main = 1.7, font.main= 4)
})

next3d()
con=plot3d(Y3,box=F,size=0,type='n',xlim=c(min(Y3[,1]),max(Y3[,1])), ylim=c(min(Y3[,2]),max(Y3[,2])), zlim=c(min(Y3[,3]),max(Y3[,3])),xlab='', ylab='', zlab='')
le = rep(5e-9,5)
rMST.contour = function(x, y, z){
  dMST(Y=cbind(x, y, z),mu=xi[,1],S=sig[,,1],la=la3[,1],nu=nu) }
contour3d(rMST.contour,level=le[1],x,y,z,add=T,fill = T,alpha = 0.3, color=colors[3])
rgl.viewpoint(theta = 75, phi = 20)
bgplot3d({
  plot.new()
  title(main = expression(paste('(c) ', bolditalic(lambda)[3] , '=(0,-5,5)')), line = 1, cex.main = 1.7, font.main= 4)
})

next3d()
con=plot3d(Y4,box=F,size=0,type='n',xlim=c(min(Y4[,1]),max(Y4[,1])), ylim=c(min(Y4[,2]),max(Y4[,2])), zlim=c(min(Y4[,3]),max(Y4[,3])),xlab='', ylab='', zlab='')
le = rep(5e-9,5)
rMST.contour = function(x, y, z){
  dMST(Y=cbind(x, y, z),mu=xi[,1],S=sig[,,1],la=la4[,1],nu=nu) }
contour3d(rMST.contour,level=le[1],x,y,z,add=T,fill = T,alpha = 0.3, color=colors[4])
rgl.viewpoint(theta = 75, phi = 20)
bgplot3d({
  plot.new()
  title(main = expression(paste('(d) ', bolditalic(lambda)[4] , '=(0,-5,-5)')), line = 1, cex.main = 1.7, font.main= 4)
})
highlevel(integer())
rgl.snapshot(paste(WD.PATH, 'results/rMST_3Dplot.png', sep=''), fmt = 'png')


