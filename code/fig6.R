library(moments)
library(mvtnorm)
library(MomTrunc)
library(tmvtnorm)
library(mclust)
library(matrixcalc)
library(rgl)
library(misc3d)
library(devtools)
library(EMMIXuskew)
library(plot3D)
library(ggplot2)
library(gridExtra)
load(paste(WD.PATH, 'data/automobile.RData', sep=''))

#3D scatter plot
factor=cbind(fit.rst.na5[[6]]$uhat.new,p.clus)
fa = factor[,-ncol(factor)]
G1 = factor[which(factor[,ncol(factor)]==1),]
G2 = factor[which(factor[,ncol(factor)]==2),]
G3 = factor[which(factor[,ncol(factor)]==3),]
G4 = factor[which(factor[,ncol(factor)]==4),]
G5 = factor[which(factor[,ncol(factor)]==5),]
sym = c(15,16,17,18,8)
colors = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral') 
sym2 = c(16,15,17,18,8)
colors2 = c('darkolivegreen3','dodgerblue2','mediumpurple2','gold1','lightcoral') 

postscript(paste(WD.PATH, 'results/auto_3D_plot.eps', sep=''), width = 8, height = 8,paper = 'special',horizontal = FALSE)
points3D(x=G1[,1],y=G1[,2],z=G1[,3],bty = 'b2',colkey = F,
         xlab='factor 1',ylab='factor 2',zlab='factor 3',
         phi = 40, theta = 20, col=colors[2], pch=sym[1], cex=0.7,
         xlim=c(min(factor[,1]),max(factor[,1])),ylim=c(min(factor[,2]),max(factor[,2])),
         zlim=c(min(factor[,3]),max(factor[,3])))
points3D(x=G2[,1],y=G2[,2],z=G2[,3],pch=sym[2],add=T,col=colors[1],cex=0.7)
points3D(x=G3[,1],y=G3[,2],z=G3[,3],pch=sym[3], add=T,col=colors[3],cex=0.7)
points3D(x=G4[,1],y=G4[,2],z=G4[,3],pch=sym[4], add=T,col=colors[4],cex=0.7)
points3D(x=G5[,1],y=G5[,2],z=G5[,3],pch=sym[5], add=T,col=colors[5],cex=0.7)
legend('bottomleft',c('Cluster1 (n1=64)','Cluster2 (n2=29)','Cluster3 (n3=65)','Cluster4 (n4=28)','Cluster5 (n5=19)'),pch=sym2,col=colors,cex=0.9,yjust=1,bty = 'n')
dev.off()

#scatter plot
fa2=as.data.frame(factor)
a=ggplot(fa2, aes(x=fa2[,1], y=fa2[,2], group=as.factor(p.clus))) + 
  geom_point(aes(color=as.factor(p.clus), shape=as.factor(p.clus)), size=1) +
  scale_colour_manual(values = colors2) +
  scale_shape_manual(values=sym) +
  theme_bw()+
  theme(legend.position='none')+
  labs(y='factor 2',x='factor 1')
a=a+labs(shape = 'Cluster', colour = 'Cluster')+ theme(aspect.ratio=1)
b=ggplot(fa2, aes(x=fa2[,1], y=fa2[,3], group=as.factor(p.clus))) + 
  geom_point(aes(color=as.factor(p.clus), shape=as.factor(p.clus)), size=1) +
  scale_colour_manual(values = colors2) +
  scale_shape_manual(values=sym) +
  theme_bw()+
  theme(legend.position='none')+
  labs(y='factor 3',x='factor 1')
b=b+labs(shape = 'Cluster', colour = 'Cluster')+ theme(aspect.ratio=1)
c=ggplot(fa2, aes(x=fa2[,2], y=fa2[,3], group=as.factor(p.clus))) + 
  geom_point(aes(color=as.factor(p.clus), shape=as.factor(p.clus)), size=1) +
  scale_colour_manual(values = colors2) +
  scale_shape_manual(values=sym) +
  theme_bw()+
  theme(legend.position='none')+
  labs(y='factor 3',x='factor 2')
c=c+labs(shape = 'Cluster', colour = 'Cluster')+ theme(aspect.ratio=1)

postscript(paste(WD.PATH, 'results/auto_2D_plot.eps', sep=''), width = 4, height = 6,paper = 'special',horizontal = FALSE)
grid.arrange(a, b, c, nrow=3, ncol=1)
dev.off()

