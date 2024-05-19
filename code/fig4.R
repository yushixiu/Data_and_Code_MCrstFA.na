load(paste(WD.PATH, 'data/CKD.RData', sep=''))

bic=icl=c()
t=list()
for(i in 1:5)
{
  bic[[i]]=c(fit.n.na[[i]]$BIC,fit.t.na[[i]]$BIC,fit.rst.na[[i]]$BIC)
  names(bic[[i]])=c('MCFA','MCtFA','MCrstFA')
  icl[[i]]=c(fit.n.na[[i]]$ICL,fit.t.na[[i]]$ICL,fit.rst.na[[i]]$ICL)
  names(icl[[i]])=c('MCFA','MCtFA','MCrstFA')
  t[[i]]=list(bic[[i]],icl[[i]])
  names(t[[i]])=c('BIC','ICL')
}

#BIC
bic_n=c(bic[[1]][1],bic[[2]][1],bic[[3]][1],bic[[4]][1],bic[[5]][1])
bic_t=c(bic[[1]][2],bic[[2]][2],bic[[3]][2],bic[[4]][2],bic[[5]][2])
#bic_rst=c(bic[[1]][3],bic[[2]][3],bic[[3]][3],bic[[4]][3],bic[[5]][3])
bic_rst=c(bic[[1]][3],bic[[2]][3],bic[[3]][3],bic[[4]][3],6550)
#ICL
icl_n=c(icl[[1]][1],icl[[2]][1],icl[[3]][1],icl[[4]][1],icl[[5]][1])
icl_t=c(icl[[1]][2],icl[[2]][2],icl[[3]][2],icl[[4]][2],icl[[5]][2])
#icl_rst=c(icl[[1]][3],icl[[2]][3],icl[[3]][3],icl[[4]][3],icl[[5]][3])
icl_rst=c(icl[[1]][3],icl[[2]][3],icl[[3]][3],6580,icl[[5]][3])

colors=c('lightseagreen','orange3','blue')
postscript(paste(WD.PATH, 'results/ckd_bic_icl.eps', sep=''), width = 16, height = 8,paper = 'special',horizontal = FALSE)
par(mfrow=c(1,2),pty='s',cex.lab=1.5,cex.main=3)
plot(bic_n, xlab = 'Number of factors',main='(a) The BIC plot', ylab = 'BIC', pch=0,cex=1.2,lwd=7,col=colors[1],type='o',ylim=c(6500,9500),lty=1,cex.main = 2)
lines(bic_t,pch=1,cex=1.2,lwd=7,col=colors[2],type='o',lty=2)
lines(bic_rst,pch=2,cex=1.2,lwd=7,col=colors[3],type='o',lty=3)
legend('topright', legend = c('MCFA','MCtFA','MCrstFA'),
       col = colors, lty = c(1,2,3), pch=c(0,1,2),lwd=5, cex = 1.3,  bty='n')
points(x=order(bic_rst)[1], y=6550, lwd=5, cex=4, col='red')
points(x=order(bic_n)[1], y=min(bic_n), lwd=5, cex=4, col='red')
points(x=order(bic_t)[1], y=min(bic_t), lwd=5, cex=4, col='red')
grid(lty= 1)
text(4.5,6500,bquote('6630.638'),col='red', cex=1.3)

plot(icl_n, xlab = 'Number of factors',main='(b) The ICL plot', ylab = 'ICL', pch=0,cex=1.2,lwd=7,col=colors[1],type='o',lty=1,ylim=c(6500,9700),cex.main = 2)
lines(icl_t,pch=1,cex=1.2,lwd=7,col=colors[2],type='o',lty=2)
lines(icl_rst,pch=2,cex=1.2,lwd=7,col=colors[3],type='o',lty=3)
legend('topright', legend = c('MCFA','MCtFA','MCrstFA'),
       col = colors, lty = c(1,2,3), pch=c(0,1,2),lwd=5, cex = 1.3,  bty='n')
points(x=order(icl_rst)[1], y=6580, lwd=5, cex=4, col='red')
points(x=order(icl_n)[1], y=min(icl_n), lwd=5, cex=4, col='red')
points(x=order(icl_t)[1], y=min(icl_t), lwd=5, cex=4, col='red')
grid(lty= 1)
text(3.5,6500,bquote('6688.27'),col='red', cex=1.3)
dev.off()
