#library(moments)
load(paste(WD.PATH, 'data/CKD.RData', sep=''))
nFA = fit.n.na[[4]]$uhat.new
tFA = fit.t.na[[4]]$uhat
rSTFA = fit.rst.na[[4]]$uhat.new

# MCrstFA vs MCFA
postscript(paste(WD.PATH, 'results/fig6a.eps', sep=''), width = 7, height = 7,paper = 'special',horizontal = FALSE)
par(oma = c(4,4,0,1))
zones=matrix(c(1,0,0,5,0, 2,0,0,6,0, 3,4,0,7,8, 9,0,0,13,0, 10,0,0,14,0, 11,12,0,15,16), ncol=5, byrow=TRUE)
nf=layout(zones, widths=c(5/16,2/16,2/16,5/16,2/16), heights=c(2/18,2/18,5/18,2/18,2/18,5/18))
#layout.show(nf)

# Factor 1
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 1', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,1], breaks=25, plot=FALSE)
y1hist = hist(nFA[ ,1], breaks=25, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,1], nFA[ ,1], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='darkorange')


# Factor 2
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 2', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,2], breaks=25, plot=FALSE)
y1hist = hist(nFA[ ,2], breaks=25, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,2], nFA[ ,2], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='darkorange')

# Factor 3
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 3', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,3], breaks=25, plot=FALSE)
y1hist = hist(nFA[ ,3], breaks=25, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,3], nFA[ ,3], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='darkorange')

# Factor 4
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 4', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,4], breaks=25, plot=FALSE)
y1hist = hist(nFA[ ,4], breaks=25, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,4], nFA[ ,4], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='darkorange')
title(main = list("(a) MCrstFA vs. MCFA", cex = 2), outer=T, line = -2)
dev.off()



# MCrstFA vs MCtFA
postscript(paste(WD.PATH, 'results/fig6b.eps', sep=''), width = 7, height = 7,paper = 'special',horizontal = FALSE)
par(oma = c(4,4,0,1))
zones=matrix(c(1,0,0,5,0, 2,0,0,6,0, 3,4,0,7,8, 9,0,0,13,0, 10,0,0,14,0, 11,12,0,15,16), ncol=5, byrow=TRUE)
nf=layout(zones, widths=c(5/16,2/16,2/16,5/16,2/16), heights=c(2/18,2/18,5/18,2/18,2/18,5/18))
#layout.show(nf)

# Factor 1
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 1', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,1], breaks=22, plot=FALSE)
y1hist = hist(tFA[ ,1], breaks=22, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,1], tFA[ ,1], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCtFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='deeppink')


# Factor 2
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 2', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,2], breaks=22, plot=FALSE)
y1hist = hist(tFA[ ,2], breaks=22, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,2], tFA[ ,2], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCtFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='deeppink')

# Factor 3
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 3', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,3], breaks=22, plot=FALSE)
y1hist = hist(tFA[ ,3], breaks=22, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,3], tFA[ ,3], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCtFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='deeppink')

# Factor 4
par(mar=c(0,0,0,0))
plot(c(0, 1),c(0, 1), type='n', xaxt="n", yaxt="n",axes=F)
text(0.5, 0.2, 'Factor 4', cex=1.5, font=1)

x1hist = hist(rSTFA[ ,4], breaks=22, plot=FALSE)
y1hist = hist(tFA[ ,4], breaks=22, plot=FALSE)
top1 = max(c(x1hist$counts, y1hist$counts))

# top histogram
par(mar=c(0,0,0,0))
barplot(x1hist$counts, axes=FALSE, space=0, col='aquamarine')

# scatter plot
par(mar=c(0,0,0,0))
plot(rSTFA[ ,4], tFA[ ,4], pch=16, col='cyan', cex=0.8)
lines(-15:15, -15:15, lty=2, col=1)
mtext('MCrstFA', side=1, line=2, cex=0.8)
mtext('MCtFA', side=2, line=2, cex=0.8)
# side histogram
par(mar=c(0,0,0,1))
barplot(y1hist$counts, axes=FALSE, space=0, horiz=TRUE, col='deeppink')
title(main = list("(b) MCrstFA vs. MCtFA", cex = 2), outer=T, line = -2)
dev.off()



