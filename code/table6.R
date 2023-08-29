load(paste(WD.PATH, 'data/CKD.RData', sep=''))
#post.clus
p1=fit.n.na[[4]]$post.clus
p2=fit.t.na[[4]]$post.clus
p3=fit.rst.na[[4]]$post.clus
ARI=c(fit.n.na[[4]]$ARI,fit.t.na[[4]]$ARI,fit.rst.na[[4]]$ARI)
CCR=c(fit.n.na[[4]]$CCR,fit.t.na[[4]]$CCR,fit.rst.na[[4]]$CCR)
tab6_1=as.matrix(cbind(table(true.clus,p1),table(true.clus,p2),table(true.clus,p3)))
row.names(tab6_1)=c('Non-CKD','CKD')
colnames(tab6_1)=c('MCFA_1','MCFA_2','MCtFA_1','MCtFA_2','MCrstFA_1','MCrstFA_2')
tab6_2=rbind(CCR,ARI)
table6=list(tab6_1,round(tab6_2,3))
file_path=paste(WD.PATH, 'results/Table6.csv', sep='')
capture.output(table6, file=file_path)
