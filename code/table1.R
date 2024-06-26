#Table1
bic_1=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/bic1.txt', sep=''), sep='')[,-1])
icl_1=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/icl1.txt', sep=''), sep='')[,-1])
ari_1=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ari1.txt', sep=''), sep='')[,-1])
ccr_1=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ccr1.txt', sep=''), sep='')[,-1])
mspe_1=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mspe1.txt', sep=''), sep='')[,-1])
bic_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/bic2.txt', sep=''), sep='')[,-1])
icl_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/icl2.txt', sep=''), sep='')[,-1])
ari_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ari2.txt', sep=''), sep='')[,-1])
ccr_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ccr2.txt', sep=''), sep='')[,-1])
mspe_2=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mspe2.txt', sep=''), sep='')[,-1])
bic_3=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/bic3.txt', sep=''), sep='')[,-1])
icl_3=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/icl3.txt', sep=''), sep='')[,-1])
ari_3=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ari3.txt', sep=''), sep='')[,-1])
ccr_3=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ccr3.txt', sep=''), sep='')[,-1])
mspe_3=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mspe3.txt', sep=''), sep='')[,-1])
bic_4=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/bic4.txt', sep=''), sep='')[,-1])
icl_4=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/icl4.txt', sep=''), sep='')[,-1])
ari_4=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ari4.txt', sep=''), sep='')[,-1])
ccr_4=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/ccr4.txt', sep=''), sep='')[,-1])
mspe_4=as.matrix(read.table(paste(WD.PATH, 'results/Sim1_TabFig/mspe4.txt', sep=''), sep='')[,-1])

source(paste(WD.PATH2, 'Sim1tab1.R', sep=''))
sim_tab_10=sim1.tab(m,aic=aic_1,bic=bic_1,icl=icl_1,ari=ari_1,ccr=ccr_1,mspe=mspe_1)
sim_tab_20=sim1.tab(m,aic=aic_2,bic=bic_2,icl=icl_2,ari=ari_2,ccr=ccr_2,mspe=mspe_2)
sim_tab_30=sim1.tab(m,aic=aic_3,bic=bic_3,icl=icl_3,ari=ari_3,ccr=ccr_3,mspe=mspe_3)
sim_tab_40=sim1.tab(m,aic=aic_4,bic=bic_4,icl=icl_4,ari=ari_4,ccr=ccr_4,mspe=mspe_4)

tab_mis10=rbind(sim_tab_10$TABLE[1,],sim_tab_10$TABLE.sd[1,],sim_tab_10$TABLE.freq[1,],sim_tab_10$TABLE[2,],sim_tab_10$TABLE.sd[2,],sim_tab_10$TABLE.freq[2,],sim_tab_10$TABLE[3,],sim_tab_10$TABLE.sd[3,],sim_tab_10$TABLE.freq[3,],sim_tab_10$TABLE[4,],sim_tab_10$TABLE.sd[4,],sim_tab_10$TABLE.freq[4,],sim_tab_10$TABLE[5,],sim_tab_10$TABLE.sd[5,],sim_tab_10$TABLE.freq[5,])
tab_mis20=rbind(sim_tab_20$TABLE[1,],sim_tab_20$TABLE.sd[1,],sim_tab_20$TABLE.freq[1,],sim_tab_20$TABLE[2,],sim_tab_20$TABLE.sd[2,],sim_tab_20$TABLE.freq[2,],sim_tab_20$TABLE[3,],sim_tab_20$TABLE.sd[3,],sim_tab_20$TABLE.freq[3,],sim_tab_20$TABLE[4,],sim_tab_20$TABLE.sd[4,],sim_tab_20$TABLE.freq[4,],sim_tab_20$TABLE[5,],sim_tab_20$TABLE.sd[5,],sim_tab_20$TABLE.freq[5,])
tab_mis30=rbind(sim_tab_30$TABLE[1,],sim_tab_30$TABLE.sd[1,],sim_tab_30$TABLE.freq[1,],sim_tab_30$TABLE[2,],sim_tab_30$TABLE.sd[2,],sim_tab_30$TABLE.freq[2,],sim_tab_30$TABLE[3,],sim_tab_30$TABLE.sd[3,],sim_tab_30$TABLE.freq[3,],sim_tab_30$TABLE[4,],sim_tab_30$TABLE.sd[4,],sim_tab_30$TABLE.freq[4,],sim_tab_30$TABLE[5,],sim_tab_30$TABLE.sd[5,],sim_tab_30$TABLE.freq[5,])
tab_mis40=rbind(sim_tab_40$TABLE[1,],sim_tab_40$TABLE.sd[1,],sim_tab_40$TABLE.freq[1,],sim_tab_40$TABLE[2,],sim_tab_40$TABLE.sd[2,],sim_tab_40$TABLE.freq[2,],sim_tab_40$TABLE[3,],sim_tab_40$TABLE.sd[3,],sim_tab_40$TABLE.freq[3,],sim_tab_40$TABLE[4,],sim_tab_40$TABLE.sd[4,],sim_tab_40$TABLE.freq[4,],sim_tab_40$TABLE[5,],sim_tab_40$TABLE.sd[5,],sim_tab_40$TABLE.freq[5,])
row.names(tab_mis10)=row.names(tab_mis20)=row.names(tab_mis30)=row.names(tab_mis40)=c('BIC','BIC(sd)','BIC(freq)','ICL','ICL(sd)','ICL(freq)','ARI','ARI(sd)','ARI(freq)','CCR','CCR(sd)','CCR(freq)','MSPE','MSPE(sd)','MSPE(freq)')
table1=cbind(tab_mis10,tab_mis20,tab_mis30,tab_mis40)
table1

write.csv(table1, paste(WD.PATH, 'results/Table1.csv', sep=''), row.names = TRUE)

