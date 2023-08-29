load(paste(WD.PATH, 'data/CKD.RData', sep=''))
#count on na by row
na.Freq=rep(NA,400)
for(i in 1:400){
  na.Freq[i]=length(which(is.na(da[i,-12])==TRUE))
}
summary(as.factor(na.Freq))
na.Freq1=na.Freq[which(da[,ncol(da)]==1)] #length(na.Freq1)=150
na.Freq2=na.Freq[which(da[,ncol(da)]==2)] #length(na.Freq2)=250
na.Freq_table=data.frame('number of missing items'=c(0,1,2,3,4,5,6,7,8,9,10,''),
                         'No.CKD'=c(length(which(na.Freq1==0)),
                                    length(which(na.Freq1==1)),
                                    length(which(na.Freq1==2)),
                                    length(which(na.Freq1==3)),
                                    length(which(na.Freq1==4)),
                                    length(which(na.Freq1==5)),
                                    length(which(na.Freq1==6)),
                                    length(which(na.Freq1==7)),
                                    length(which(na.Freq1==8)),
                                    length(which(na.Freq1==9)),
                                    length(which(na.Freq1==10)),
                                    150),
                         'persents1'=c('85.33%','3.33%','6.00%','2.00%','2.00%','1.33%','0.00%','0.00%','0.00%','0.00%','0.00%','37.50%'),
                         'CKD'=c(length(which(na.Freq2==0)),
                                 length(which(na.Freq2==1)),
                                 length(which(na.Freq2==2)),
                                 length(which(na.Freq2==3)),
                                 length(which(na.Freq2==4)),
                                 length(which(na.Freq2==5)),
                                 length(which(na.Freq2==6)),
                                 length(which(na.Freq2==7)),
                                 length(which(na.Freq2==8)),
                                 length(which(na.Freq2==9)),
                                 length(which(na.Freq2==10)),
                                 250),
                         'persents2'=c('34.80%','6.40%','18.00%','9.60%','14.80%','5.20%','6.40%','2.40%','0.80%','1.20%','0.40%','62.50%'),
                         'Total'=c(215,21,54,27,40,15,16,6,2,3,1,400),
                         'persents3'=c('53.75%','8.40%','21.60%','10.80%','16.00%','6.00%','6.40%','2.40%','0.80%','1.20%','0.40%','100%')
)
na.Freq_table

G1 = da[which(da[,ncol(da)]==1),]
G2 = da[which(da[,ncol(da)]==2),]
length(which(na.Freq1!='0'))
length(which(is.na(as.vector(t(G1[,-12])))=='TRUE'))
length(which(na.Freq2!='0'))
length(which(is.na(as.vector(t(G2[,-12])))=='TRUE'))
length(which(na.Freq!='0'))
length(which(is.na(as.vector(t(da[,-12])))=='TRUE'))
na_v1=na_v2=c()
for(i in 1:11)
{
  na.posi1=is.na(G1[,i])
  na_v1[i]=sum(na.posi1)
  na.posi2=is.na(G2[,i])
  na_v2[i]=sum(na.posi2)
}
na_v1
na_v2
#54/(150*11)
Total_table=data.frame(t(na.Freq_table),
                       'NA-patient'=c('',22,'5.50%',163,'40.75%',185,'46.25%'),
                       'NA-var'=c('',11,'100%',11,'100%',11,'100%'),
                       'NA-obs'=c('',54,'3.27%',582,'21.16%',636,'14.45%')
)

colnames(Total_table)[12]='Patients'
write.csv(Total_table, paste(WD.PATH, 'results/Table4.csv', sep=''), row.names = TRUE)

