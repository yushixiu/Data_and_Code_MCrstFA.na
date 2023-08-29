load(paste(WD.PATH, 'data/automobile.RData', sep=''))
suppressWarnings({
  Y=as.data.frame(lapply(data[,c(2,10:14,17,19:26)],as.numeric),na.rm=T)
})
na.posi=is.na(Y)
na.num=apply(na.posi,2,sum)
min=round(apply(Y,2,min,na.rm=T),2)
max=round(apply(Y,2,max,na.rm=T),2)
mean=round(colMeans(Y,na.rm=T),2)
da=as.matrix(cbind(na.num,min,max,mean))
miss.rate=da[,1]/(205*15)
da=cbind(da,round(miss.rate,4))
colnames(da)=c('No. missing values','Min','Max','Sample mean','Missingness')
rownames(da)=c('Normalized losses','Wheelbase','Length','Width','Height','Curb weight','Engine size','Bore','Stroke','Compression ratio','Horsepower','Peak RPM','City MPG','Highway MPG','Price')
table2=da[,-1]
write.csv(table2, paste(WD.PATH, 'results/Table2.csv', sep=''), row.names = TRUE)


