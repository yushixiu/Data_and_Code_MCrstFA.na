library(gclus)
library(mclust)
library(EMMIXmfa)
library(cpca)
require(MASS)
load(paste(WD.PATH, 'data/automobile.RData', sep=''))

#bar plot
library(ggplot2)
library(ggforce)
da_c=data[,c(1,3:9,15:16,18)]
da_c$symboling=as.character(da_c$symboling)
da_c=cbind(da_c,p.clus)
da_c$p.clus=as.factor(da_c$p.clus)
da_c[da_c=='?']=NA
names(da_c)=c('symboling','make','fuel','aspiration','doors','body','wheels','location','type','cylinders','system','pclus')

b1=ggplot(da_c,aes(x=factor(symboling,levels=c('-2','-1','0','1','2','3')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b2=ggplot(da_c,aes(x=factor(fuel,labels=c('Diesel','Gas')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b3=ggplot(da_c,aes(x=factor(aspiration,labels=c('Standard','Tubro')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b4=ggplot(data=subset(da_c, !is.na(doors)),aes(x=factor(doors,levels = c('two','four'),labels=c('Two','Four')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='Number of Doors')

b5=ggplot(da_c,aes(x=factor(body,levels=c('hardtop','wagon','sedan','hatchback','convertible'),labels=c('Hardtop','Wagon','Sedan','Hatchback','Convertible')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b6=ggplot(da_c,aes(x=factor(wheels,labels=c('Four Wheel','Front Wheel','Rear Wheel')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='Drive Wheels')

b7=ggplot(da_c,aes(x=factor(location,labels=c('Front','Rear')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b8=ggplot(da_c,aes(x=factor(type,labels=c('DOHC','DOHCV','L','OHC','OHCF','OHCV','ROTOR')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='Engine Type')

b9=ggplot(da_c,aes(x=factor(cylinders,levels=c('two','three','four','five','six','eight','twelve'),labels=c('Two','Three','Four','Five','Six','Eight','Twelve')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b10=ggplot(da_c,aes(x=factor(system,labels=c('1BBL','2BBL','4BBL','IDI','MFI','MPFI','SPDI','SPFI')),fill=factor(pclus,labels=c('1','2','3','4','5'))))+
  geom_bar(position = 'stack')+
  scale_fill_manual(breaks = c('2','1','3','4','5'),values = c('dodgerblue2','darkolivegreen3','mediumpurple2','gold1','lightcoral'),labels=c('1','2','3','4','5'))+
  labs(y='',fill='Cluster',x='')

b1=b1+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('Symboling')
b2=b2+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('(a) Fuel type')
b3=b3+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('(b) Aspiration')
b5=b5+theme_bw() + theme(panel.grid=element_blank(),axis.text = element_text(size = 7),plot.title = element_text(hjust = 0.5))+ggtitle('(c) Body Style')
b7=b7+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('(d) Engine Location')
b9=b9+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('(e) Number of Cylinders')
b10=b10+theme_bw() + theme(panel.grid=element_blank(),plot.title = element_text(hjust = 0.5))+ggtitle('(f) Fuel System')

library(gridExtra)
postscript(paste(WD.PATH, 'results/barplot.eps', sep=''), width = 8, height = 12,paper = 'special',horizontal = FALSE)
grid.arrange(b2, b3, b5, b7, b9, b10, nrow=3, ncol=2)
dev.off()












