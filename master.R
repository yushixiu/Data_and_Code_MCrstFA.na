rm(list = ls())
WD.PATH = paste(getwd(),'/Data-and-Code-JMVA/',sep="")

# Re-produce Figure 1
source(paste(WD.PATH, 'code/fig1.R', sep=''))

# Re-produce Experiment 1 results: Figure S.1 & Table 1, Table 2
m=100
source(paste(WD.PATH, 'code/simulation1.R', sep=''))
source(paste(WD.PATH, 'code/figS.1.R', sep=''))
source(paste(WD.PATH, 'code/table1.R', sep=''))
source(paste(WD.PATH, 'code/table2.R', sep=''))

# Re-produce Experiment 2 results: Table S.1, Table S.2, Table S.3, Table S.4
m=100
source(paste(WD.PATH, 'code/tableS.1.R', sep=''))
source(paste(WD.PATH, 'code/tableS.2.R', sep=''))
source(paste(WD.PATH, 'code/tableS.3.R', sep=''))
source(paste(WD.PATH, 'code/tableS.4.R', sep=''))

# Re-produce real data (Automobile dataset) results: Table 3, Figure 2, Figure 3, Figure S.2, Figure S.3
source(paste(WD.PATH, 'code/table3.R', sep=''))
source(paste(WD.PATH, 'code/fig2.R', sep=''))
source(paste(WD.PATH, 'code/fig3.R', sep=''))
source(paste(WD.PATH, 'code/figS.2.R', sep=''))
source(paste(WD.PATH, 'code/figS.3.R', sep=''))

# Re-produce real data (CKD dataset) results: Table 4, Table 5, Table S.5, Table S.7, Figure 4, Figure S.4
source(paste(WD.PATH, 'code/table4.R', sep=''))
source(paste(WD.PATH, 'code/table5.R', sep=''))
source(paste(WD.PATH, 'code/tableS.5.R', sep=''))
source(paste(WD.PATH, 'code/tableS.7.R', sep=''))
source(paste(WD.PATH, 'code/fig4.R', sep=''))
source(paste(WD.PATH, 'code/figS.4.R', sep=''))

# Re-produce 'automobile.Rdata' and 'CKD.Rdata'
source(paste(WD.PATH, 'code/autoRdata.R', sep=''))
source(paste(WD.PATH, 'code/ckdRdata.R', sep=''))

