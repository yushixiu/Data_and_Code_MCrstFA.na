rm(list = ls())
WD.PATH = paste(getwd(),'/Data-and-Code-CAM/',sep="")

# Re-produce Figure 1
source(paste(WD.PATH, 'code/fig1.R', sep=''))

# Re-produce Experiment 1 results: Figure 2 & Table 1
m=100
source(paste(WD.PATH, 'code/simulation1.R', sep=''))
source(paste(WD.PATH, 'code/fig2.R', sep=''))
source(paste(WD.PATH, 'code/table1.R', sep=''))

# Re-produce Experiment 2 results: Table S.1, Table S.2, Table S.3, Table S.4
m=100
source(paste(WD.PATH, 'code/tableS.1.R', sep=''))
source(paste(WD.PATH, 'code/tableS.2.R', sep=''))
source(paste(WD.PATH, 'code/tableS.3.R', sep=''))
source(paste(WD.PATH, 'code/tableS.4.R', sep=''))

# Re-produce real data (Automobile dataset) results: Table 2, Figure 3, Figure 4, Figure 5, Figure 6
source(paste(WD.PATH, 'code/table2.R', sep=''))
source(paste(WD.PATH, 'code/fig3.R', sep=''))
source(paste(WD.PATH, 'code/fig4.R', sep=''))
source(paste(WD.PATH, 'code/fig5.R', sep=''))
source(paste(WD.PATH, 'code/fig6.R', sep=''))

# Re-produce real data (CKD dataset) results: Table 4, Table 5, Table 6, Table 7, Figure 7, Figure 8
source(paste(WD.PATH, 'code/table4.R', sep=''))
source(paste(WD.PATH, 'code/table5.R', sep=''))
source(paste(WD.PATH, 'code/table6.R', sep=''))
source(paste(WD.PATH, 'code/table7.R', sep=''))
source(paste(WD.PATH, 'code/fig7.R', sep=''))
source(paste(WD.PATH, 'code/fig8.R', sep=''))

# Re-produce 'automobile.Rdata' and 'CKD.Rdata'
source(paste(WD.PATH, 'code/autoRdata.R', sep=''))
source(paste(WD.PATH, 'code/ckdRdata.R', sep=''))

