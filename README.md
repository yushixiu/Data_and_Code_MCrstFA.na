# Data and Code

## Supplementary Material for "Mixtures of skewed and heavy-tailed common factor analyzers for learning incomplete multivariate data" by Wan-Lun Wang, Luis M. Castro, Shi-Xiu Yu and Tsung-I Lin

### Author responsible for the code

For questions, comments or remarks about the code please contact responsible author, Tsung-I Lin (tilin@nchu.edu.tw)

### Configurations

The code was written/evaluated in R with the following software
versions: R version 4.2.1 (2022-06-23 ucrt) Platform:
x86_64-w64-mingw32/x64 (64-bit) Running under: Windows 10 x64 (build 22621)

Matrix products: default

locale: [1] LC_COLLATE=Chinese (Traditional)\_Taiwan.utf8
LC_CTYPE=Chinese (Traditional)\_Taiwan.utf8\
[3] LC_MONETARY=Chinese (Traditional)\_Taiwan.utf8 LC_NUMERIC=C\
[5] LC_TIME=Chinese (Traditional)\_Taiwan.utf8

attached base packages: [1] stats4 stats graphics grDevices utils
datasets methods base

other attached packages: [1] GIGrvg_0.8 cubature_2.0.4.5
EMMIXuskew_0.11-6 devtools_2.4.5 usethis_2.1.6\
[6] matrixcalc_1.0-6 tmvtnorm_1.5 gmm_1.7 sandwich_3.0-2 Matrix_1.5-3\
[11] MomTrunc_6.0 mvtnorm_1.1-3 moments_0.14.1 gridExtra_2.3
ggforce_0.4.1\
[16] ggplot2_3.4.0 MASS_7.3-58.1 cpca_0.1.2 EMMIXmfa_2.0.11
mclust_6.0.0\
[21] gclus_1.3.2 cluster_2.1.3 plot3D_1.4 misc3d_0.9-1 rgl_0.110.2

loaded via a namespace (and not attached): [1] fs_1.5.2 tools_4.2.1
profvis_0.3.7 utf8_1.2.2 R6_2.5.1\
[6] hypergeo_1.2-13 DBI_1.1.3 colorspace_2.0-3 urlchecker_1.0.1
withr_2.5.0\
[11] tidyselect_1.2.0 prettyunits_1.1.1 processx_3.8.0 compiler_4.2.1
cli_3.4.1\
[16] tlrmvnmvt_1.1.2 scales_1.2.1 callr_3.7.3 stringr_1.5.0
digest_0.6.30\
[21] base64enc_0.1-3 pkgconfig_2.0.3 htmltools_0.5.3 sessioninfo_1.2.2
fastmap_1.1.0\
[26] htmlwidgets_1.5.4 rlang_1.0.6 rstudioapi_0.14 shiny_1.7.3
farver_2.1.1\
[31] generics_0.1.3 zoo_1.8-11 jsonlite_1.8.3 dplyr_1.0.10
magrittr_2.0.3\
[36] Rcpp_1.0.9 munsell_0.5.0 fansi_1.0.3 lifecycle_1.0.3 stringi_1.7.8\
[41] pkgbuild_1.4.0 grid_4.2.1 promises_1.2.0.1 crayon_1.5.2
miniUI_0.1.1.1\
[46] contfrac_1.1-12 lattice_0.20-45 knitr_1.41 ps_1.7.2 pillar_1.8.1\
[51] tcltk_4.2.1 pkgload_1.3.2 glue_1.6.2 remotes_2.4.2 deSolve_1.34\
[56] vctrs_0.5.0 tweenr_2.0.2 httpuv_1.6.6 gtable_0.3.1 purrr_0.3.5\
[61] polyclip_1.10-4 assertthat_0.2.1 cachem_1.0.6 xfun_0.35 mime_0.12\
[66] xtable_1.8-4 later_1.3.0 tibble_3.1.8 elliptic_1.4-0 memoise_2.0.1\
[71] ellipsis_0.3.2

### Descriptions of the codes

Please extract the file "Data-and-Code-CAM.zip" to the "current working
directory" of the R package. The getwd() function shall determine an
absolute pathname of the "current working directory".

Before running the codes 'fig1.R', 'fig2.R', 'fig4.R', 'fig5.R',
'fig6.R', 'table1.R', 'tableS.1.R', 'tableS.2.R', 'tableS.3.R',
'tableS.4.R', 'simulation1.R' and 'simulation2.R', one needs to install
the following R packages:

    install.packages("mvtnorm")  Version: 1.1-3
    install.packages("tmvtnorm") Version: 1.5
    install.packages("cubature") Version： 2.0.4.5
    install.packages("MomTrunc")   Version： 6.0
    install.packages("moments")   Version： 0.14.1
    install.packages("gclus")   Version： 1.3.2
    install.packages("mclust")   Version： 6.0.0
    install.packages("EMMIXmfa")   Version： 2.0.11
    install.packages("EMMIXuskew")   Version： 0.11-6
    install.packages("cpca")   Version： 0.1.2
    install.packages("rgl")   Version： 0.110.2
    install.packages("MASS")   Version： 7.3-58.1
    install.packages("misc3d")   Version： 0.9-1
    install.packages("plot3D")   Version： 1.4
    install.packages("GIGrvg")   Version： 0.8
    install.packages("matrixcalc")   Version： 1.0-6
    install.packages("devtools")   Version： 2.4.5
    install.packages("ggplot2")   Version： 3.4.0

R codes for the implementation of our methodology are provided.

#### Subfolder: ./function

./function contains the program (function) of

       (1) 'initial.R' for setting the initial values;
       (2) 'MCrSTFA.R' for calculating the parameter estimates via the ECME algorithm under the MCrstFA model;
       (3) 'MCFA.na.R' for calculating the parameter estimates via the ECME algorithm under the MCFA model with missing information;
       (4) 'MCtFA.na.R' for calculating the parameter estimates via the ECME algorithm under the MCtFA model with missing information;
       (5) 'MCrstFA.na.R' for calculating the parameter estimates via the ECME algorithm under the MCrstFA model with missing information;
       (6) 'gener_na.R' for randomly generating missing values;
       (7) 'rMST.R' for calculating the pdf and cdf of the restricted skew-t distributions
       (8) 'rMNIG.R' for calculating the pdf of the MNIG distributions;
       (9) 'MCrSTFA_na_se.R' for computing the score functions of parameter estimates; and
       (10) 'Sim1tab1.R' for computing the BIC, ICL, ARI, CCR, and MSPE values in Table1.

#### Subfolder: ./code

./code contains

       (1) 'fig1.R' main script for drawing the 3-dimensional density contours of the rMST distribution;

###### Note for Section 2 - Notation and prerequisites:

To draw the 3-dimensional density contours as shown in Figure 1, please
source the 'rMST.R' scripts in subfolder './function/', and then run the
'fig1.R' scripts in subfolder './code/'. The results have been stored in
'./results/'.

       (2) 'fig2.R' main script for drawing the 3D scatter plots of observed data points with 20% missing values and their predicted values by the MCFA, MCtFA and MCrstFA models;
       (3) 'table1.R' main script for performing comparisons of MCFA, MCtFA and MCrstFA models based on 100 replications;
       (4) 'simulation1.R' main script for re-generating part of intermediate results for Experiment 1;

###### Note for Section 5.1 - Experiment 1:

R code 'simulation1.R' generates the intermediate results of Table 1 and
Figure 2 in the manuscript. Because the code takes a huge amount of time
to run, we record these intermediate results so that one can use the R
codes 'fig2.R' and 'table1.R' to obtain the final results based on files
stored in './results/' subfolders.

To reproduce the results presented in Figure 2, please source the
'rMNIG.R' scripts in subfolder './function/', and then run the 'fig2.R'
scripts in subfolder './code/' and the '.txt' files in the subfolders
'./results/Sim1_TabFig'. To reproduce the results presented in Table 1,
please source the 'rMNIG.R', 'MCFA.na.R', 'MCtFA.na.R', 'MCrstFA.na.R',
'gener_na.R', 'initial.R' and 'Sim1tab1.R' scripts in subfolder
'./function/', and then run the 'table1.R' scripts in subfolder
'./code/' and the '.txt' files in the subfolders
'./results/Sim1_TabFig'.

       (5) 'simulation2.R' main script for re-generating part of intermediate results for Experiment 2;
       (6) 'tableS.1.R' main script for RMSE, STD and IMSE values of parameter estimates for various sample sizes in the r=0% missing value scenario;
       (7) 'tableS.2.R' main script for RMSE, STD and IMSE values of parameter estimates for various sample sizes in the r=10% missing value scenario;
       (8) 'tableS.3.R' main script for RMSE, STD and IMSE values of parameter estimates for various sample sizes in the r=20% missing value scenario;
       (9) 'tableS.4.R' main script for RMSE, STD and IMSE values of parameter estimates for various sample sizes in the r=30% missing value scenario;
       (10) 'RMSE.R' main script for calculating root mean square error of parameter estimates;
       (11) 'STD.R' main script for calculating sample standard deviation of the parameters;
       (12) 'IMSE.R' main script for calculating the information matrix-based standard errors of parameter estimates;

###### Note for Section 5.2 - Experiment 2:

R code 'simulation2.R' generates the intermediate results of Tables
S.1-S.4 in the supplementary materials. Because the code takes a huge
amount of time to run, we record these intermediate results so that one
can use the R codes 'tableS.1.R', 'tableS.2.R', 'tableS.3.R' and
'tableS.4.R' to obtain the final results based on files stored in
'./results/' subfolders.

To reproduce the results presented in Tables S.1-S.4, please source the
'MCrSTFA.R', 'MCrstFA.na.R', 'MCrSTFA_na_se.R' and 'gener_na.R' scripts
in subfolder './function/', and then run the 'tableS.1.R', 'tableS.2.R',
'tableS.3.R' and 'tableS.4.R' scripts in subfolder './code/'.

TableS.1 just run the '.txt' files in the subfolders
'./results/Sim2_TableS.1(r=0)', and then run the 'RMSE.R', 'STD.R' and
'IMSE.R' scripts in subfolder './code/'. TableS.2 just run the '.txt'
files in the subfolders './results/Sim2_TableS.2(r=10)', and then run
the 'RMSE.R', 'STD.R' and 'IMSE.R' scripts in subfolder './code/'.
TableS.3 just run the '.txt' files in the subfolders
'./results/Sim2_TableS.3(r=20)', and then run the 'RMSE.R', 'STD.R' and
'IMSE.R' scripts in subfolder './code/'. TableS.4 just run the '.txt'
files in the subfolders './results/Sim2_TableS.4(r=30)', and then run
the 'RMSE.R', 'STD.R' and 'IMSE.R' scripts in subfolder './code/'.

       (13) 'table2.R' main script for providing precise information on some selected attributes;
       (14) 'fig3.R' main script for drawing BIC and ICL diagrams for the MCFA, MCtFA and MCrstFA models fitted to the automobile dataset;
       (15) 'fig4.R' main script for drawing the histograms and pairwise scatter plots of a subset of continuous variables in the automobile dataset;
       (16) 'fig5.R' main script for drawing stacked bar charts of a subset of categorical attributes in the automobile dataset;
       (17) 'fig6.R' main script for drawing 2D and 3D scatter plots protracting by the first, second and fifth factors of the automobile dataset;

###### Note for Section 6.1 - Automobile dataset:

Figures 3-6 just load 'automobile.RData' files in the './data/'. The
resulting Table 2 and Figures 3-6 has been stored in the './results/'
subfolder.

       (18) 'table4.R' main script for summarizing the missing information in the CKD dataset;
       (19) 'table5.R' main script for calculating the sample correlation coefficients for each pair of attributes within the two groups;
       (20) 'table6.R' main script for comparing the clustering performance under the MCFA, MCtFA and MCrstFA models by ARI and CCR;
       (21) 'table7.R' main script for showing the ML estimates and standard errors for the best chosen model;
       (22) 'fig7.R' main script for drawing the pairwise scatter plots and the marginal histograms;
       (23) 'fig8.R' main script for drawing the BIC and ICL plots for the MCFA, MCtFA and MCrstFA models fitted to the CKD dataset.

###### Note for Section 6.2 - Chronic kidney disease (CKD) dataset:

Tables 6-7 and Figures 7-8 just load 'CKD.RData' files in the './data/'.
The resulting Tables 4-7 and Figures 7-8 has been stored in the
'./results/' subfolder.

       (24) 'autoRdata.R' main script for model fitting to automobile dataset results and saved in 'automobile.RData';
       (25) 'ckdRdata.R' main script for model fitting to CKD dataset results and saved in 'CKD.RData'.

#### Subfolder: ./data

./data contains

      (1) automobile.RData: the analysis results of the automobile dataset (g=5, q=6);
      (2) CKD.RData: the analysis results of the CKD dataset (g=2, q=4);
      (3) imports-85.data: the dataset sources in 'automobile.RData' consists of three types of auto entities;
      (4) ckd.txt: the dataset sources in 'CKD.RData' can be used to predict the chronic kidney disease.

#### Subfolder: ./results

./results contains

      (1) rMST_3Dplot.png: pdfs for restricted skew-t (rMST) distribution under 4 different specifications of skewness parameters;
      (2) sim1_scatter.eps: 3D scatter plots for the r=20% simulated case show the observed data points overlaid with the predicted values corresponding to the observed data;
      (3) BIC_plot.eps: diagrams of the MCFA, MCtFA and MCrstFA models showing the model comparison in terms of BIC;
      (4) ICL_plot.eps: diagrams of the MCFA, MCtFA and MCrstFA models showing the model comparison in terms of ICL;
      (5) scatter_plot.eps: histograms and pairwise scatter plots of 'price' and 4 standardized continuous attributes and the correlation coefficients between each pair of variables are shown on the upper triangular panel;
      (6) barplot.eps: stacked bar charts of some selected categorical variables;
      (7) auto_2D_plot.eps: 2D sactter plots of the data using the 1st, 2nd and 5th factor scores calculated in the fit of MCrstFA model with g=5 and q=6;
      (8) auto_3D_plot.eps: 3D sactter plot of the data using the 1st, 2nd and 5th factor scores calculated in the fit of MCrstFA model with g=5 and q=6;
      (9) ckd_scatter.eps: pairwise scatter plots and the marginal histograms of the 11 attributes of the CKD and non-CKD patients;
      (10) ckd_bic_icl.eps: diagrams for q=1-5 compare the performance of fitting results in terms of BIC and ICL;
      (11) Table1.csv: summary table of the average values (Mean) of the five criteria for r=10%, 20%, and 30%, and their respective standard deviations (Std) and frequencies (Freq);
      (12) Table2.csv: characterization of the automobile dataset contains 205 cars with 25 attributes, except for the ’Make’ attribute;
      (13) Table4.csv: detailed account of different missing fractions on the basis of patient-wise, attribute-wise and observation-wise placements;
      (14) Table5.csv: observed pairwise correlations for eleven attributes;
      (15) Table6.csv: cross-tabulations of true and predicted class memberships and the corresponding CCR and ARI values for three mixtures common factor analyzers (g=2, q=4) fitted to the CKD dataset;
      (16) Table7.csv: ML estimates and standard errors obtained from fitting the MCrstFA model with g=2 and q=4 for the CKD dataset;
      (17) TableS.1.csv: simulation results for assessing the precision of parameters estimates along with the associate standard errors across various samples sizes under the scenario of r=0% missing values;
      (18) TableS.2.csv: simulation results for assessing the precision of parameters estimates along with the associate standard errors across various samples sizes under the scenario of r=10% missing values;
      (19) TableS.3.csv: simulation results for assessing the precision of parameters estimates along with the associate standard errors across various samples sizes under the scenario of r=20% missing values;
      (20) TableS.4.csv: simulation results for assessing the precision of parameters estimates along with the associate standard errors across various samples sizes under the scenario of r=30% missing values;
      (21) Sim1_TabFig; the subfolder contains model fitting results (the MCFA, MCtFA and MCrstFA models), clustering performance (ARI and CCR), and model selection criterion (BIC and ICL) under three cases of missing value from Experiment 1;
      (22) Sim2_TableS.1(r=0): the subfolder contains the parameters estimates and the associate standard errors under r=0% missing values from Experiment 2;
      (23) Sim2_TableS.2(r=10): the subfolder contains the parameters estimates and the associate standard errors under r=10% missing values from Experiment 2;
      (24) Sim2_TableS.3(r=20): the subfolder contains the parameters estimates and the associate standard errors under r=20% missing values from Experiment 2;
      (25) Sim2_TableS.4(r=30): the subfolder contains the parameters estimates and the associate standard errors under r=30% missing values from Experiment 2.

###### Note for ./results:

Since the initial seed is not given, the numerical results in
'./results/Sim1_TabFig', './results/Sim2_TableS.1(r=0)', './results/Sim2_TableS.2(r=10)', './results/Sim2_TableS.3(r=20)' and
'./results/Sim2_TableS.4(r=30)' obtained by each program execution may not be the same.

###### Additional Remark

One can directly run each 'source(.)' described in 'master.R' file in the seperate R session to obtain the results.
