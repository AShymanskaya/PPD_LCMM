# Python and R Code for EDA and LCMM Analysis

Python and R Code used for analysis in the publication "Characterization of Depressive Symptom Trajectories in Women Between Childbirth and Diagnosis" by 
Natalia Chechko, Susanne Stickel, Elena Losse, Aliaksandra Shymanskaya, and Ute Habel.

Please cite this paper when using the code for your research. 

The following order of running the scripts is suggested: 
1)  missing_data_analysis_git.R - data imputation
2)  data_prep_git.py - preparation of imputed data in the long format for lcmm analysis 
3)  m1_lcmm_git.R-m6_lcmm_git.R - lcmm modeling for the EPDS questionnaire
    OR 
    m1_MPAS_git.R - m6_MPAS_git.R - lcmm modeling for the MPAS questionnaire
    OR
    mult_lcmm_LF_git.R - mult_lcmm_QFQRQM_git.R - multlcmm for the EPDS/MPAS questionnaire
3a) mult_lcmm_gridsearch_git - gridsearch of the best performing multlcmm models 
4)  save_full_sample_git.py - save full sample with features and clusters
5)  comparison_of_clusters_lcmm_git.R - compare lcmm clusters based on their features
5a) comparison_of_clusters_multlcmm_git.R - compare multlcmm clusters based on their features

LCMM analysis performed using lcmm package in R. https://cran.r-project.org/web/packages/lcmm/vignettes/introduction.html
