#calculate gridsearch only for the best performing multlcmm models
mQFQMLR1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                     random = ~ 1+ MPAS,idiag = FALSE,
                     ng = 1, subject='index_MPAS', 
                     data = data_df,
                     link = 'linear')

mlcmm_QFQMLR4_tf_gridsearch <- gridsearch(multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
                                                   mixture  = ~ 1+ MPAS+ I(MPAS^2) ,
                                                   random = ~ 1+ MPAS   ,
                                                   ng = 4, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
                                                   data = data_df,
                                                   link = 'linear'), rep=100, maxiter=50, minit=mQFQMLR1_f)


mQF1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                 random = ~ 1,idiag = FALSE,
                 ng = 1, subject='index_MPAS', 
                 data = data_df,
                 link = 'linear')
mlcmm_QFQM5_tf_gridsearch <- gridsearch(multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
                                                 mixture  = ~ 1+ MPAS+ I(MPAS^2),
                                                 random = ~ 1   ,
                                                 ng = 5, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
                                                 data = data_df,
                                                 link = 'linear'), rep=100, maxiter=50, minit=mQF1_f)





mlcmm_QFQM6_tf_gridsearch <- gridsearch(multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
                                                 mixture  = ~ 1+ MPAS+ I(MPAS^2),
                                                 random = ~ 1   ,
                                                 ng = 6, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
                                                 data = data_df,
                                                 link = 'linear'), rep=100, maxiter=50, minit=mQF1_f)




mQF1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                 random = ~ 1,idiag = TRUE,
                 ng = 1, subject='index_MPAS', 
                 data = data_df,
                 link = 'linear')
mlcmm_QFQM6_tt_gridsearch <- gridsearch(multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
                                                 mixture  = ~ 1+ MPAS+ I(MPAS^2),
                                                 random = ~ 1   ,
                                                 ng = 5, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
                                                 data = data_df,
                                                 link = 'linear'), rep=100, maxiter=50, minit=mQF1_t)




mlcmm_QFQM6_tt_gridsearch <- gridsearch(multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
                                                 mixture  = ~ 1+ MPAS+ I(MPAS^2),
                                                 random = ~ 1   ,
                                                 ng = 6, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
                                                 data = data_df,
                                                 link = 'linear'), rep=100, maxiter=50, minit=mQF1_t)



summarytable(mlcmm_QFQMLR4_tf_gridsearch,mlcmm_QFQM5_tf_gridsearch,mlcmm_QFQM6_tf_gridsearch,
             #mlcmm_QFQM6_tt_gridsearch, 
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM_grid
kable(mQFQM_grid, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mult_gridsearch.html")


