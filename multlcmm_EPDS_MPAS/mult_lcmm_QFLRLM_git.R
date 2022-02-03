
library( LCTMtools )
library(readxl)

data_df <- read_excel("long_melt_epds_mpas.xlsx")
data_df = as.data.frame(data_df)

data_df <-data_df[c(2,3,4,7)]
data_df$value_EPDS <-data_df$value_EPDS+1
data_df$value_MPAS <-data_df$value_MPAS+1
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T1'] <- 6
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T2'] <- 9
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T3'] <- 12
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T4'] <- 15

data_df$MPAS <- as.numeric(data_df$MPAS)
data_df$index_MPAS <-data_df$index_MPAS+1
data_df$index_MPAS <- as.integer(data_df$index_MPAS)
data_df$value_MPAS <- as.integer(data_df$value_MPAS)
data_df$value_EPDS <- as.integer(data_df$value_EPDS)

data_df$normMPAS <- data_df$value_MPAS

data_df$normEPDS<-max(data_df$value_EPDS)-data_df$value_EPDS+1


hist(data_df$value_MPAS, breaks=31,main="MPAS distribution")
hist(data_df$normEPDS, breaks=31,main="EPDS distribution")

mQFLRLM1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                     random = ~ 1+ MPAS ,idiag = FALSE,
                     ng = 1, subject='index_MPAS', 
                     data = data_df,
                     link = 'linear')
mQFLRLM1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                     random = ~ 1+ MPAS ,idiag = TRUE,
                     ng = 1, subject='index_MPAS', 
                     data = data_df,
                     link = 'linear')

mlcmm_QFLRLM_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFLRLM1_f)
  }
mlcmm_QFLRLM_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFLRLM1_f)
  }
mlcmm_QFLRLM_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFLRLM1_t)
  }
mlcmm_QFLRLM_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFLRLM1_t)
  }

multQFLRLM2_tf <- mlcmm_QFLRLM_tf(2);
multQFLRLM2_ff <- mlcmm_QFLRLM_ff(2); 
multQFLRLM2_ft <- mlcmm_QFLRLM_ft(2); 
multQFLRLM2_tt <- mlcmm_QFLRLM_tt(2); 

multQFLRLM3_tf <- mlcmm_QFLRLM_tf(3);
multQFLRLM3_ff <- mlcmm_QFLRLM_ff(3); 
multQFLRLM3_ft <- mlcmm_QFLRLM_ft(3); 
multQFLRLM3_tt <- mlcmm_QFLRLM_tt(3); 



multQFLRLM4_tf <- mlcmm_QFLRLM_tf(4);
multQFLRLM4_ff <- mlcmm_QFLRLM_ff(4); 
multQFLRLM4_ft <- mlcmm_QFLRLM_ft(4); 
multQFLRLM4_tt <- mlcmm_QFLRLM_tt(4);  
multQFLRLM5_tf <- mlcmm_QFLRLM_tf(5);
multQFLRLM5_ff <- mlcmm_QFLRLM_ff(5); 
multQFLRLM5_ft <- mlcmm_QFLRLM_ft(5); 
multQFLRLM5_tt <- mlcmm_QFLRLM_tt(5); 

multQFLRLM6_tf <- mlcmm_QFLRLM_tf(6);
multQFLRLM6_ff <- mlcmm_QFLRLM_ff(6); 
multQFLRLM6_ft <- mlcmm_QFLRLM_ft(6); 
multQFLRLM6_tt <- mlcmm_QFLRLM_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQR1_t,mQFQR1_f,
             multQFLRLM2_tf,multQFLRLM2_ff,multQFLRLM2_ft,multQFLRLM2_tt,
             multQFLRLM3_tf,multQFLRLM3_ff,multQFLRLM3_ft,multQFLRLM3_tt,
             multQFLRLM4_tf,multQFLRLM4_ff,multQFLRLM4_ft,multQFLRLM4_tt,
             multQFLRLM5_tf,multQFLRLM5_ff,multQFLRLM5_ft,multQFLRLM5_tt,
             multQFLRLM6_tf,multQFLRLM6_ff,multQFLRLM6_ft,multQFLRLM6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM
kable(mQFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQFLRLM_MPAS_EPDS.html")


