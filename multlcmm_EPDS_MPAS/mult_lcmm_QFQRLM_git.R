
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

mQFQRLM1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1+ MPAS + I(MPAS^2),idiag = FALSE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')
mQFQRLM1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1+ MPAS + I(MPAS^2),idiag = TRUE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')

mlcmm_QFQRLM_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQRLM1_f)
  }
mlcmm_QFQRLM_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQRLM1_f)
  }
mlcmm_QFQRLM_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQRLM1_t)
  }
mlcmm_QFQRLM_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQRLM1_t)
  }

multQFQRLM2_tf <- mlcmm_QFQRLM_tf(2);
multQFQRLM2_ff <- mlcmm_QFQRLM_ff(2); 
multQFQRLM2_ft <- mlcmm_QFQRLM_ft(2); 
multQFQRLM2_tt <- mlcmm_QFQRLM_tt(2); 

multQFQRLM3_tf <- mlcmm_QFQRLM_tf(3);
multQFQRLM3_ff <- mlcmm_QFQRLM_ff(3); 
multQFQRLM3_ft <- mlcmm_QFQRLM_ft(3); 
multQFQRLM3_tt <- mlcmm_QFQRLM_tt(3); 



multQFQRLM4_tf <- mlcmm_QFQRLM_tf(4);
multQFQRLM4_ff <- mlcmm_QFQRLM_ff(4); 
multQFQRLM4_ft <- mlcmm_QFQRLM_ft(4); 
multQFQRLM4_tt <- mlcmm_QFQRLM_tt(4);  
multQFQRLM5_tf <- mlcmm_QFQRLM_tf(5);
multQFQRLM5_ff <- mlcmm_QFQRLM_ff(5); 
multQFQRLM5_ft <- mlcmm_QFQRLM_ft(5); 
multQFQRLM5_tt <- mlcmm_QFQRLM_tt(5); 

multQFQRLM6_tf <- mlcmm_QFQRLM_tf(6);
multQFQRLM6_ff <- mlcmm_QFQRLM_ff(6); 
multQFQRLM6_ft <- mlcmm_QFQRLM_ft(6); 
multQFQRLM6_tt <- mlcmm_QFQRLM_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQR1_t,mQFQR1_f,
             multQFQRLM2_tf,multQFQRLM2_ff,multQFQRLM2_ft,multQFQRLM2_tt,
             multQFQRLM3_tf,multQFQRLM3_ff,multQFQRLM3_ft,multQFQRLM3_tt,
             multQFQRLM4_tf,multQFQRLM4_ff,multQFQRLM4_ft,multQFQRLM4_tt,
             multQFQRLM5_tf,multQFQRLM5_ff,multQFQRLM5_ft,multQFQRLM5_tt,
             multQFQRLM6_tf,multQFQRLM6_ff,multQFQRLM6_ft,multQFQRLM6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM
kable(mQFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQFQRLM_MPAS_EPDS.html")

