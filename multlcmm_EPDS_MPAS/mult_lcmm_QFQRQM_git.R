
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

mQFQR1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1+ MPAS + I(MPAS^2),idiag = FALSE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')
mQFQR1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1+ MPAS + I(MPAS^2),idiag = TRUE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')

mlcmm_QFQRQM_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2),
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQR1_f)
  }
mlcmm_QFQRQM_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2),
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQR1_f)
  }
mlcmm_QFQRQM_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2),
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQR1_t)
  }
mlcmm_QFQRQM_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2),
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQR1_t)
  }

multQFQRQM2_tf <- mlcmm_QFQRQM_tf(2);
multQFQRQM2_ff <- mlcmm_QFQRQM_ff(2); 
multQFQRQM2_ft <- mlcmm_QFQRQM_ft(2); 
multQFQRQM2_tt <- mlcmm_QFQRQM_tt(2); 

multQFQRQM3_tf <- mlcmm_QFQRQM_tf(3);
multQFQRQM3_ff <- mlcmm_QFQRQM_ff(3); 
multQFQRQM3_ft <- mlcmm_QFQRQM_ft(3); 
multQFQRQM3_tt <- mlcmm_QFQRQM_tt(3); 



multQFQRQM4_tf <- mlcmm_QFQRQM_tf(4);
multQFQRQM4_ff <- mlcmm_QFQRQM_ff(4); 
multQFQRQM4_ft <- mlcmm_QFQRQM_ft(4); 
multQFQRQM4_tt <- mlcmm_QFQRQM_tt(4);  
multQFQRQM5_tf <- mlcmm_QFQRQM_tf(5);
multQFQRQM5_ff <- mlcmm_QFQRQM_ff(5); 
multQFQRQM5_ft <- mlcmm_QFQRQM_ft(5); 
multQFQRQM5_tt <- mlcmm_QFQRQM_tt(5); 

multQFQRQM6_tf <- mlcmm_QFQRQM_tf(6);
multQFQRQM6_ff <- mlcmm_QFQRQM_ff(6); 
multQFQRQM6_ft <- mlcmm_QFQRQM_ft(6); 
multQFQRQM6_tt <- mlcmm_QFQRQM_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQR1_t,mQFQR1_f,
             multQFQRQM2_tf,multQFQRQM2_ff,multQFQRQM2_ft,multQFQRQM2_tt,
             multQFQRQM3_tf,multQFQRQM3_ff,multQFQRQM3_ft,multQFQRQM3_tt,
             multQFQRQM4_tf,multQFQRQM4_ff,multQFQRQM4_ft,multQFQRQM4_tt,
             multQFQRQM5_tf,multQFQRQM5_ff,multQFQRQM5_ft,multQFQRQM5_tt,
             multQFQRQM6_tf,multQFQRQM6_ff,multQFQRQM6_ft,multQFQRQM6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM
kable(mQFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQFQRQM_MPAS_EPDS.html")


