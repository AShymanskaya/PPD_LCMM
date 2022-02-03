
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

mlcmm_QFQR_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQR1_f)
  }
mlcmm_QFQR_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQR1_f)
  }
mlcmm_QFQR_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQR1_t)
  }
mlcmm_QFQR_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 + MPAS+ I(MPAS^2)  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQR1_t)
  }

multQFQR2_tf <- mlcmm_QFQR_tf(2);
multQFQR2_ff <- mlcmm_QFQR_ff(2); 
multQFQR2_ft <- mlcmm_QFQR_ft(2); 
multQFQR2_tt <- mlcmm_QFQR_tt(2); 

multQFQR3_tf <- mlcmm_QFQR_tf(3);
multQFQR3_ff <- mlcmm_QFQR_ff(3); 
multQFQR3_ft <- mlcmm_QFQR_ft(3); 
multQFQR3_tt <- mlcmm_QFQR_tt(3); 



multQFQR4_tf <- mlcmm_QFQR_tf(4);
multQFQR4_ff <- mlcmm_QFQR_ff(4); 
multQFQR4_ft <- mlcmm_QFQR_ft(4); 
multQFQR4_tt <- mlcmm_QFQR_tt(4);  
multQFQR5_tf <- mlcmm_QFQR_tf(5);
multQFQR5_ff <- mlcmm_QFQR_ff(5); 
multQFQR5_ft <- mlcmm_QFQR_ft(5); 
multQFQR5_tt <- mlcmm_QFQR_tt(5); 

multQFQR6_tf <- mlcmm_QFQR_tf(6);
multQFQR6_ff <- mlcmm_QFQR_ff(6); 
multQFQR6_ft <- mlcmm_QFQR_ft(6); 
multQFQR6_tt <- mlcmm_QFQR_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQR1_t,mQFQR1_f,
             multQFQR2_tf,multQFQR2_ff,multQFQR2_ft,multQFQR2_tt,
             multQFQR3_tf,multQFQR3_ff,multQFQR3_ft,multQFQR3_tt,
             multQFQR4_tf,multQFQR4_ff,multQFQR4_ft,multQFQR4_tt,
             multQFQR5_tf,multQFQR5_ff,multQFQR5_ft,multQFQR5_tt,
             multQFQR6_tf,multQFQR6_ff,multQFQR6_ft,multQFQR6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM
kable(mQFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQFQR_MPAS_EPDS.html")



