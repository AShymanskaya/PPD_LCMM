
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

mLFM1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
                   random = ~ 1,idiag = FALSE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')
mLFM1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS ,
                   random = ~ 1,idiag = TRUE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')

mlcmm_LFM_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1   ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLFM1_f)
  }
mlcmm_LFM_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1   ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLFM1_f)
  }
mlcmm_LFM_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLFM1_t)
  }
mlcmm_LFM_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS   ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLFM1_t)
  }

multLFM2_tf <- mlcmm_LFM_tf(2);
multLFM2_ff <- mlcmm_LFM_ff(2); 
multLFM2_ft <- mlcmm_LFM_ft(2); 
multLFM2_tt <- mlcmm_LFM_tt(2); 

multLFM3_tf <- mlcmm_LFM_tf(3);
multLFM3_ff <- mlcmm_LFM_ff(3); 
multLFM3_ft <- mlcmm_LFM_ft(3); 
multLFM3_tt <- mlcmm_LFM_tt(3); 



multLFM4_tf <- mlcmm_LFM_tf(4);
multLFM4_ff <- mlcmm_LFM_ff(4); 
multLFM4_ft <- mlcmm_LFM_ft(4); 
multLFM4_tt <- mlcmm_LFM_tt(4);  
multLFM5_tf <- mlcmm_LFM_tf(5);
multLFM5_ff <- mlcmm_LFM_ff(5); 
multLFM5_ft <- mlcmm_LFM_ft(5); 
multLFM5_tt <- mlcmm_LFM_tt(5); 

multLFM6_tf <- mlcmm_LFM_tf(6);
multLFM6_ff <- mlcmm_LFM_ff(6); 
multLFM6_ft <- mlcmm_LFM_ft(6); 
multLFM6_tt <- mlcmm_LFM_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mLFM1_t,mLFM1_f,
             multLFM2_tf,multLFM2_ff,multLFM2_ft,multLFM2_tt,
             multLFM3_tf,multLFM3_ff,multLFM3_ft,multLFM3_tt,
             multLFM4_tf,multLFM4_ff,multLFM4_ft,multLFM4_tt,
             multLFM5_tf,multLFM5_ff,multLFM5_ft,multLFM5_tt,
             multLFM6_tf,multLFM6_ff,multLFM6_ft,multLFM6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mLFMQM
kable(mLFMQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mLFM_MPAS_EPDS.html")

