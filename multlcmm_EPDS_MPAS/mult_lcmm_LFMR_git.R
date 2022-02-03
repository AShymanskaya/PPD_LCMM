
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

mLFMR1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
                     random = ~ 1+ MPAS,idiag = FALSE,
                     ng = 1, subject='index_MPAS', 
                     data = data_df,
                     link = 'linear')
mLFMR1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS ,
                     random = ~ 1+ MPAS,idiag = TRUE,
                     ng = 1, subject='index_MPAS', 
                     data = data_df,
                     link = 'linear')

mlcmm_LFMR_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1 + MPAS  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLFMR1_f)
  }
mlcmm_LFMR_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS,
             random = ~ 1+ MPAS   ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLFMR1_f)
  }
mlcmm_LFMR_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1+ MPAS ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLFMR1_t)
  }
mlcmm_LFMR_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS   ,
             mixture  = ~ 1+ MPAS ,
             random = ~ 1+ MPAS  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLFMR1_t)
  }

multLFMR2_tf <- mlcmm_LFMR_tf(2);
multLFMR2_ff <- mlcmm_LFMR_ff(2); 
multLFMR2_ft <- mlcmm_LFMR_ft(2); 
multLFMR2_tt <- mlcmm_LFMR_tt(2); 

multLFMR3_tf <- mlcmm_LFMR_tf(3);
multLFMR3_ff <- mlcmm_LFMR_ff(3); 
multLFMR3_ft <- mlcmm_LFMR_ft(3); 
multLFMR3_tt <- mlcmm_LFMR_tt(3); 



multLFMR4_tf <- mlcmm_LFMR_tf(4);
multLFMR4_ff <- mlcmm_LFMR_ff(4); 
multLFMR4_ft <- mlcmm_LFMR_ft(4); 
multLFMR4_tt <- mlcmm_LFMR_tt(4);  
multLFMR5_tf <- mlcmm_LFMR_tf(5);
multLFMR5_ff <- mlcmm_LFMR_ff(5); 
multLFMR5_ft <- mlcmm_LFMR_ft(5); 
multLFMR5_tt <- mlcmm_LFMR_tt(5); 

multLFMR6_tf <- mlcmm_LFMR_tf(6);
multLFMR6_ff <- mlcmm_LFMR_ff(6); 
multLFMR6_ft <- mlcmm_LFMR_ft(6); 
multLFMR6_tt <- mlcmm_LFMR_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mLFMR1_t,mLFMR1_f,
             multLFMR2_tf,multLFMR2_ff,multLFMR2_ft,multLFMR2_tt,
             multLFMR3_tf,multLFMR3_ff,multLFMR3_ft,multLFMR3_tt,
             multLFMR4_tf,multLFMR4_ff,multLFMR4_ft,multLFMR4_tt,
             multLFMR5_tf,multLFMR5_ff,multLFMR5_ft,multLFMR5_tt,
             multLFMR6_tf,multLFMR6_ff,multLFMR6_ft,multLFMR6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mLFMRQM
kable(mLFMRQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mLFMR_MPAS_EPDS.html")

