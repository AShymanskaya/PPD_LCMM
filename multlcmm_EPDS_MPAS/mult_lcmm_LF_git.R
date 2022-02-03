
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

mLF1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
                  random = ~ 1,idiag = FALSE,
                  ng = 1, subject='index_MPAS', 
                  data = data_df,
                  link = 'linear')
mLF1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS ,
                  random = ~ 1,idiag = TRUE,
                  ng = 1, subject='index_MPAS', 
                  data = data_df,
                  link = 'linear')

mlcmm_LF_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1 ,
             random = ~ 1   ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLF1_f)
  }
mlcmm_LF_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1,
             random = ~ 1   ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mLF1_f)
  }
mlcmm_LF_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS  ,
             mixture  = ~ 1 ,
             random = ~ 1,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLF1_t)
  }
mlcmm_LF_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS   ,
             mixture  = ~ 1 ,
             random = ~ 1  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mLF1_t)
  }

multLF2_tf <- mlcmm_LF_tf(2);
multLF2_ff <- mlcmm_LF_ff(2); 
multLF2_ft <- mlcmm_LF_ft(2); 
multLF2_tt <- mlcmm_LF_tt(2); 

multLF3_tf <- mlcmm_LF_tf(3);
multLF3_ff <- mlcmm_LF_ff(3); 
multLF3_ft <- mlcmm_LF_ft(3); 
multLF3_tt <- mlcmm_LF_tt(3); 



multLF4_tf <- mlcmm_LF_tf(4);
multLF4_ff <- mlcmm_LF_ff(4); 
multLF4_ft <- mlcmm_LF_ft(4); 
multLF4_tt <- mlcmm_LF_tt(4);  
multLF5_tf <- mlcmm_LF_tf(5);
multLF5_ff <- mlcmm_LF_ff(5); 
multLF5_ft <- mlcmm_LF_ft(5); 
multLF5_tt <- mlcmm_LF_tt(5); 

multLF6_tf <- mlcmm_LF_tf(6);
multLF6_ff <- mlcmm_LF_ff(6); 
multLF6_ft <- mlcmm_LF_ft(6); 
multLF6_tt <- mlcmm_LF_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mLF1_t,mLF1_f,
             multLF2_tf,multLF2_ff,multLF2_ft,multLF2_tt,
             multLF3_tf,multLF3_ff,multLF3_ft,multLF3_tt,
             multLF4_tf,multLF4_ff,multLF4_ft,multLF4_tt,
             multLF5_tf,multLF5_ff,multLF5_ft,multLF5_tt,
             multLF6_tf,multLF6_ff,multLF6_ft,multLF6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mLFQM
kable(mLFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mLF_MPAS_EPDS.html")

