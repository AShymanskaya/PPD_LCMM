
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

mQF1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1,idiag = FALSE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')
mQF1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                   random = ~ 1,idiag = TRUE,
                   ng = 1, subject='index_MPAS', 
                   data = data_df,
                   link = 'linear')

mlcmm_QF_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1   ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQF1_f)
  }
mlcmm_QF_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1  ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQF1_f)
  }
mlcmm_QF_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQF1_t)
  }
mlcmm_QF_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1,
             random = ~ 1 ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQF1_t)
  }

multQF2_tf <- mlcmm_QF_tf(2);
multQF2_ff <- mlcmm_QF_ff(2); 
multQF2_ft <- mlcmm_QF_ft(2); 
multQF2_tt <- mlcmm_QF_tt(2); 

multQF3_tf <- mlcmm_QF_tf(3);
multQF3_ff <- mlcmm_QF_ff(3); 
multQF3_ft <- mlcmm_QF_ft(3); 
multQF3_tt <- mlcmm_QF_tt(3); 



multQF4_tf <- mlcmm_QF_tf(4);
multQF4_ff <- mlcmm_QF_ff(4); 
multQF4_ft <- mlcmm_QF_ft(4); 
multQF4_tt <- mlcmm_QF_tt(4);  
multQF5_tf <- mlcmm_QF_tf(5);
multQF5_ff <- mlcmm_QF_ff(5); 
multQF5_ft <- mlcmm_QF_ft(5); 
multQF5_tt <- mlcmm_QF_tt(5); 

multQF6_tf <- mlcmm_QF_tf(6);
multQF6_ff <- mlcmm_QF_ff(6); 
multQF6_ft <- mlcmm_QF_ft(6); 
multQF6_tt <- mlcmm_QF_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQR1_t,mQFQR1_f,
             multQF2_tf,multQF2_ff,multQF2_ft,multQF2_tt,
             multQF3_tf,multQF3_ff,multQF3_ft,multQF3_tt,
             multQF4_tf,multQF4_ff,multQF4_ft,multQF4_tt,
             multQF5_tf,multQF5_ff,multQF5_ft,multQF5_tt,
             multQF6_tf,multQF6_ff,multQF6_ft,multQF6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQM
kable(mQFQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQF_MPAS_EPDS.html")


