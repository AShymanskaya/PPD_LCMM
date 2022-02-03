
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

mQFQMLR1_f<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                 random = ~ 1+ MPAS,idiag = FALSE,
                 ng = 1, subject='index_MPAS', 
                 data = data_df,
                 link = 'linear')
mQFQMLR1_t<-multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS + I(MPAS^2) ,
                 random = ~ 1+ MPAS,idiag = TRUE,
                 ng = 1, subject='index_MPAS', 
                 data = data_df,
                 link = 'linear')

mlcmm_QFQMLR_ff <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2) ,
             random = ~ 1 + MPAS  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQMLR1_f)
  }
mlcmm_QFQMLR_tf <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2) ,
             random = ~ 1+ MPAS   ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=FALSE,
             data = data_df,
             link = 'linear', B=mQFQMLR1_f)
  }
mlcmm_QFQMLR_tt <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2) ,
             random = ~ 1+ MPAS ,
             ng = x, subject='index_MPAS', nwg=TRUE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQMLR1_t)
  }
mlcmm_QFQMLR_ft <- 
  function(x){
    multlcmm(fixed =  normMPAS + normEPDS ~ 1+ MPAS+ I(MPAS^2)   ,
             mixture  = ~ 1+ MPAS+ I(MPAS^2) ,
             random = ~ 1+ MPAS  ,
             ng = x, subject='index_MPAS', nwg=FALSE,idiag=TRUE,
             data = data_df,
             link = 'linear', B=mQFQMLR1_t)
  }

multQFQMLR2_tf <- mlcmm_QFQMLR_tf(2);
multQFQMLR2_ff <- mlcmm_QFQMLR_ff(2); 
multQFQMLR2_ft <- mlcmm_QFQMLR_ft(2); 
multQFQMLR2_tt <- mlcmm_QFQMLR_tt(2); 

multQFQMLR3_tf <- mlcmm_QFQMLR_tf(3);
multQFQMLR3_ff <- mlcmm_QFQMLR_ff(3); 
multQFQMLR3_ft <- mlcmm_QFQMLR_ft(3); 
multQFQMLR3_tt <- mlcmm_QFQMLR_tt(3); 



multQFQMLR4_tf <- mlcmm_QFQMLR_tf(4);
multQFQMLR4_ff <- mlcmm_QFQMLR_ff(4); 
multQFQMLR4_ft <- mlcmm_QFQMLR_ft(4); 
multQFQMLR4_tt <- mlcmm_QFQMLR_tt(4);  
multQFQMLR5_tf <- mlcmm_QFQMLR_tf(5);
multQFQMLR5_ff <- mlcmm_QFQMLR_ff(5); 
multQFQMLR5_ft <- mlcmm_QFQMLR_ft(5); 
multQFQMLR5_tt <- mlcmm_QFQMLR_tt(5); 

multQFQMLR6_tf <- mlcmm_QFQMLR_tf(6);
multQFQMLR6_ff <- mlcmm_QFQMLR_ff(6); 
multQFQMLR6_ft <- mlcmm_QFQMLR_ft(6); 
multQFQMLR6_tt <- mlcmm_QFQMLR_tt(6); 


#summary(t2); postprob(t2); plot(t2,which="linkfunction")


summarytable(mQFQMLR1_t,mQFQMLR1_f,
             multQFQMLR2_tf,multQFQMLR2_ff,multQFQMLR2_ft,multQFQMLR2_tt,
             multQFQMLR3_tf,multQFQMLR3_ff,multQFQMLR3_ft,multQFQMLR3_tt,
             multQFQMLR4_tf,multQFQMLR4_ff,multQFQMLR4_ft,multQFQMLR4_tt,
             multQFQMLR5_tf,multQFQMLR5_ff,multQFQMLR5_ft,multQFQMLR5_tt,
             multQFQMLR6_tf,multQFQMLR6_ff,multQFQMLR6_ft,multQFQMLR6_tt, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->mQFQMLRQM
kable(mQFQMLRQM, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "mQFQMLR_MPAS_EPDS.html")


