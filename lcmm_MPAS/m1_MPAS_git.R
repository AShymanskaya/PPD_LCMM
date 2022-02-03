#m1

library( LCTMtools )
library(readxl)

data_df <- read_excel("long_melt_mpas.xlsx")
data_df <-data_df[c(2:4)]
data_df$value <-data_df$value+1
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T1'] <- 6
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T2'] <- 9
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T3'] <- 12
data_df$"MPAS"[data_df$"MPAS"=='MPAS_T4'] <- 15

data_df$MPAS <- as.numeric(data_df$MPAS)
data_df$index <- as.integer(data_df$index)
data_df$value <- as.integer(data_df$value)
m0ft_MPAS <- lcmm(fixed = value~1,
             random = ~ 1,
             ng = 1, nwg = FALSE,
             idiag = TRUE,  
             data = data.frame(data_df), subject = "index") 
m0ff_MPAS <- lcmm(fixed = value~1,
             random = ~ 1,
             ng = 1, nwg = FALSE,
             idiag = FALSE,  
             data = data.frame(data_df), subject = "index") 


mLF1ff_MPAS <- lcmm(fixed = value~1+EPDS,
               random = ~ 1,
               ng = 1, nwg = FALSE,
               idiag = FALSE, 
               data = data.frame(data_df), subject = "index") 
mLF1ft_MPAS <- lcmm(fixed = value~1+EPDS,
               random = ~ 1,
               ng = 1, nwg = FALSE,
               idiag = TRUE, 
               data = data.frame(data_df), subject = "index") 


mLFR1ff_MPAS <- lcmm(fixed = value~1+EPDS,
                random = ~ 1+EPDS,
                ng = 1,nwg=FALSE,
                idiag = FALSE, 
                data = data.frame(data_df), subject = "index") 
mLFR1ft_MPAS <- lcmm(fixed = value~1+EPDS,
                random = ~ 1+EPDS,
                ng = 1,nwg=FALSE,
                idiag = TRUE, 
                data = data.frame(data_df), subject = "index") 

mQF1ff_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
               random = ~ 1,
               ng = 1,nwg=FALSE,
               idiag = FALSE, 
               data = data.frame(data_df), subject = "index") 
mQF1ft_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
               random = ~ 1,
               ng = 1,nwg=FALSE,
               idiag = TRUE, 
               data = data.frame(data_df), subject = "index") 
mQFLR1ff_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS,
                 ng = 1,nwg=FALSE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index") 
mQFLR1ft_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS,
                 ng = 1,nwg=FALSE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index") 

mQFQR1ff_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS+I(EPDS^2),
                 ng = 1,nwg=FALSE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index") 
mQFQR1ft_MPAS <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS+I(EPDS^2),
                 ng = 1,nwg=FALSE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index") 

summarytable(m0ff_MPAS,m0ft_MPAS,mLF1ff_MPAS,mLF1ft_MPAS,mLFR1ff_MPAS,mLFR1ft_MPAS,mQF1ff_MPAS,mQF1ft_MPAS,mQFLR1ff_MPAS,mQFLR1ft_MPAS,mQFQR1ff_MPAS,mQFQR1ft_MPAS, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t1_MPAS
kable(t1_MPAS, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m1_MPAS.html")