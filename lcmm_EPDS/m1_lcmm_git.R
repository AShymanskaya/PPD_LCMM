#m1

library( LCTMtools )
library(readxl)
library( lcmm )
library(knitr)

data_df <- read_excel("long_melt_epds.xlsx")
data_df <-data_df[c(2:4)]
data_df$value <-data_df$value+1
data_df$"EPDS"[data_df$"EPDS"=='EPDS_T0'] <- 3
data_df$"EPDS"[data_df$"EPDS"=='EPDS_T1'] <- 6
data_df$"EPDS"[data_df$"EPDS"=='EPDS_T2'] <- 9
data_df$"EPDS"[data_df$"EPDS"=='EPDS_T3'] <- 12
data_df$"EPDS"[data_df$"EPDS"=='EPDS_T4'] <- 15

data_df$EPDS <- as.numeric(data_df$EPDS)
data_df$index <- as.integer(data_df$index)
data_df$value <- as.integer(data_df$value)
m0ft <- lcmm(fixed = value~1,
             random = ~ 1,
             ng = 1, nwg = FALSE,
             idiag = TRUE,  
             data = data.frame(data_df), subject = "index") 
m0ff <- lcmm(fixed = value~1,
             random = ~ 1,
             ng = 1, nwg = FALSE,
             idiag = FALSE,  
             data = data.frame(data_df), subject = "index") 


mLF1ff <- lcmm(fixed = value~1+EPDS,
               random = ~ 1,
               ng = 1, nwg = FALSE,
               idiag = FALSE, 
               data = data.frame(data_df), subject = "index") 
mLF1ft <- lcmm(fixed = value~1+EPDS,
               random = ~ 1,
               ng = 1, nwg = FALSE,
               idiag = TRUE, 
               data = data.frame(data_df), subject = "index") 


mLFR1ff <- lcmm(fixed = value~1+EPDS,
                random = ~ 1+EPDS,
                ng = 1,nwg=FALSE,
                idiag = FALSE, 
                data = data.frame(data_df), subject = "index") 
mLFR1ft <- lcmm(fixed = value~1+EPDS,
                random = ~ 1+EPDS,
                ng = 1,nwg=FALSE,
                idiag = TRUE, 
                data = data.frame(data_df), subject = "index") 

mQF1ff <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
             random = ~ 1,
             ng = 1,nwg=FALSE,
             idiag = FALSE, 
             data = data.frame(data_df), subject = "index") 
mQF1ft <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
             random = ~ 1,
             ng = 1,nwg=FALSE,
             idiag = TRUE, 
             data = data.frame(data_df), subject = "index") 
mQFLR1ff <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS,
                 ng = 1,nwg=FALSE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index") 
mQFLR1ft <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                 random = ~ 1+EPDS,
                 ng = 1,nwg=FALSE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index") 

mQFQR1ff <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
               random = ~ 1+EPDS+I(EPDS^2),
               ng = 1,nwg=FALSE,
               idiag = FALSE, 
               data = data.frame(data_df), subject = "index") 
mQFQR1ft <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
               random = ~ 1+EPDS+I(EPDS^2),
               ng = 1,nwg=FALSE,
               idiag = TRUE, 
               data = data.frame(data_df), subject = "index") 

summarytable(m0ff,m0ft,mLF1ff,mLF1ft,mLFR1ff,mLFR1ft,mQF1ff,mQF1ft,mQFLR1ff,mQFLR1ft,mQFQR1ff,mQFQR1ft, which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t1
kable(t1, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m1_lcmm.html")