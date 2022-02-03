#calculate Lo-Mendell-Rubin likelihood ratio test
#calc_lrt(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes)
library( LCTMtools )
library(dplyr)
library(tidyLPA)
library(ggplot2)
library(writexl)

nr_subjects=598
LRTp_45 = calc_lrt(nr_subjects, mQFQMQR4tf_nr$loglik,25, mQFQMQR4tf_nr$ng, mQFQMQR5tf_nr$loglik, 30, mQFQMQR5tf_nr$ng)
LRTp_67 = calc_lrt(nr_subjects, mQFQM6ff_nr$loglik,21, mQFQM6ff_nr$ng, mQFQM7ff_nr$loglik, 29, mQFQM7ff_nr$ng)


datnew   <- data.frame(EPDS = seq(3, 15, length = 2990))
plotpred <- predictY(mQFQM6ff_nr , datnew, var.time ="EPDS", draws = TRUE)
par(oma=c(0.5,0.1,0.5,1)) 
par(mar=c(4,4,4,4.4))
plot(plotpred, lty=1, xlab="EPDS Week", ylab="EPDS Score",cex=0.75,xpd=TRUE,inset=c(-0.22,0))


data_df <- read_excel("long_melt_epds_17022021_AS.xlsx")
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

classes<-rep(mQFQM6ff_nr$pprob$class, each=5)
data_df$true_class<-as.factor(classes)
ggplot(data_df, aes(x = EPDS, y = value)) + geom_line(aes(color = true_class, group = index)) + xlab("Weeks") + ylab("EPDS") + labs(color = "Class Assignment")

#save predicted classes for the selected model
write_xlsx(data_df, 'lcmm_clusters_6.xlsx')




