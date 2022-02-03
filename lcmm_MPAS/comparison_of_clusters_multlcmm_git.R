library(finalfit)
library(dplyr)
library(mice)
library(ggplot2)
library(writexl)
library(VIM)
library(compareGroups)

LRTp_45 = calc_lrt(598, mlcmm_QFQMLR4_tf_gridsearch$loglik,17, mlcmm_QFQMLR4_tf_gridsearch$ng, mlcmm_QFQM5_tf_gridsearch$loglik, 19, mlcmm_QFQM5_tf_gridsearch$ng)

datnew   <- data.frame(MPAS = seq(6, 15, length = 2990))
plotpred <- predictY(mlcmm_QFQMLR4_tf_gridsearch  , datnew, var.time ="MPAS", draws = TRUE,ndraws = 100)
par(oma=c(0.5,0.1,0.5,1)) 
par(mar=c(4,4,4,4.4))# all sides have 3 lines of space
plot(plotpred, lty=1, xlab="MPAS Week", ylab="MPAS Score",cex=0.75,xpd=TRUE,inset=c(-0.22,0))
plot(plotpred, lty=1, xlab="EPDS Week", ylab="EPDS Score",cex=0.75,xpd=TRUE, outcome = 2, inset=c(-0.22,0)) 

classes<-rep(mlcmm_QFQMLR4_tf_gridsearch $pprob$class, each=4)
data_df$true_class<-as.factor(classes)


write_xlsx(data_df, 'multlcmm_clusters_4.xlsx')

#clusters_4_multLCMM is from python script, full cohort with all features and clusters. Run in python:
#load clusters from lcmm classification 
#df_full = pd.read_excel(r'multlcmm_clusters_4.xlsx')
#df=pd.DataFrame(df_full)
#df=df.pivot(index='index_MPAS', columns='MPAS', values='true_class')
#df=df[6]
#df=pd.DataFrame(df).reset_index().drop(columns='index_MPAS')
#df_full['cluster']=df[6]
#
#writer = pd.ExcelWriter('clusters_4_multLCMM.xlsx', engine='xlsxwriter')
#
# Convert the dataframe to an XlsxWriter Excel object.
#df_full.to_excel(writer, sheet_name='Sheet1')
#
# Close the Pandas Excel writer and output the Excel file.
#writer.save()
cl_4_multlcmm <- read_excel("clusters_4_multLCMM.xlsx")
col_names_binary <- names(cl_4_multlcmm[,c(3:7,13:16,23,25,26,30:36)])
cl_4_multlcmm[,col_names_binary] <- lapply(cl_4_multlcmm[,col_names_binary] , factor)
col_names_ordered <- names(cl_4_multlcmm[,c(17,18,24,27:29)])
cl_4_multlcmm[,col_names_ordered] <- lapply(cl_4_multlcmm[,col_names_ordered] , ordered)




dependent<-'cluster'
explanatory<-names(cl_4_multlcmm)[c(3:35)]
cl_4_multlcmm %>%
  c(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_4cl_MPAS_EPDS.html")

cl_1_2<-cl_4_multlcmm[((cl_4_multlcmm$cluster==1)| (cl_4_multlcmm$cluster==2)),]
col_names_binary <- names(cl_1_2[,c(36)])
cl_1_2[,col_names_binary] <- lapply(cl_1_2[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_1_2)[c(3:35)]
cl_1_2 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_1vs2cl_MPAS_EPDS.html")


cl_2_3<-cl_4_multlcmm[((cl_4_multlcmm$cluster==2)| (cl_4_multlcmm$cluster==3)),]
col_names_binary <- names(cl_2_3[,c(36)])
cl_2_3[,col_names_binary] <- lapply(cl_2_3[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_2_3)[c(3:35)]
cl_2_3 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_2vs3cl_MPAS_EPDS.html")

cl_2_4<-cl_4_multlcmm[((cl_4_multlcmm$cluster==2)| (cl_4_multlcmm$cluster==4)),]
col_names_binary <- names(cl_2_4[,c(36)])
cl_2_4[,col_names_binary] <- lapply(cl_2_4[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_2_4)[c(3:35)]
cl_2_4 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_2vs4cl_MPAS_EPDS.html")

cl_3_4<-cl_4_multlcmm[((cl_4_multlcmm$cluster==3)| (cl_4_multlcmm$cluster==4)),]
col_names_binary <- names(cl_3_4[,c(36)])
cl_3_4[,col_names_binary] <- lapply(cl_3_4[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_3_4)[c(3:35)]
cl_3_4 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_3vs4cl_MPAS_EPDS.html")
