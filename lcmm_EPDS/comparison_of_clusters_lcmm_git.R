


library(finalfit)
library(dplyr)
library(mice)

library(VIM)
#load data to compare clusters based on their features 

cl_6_lcmm <- read_excel("clusters_6_LCMM.xlsx")
col_names_binary <- names(cl_6_lcmm[,c(3:7,13:16,23,25,26,30:36)])
cl_6_lcmm[,col_names_binary] <- lapply(cl_6_lcmm[,col_names_binary] , factor)
col_names_ordered <- names(cl_6_lcmm[,c(17,18,24,27:29)])
cl_6_lcmm[,col_names_ordered] <- lapply(cl_6_lcmm[,col_names_ordered] , ordered)

dependent<-'cluster'
explanatory<-names(cl_6_lcmm)[c(3:35)]
cl_6_lcmm %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_6cl_EPDS.html")

cl_1_5<-cl_6_lcmm[((cl_6_lcmm$cluster==1)| (cl_6_lcmm$cluster==5)),]
col_names_binary <- names(cl_1_5[,c(36)])
cl_1_5[,col_names_binary] <- lapply(cl_1_5[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_1_5)[c(3:35)]
cl_1_5 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_1vs5cl_EPDS.html")


cl_2_5<-cl_6_lcmm[((cl_6_lcmm$cluster==2)| (cl_6_lcmm$cluster==5)),]
col_names_binary <- names(cl_2_5[,c(36)])
cl_2_5[,col_names_binary] <- lapply(cl_2_5[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_2_5)[c(3:35)]
cl_2_5 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_2vs5cl_EPDS.html")


cl_3_5<-cl_6_lcmm[((cl_6_lcmm$cluster==3)| (cl_6_lcmm$cluster==5)),]
col_names_binary <- names(cl_3_5[,c(36)])
cl_3_5[,col_names_binary] <- lapply(cl_3_5[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_3_5)[c(3:35)]
cl_3_5 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_3vs5cl_EPDS.html")

cl_4_5<-cl_6_lcmm[((cl_6_lcmm$cluster==4)| (cl_6_lcmm$cluster==5)),]
col_names_binary <- names(cl_4_5[,c(36)])
cl_4_5[,col_names_binary] <- lapply(cl_4_5[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_4_5)[c(3:35)]
cl_4_5 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_4vs5cl_EPDS.html")

cl_6_5<-cl_6_lcmm[((cl_6_lcmm$cluster==6)| (cl_6_lcmm$cluster==5)),]
col_names_binary <- names(cl_6_5[,c(36)])
cl_6_5[,col_names_binary] <- lapply(cl_6_5[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_6_5)[c(3:35)]
cl_6_5 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_6vs5cl_EPDS.html")



cl_2_4<-cl_6_lcmm[((cl_6_lcmm$cluster==2)| (cl_6_lcmm$cluster==4)),]
col_names_binary <- names(cl_2_4[,c(36)])
cl_2_4[,col_names_binary] <- lapply(cl_2_4[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_2_4)[c(8:12,14:35)]
cl_2_4 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_2vs4cl_EPDS.html")



cl_1_3<-cl_6_lcmm[((cl_6_lcmm$cluster==1)| (cl_6_lcmm$cluster==3)),]
col_names_binary <- names(cl_1_3[,c(36)])
cl_1_3[,col_names_binary] <- lapply(cl_1_3[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_1_3)[c(8:12,14:35)]
cl_1_3 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_1vs3cl_EPDS.html")



cl_1_6<-cl_6_lcmm[((cl_6_lcmm$cluster==1)| (cl_6_lcmm$cluster==6)),]
col_names_binary <- names(cl_1_6[,c(36)])
cl_1_6[,col_names_binary] <- lapply(cl_1_6[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_1_6)[c(8:12,14,16:35)]
cl_1_6 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_1vs6cl_EPDS.html")

cl_3_6<-cl_6_lcmm[((cl_6_lcmm$cluster==3)| (cl_6_lcmm$cluster==6)),]
col_names_binary <- names(cl_3_6[,c(36)])
cl_3_6[,col_names_binary] <- lapply(cl_3_6[,col_names_binary] , factor)

dependent<-'cluster'
explanatory<-names(cl_3_6)[c(8:12,14,16:35)]
cl_3_6 %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features_3vs6cl_EPDS.html")
