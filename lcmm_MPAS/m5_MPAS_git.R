library( LCTMtools )
library(readxl)
library(finalfit)
library(dplyr)
library(mice)
library(ggplot2)
library(VIM)
library(writexl)


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
m05tf_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 5, nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = FALSE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m05tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 5, nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index"))


m05ff_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 5, nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = FALSE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m05ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 5, nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index"))



m05ft_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 5, nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = TRUE, data = data.frame(data_df), subject = "index"))

m05ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 5, nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index"))
m05tt_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 5, nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = TRUE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m05tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 5, nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index"))

mLF5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,nwg=TRUE,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,nwg=TRUE,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,nwg=FALSE,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,nwg=FALSE,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,nwg=FALSE,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,nwg=FALSE,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,nwg=TRUE,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,nwg=TRUE,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index")) 

mLFR5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 5,nwg=TRUE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 5,nwg=TRUE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 5,nwg=FALSE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 5,nwg=FALSE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFR5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 5,nwg=FALSE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 5,nwg=FALSE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFR5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 5,nwg=TRUE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 5,nwg=TRUE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 



mLFM5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 5,nwg=TRUE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 5,nwg=TRUE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFM5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 5,nwg=FALSE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 5,nwg=FALSE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFM5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 5,nwg=FALSE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 5,nwg=FALSE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFM5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 5,nwg=TRUE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 5,nwg=TRUE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 


mLFRM5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 


mQF5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                            random = ~ 1,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index") )
mQF5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                             random = ~ 1,
                                                                                                                             ng = 1,
                                                                                                                             idiag = FALSE, 
                                                                                                                             data = data.frame(data_df), subject = "index") )


mQF5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 5,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                            random = ~ 1,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index") )

mQF5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 5,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                             random = ~ 1,
                                                                                                                             ng = 1,
                                                                                                                             idiag = TRUE, 
                                                                                                                             data = data.frame(data_df), subject = "index") )


mQFLR5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index") )

mQFLR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = FALSE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )

mQFLR5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index") )

mQFLR5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = FALSE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )


mQFLR5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index") )
mQFLR5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = TRUE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )

mQFLR5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 5,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index") )
mQFLR5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 5,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = TRUE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )


mQFQR5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 5,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 


mQFQR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 5,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQR5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 5,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mQFQR5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 5,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQR5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 5,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQR5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 5,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQR5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 5,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQR5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 5,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 5,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 5,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQM5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 5,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 5,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 5,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 5,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 5,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 5,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQMLR5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 5,nwg=TRUE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = FALSE, 
                                                                                 data = data.frame(data_df), subject = "index")) 

mQFQMLR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 5,nwg=TRUE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 5,nwg=FALSE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = FALSE, 
                                                                                 data = data.frame(data_df), subject = "index")) 
mQFQMLR5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 5,nwg=FALSE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 




mQFQMLR5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 5,nwg=FALSE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = TRUE, 
                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 5,nwg=FALSE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 5,nwg=TRUE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = TRUE, 
                                                                                 data = data.frame(data_df), subject = "index")) 
mQFQMLR5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 5,nwg=TRUE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR5tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 5,nwg=TRUE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = FALSE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 5,nwg=TRUE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, data = data.frame(data_df), subject = "index"))


mQFQMQR5ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 5,nwg=FALSE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = FALSE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR5ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 5,nwg=FALSE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, data = data.frame(data_df), subject = "index"))

mQFQMQR5ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 5,nwg=FALSE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = TRUE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR5ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 5,nwg=FALSE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR5tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 5,nwg=TRUE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = TRUE, 
                                                                                  data = data.frame(data_df), subject = "index"))


mQFQMQR5tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 5,nwg=TRUE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

summarytable(m05tf_nr_MPAS,m05tf_nr_gridsearch_MPAS,m05ff_nr_MPAS,m05ff_nr_gridsearch_MPAS,m05ft_nr_MPAS,m05ft_nr_gridsearch_MPAS,m05tt_nr_MPAS,m05tt_nr_gridsearch_MPAS,
             mLF5tf_nr_MPAS,mLF5tf_nr_gridsearch_MPAS,mLF5ff_nr_MPAS,mLF5ff_nr_gridsearch_MPAS,mLF5ft_nr_MPAS,mLF5ft_nr_gridsearch_MPAS,mLF5tt_nr_MPAS,mLF5tt_nr_gridsearch_MPAS,
             mLFR5tf_nr_MPAS,mLFR5tf_nr_gridsearch_MPAS,mLFR5ff_nr_MPAS,mLFR5ff_nr_gridsearch_MPAS,mLFR5ft_nr_MPAS,mLFR5ft_nr_gridsearch_MPAS,mLFR5tt_nr_MPAS,mLFR5tt_nr_gridsearch_MPAS,
             mLFM5tf_nr_MPAS,mLFM5tf_nr_gridsearch_MPAS,mLFM5ff_nr_MPAS,mLFM5ff_nr_gridsearch_MPAS,mLFM5ft_nr_MPAS,mLFM5ft_nr_gridsearch_MPAS,mLFM5tt_nr_MPAS,mLFM5tt_nr_gridsearch_MPAS,
             mLFRM5tf_nr_MPAS,mLFRM5tf_nr_gridsearch_MPAS,mLFRM5ff_nr_MPAS,mLFRM5ff_nr_gridsearch_MPAS,mLFRM5ft_nr_MPAS,mLFRM5ft_nr_gridsearch_MPAS,mLFRM5tt_nr_MPAS,mLFRM5tt_nr_gridsearch_MPAS,
             mQF5tf_nr_MPAS,mQF5tf_nr_gridsearch_MPAS,mQF5ft_nr_MPAS,mQF5ft_nr_gridsearch_MPAS,
             mQFLR5tf_nr_MPAS,mQFLR5tf_nr_gridsearch_MPAS,mQFLR5ff_nr_MPAS,mQFLR5ff_nr_gridsearch_MPAS,mQFLR5ft_nr_MPAS,mQFLR5ft_nr_gridsearch_MPAS,mQFLR5tt_nr_MPAS,mQFLR5tt_nr_gridsearch_MPAS,
             mQFQR5tf_nr_MPAS,mQFQR5tf_nr_gridsearch_MPAS,mQFQR5ff_nr_MPAS,mQFQR5ff_nr_gridsearch_MPAS,mQFQR5ft_nr_MPAS,mQFQR5ft_nr_gridsearch_MPAS,mQFQR5tt_nr_MPAS,mQFQR5tt_nr_gridsearch_MPAS,
             mQFQM5tf_nr_MPAS,mQFQM5tf_nr_gridsearch_MPAS,mQFQM5ff_nr_MPAS,mQFQM5ff_nr_gridsearch_MPAS,mQFQM5ft_nr_MPAS,mQFQM5ft_nr_gridsearch_MPAS,mQFQM5tt_nr_MPAS,mQFQM5tt_nr_gridsearch_MPAS,
             mQFQMLR5tf_nr_MPAS,mQFQMLR5tf_nr_gridsearch_MPAS,mQFQMLR5ff_nr_MPAS,mQFQMLR5ff_nr_gridsearch_MPAS,mQFQMLR5ft_nr_MPAS,mQFQMLR5ft_nr_gridsearch_MPAS,mQFQMLR5tt_nr_MPAS,mQFQMLR5tt_nr_gridsearch_MPAS,
             mQFQMQR5tf_nr_MPAS,mQFQMQR5tf_nr_gridsearch_MPAS,mQFQMQR5ff_nr_MPAS,mQFQMQR5ff_nr_gridsearch_MPAS,mQFQMQR5ft_nr_MPAS,mQFQMQR5ft_nr_gridsearch_MPAS,mQFQMQR5tt_nr_MPAS,mQFQMQR5tt_nr_gridsearch_MPAS, 
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t5_MPAS
kable(t5_MPAS, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m5_MPAS.html")


mQFQR5tf_nr_MPAS


datnew   <- data.frame(MPAS = seq(6, 15, length = 2990))
plotpred <- predictY(mQFQR4tf_nr_MPAS , datnew, var.time ="MPAS", draws = TRUE)
par(oma=c(0.5,0.1,0.5,1)) 
par(mar=c(4,4,4,4.4))# all sides have 3 lines of space
plot(plotpred, lty=1, xlab="MPAS Week", ylab="MPAS Score",cex=0.75,xpd=TRUE,inset=c(-0.22,0))

mQFQM6tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 6,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 


mQFQR5tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 5,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

postprob(mQFQR4tf_nr_MPAS)

LRTp_45 = calc_lrt(598, mQFQM4tt_nr_MPAS$loglik,20, mQFQM4tt_nr_MPAS$ng, mQFQM5tt_nr_MPAS$loglik, 25, mQFQM5tt_nr_MPAS$ng)

