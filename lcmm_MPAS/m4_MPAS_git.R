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

m04tf_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 4, nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = FALSE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m04tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 4, nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index"))


m04ff_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 4, nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = FALSE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m04ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 4, nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index"))



m04ft_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 4, nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = TRUE, data = data.frame(data_df), subject = "index"))

m04ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 4, nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index"))
m04tt_nr_MPAS <- lcmm(fixed = value~1,
                      mixture = ~ 1,
                      random = ~ 1,
                      ng = 4, nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                           random = ~ 1,
                                                                           ng = 1, 
                                                                           idiag = TRUE, 
                                                                           data = data.frame(data_df), subject = "index")) 
m04tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1,
                                            mixture = ~ 1,
                                            random = ~ 1,
                                            ng = 4, nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"),
                                       rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index"))

mLF4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,nwg=TRUE,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,nwg=TRUE,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,nwg=FALSE,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,nwg=FALSE,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,nwg=FALSE,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,nwg=FALSE,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index")) 
mLF4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,nwg=TRUE,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                            random = ~ 1,
                                                                            ng = 1, 
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mLF4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,nwg=TRUE,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"),
                                        rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                        random = ~ 1,
                                                                        ng = 1, 
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index")) 

mLFR4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 4,nwg=TRUE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 4,nwg=TRUE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 4,nwg=FALSE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 4,nwg=FALSE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFR4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 4,nwg=FALSE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 4,nwg=FALSE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFR4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1,
                        random = ~ 1+MPAS,
                        ng = 4,nwg=TRUE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+MPAS,
                                                                              random = ~ 1+MPAS,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mLFR4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1,
                                              random = ~ 1+MPAS,
                                              ng = 4,nwg=TRUE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1+MPAS,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 



mLFM4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 4,nwg=TRUE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 4,nwg=TRUE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mLFM4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 4,nwg=FALSE,
                        idiag = FALSE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 4,nwg=FALSE,
                                              idiag = FALSE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFM4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 4,nwg=FALSE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 4,nwg=FALSE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFM4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                        mixture = ~ 1+MPAS,
                        random = ~ 1,
                        ng = 4,nwg=TRUE,
                        idiag = TRUE, 
                        data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS,
                                                                             random = ~ 1,
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mLFM4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                              mixture = ~ 1+MPAS,
                                              random = ~ 1,
                                              ng = 4,nwg=TRUE,
                                              idiag = TRUE, 
                                              data = data.frame(data_df), subject = "index"),
                                         rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                         random = ~ 1,
                                                                         ng = 1, 
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 


mLFRM4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mLFRM4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS,
                         mixture = ~ 1+MPAS,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS,
                                                                               random = ~ 1+MPAS,
                                                                               ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index")) 
mLFRM4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS,
                                               mixture = ~ 1+MPAS,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS,
                                                                                                                               random = ~ 1+MPAS,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 


mQF4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,
                       idiag = FALSE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                            random = ~ 1,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index") )
mQF4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,
                                             idiag = FALSE, 
                                             data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                             random = ~ 1,
                                                                                                                             ng = 1,
                                                                                                                             idiag = FALSE, 
                                                                                                                             data = data.frame(data_df), subject = "index") )


mQF4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                       mixture = ~ 1,
                       random = ~ 1,
                       ng = 4,
                       idiag = TRUE, 
                       data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                            random = ~ 1,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index") )

mQF4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                             mixture = ~ 1,
                                             random = ~ 1,
                                             ng = 4,
                                             idiag = TRUE, 
                                             data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                             random = ~ 1,
                                                                                                                             ng = 1,
                                                                                                                             idiag = TRUE, 
                                                                                                                             data = data.frame(data_df), subject = "index") )


mQFLR4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index") )

mQFLR4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = FALSE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )

mQFLR4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = FALSE, 
                                                                               data = data.frame(data_df), subject = "index") )

mQFLR4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = FALSE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )


mQFLR4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index") )
mQFLR4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = TRUE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )

mQFLR4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS,
                         ng = 4,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                               random = ~ 1+MPAS,ng = 1,
                                                                               idiag = TRUE, 
                                                                               data = data.frame(data_df), subject = "index") )
mQFLR4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS,
                                               ng = 4,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                random = ~ 1+MPAS,ng = 1,
                                                                                                                                idiag = TRUE, 
                                                                                                                                data = data.frame(data_df), subject = "index") )


mQFQR4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 4,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 


mQFQR4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 4,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQR4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 4,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 
mQFQR4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 4,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQR4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 4,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQR4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 4,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQR4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1,
                         random = ~ 1+MPAS+I(MPAS^2),
                         ng = 4,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1+MPAS+I(MPAS^2),
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQR4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1,
                                               random = ~ 1+MPAS+I(MPAS^2),
                                               ng = 4,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 4,nwg=TRUE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 4,nwg=TRUE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQM4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 4,nwg=FALSE,
                         idiag = FALSE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = FALSE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 4,nwg=FALSE,
                                               idiag = FALSE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = FALSE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 4,nwg=FALSE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 4,nwg=FALSE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 

mQFQM4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                         mixture = ~ 1+MPAS+I(MPAS^2),
                         random = ~ 1,
                         ng = 4,nwg=TRUE,
                         idiag = TRUE, 
                         data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                              random = ~ 1,
                                                                              ng = 1,
                                                                              idiag = TRUE, 
                                                                              data = data.frame(data_df), subject = "index")) 

mQFQM4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                               mixture = ~ 1+MPAS+I(MPAS^2),
                                               random = ~ 1,
                                               ng = 4,nwg=TRUE,
                                               idiag = TRUE, 
                                               data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                               random = ~ 1,
                                                                                                                               ng = 1,
                                                                                                                               idiag = TRUE, 
                                                                                                                               data = data.frame(data_df), subject = "index")) 



mQFQMLR4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 4,nwg=TRUE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = FALSE, 
                                                                                 data = data.frame(data_df), subject = "index")) 

mQFQMLR4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 4,nwg=TRUE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 4,nwg=FALSE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = FALSE, 
                                                                                 data = data.frame(data_df), subject = "index")) 
mQFQMLR4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 4,nwg=FALSE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 




mQFQMLR4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 4,nwg=FALSE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = TRUE, 
                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 4,nwg=FALSE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, 
                                                                                                                                 data = data.frame(data_df), subject = "index")) 


mQFQMLR4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS,
                           ng = 4,nwg=TRUE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                 random = ~ 1+MPAS,
                                                                                 ng = 1,
                                                                                 idiag = TRUE, 
                                                                                 data = data.frame(data_df), subject = "index")) 
mQFQMLR4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS,
                                                 ng = 4,nwg=TRUE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS,
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR4tf_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 4,nwg=TRUE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = FALSE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR4tf_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 4,nwg=TRUE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, data = data.frame(data_df), subject = "index"))


mQFQMQR4ff_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 4,nwg=FALSE,
                           idiag = FALSE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = FALSE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR4ff_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 4,nwg=FALSE,
                                                 idiag = FALSE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = FALSE, data = data.frame(data_df), subject = "index"))

mQFQMQR4ft_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 4,nwg=FALSE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = TRUE, 
                                                                                  data = data.frame(data_df), subject = "index"))

mQFQMQR4ft_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 4,nwg=FALSE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR4tt_nr_MPAS <- lcmm(fixed = value~1+MPAS+I(MPAS^2),
                           mixture = ~ 1+MPAS+I(MPAS^2),
                           random = ~ 1+MPAS+I(MPAS^2),
                           ng = 4,nwg=TRUE,
                           idiag = TRUE, 
                           data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                  random = ~ 1+MPAS+I(MPAS^2),
                                                                                  ng = 1,
                                                                                  idiag = TRUE, 
                                                                                  data = data.frame(data_df), subject = "index"))


mQFQMQR4tt_nr_gridsearch_MPAS <- gridsearch(lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                 mixture = ~ 1+MPAS+I(MPAS^2),
                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                 ng = 4,nwg=TRUE,
                                                 idiag = TRUE, 
                                                 data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+MPAS+I(MPAS^2),
                                                                                                                                 random = ~ 1+MPAS+I(MPAS^2),
                                                                                                                                 ng = 1,
                                                                                                                                 idiag = TRUE, data = data.frame(data_df), subject = "index"))

summarytable(m04tf_nr_MPAS,m04tf_nr_gridsearch_MPAS,m04ff_nr_MPAS,m04ff_nr_gridsearch_MPAS,m04ft_nr_MPAS,m04ft_nr_gridsearch_MPAS,m04tt_nr_MPAS,m04tt_nr_gridsearch_MPAS,
             mLF4tf_nr_MPAS,mLF4tf_nr_gridsearch_MPAS,mLF4ff_nr_MPAS,mLF4ff_nr_gridsearch_MPAS,mLF4ft_nr_MPAS,mLF4ft_nr_gridsearch_MPAS,mLF4tt_nr_MPAS,mLF4tt_nr_gridsearch_MPAS,
             mLFR4tf_nr_MPAS,mLFR4tf_nr_gridsearch_MPAS,mLFR4ff_nr_MPAS,mLFR4ff_nr_gridsearch_MPAS,mLFR4ft_nr_MPAS,mLFR4ft_nr_gridsearch_MPAS,mLFR4tt_nr_MPAS,mLFR4tt_nr_gridsearch_MPAS,
             mLFM4tf_nr_MPAS,mLFM4tf_nr_gridsearch_MPAS,mLFM4ff_nr_MPAS,mLFM4ff_nr_gridsearch_MPAS,mLFM4ft_nr_MPAS,mLFM4ft_nr_gridsearch_MPAS,mLFM4tt_nr_MPAS,mLFM4tt_nr_gridsearch_MPAS,
             mLFRM4tf_nr_MPAS,mLFRM4tf_nr_gridsearch_MPAS,mLFRM4ff_nr_MPAS,mLFRM4ff_nr_gridsearch_MPAS,mLFRM4ft_nr_MPAS,mLFRM4ft_nr_gridsearch_MPAS,mLFRM4tt_nr_MPAS,mLFRM4tt_nr_gridsearch_MPAS,
             mQF4tf_nr_MPAS,mQF4tf_nr_gridsearch_MPAS,mQF4ft_nr_MPAS,mQF4ft_nr_gridsearch_MPAS,
             mQFLR4tf_nr_MPAS,mQFLR4tf_nr_gridsearch_MPAS,mQFLR4ff_nr_MPAS,mQFLR4ff_nr_gridsearch_MPAS,mQFLR4ft_nr_MPAS,mQFLR4ft_nr_gridsearch_MPAS,mQFLR4tt_nr_MPAS,mQFLR4tt_nr_gridsearch_MPAS,
             mQFQR4tf_nr_MPAS,mQFQR4tf_nr_gridsearch_MPAS,mQFQR4ff_nr_MPAS,mQFQR4ff_nr_gridsearch_MPAS,mQFQR4ft_nr_MPAS,mQFQR4ft_nr_gridsearch_MPAS,mQFQR4tt_nr_MPAS,mQFQR4tt_nr_gridsearch_MPAS,
             mQFQM4tf_nr_MPAS,mQFQM4tf_nr_gridsearch_MPAS,mQFQM4ff_nr_MPAS,mQFQM4ff_nr_gridsearch_MPAS,mQFQM4ft_nr_MPAS,mQFQM4ft_nr_gridsearch_MPAS,mQFQM4tt_nr_MPAS,mQFQM4tt_nr_gridsearch_MPAS,
             mQFQMLR4tf_nr_MPAS,mQFQMLR4tf_nr_gridsearch_MPAS,mQFQMLR4ff_nr_MPAS,mQFQMLR4ff_nr_gridsearch_MPAS,mQFQMLR4ft_nr_MPAS,mQFQMLR4ft_nr_gridsearch_MPAS,mQFQMLR4tt_nr_MPAS,mQFQMLR4tt_nr_gridsearch_MPAS,
             mQFQMQR4tf_nr_MPAS,mQFQMQR4tf_nr_gridsearch_MPAS,mQFQMQR4ff_nr_MPAS,mQFQMQR4ff_nr_gridsearch_MPAS,mQFQMQR4ft_nr_MPAS,mQFQMQR4ft_nr_gridsearch_MPAS,mQFQMQR4tt_nr_MPAS,mQFQMQR4tt_nr_gridsearch_MPAS, 
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t4_MPAS
kable(t4_MPAS, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m4_MPAS.html")

classes<-rep(mQFQMQR4tf_nr_MPAS $pprob$class, each=4)
data_df$true_class<-as.factor(classes)

ggplot(data_df, aes(x = MPAS, y = value)) + geom_line(aes(color = true_class, group = index)) + xlab("Weeks") + ylab("MPAS") + labs(color = "Class Assignment")

write_xlsx(data_df, 'lcmm_mpas_clusters_4.xlsx')



#after prescribing the proper clusters to the full dataset. analogy to python script for lcmm (save_full_sample.py)
cl_6_lcmm <- read_excel("clusters_4_MPAS.xlsx")
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
  cat(., file = "full_features_4cl_MPAS.html")
