
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
m06tf_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 6, nwg=TRUE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = FALSE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m06tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 6, nwg=TRUE,
                                       idiag = FALSE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=60, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = FALSE, 
                                                                  data = data.frame(data_df), subject = "index"))


m06ff_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 6, nwg=FALSE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = FALSE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m06ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 6, nwg=FALSE,
                                       idiag = FALSE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=60, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = FALSE, 
                                                                  data = data.frame(data_df), subject = "index"))



m06ft_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 6, nwg=FALSE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = TRUE, data = data.frame(data_df), subject = "index"))

m06ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 6, nwg=FALSE,
                                       idiag = TRUE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=60, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = TRUE, 
                                                                  data = data.frame(data_df), subject = "index"))
m06tt_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 6, nwg=TRUE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = TRUE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m06tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 6, nwg=TRUE,
                                       idiag = TRUE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=60, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = TRUE, 
                                                                  data = data.frame(data_df), subject = "index"))

mLF6tf_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,nwg=TRUE,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,nwg=TRUE,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = FALSE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF6ff_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,nwg=FALSE,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,nwg=FALSE,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = FALSE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF6ft_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,nwg=FALSE,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,nwg=FALSE,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = TRUE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF6tt_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,nwg=TRUE,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,nwg=TRUE,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = TRUE, 
                                                                   data = data.frame(data_df), subject = "index")) 

mLFR6tf_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 6,nwg=TRUE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 6,nwg=TRUE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFR6ff_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 6,nwg=FALSE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 6,nwg=FALSE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFR6ft_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 6,nwg=FALSE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 6,nwg=FALSE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFR6tt_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 6,nwg=TRUE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 6,nwg=TRUE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 



mLFM6tf_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 6,nwg=TRUE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 6,nwg=TRUE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFM6ff_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 6,nwg=FALSE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 6,nwg=FALSE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFM6ft_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 6,nwg=FALSE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 6,nwg=FALSE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFM6tt_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 6,nwg=TRUE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 6,nwg=TRUE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 


mLFRM6tf_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM6ff_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM6ft_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM6tt_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQF6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                       random = ~ 1,
                                                                       ng = 1,
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index") )
mQF6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                        random = ~ 1,
                                                                                                                        ng = 1,
                                                                                                                        idiag = FALSE, 
                                                                                                                        data = data.frame(data_df), subject = "index") )



mQF6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 6,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                       random = ~ 1,
                                                                       ng = 1,
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index") )

mQF6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 6,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                        random = ~ 1,
                                                                                                                        ng = 1,
                                                                                                                        idiag = TRUE, 
                                                                                                                        data = data.frame(data_df), subject = "index") )


mQFLR6tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index") )

mQFLR6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = FALSE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )

mQFLR6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index") )

mQFLR6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = FALSE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )


mQFLR6ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index") )
mQFLR6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = TRUE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )

mQFLR6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 6,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index") )
mQFLR6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 6,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = TRUE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )


mQFQR6tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 6,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 


mQFQR6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 6,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQR6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 6,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mQFQR6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 6,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQR6ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 6,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQR6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 6,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQR6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 6,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQR6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 6,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM6tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 6,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 6,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQM6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 6,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 6,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM6ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 6,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 6,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 6,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 6,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQMLR6tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 6,nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 

mQFQMLR6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 6,nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 6,nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mQFQMLR6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 6,nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 




mQFQMLR6ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 6,nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 6,nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 6,nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mQFQMLR6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 6,nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR6tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 6,nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR6tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 6,nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, data = data.frame(data_df), subject = "index"))


mQFQMQR6ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 6,nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR6ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 6,nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, data = data.frame(data_df), subject = "index"))

mQFQMQR6ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 6,nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR6ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 6,nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR6tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 6,nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))


mQFQMQR6tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 6,nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=60, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))
mQFQM7ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 7,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 


summarytable(m06tf_nr,m06tf_nr_gridsearch,m06ff_nr,m06ff_nr_gridsearch,m06ft_nr,m06ft_nr_gridsearch,m06tt_nr,m06tt_nr_gridsearch,
             mLF6tf_nr,mLF6tf_nr_gridsearch,mLF6ff_nr,mLF6ff_nr_gridsearch,mLF6ft_nr,mLF6ft_nr_gridsearch,mLF6tt_nr,mLF6tt_nr_gridsearch,
             mLFR6tf_nr,mLFR6tf_nr_gridsearch,mLFR6ff_nr,mLFR6ff_nr_gridsearch,mLFR6ft_nr,mLFR6ft_nr_gridsearch,mLFR6tt_nr,mLFR6tt_nr_gridsearch,
             mLFM6tf_nr,mLFM6tf_nr_gridsearch,mLFM6ff_nr,mLFM6ff_nr_gridsearch,mLFM6ft_nr,mLFM6ft_nr_gridsearch,mLFM6tt_nr,mLFM6tt_nr_gridsearch,
             mLFRM6tf_nr,mLFRM6tf_nr_gridsearch,mLFRM6ff_nr,mLFRM6ff_nr_gridsearch,mLFRM6ft_nr,mLFRM6ft_nr_gridsearch,mLFRM6tt_nr,mLFRM6tt_nr_gridsearch,
             mQF6ff_nr,mQF6ff_nr_gridsearch,mQF6tt_nr,mQF6tt_nr_gridsearch,
             mQFLR6tf_nr,mQFLR6tf_nr_gridsearch,mQFLR6ff_nr,mQFLR6ff_nr_gridsearch,mQFLR6ft_nr,mQFLR6ft_nr_gridsearch,mQFLR6tt_nr,mQFLR6tt_nr_gridsearch,
             mQFQR6tf_nr,mQFQR6tf_nr_gridsearch,mQFQR6ff_nr,mQFQR6ff_nr_gridsearch,mQFQR6ft_nr,mQFQR6ft_nr_gridsearch,mQFQR6tt_nr,mQFQR6tt_nr_gridsearch,
             mQFQM6tf_nr,mQFQM6tf_nr_gridsearch,mQFQM6ff_nr,mQFQM6ff_nr_gridsearch,mQFQM6ft_nr,mQFQM6ft_nr_gridsearch,mQFQM6tt_nr,mQFQM6tt_nr_gridsearch,
             mQFQMLR6tf_nr,mQFQMLR6tf_nr_gridsearch,mQFQMLR6ff_nr,mQFQMLR6ff_nr_gridsearch,mQFQMLR6ft_nr,mQFQMLR6ft_nr_gridsearch,mQFQMLR6tt_nr,mQFQMLR6tt_nr_gridsearch,
             mQFQMQR6tf_nr,mQFQMQR6tf_nr_gridsearch,mQFQMQR6ff_nr,mQFQMQR6ff_nr_gridsearch,mQFQMQR6ft_nr,mQFQMQR6ft_nr_gridsearch,mQFQMQR6tt_nr,mQFQMQR6tt_nr_gridsearch, 
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t6
kable(t6, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m6_lcmm.html")

