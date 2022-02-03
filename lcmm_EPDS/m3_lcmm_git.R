
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

m03tf_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 3, nwg=TRUE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = FALSE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m03tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 3, nwg=TRUE,
                                       idiag = FALSE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = FALSE, 
                                                                  data = data.frame(data_df), subject = "index"))


m03ff_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 3, nwg=FALSE,
                 idiag = FALSE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = FALSE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m03ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 3, nwg=FALSE,
                                       idiag = FALSE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = FALSE, 
                                                                  data = data.frame(data_df), subject = "index"))



m03ft_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 3, nwg=FALSE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = TRUE, data = data.frame(data_df), subject = "index"))

m03ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 3, nwg=FALSE,
                                       idiag = TRUE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = TRUE, 
                                                                  data = data.frame(data_df), subject = "index"))
m03tt_nr <- lcmm(fixed = value~1,
                 mixture = ~ 1,
                 random = ~ 1,
                 ng = 3, nwg=TRUE,
                 idiag = TRUE, 
                 data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1,
                                                                      random = ~ 1,
                                                                      ng = 1, 
                                                                      idiag = TRUE, 
                                                                      data = data.frame(data_df), subject = "index")) 
m03tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1,
                                       mixture = ~ 1,
                                       random = ~ 1,
                                       ng = 3, nwg=TRUE,
                                       idiag = TRUE, 
                                       data = data.frame(data_df), subject = "index"),
                                  rep=100, maxiter=50, minit=lcmm(fixed = value~1,
                                                                  random = ~ 1,
                                                                  ng = 1, 
                                                                  idiag = TRUE, 
                                                                  data = data.frame(data_df), subject = "index"))

mLF3tf_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,nwg=TRUE,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,nwg=TRUE,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = FALSE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF3ff_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,nwg=FALSE,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,nwg=FALSE,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = FALSE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF3ft_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,nwg=FALSE,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,nwg=FALSE,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = TRUE, 
                                                                   data = data.frame(data_df), subject = "index")) 
mLF3tt_nr <- lcmm(fixed = value~1+EPDS,
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,nwg=TRUE,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                       random = ~ 1,
                                                                       ng = 1, 
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index")) 
mLF3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,nwg=TRUE,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"),
                                   rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                   random = ~ 1,
                                                                   ng = 1, 
                                                                   idiag = TRUE, 
                                                                   data = data.frame(data_df), subject = "index")) 

mLFR3tf_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 3,nwg=TRUE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 3,nwg=TRUE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFR3ff_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 3,nwg=FALSE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 3,nwg=FALSE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFR3ft_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 3,nwg=FALSE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 3,nwg=FALSE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFR3tt_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1,
                   random = ~ 1+EPDS,
                   ng = 3,nwg=TRUE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B= lcmm(fixed = value~1+EPDS,
                                                                         random = ~ 1+EPDS,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mLFR3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1,
                                         random = ~ 1+EPDS,
                                         ng = 3,nwg=TRUE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1+EPDS,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 



mLFM3tf_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 3,nwg=TRUE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 3,nwg=TRUE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 

mLFM3ff_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 3,nwg=FALSE,
                   idiag = FALSE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = FALSE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 3,nwg=FALSE,
                                         idiag = FALSE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = FALSE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFM3ft_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 3,nwg=FALSE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 3,nwg=FALSE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 
mLFM3tt_nr <- lcmm(fixed = value~1+EPDS,
                   mixture = ~ 1+EPDS,
                   random = ~ 1,
                   ng = 3,nwg=TRUE,
                   idiag = TRUE, 
                   data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS,
                                                                        random = ~ 1,
                                                                        ng = 1,
                                                                        idiag = TRUE, 
                                                                        data = data.frame(data_df), subject = "index"))

mLFM3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                         mixture = ~ 1+EPDS,
                                         random = ~ 1,
                                         ng = 3,nwg=TRUE,
                                         idiag = TRUE, 
                                         data = data.frame(data_df), subject = "index"),
                                    rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                    random = ~ 1,
                                                                    ng = 1, 
                                                                    idiag = TRUE, 
                                                                    data = data.frame(data_df), subject = "index")) 


mLFRM3tf_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM3ff_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM3ft_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mLFRM3tt_nr <- lcmm(fixed = value~1+EPDS,
                    mixture = ~ 1+EPDS,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS,
                                                                          random = ~ 1+EPDS,
                                                                          ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index")) 
mLFRM3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS,
                                          mixture = ~ 1+EPDS,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS,
                                                                                                                          random = ~ 1+EPDS,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 


mQF3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,
                  idiag = FALSE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                       random = ~ 1,
                                                                       ng = 1,
                                                                       idiag = FALSE, 
                                                                       data = data.frame(data_df), subject = "index") )
mQF3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,
                                        idiag = FALSE, 
                                        data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                        random = ~ 1,
                                                                                                                        ng = 1,
                                                                                                                        idiag = FALSE, 
                                                                                                                        data = data.frame(data_df), subject = "index") )


mQF3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                  mixture = ~ 1,
                  random = ~ 1,
                  ng = 3,
                  idiag = TRUE, 
                  data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                       random = ~ 1,
                                                                       ng = 1,
                                                                       idiag = TRUE, 
                                                                       data = data.frame(data_df), subject = "index") )

mQF3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                        mixture = ~ 1,
                                        random = ~ 1,
                                        ng = 3,
                                        idiag = TRUE, 
                                        data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                        random = ~ 1,
                                                                                                                        ng = 1,
                                                                                                                        idiag = TRUE, 
                                                                                                                        data = data.frame(data_df), subject = "index") )


mQFLR3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index") )

mQFLR3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = FALSE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )

mQFLR3ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = FALSE, 
                                                                          data = data.frame(data_df), subject = "index") )

mQFLR3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = FALSE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )


mQFLR3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index") )
mQFLR3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = TRUE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )

mQFLR3tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS,
                    ng = 3,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                          random = ~ 1+EPDS,ng = 1,
                                                                          idiag = TRUE, 
                                                                          data = data.frame(data_df), subject = "index") )
mQFLR3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS,
                                          ng = 3,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                           random = ~ 1+EPDS,ng = 1,
                                                                                                                           idiag = TRUE, 
                                                                                                                           data = data.frame(data_df), subject = "index") )


mQFQR3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 3,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 


mQFQR3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 3,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQR3ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 3,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 
mQFQR3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 3,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQR3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 3,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQR3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 3,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQR3tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1,
                    random = ~ 1+EPDS+I(EPDS^2),
                    ng = 3,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1+EPDS+I(EPDS^2),
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQR3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1,
                                          random = ~ 1+EPDS+I(EPDS^2),
                                          ng = 3,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 3,nwg=TRUE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 3,nwg=TRUE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQM3ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 3,nwg=FALSE,
                    idiag = FALSE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = FALSE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 3,nwg=FALSE,
                                          idiag = FALSE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = FALSE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 3,nwg=FALSE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 3,nwg=FALSE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 

mQFQM3tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                    mixture = ~ 1+EPDS+I(EPDS^2),
                    random = ~ 1,
                    ng = 3,nwg=TRUE,
                    idiag = TRUE, 
                    data = data.frame(data_df), subject = "index",B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                         random = ~ 1,
                                                                         ng = 1,
                                                                         idiag = TRUE, 
                                                                         data = data.frame(data_df), subject = "index")) 

mQFQM3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                          mixture = ~ 1+EPDS+I(EPDS^2),
                                          random = ~ 1,
                                          ng = 3,nwg=TRUE,
                                          idiag = TRUE, 
                                          data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                          random = ~ 1,
                                                                                                                          ng = 1,
                                                                                                                          idiag = TRUE, 
                                                                                                                          data = data.frame(data_df), subject = "index")) 



mQFQMLR3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 3,nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 

mQFQMLR3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 3,nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR3ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 3,nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = FALSE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mQFQMLR3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 3,nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 




mQFQMLR3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 3,nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 3,nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, 
                                                                                                                            data = data.frame(data_df), subject = "index")) 


mQFQMLR3tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS,
                      ng = 3,nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                            random = ~ 1+EPDS,
                                                                            ng = 1,
                                                                            idiag = TRUE, 
                                                                            data = data.frame(data_df), subject = "index")) 
mQFQMLR3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS,
                                            ng = 3,nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS,
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR3tf_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 3,nwg=TRUE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR3tf_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 3,nwg=TRUE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, data = data.frame(data_df), subject = "index"))


mQFQMQR3ff_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 3,nwg=FALSE,
                      idiag = FALSE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = FALSE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR3ff_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 3,nwg=FALSE,
                                            idiag = FALSE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = FALSE, data = data.frame(data_df), subject = "index"))

mQFQMQR3ft_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 3,nwg=FALSE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))

mQFQMQR3ft_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 3,nwg=FALSE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))

mQFQMQR3tt_nr <- lcmm(fixed = value~1+EPDS+I(EPDS^2),
                      mixture = ~ 1+EPDS+I(EPDS^2),
                      random = ~ 1+EPDS+I(EPDS^2),
                      ng = 3,nwg=TRUE,
                      idiag = TRUE, 
                      data = data.frame(data_df), subject = "index", B= lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                             random = ~ 1+EPDS+I(EPDS^2),
                                                                             ng = 1,
                                                                             idiag = TRUE, 
                                                                             data = data.frame(data_df), subject = "index"))


mQFQMQR3tt_nr_gridsearch <- gridsearch(lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                            mixture = ~ 1+EPDS+I(EPDS^2),
                                            random = ~ 1+EPDS+I(EPDS^2),
                                            ng = 3,nwg=TRUE,
                                            idiag = TRUE, 
                                            data = data.frame(data_df), subject = "index"), rep=100, maxiter=50, minit=lcmm(fixed = value~1+EPDS+I(EPDS^2),
                                                                                                                            random = ~ 1+EPDS+I(EPDS^2),
                                                                                                                            ng = 1,
                                                                                                                            idiag = TRUE, data = data.frame(data_df), subject = "index"))

summarytable(m03tf_nr,m03tf_nr_gridsearch,m03ff_nr,m03ff_nr_gridsearch,m03ft_nr,m03ft_nr_gridsearch,m03tt_nr,m03tt_nr_gridsearch,
             mLF3tf_nr,mLF3tf_nr_gridsearch,mLF3ff_nr,mLF3ff_nr_gridsearch,mLF3ft_nr,mLF3ft_nr_gridsearch,mLF3tt_nr,mLF3tt_nr_gridsearch,
             mLFR3tf_nr,mLFR3tf_nr_gridsearch,mLFR3ff_nr,mLFR3ff_nr_gridsearch,mLFR3ft_nr,mLFR3ft_nr_gridsearch,mLFR3tt_nr,mLFR3tt_nr_gridsearch,
             mLFM3tf_nr,mLFM3tf_nr_gridsearch,mLFM3ff_nr,mLFM3ff_nr_gridsearch,mLFM3ft_nr,mLFM3ft_nr_gridsearch,mLFM3tt_nr,mLFM3tt_nr_gridsearch,
             mLFRM3tf_nr,mLFRM3tf_nr_gridsearch,mLFRM3ff_nr,mLFRM3ff_nr_gridsearch,mLFRM3ft_nr,mLFRM3ft_nr_gridsearch,mLFRM3tt_nr,mLFRM3tt_nr_gridsearch,
             mQF3tf_nr,mQF3tf_nr_gridsearch,mQF3ft_nr,mQF3ft_nr_gridsearch,
             mQFLR3tf_nr,mQFLR3tf_nr_gridsearch,mQFLR3ff_nr,mQFLR3ff_nr_gridsearch,mQFLR3ft_nr,mQFLR3ft_nr_gridsearch,mQFLR3tt_nr,mQFLR3tt_nr_gridsearch,
             mQFQR3tf_nr,mQFQR3tf_nr_gridsearch,mQFQR3ff_nr,mQFQR3ff_nr_gridsearch,mQFQR3ft_nr,mQFQR3ft_nr_gridsearch,mQFQR3tt_nr,mQFQR3tt_nr_gridsearch,
             mQFQM3tf_nr,mQFQM3tf_nr_gridsearch,mQFQM3ff_nr,mQFQM3ff_nr_gridsearch,mQFQM3ft_nr,mQFQM3ft_nr_gridsearch,mQFQM3tt_nr,mQFQM3tt_nr_gridsearch,
             mQFQMLR3tf_nr,mQFQMLR3tf_nr_gridsearch,mQFQMLR3ff_nr,mQFQMLR3ff_nr_gridsearch,mQFQMLR3ft_nr,mQFQMLR3ft_nr_gridsearch,mQFQMLR3tt_nr,mQFQMLR3tt_nr_gridsearch,
             mQFQMQR3tf_nr,mQFQMQR3tf_nr_gridsearch,mQFQMQR3ff_nr,mQFQMQR3ff_nr_gridsearch,mQFQMQR3ft_nr,mQFQMQR3ft_nr_gridsearch,mQFQMQR3tt_nr,mQFQMQR3tt_nr_gridsearch, 
             which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy", "%class"))->t3
kable(t3, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "m3_lcmm.html")