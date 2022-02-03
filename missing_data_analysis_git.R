#missing data handling



# Make sure finalfit is up-to-date 
library(finalfit) 
library(readxl)
library(stringr)
library(dplyr)
library(mice)
library(VIM)
library(Rcpp)
library(writexl)

data_df <- read_excel("full_sample_w_EPDS_with_nans.xlsx")
new_names<-str_replace_all(names(data_df), fixed(" "), "_")
names(data_df) <-new_names
dependent = "target"

explanatory = names(data_df)[c(2:6,8:43)]

col_names_binary <- names(data_df[,c(3:6,8:10,17,20:22,24,30:38)])
data_df[,col_names_binary] <- lapply(data_df[,col_names_binary] , factor)
col_names_ordered <- names(data_df[,c(11,12,18,28)])
data_df[,col_names_ordered] <- lapply(data_df[,col_names_ordered] , ordered)


# Multivariate Imputation by Chained Equations (mice)

mice_plot <- aggr(data_df[,c(2:6,8:38)], col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(data_df[,c(2:6,8:38)]), cex.axis=.7,only.miss = TRUE,
                    gap=3, ylab=c("Missing data in %","Pattern of missing values"))
init <- mice(data_df[,c(2:6,8:38)], maxit=0)
table(init$nmis)

meth = init$method
predM = init$predictorMatrix
str(data_df[explanatory])
#binary
meth[explanatory[c(2:7,15,19,20,22,28:36)]]="logreg"
#ordered
meth[explanatory[c(9,10,16,26)]]="polr"
#numerical
meth[explanatory[c(11:14,17,21,23:25,27)]]="pmm"
meth[explanatory[c(1)]]="mean"
#factor
meth[explanatory[c(8,18)]]="polyreg"

set.seed(103)
imputed = mice(data_df[explanatory], method=meth, predictorMatrix=predM, m=5)
head(imputed$loggedEvents, 10)

imputed_Data <- complete(imputed)
summary(imputed_Data)
sapply(imputed_Data, function(x) sum(is.na(x)))
sapply(data_df[explanatory], function(x) sum(is.na(x)))

write_xlsx(imputed_Data, 'imputed_data_170221.xlsx')

dependent = "target"
data_df <- read_excel("full_sample_17022021_AS.xlsx")
col_names_binary <- names(data_df[,c(3:9,16,19,20,21,23,29:38)])
data_df[,col_names_binary] <- lapply(data_df[,col_names_binary] , factor)
col_names_ordered <- names(data_df[,c(10,11,17,27)])
data_df[,col_names_ordered] <- lapply(data_df[,col_names_ordered] , ordered)

explanatory<-names(data_df)[c(2:11,16:37)]
data_df %>%
  summary_factorlist(dependent, explanatory, p=TRUE)->t

kable(t, "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "full_features.html")


