## Load packages
#install.packages('devtools')
library(devtools)
#install.packages(c('bindrcpp','glue','pkgconfig','tibble','plyr','dplyr'))

#install_github('MoBioDiv/Mobr')
library(mobr)

## Read in data
call_2018 <- read.csv("~/Callcensus2018.csv")
call_hx <- read.csv("~/Historic_call_census.csv")
comm_2018 <- read.csv("~/FMNFherps/ponds-speciesMINNOWANDCALL.csv", row.names = 1)
comm_hx <- read.csv("~/Historic_call_minnow_total.csv")
env  <- read.csv("C:/Users/mcglinnlab/Dropbox/Master env file.csv")

#
comm = tapply(raw_data, list("sp", "site", "date"), length)

##Create mob_in matrix
comm_2018 = as.matrix(comm_2018)
comm_2018 = ifelse(is.na(comm_2018), 0, comm_2018)
comm_hx = as.matrix(comm_hx)
comm_hx = ifelse(is.na(comm_hx), 0, comm_hx)
herp_mob_in_2018 <- make_mob_in(comm_2018, env, coord_names = c("X", "Y"),
                                latlong=TRUE)

## Univariate analyses
herp_stats_2018 <- get_mob_stats(herp_mob_in_2018,
                                 group_var = "ff_last_8yr")


