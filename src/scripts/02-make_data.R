#################################################################################
## Computing cost of mastitis in Canadian dairies: making data (simulations)
## Denis Haine
#################################################################################

##----load libraries-------------------------------------------------------
library(tidyverse)
library(pbapply)
library(purrr)

##----load data------------------------------------------------------------
avg_canada <- readRDS("avg_canada.rds")

## ----create data---------------------------------------------------------
N <- length(avg_canada$prod_day)
set.seed(123)
dat <- pbreplicate(1000,
                 matrix(c(avg_canada$prod_305,  # prod_305
                   rgamma(n = N, shape = 0.1, rate = 0.0004) *
                     avg_canada$n_cows/100,  # scmsmp
                 rexp(n = N, rate = 0.04),  # inc_risk
                 rbeta(n = N, shape1 = 0.44, shape2 = 0.18),  # cmtrt_prop
                 rgamma(n = N, shape = 3.27, rate = 0.13),  # drug_totprice
                 rbeta(n = N, shape1 = 0.2, shape2 = 3.47),  # cmvet_prop
                 rlnorm(n = N, meanlog = 4.6, sdlog = 0.41),  # cmvet_cost
                 rgamma(n = N, shape = 1.15, rate = 0.8),  # labour_CM
                 rgamma(n = N, shape = 0.24, rate = 0.001) *
                 avg_canada$n_cows/100,  # cmsmp
                 avg_canada$n_cows,  # n_cows
                 avg_canada$milkLoss_SCM,  # milKLoss_SCM
                 rgamma(n = N, shape = 0.06, rate = 0.0002) *
                   avg_canada$n_cows/100,  # vetcost_scc
                 avg_canada$province,
                 avg_canada$btscc,
                 rgamma(n = N, shape = 0.12, rate = 0.0009) *
                 avg_canada$n_cows/100,  # ins_cost
                 avg_canada$L1_prop,  # L1_prop
                 rbeta(n = N, shape1 = 0.22, shape2 = 15.6),  # L1_cullprop
                 rbeta(n = N, shape1 = 0.05, shape2 = 20),  # L1_deadprop
                 rbeta(n = N, shape1 = 0.83, shape2 = 17.5),  # L2_cullprop
                 rbeta(n = N, shape1 = 0.26, shape2 = 46.4),  # L2_deadprop
                 rnorm(n = N, mean = 2450, sd = 450),  # rearing_cost
                 rlogis(n = N, location = 1109, scale = 165),  # meatvalue
                 rgamma(n = N, shape = 0.25, rate = 0.01),  # salvage
                 rbeta(n = N, shape1 = 0.21, shape2 = 15),  # L1_cullprop2
                 rbeta(n = N, shape1 = 0.93, shape2 = 19),  # L2_cullprop2
                 rlnorm(n = N, meanlog = 6.8, sdlog = 1.3),  # meatvalue_scc
                 rexp(n = N, rate = 0.0008) * avg_canada$n_cows/100,  # premtd_cost
                 rgamma(n = N, shape = 1.79, rate = 0.0009) *
                 avg_canada$n_cows/100,  # postmtd_cost
                 rgamma(n = N, shape = 1.12, rate = 0.0006) *
                 avg_canada$n_cows/100,  # dct_cost
                 rexp(n = N, rate = 0.004) * avg_canada$n_cows/100,  # gloves_cost
                 rgamma(n = N, shape = 0.26, rate = 0.0006) *
                 avg_canada$n_cows/100,  # vacc_cost
                 rgamma(n = N, shape = 5.9, rate = 1.82),  # trt_time
                 rnorm(n = N, mean = 3.2, sd = 0.6),  # drug_wd
                 avg_canada$prod_day,  # prod_day
                 rbeta(n = N, shape1 = 0.17, shape2 = 2.21),  # excl_nbr
                 rgamma(n = N, shape = 0.6, rate = 0.23),  # milkdisc_notrt
                 rbeta(n = N, shape1 = 0.17, shape2 = 0.26),  # milkfed_prop
                 rnbinom(n = N, size = 0.19, mu = 30.26),  # daysout_scm
                 avg_canada$milkLoss_SCM5,  # 10% reduction
                 avg_canada$milkLoss_SCM10,  # 20% reduction
                 avg_canada$btscc5,  # 10% reduction
                 avg_canada$btscc10  # 20% reduction                 
                 ),
                 dimnames = list(c(rep(NA, N)),
                                 c("prod_305", "scmsmp", "inc_risk",
                                   "cmtrt_prop", "drug_totprice", "cmvet_prop",
                                   "cmvet_cost", "labour_CM", "cmsmp",
                                   "n_cows", "milkLoss_SCM", "vetcost_scc",
                                   "province", "btscc", "ins_cost",
                                   "L1_prop", "L1_cullprop", "L1_deadprop",
                                   "L2_cullprop", "L2_deadprop", "rearing_cost",
                                   "meatvalue", "salvage", "L1_cullprop2",
                                   "L2_cullprop2", "meatvalue_scc", "premtd_cost",
                                   "postmtd_cost", "dct_cost", "gloves_cost",
                                   "vacc_cost", "trt_time", "drug_wd",
                                   "prod_day", "excl_nbr", "milkdisc_notrt",
                                   "milkfed_prop", "daysout_scm", "milkLoss_SCM5",
                                   "milkLoss_SCM10", "btscc5", "btscc10"
                                   )),
                 ncol = 42),
                 simplify = FALSE)

## datasets without non-DHI herds
saveRDS(dat, "dat.rds")

### Adding non-DHi herds
## Province N_herd_DHI N_farms_2015 To_add
## NB        140         206           +66
## NL          6          32           +26
## NS        143         225           +82
## PE        110         174           +64
## QC       4914        5766          +852
## ON       2613        3834         +1221
## MB        197         299          +102
## SK        108         163           +55
## AB        384         547          +163
## BC        287         437          +150
## Total    8902       11683

source("../lib/stratified.R")

## convert matrices to data frames
dat_df <- dat %>% map(., function (x) data.frame(x, row.names = NULL))
## extract random herds by province
rdm_herd <- dat_df %>% map(.,
                           function (x) stratified(x,
                                                   "province",
                                                   size = c("1" = 66,
                                                            "2" = 26,
                                                            "3" = 82,
                                                            "4" = 64,
                                                            "5" = 852,
                                                            "6" = 1221,
                                                            "7" = 102,
                                                            "8" = 55,
                                                            "9" = 163,
                                                            "10" = 150),
                                                   replace = TRUE))
## put original herds and sampled ones into a list
dat_list <- list(dat_df, rdm_herd)
## append them together
dat_dhi <- do.call(Map, c(f = rbind, dat_list))

## datasets with non-DHI herds
saveRDS(dat_dhi, "dat_dhi.rds")
