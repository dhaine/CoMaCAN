###############################################################################
## CoMaCAN: fitting distributions
##############################################################################


library(fitdistrplus)
## Incidence of clinical mastitis (per 100 cow-year)
qqnorm(ecomod$inc_risk)
summary(ecomod$inc_risk)
descdist(ecomod$inc_risk, discrete = FALSE, boot = 1000)
plotdist(ecomod$inc_risk, histo = TRUE, demp = TRUE)
fitdist(ecomod$inc_risk, "exp")
inc_risk.D <- rexp(n = 12000, rate = 0.04)
hist(inc_risk.D, prob = TRUE, breaks = 100)
lines(density(inc_risk.D))

## Proportion of CM receiving treatment
qqnorm(ecomod$cmtrt_prop)
summary(ecomod$cmtrt_prop)
descdist(ecomod$cmtrt_prop, discrete = FALSE, boot = 1000)
plotdist(ecomod$cmtrt_prop, histo = TRUE, demp = TRUE)
fitdist(ecomod$cmtrt_prop, "beta", method = "mme")
cmtrt_prop.D <- rbeta(n = 12000, shape1 = 0.44, shape2 = 0.18)
hist(cmtrt_prop.D, prob = TRUE, breaks = 100)
lines(density(cmtrt_prop.D))

## Drug price
qqnorm(ecomod$drug_totprice)
summary(ecomod$drug_totprice)
drug_totprice2 <- ecomod[ecomod$drug_totprice > 0,]$drug_totprice
descdist(drug_totprice2, discrete = FALSE, boot = 1000)
plotdist(drug_totprice2, histo = TRUE, demp = TRUE)
fitdist(drug_totprice2, "gamma", method = "mme")
drug_totprice2.D <- rgamma(n = 12000, shape = 3.27, rate = 0.13)
hist(drug_totprice2.D, prob = TRUE, breaks = 100)
lines(density(drug_totprice2.D))

## Treatment time
qqnorm(ecomod$trt_time)
summary(ecomod$trt_time)
trt_time2 <- ecomod[!is.na(ecomod$trt_time),]$trt_time
descdist(trt_time2, discrete = FALSE, boot = 1000)
plotdist(trt_time2, histo = TRUE, demp = TRUE)
fitdist(trt_time2, "gamma")
trt_time2.D <- rgamma(n = 12000, shape = 5.9, rate = 1.82)
hist(trt_time2.D, prob = TRUE, breaks = 100)
lines(density(trt_time2.D))

## Withdrawal time
qqnorm(ecomod$drug_wd)
summary(ecomod$drug_wd)
drug_wd2 <- ecomod[!is.na(ecomod$drug_wd),]$drug_wd
descdist(drug_wd2, discrete = FALSE, boot = 1000)
plotdist(drug_wd2, histo = TRUE, demp = TRUE)
fitdist(drug_wd2, "norm")
drug_wd.D <- rnorm(n = 12000, mean = 3.2, sd = 0.6)
hist(drug_wd.D, prob = TRUE, breaks = 100)
lines(density(drug_wd.D))

## Number of days before milk is put back in milk tank
qqnorm(ecomod$milkdisc_notrt)
summary(ecomod$milkdisc_notrt)
milkdisc_notrt2 <- ecomod[!is.na(ecomod$milkdisc_notrt),]$milkdisc_notrt
descdist(milkdisc_notrt2, discrete = FALSE, boot = 1000)
plotdist(milkdisc_notrt2, histo = TRUE, demp = TRUE)
fitdist(milkdisc_notrt2, "gamma", method = "mme")
milkdisc_notrt.D <- rgamma(n = 12000, shape = 0.6, rate = 0.23)
hist(milkdisc_notrt.D, prob = TRUE, breaks = 100)
lines(density(milkdisc_notrt.D))

## Proportion of milk fed to calves
qqnorm(ecomod$milkfed_prop)
summary(ecomod$milkfed_prop)
milkfed_prop2 <- ecomod[!is.na(ecomod$milkfed_prop),]$milkfed_prop
descdist(milkfed_prop2, discrete = FALSE, boot = 1000)
plotdist(milkfed_prop2, histo = TRUE, demp = TRUE)
fitdist(milkfed_prop2, "beta", method = "mme")
milkfed_prop2.D <- rbeta(n = 12000, shape1 = 0.17, shape2 = 0.26)
hist(milkfed_prop2.D, prob = TRUE, breaks = 100)
lines(density(milkfed_prop2.D))

## Number of cows excluded from bulktank due to high SCC
excl_nbr <- ecomod$excl_nbr/ecomod$cow_nbr
excl_nbr <- subset(excl_nbr, !is.na(excl_nbr))
qqnorm(excl_nbr)
descdist(excl_nbr, discrete = FALSE, boot = 1000)
plotdist(excl_nbr, histo = TRUE, demp = TRUE)
fitdist(excl_nbr, "beta", method = "mme")
excl_nbr.D <- rbeta(n = 12000, shape1 = 0.17, shape2 = 2.21)
hist(excl_nbr.D, prob = TRUE, breaks = 100)
lines(density(excl_nbr.D))

## Number of days out of bulktank due to SCM
daysout_scm2 <- round(ecomod[ecomod$daysout_scm < 305,]$daysout_scm)
qqnorm(daysout_scm2)
descdist(daysout_scm2, discrete = TRUE, boot = 1000)
plotdist(daysout_scm2, histo = TRUE, demp = TRUE)
fitdist(daysout_scm2, "nbinom")
daysout_scm.D <- rnbinom(n = 12000, size = 0.19, mu = 30.26)
hist(daysout_scm.D, prob = TRUE, breaks = 100)
lines(density(daysout_scm.D))

## Proportion of clinical mastitis seen by a vet
qqnorm(ecomod$cmvet_prop)
summary(ecomod$cmvet_prop)
cmvet_prop2 <- ecomod[!is.na(ecomod$cmvet_prop),]$cmvet_prop
descdist(cmvet_prop2, discrete = FALSE, boot = 1000)
plotdist(cmvet_prop2, histo = TRUE, demp = TRUE)
fitdist(cmvet_prop2, "beta", method = "mme")
cmvet_prop2.D <- rbeta(n = 145, shape1 = 0.2, shape2 = 3.47)
hist(cmvet_prop2.D, prob = TRUE, breaks = 100)
lines(density(cmvet_prop2.D))

## Vet fees for a clinical mastitis
qqnorm(ecomod$cmvet_cost)
summary(ecomod$cmvet_cost)
descdist(ecomod$cmvet_cost, discrete = FALSE, boot = 1000)
plotdist(ecomod$cmvet_cost, histo = TRUE, demp = TRUE)
fitdist(ecomod$cmvet_cost, "lnorm")
cmvet_cost.D <- rlnorm(n = 12000, meanlog = 4.6, sdlog = 0.41)
hist(cmvet_cost.D, prob = TRUE, breaks = 100)
lines(density(cmvet_cost.D))

## Cost of veterinary advices (udder health) per 100 cows
qqnorm(ecomod$vet_scccost)
summary(ecomod$vet_scccost)
descdist(ecomod$vet_scccost, discrete = FALSE, boot = 1000)
plotdist(ecomod$vet_scccost, histo = TRUE, demp = TRUE)
fitdist(ecomod$vet_scccost, "gamma", method = "mme")
vet_scccost.D <- rgamma(n = 12000, shape = 0.06, rate = 0.0002)
hist(vet_scccost.D, prob = TRUE, breaks = 100)
lines(density(vet_scccost.D))

## Labour time per clinical mastitis case
qqnorm(ecomod$trt_labour)
summary(ecomod$trt_labour)
descdist(ecomod$trt_labour, discrete = FALSE, boot = 1000)
plotdist(ecomod$trt_labour, histo = TRUE, demp = TRUE)
fitdist(ecomod$trt_labour, "gamma", method = "mme")
trt_labour2.D <- rgamma(n = 12000, shape = 1.15, rate = 0.8)
hist(trt_labour2.D, prob = TRUE, breaks = 100)
lines(density(trt_labour2.D))

## Penalty payments
#qqnorm(ecomod$penalty100)
#summary(ecomod$penalty100)
#descdist(ecomod$penalty100, discrete = FALSE, boot = 1000)
#plotdist(ecomod$penalty100, histo = TRUE, demp = TRUE)

## Insurance
qqnorm(ecomod$ins100)
summary(ecomod$ins100)
descdist(ecomod$ins100, discrete = FALSE, boot = 1000)
plotdist(ecomod$ins100, histo = TRUE, demp = TRUE)
fitdist(ecomod$ins100, "gamma", method = "mme")
ins.D <- rgamma(n = 12000, shape = 0.12, rate = 0.0009)
hist(ins.D, prob = TRUE, breaks = 100)
lines(density(ins.D))

## Milk samples cost for SCM per 100 cows
qqnorm(ecomod$ecoval_scmsmp100)
summary(ecomod$ecoval_scmsmp100)
descdist(ecomod$ecoval_scmsmp100, discrete = FALSE, boot = 1000)
plotdist(ecomod$ecoval_scmsmp100, histo = TRUE, demp = TRUE)
fitdist(ecomod$ecoval_scmsmp100, "gamma", method = "mme")
ecoval_scmsmp.D <- rgamma(n = 12000, shape = 0.1, rate = 0.0004)
hist(ecoval_scmsmp.D, prob = TRUE, breaks = 100)
lines(density(ecoval_scmsmp.D))

## Milk samples cost for CM per 100 cows
qqnorm(ecomod$ecoval_cmsmp100)
summary(ecomod$ecoval_cmsmp100)
descdist(ecomod$ecoval_cmsmp100, discrete = FALSE, boot = 1000)
plotdist(ecomod$ecoval_cmsmp100, histo = TRUE, demp = TRUE)
fitdist(ecomod$ecoval_cmsmp100, "gamma", method = "mme")
ecoval_cmsmp.D <- rgamma(n = 12000, shape = 0.24, rate = 0.001)
hist(ecoval_cmsmp.D, prob = TRUE, breaks = 100)
lines(density(ecoval_cmsmp.D))

## Pre-milking teat disinfection cost
qqnorm(ecomod$premtd_cost100)
summary(ecomod$premtd_cost100)
premtd_cost2 <- ecomod[!is.na(ecomod$premtd_cost100),]$premtd_cost100
descdist(premtd_cost2, discrete = FALSE, boot = 1000)
plotdist(premtd_cost2, histo = TRUE, demp = TRUE)
fitdist(premtd_cost2, "exp", method = "mme")
premtd_cost2.D <- rexp(n = 145, rate = 0.0008)
hist(premtd_cost2.D, prob = TRUE, breaks = 100)
lines(density(premtd_cost2.D))

## Post-milking teat disinfection cost
qqnorm(ecomod$postmtd_cost100)
summary(ecomod$postmtd_cost100)
postmtd_cost2 <- ecomod[!is.na(ecomod$postmtd_cost100),]$postmtd_cost100
descdist(postmtd_cost2, discrete = FALSE, boot = 1000)
plotdist(postmtd_cost2, histo = TRUE, demp = TRUE)
fitdist(postmtd_cost2, "gamma", method = "mme")
postmtd_cost2.D <- rgamma(n = 145, shape = 1.79, rate = 0.0009)
hist(postmtd_cost2.D, prob = TRUE, breaks = 100)
lines(density(postmtd_cost2.D))

## DCT cost
qqnorm(ecomod$dct_cost100)
summary(ecomod$dct_cost100)
dct_cost2 <- ecomod[!is.na(ecomod$dct_cost100),]$dct_cost100
descdist(dct_cost2, discrete = FALSE, boot = 1000)
plotdist(dct_cost2, histo = TRUE, demp = TRUE)
fitdist(dct_cost2, "gamma", method = "mme")
dct_cost2.D <- rgamma(n = 145, shape = 1.12, rate = 0.0006)
hist(dct_cost2.D, prob = TRUE, breaks = 100)
lines(density(dct_cost2.D))

## Gloves cost
qqnorm(ecomod$gloves_cost100)
summary(ecomod$gloves_cost100)
gloves_cost2 <- ecomod[!is.na(ecomod$gloves_cost100),]$gloves_cost100
descdist(gloves_cost2, discrete = FALSE, boot = 1000)
plotdist(gloves_cost2, histo = TRUE, demp = TRUE)
fitdist(gloves_cost2, "exp", method = "mme")
gloves_cost2.D <- rexp(n = 145, rate = 0.004)
hist(gloves_cost2.D, prob = TRUE, breaks = 100)
lines(density(gloves_cost2.D))

## Vaccines cost
qqnorm(ecomod$vacc_cost100)
summary(ecomod$vacc_cost100)
vacc_cost2 <- ecomod[!is.na(ecomod$vacc_cost100),]$vacc_cost100
descdist(vacc_cost2, discrete = FALSE, boot = 1000)
plotdist(vacc_cost2, histo = TRUE, demp = TRUE)
fitdist(vacc_cost2, "gamma", method = "mme")
vacc_cost2.D <- rgamma(n = 145, shape = 0.26, rate = 0.0006)
hist(vacc_cost2.D, prob = TRUE, breaks = 100)
lines(density(vacc_cost2.D))

## Rearing costs
qqnorm(ecomod$rearing_cost)
summary(ecomod$rearing_cost)
descdist(ecomod$rearing_cost, discrete = FALSE, boot = 1000)
plotdist(ecomod$rearing_cost, histo = TRUE, demp = TRUE)
fitdist(ecomod$rearing_cost, "norm")
rearing_cost.D <- rnorm(n = 12000, mean = 2450, sd = 450)
hist(rearing_cost.D, prob = TRUE, breaks = 100)
lines(density(rearing_cost.D))

## Meat value CM
qqnorm(ecomod$meatvalue)
summary(ecomod$meatvalue)
descdist(ecomod$meatvalue, discrete = FALSE, boot = 1000)
plotdist(ecomod$meatvalue, histo = TRUE, demp = TRUE)
fitdist(ecomod$meatvalue, "logis")
meatvalue.D <- rlogis(n = 12000, location = 1109, scale = 165)
hist(meatvalue.D, prob = TRUE, breaks = 100)
lines(density(meatvalue.D))

## Number of L1 culled due to CM
L1_cullprop <- with(ecomod, L1cull_nbr / cow_nbr)
qqnorm(L1_cullprop)
summary(L1_cullprop)
descdist(L1_cullprop, discrete = FALSE, boot = 1000)
plotdist(L1_cullprop, histo = TRUE, demp = TRUE)
fitdist(L1_cullprop, "beta", method = "mme")
L1_cullprop.D <- rbeta(n = 145, shape1 = 0.22, shape2 = 15.6)
hist(L1_cullprop.D, prob = TRUE, breaks = 100)
lines(density(L1_cullprop.D))

## Salvage value
qqnorm(ecomod$salvage)
summary(ecomod$salvage)
descdist(ecomod$salvage, discrete = FALSE, boot = 1000)
plotdist(ecomod$salvage, histo = TRUE, demp = TRUE)
fitdist(ecomod$salvage, "gamma", method = "mme")
salvage.D <- rgamma(n = 12000, shape = 0.25, rate = 0.01)
hist(salvage.D, prob = TRUE, breaks = 100)
lines(density(salvage.D))

## Number of L1 dead due to CM
L1_deadprop <- with(ecomod, L1dead_nbr / cow_nbr)
qqnorm(L1_deadprop)
summary(L1_deadprop)
descdist(L1_deadprop, discrete = FALSE, boot = 1000)
plotdist(L1_deadprop, histo = TRUE, demp = TRUE)
fitdist(L1_deadprop, "beta", method = "mme")
L1_deadprop.D <- rbeta(n = 145, shape1 = 0.05, shape2 = 20)
hist(L1_deadprop.D, prob = TRUE, breaks = 100)
lines(density(L1_deadprop.D))

## Number of L2+ culled due to CM
L2_cullprop <- with(ecomod, L2cull_nbr / cow_nbr)
qqnorm(L2_cullprop)
summary(L2_cullprop)
descdist(L2_cullprop, discrete = FALSE, boot = 1000)
plotdist(L2_cullprop, histo = TRUE, demp = TRUE)
fitdist(L2_cullprop, "beta", method = "mme")
L2_cullprop.D <- rbeta(n = 145, shape1 = 0.83, shape2 = 17.5)
hist(L2_cullprop.D, prob = TRUE, breaks = 100)
lines(density(L2_cullprop.D))

## Number of L2+ dead due to CM
L2_deadprop <- with(ecomod, L2dead_nbr / cow_nbr)
qqnorm(L2_deadprop)
summary(L2_deadprop)
descdist(L2_deadprop, discrete = FALSE, boot = 1000)
plotdist(L2_deadprop, histo = TRUE, demp = TRUE)
fitdist(L2_deadprop, "beta", method = "mme")
L2_deadprop.D <- rbeta(n = 145, shape1 = 0.26, shape2 = 46.4)
hist(L2_deadprop.D, prob = TRUE, breaks = 100)
lines(density(L2_deadprop.D))

## Meat value SCM
qqnorm(ecomod$meatvalue_scc)
summary(ecomod$meatvalue_scc)
descdist(ecomod$meatvalue_scc, discrete = FALSE, boot = 1000)
plotdist(ecomod$meatvalue_scc, histo = TRUE, demp = TRUE)
meatvalue2 <- ecomod$meatvalue_scc + 1
fitdist(meatvalue2, "lnorm")
meatvalue2.D <- rlnorm(n = 12000, meanlog = 6.8, sdlog = 1.3)
hist(meatvalue2.D, prob = TRUE, breaks = 100)
lines(density(meatvalue2.D))

## Number of L1 culled due to SCM
L1_cullprop2 <- with(ecomod, L1scccull_nbr / cow_nbr)
qqnorm(L1_cullprop2)
summary(L1_cullprop2)
descdist(L1_cullprop2, discrete = FALSE, boot = 1000)
plotdist(L1_cullprop2, histo = TRUE, demp = TRUE)
fitdist(L1_cullprop2, "beta", method = "mme")
L1_cullprop2.D <- rbeta(n = 145, shape1 = 0.21, shape2 = 15)
hist(L1_cullprop2.D, prob = TRUE, breaks = 100)
lines(density(L1_cullprop2.D))

## Number of L2+ culled due to SCM
L2_cullprop2 <- with(ecomod, L2scccull_nbr / cow_nbr)
qqnorm(L2_cullprop2)
summary(L2_cullprop2)
descdist(L2_cullprop2, discrete = FALSE, boot = 1000)
plotdist(L2_cullprop2, histo = TRUE, demp = TRUE)
fitdist(L2_cullprop2, "beta", method = "mme")
L2_cullprop2.D <- rbeta(n = 145, shape1 = 0.93, shape2 = 19)
hist(L2_cullprop2.D, prob = TRUE, breaks = 100)
lines(density(L2_cullprop2.D))
