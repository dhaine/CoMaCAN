compute_cost <- function(x) {
    # Milk yield reduction
    incidence <- ifelse(x[, "inc_risk"] > 100, 100, x[, "inc_risk"])
    incidence5 <- incidence - (incidence*0.1)
    incidence5 <- ifelse(incidence5 < 0, 0, incidence5)
    incidence10 <- incidence - (incidence*0.2)
    incidence10 <- ifelse(incidence10 < 0, 0, incidence10)
    cm_nbr <- round(incidence) * x[, "n_cows"]/100
    milkLoss_CM <- cm_nbr * 0.05 * x[, "prod_305"] * 0.78  # Eq. 1 & 2
    cm_nbr5 <- round(incidence5) * x[, "n_cows"]/100
    milkLoss_CM5 <- cm_nbr5 * 0.05 * x[, "prod_305"] * 0.78  # Eq. 1 & 2
    cm_nbr10 <- round(incidence10) * x[, "n_cows"]/100
    milkLoss_CM10 <- cm_nbr10 * 0.05 * x[, "prod_305"] * 0.78  # Eq. 1 & 2

    # Drug
    drug_spent <- (0.8 * cm_nbr * x[, "cmtrt_prop"] * x[, "drug_totprice"]) +
        (0.2 * cm_nbr * x[, "cmtrt_prop"] *
         (x[, "drug_totprice"] + 25.4))  # Eq. 9
    drug_spent5 <- (0.8 * cm_nbr5 * x[, "cmtrt_prop"] * x[, "drug_totprice"]) +
        (0.2 * cm_nbr5 * x[, "cmtrt_prop"] *
         (x[, "drug_totprice"] + 25.4))  # Eq. 9
    drug_spent10 <- (0.8 * cm_nbr10 * x[, "cmtrt_prop"] * x[, "drug_totprice"]) +
        (0.2 * cm_nbr10 * x[, "cmtrt_prop"] *
         (x[, "drug_totprice"] + 25.4))  # Eq. 9

    # Discarded milk
    milkdisc_notrt <- ifelse(x[, "milkdisc_notrt"] > 21, 21, x[, "milkdisc_notrt"])
    milkDisc_tuCM <- ((x[, "trt_time"] + x[, "drug_wd"]) * x[, "prod_day"] *
                      (cm_nbr * x[, "cmtrt_prop"])) +
                      (milkdisc_notrt * x[, "prod_day"]
                          * (1 - x[, "cmtrt_prop"]) * cm_nbr)  # Eq. 10--12
    milkDisc_tuCM5 <- ((x[, "trt_time"] + x[, "drug_wd"]) * x[, "prod_day"] *
                      (cm_nbr5 * x[, "cmtrt_prop"])) +
                      (milkdisc_notrt * x[, "prod_day"]
                          * (1 - x[, "cmtrt_prop"]) * cm_nbr5)  # Eq. 10--12
    milkDisc_tuCM10 <- ((x[, "trt_time"] + x[, "drug_wd"]) * x[, "prod_day"] *
                      (cm_nbr10 * x[, "cmtrt_prop"])) +
                      (milkdisc_notrt * x[, "prod_day"]
                          * (1 - x[, "cmtrt_prop"]) * cm_nbr10)  # Eq. 10--12
    milkDisc_fed <- x[, "milkfed_prop"] * milkDisc_tuCM
    milkDisc_fed5 <- x[, "milkfed_prop"] * milkDisc_tuCM5
    milkDisc_fed10 <- x[, "milkfed_prop"] * milkDisc_tuCM10
    milkDisc_CM <- (milkDisc_tuCM * 0.78) - (milkDisc_fed * 0.49)  # Eq. 13
    milkDisc_CM5 <- (milkDisc_tuCM5 * 0.78) - (milkDisc_fed5 * 0.49)  # Eq. 13
    milkDisc_CM10 <- (milkDisc_tuCM10 * 0.78) - (milkDisc_fed10 * 0.49)  # Eq. 13
    excl_prop <- ifelse(x[, "excl_nbr"] > 0.5, 0.5, x[, "excl_nbr"])
    excl_nbr <- excl_prop * x[, "n_cows"]
    daysout_scm <- ifelse(x[, "daysout_scm"] > 100, 100, x[, "daysout_scm"])
    milkDisc_SCM <- (x[, "prod_day"] * excl_nbr * daysout_scm * 0.78) -
        (x[, "prod_day"] * excl_nbr * daysout_scm * 0.49)  # Eq. 14-15
    milkDisc <- milkDisc_CM + milkDisc_SCM
    milkDisc5 <- milkDisc_CM5 + milkDisc_SCM
    milkDisc10 <- milkDisc_CM10 + milkDisc_SCM

    # Veterinary services
    vetcost_CM <- cm_nbr * x[, "cmvet_prop"] * x[, "cmvet_cost"]  # Eq. 16
    vetcost_CM5 <- cm_nbr5 * x[, "cmvet_prop"] * x[, "cmvet_cost"]  # Eq. 16
    vetcost_CM10 <- cm_nbr10 * x[, "cmvet_prop"] * x[, "cmvet_cost"]  # Eq. 16
    vetcost <- vetcost_CM + x[, "vetcost_scc"]
    vetcost5 <- vetcost_CM5 + x[, "vetcost_scc"]
    vetcost10 <- vetcost_CM10 + x[, "vetcost_scc"]

    # Labour
    labour_CM <- cm_nbr * x[, "labour_CM"] * 34.5  # Eq. 18
    labour_CM5 <- cm_nbr5 * x[, "labour_CM"] * 34.5  # Eq. 18
    labour_CM10 <- cm_nbr10 * x[, "labour_CM"] * 34.5  # Eq. 18

    # Product quality
    prem_loss <- ifelse(x[, "province"] %in% c(7:10) & x[, "btscc"] > 250,
                                x[, "n_cows"] * x[, "prod_day"] * 365 * 0.28/100, 
                      ifelse(x[, "province"] == 4 & x[, "btscc"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.25/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.79/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc"] > 150 &
                             x[, "btscc"] <= 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.29/100,
                             0))))
    prem_loss5 <- ifelse(x[, "province"] %in% c(7:10) & x[, "btscc5"] > 250,
                                x[, "n_cows"] * x[, "prod_day"] * 365 * 0.28/100, 
                      ifelse(x[, "province"] == 4 & x[, "btscc5"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.25/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc5"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.79/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc5"] > 150 &
                             x[, "btscc5"] <= 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.29/100,
                             0))))
    prem_loss10 <- ifelse(x[, "province"] %in% c(7:10) & x[, "btscc10"] > 250,
                                x[, "n_cows"] * x[, "prod_day"] * 365 * 0.28/100, 
                      ifelse(x[, "province"] == 4 & x[, "btscc10"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.25/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc10"] > 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.79/100,
                      ifelse(x[, "province"] == 5 & x[, "btscc10"] > 150 &
                             x[, "btscc10"] <= 200,
                             x[, "n_cows"] * x[, "prod_day"] * 365 * 0.29/100,
                             0))))
    quality_cost <- x[, "ins_cost"] + prem_loss #+ x[, "penalty"]  # Eq. 19
    quality_cost5 <- x[, "ins_cost"] + prem_loss5 #+ x[, "penalty"]  # Eq. 19
    quality_cost10 <- x[, "ins_cost"] + prem_loss10 #+ x[, "penalty"]  # Eq. 19

    # Diagnostic
    # from ecomod$ecoval_cmsmp100 and ecomod$ecoval_scmsmp100  # Eq. 20--21

    # Culling and mortality
    rearmeat_CM <- x[, "rearing_cost"] - x[, "meatvalue"]
    rearmeat_CM <- ifelse(rearmeat_CM < 0, 0, rearmeat_CM)
    L1cull_nbr <- round(x[, "n_cows"] * x[, "L1_cullprop"])
    L1cull_nbr <- ifelse(L1cull_nbr > round(x[, "n_cows"] * x[, "L1_prop"]),
                         round(x[, "n_cows"] * x[, "L1_prop"]), L1cull_nbr)
    L1cull_CM <- rearmeat_CM * L1cull_nbr  # Eq. 22
    rearsalv_CM <- x[, "rearing_cost"] + x[, "salvage"]
    L1dead_nbr <- round(x[, "n_cows"] * x[, "L1_deadprop"])
    L1dead_nbr <- ifelse((L1cull_nbr + L1dead_nbr) >
                         round(x[, "n_cows"] * x[, "L1_prop"]),
                         round(x[, "n_cows"] * x[, "L1_prop"]) - L1cull_nbr,
                         L1dead_nbr)
    L1dead_CM <- rearsalv_CM * L1dead_nbr  # Eq. 23
    L2cull_nbr <- round(x[, "n_cows"] * x[, "L2_cullprop"])
    L2cull_nbr <- ifelse(L2cull_nbr > round(x[, "n_cows"] * (1 - x[, "L1_prop"])),
                         round(x[, "n_cows"] * (1 - x[, "L1_prop"])), L2cull_nbr)
    L2cull_CM <- (1.3 * rearmeat_CM) * L2cull_nbr  # Eq. 24
    L2dead_nbr <- round(x[, "n_cows"] * x[, "L2_deadprop"])
    L2dead_nbr <- ifelse((L2cull_nbr + L2dead_nbr) >
                         round(x[, "n_cows"] * (1 - x[, "L1_prop"])),
                         round(x[, "n_cows"] * (1 - x[, "L1_prop"])) - L2cull_nbr,
                         L2dead_nbr)
    L2dead_CM <- (1.3 * rearsalv_CM) * L2dead_nbr  # Eq. 25
    meatvalue_scc <- ifelse(x[, "meatvalue_scc"] > 2500, 2500, x[, "meatvalue_scc"])
    rearmeat_SCM <- x[, "rearing_cost"] - meatvalue_scc
    rearmeat_SCM <- ifelse(rearmeat_SCM < 0, 0, rearmeat_SCM)
    L1cullscc_nbr <- round(x[, "n_cows"] * x[, "L1_cullprop2"])
    L1cullscc_nbr <- ifelse(L1cullscc_nbr > round((x[, "n_cows"] * x[, "L1_prop"]) -
                                               L1cull_nbr - L1dead_nbr),
                            round((x[, "n_cows"] * x[, "L1_prop"]) - L1cull_nbr -
                                  L1dead_nbr), L1cullscc_nbr)
    L1cull_SCM <- rearmeat_SCM * L1cullscc_nbr  # Eq. 26
    L2cullscc_nbr <- round(x[, "n_cows"] * x[, "L2_cullprop2"])
    L2cullscc_nbr <- ifelse(L2cullscc_nbr > round((x[, "n_cows"] * (1 - x[, "L1_prop"])) -
                                                  L2cull_nbr - L2dead_nbr),
                            round((x[, "n_cows"] * (1 - x[, "L1_prop"])) - L2cull_nbr -
                                  L2dead_nbr), L2cullscc_nbr)
    L2cull_SCM <- (1.3 * rearmeat_SCM) * L2cullscc_nbr  # Eq. 27
    cull_CM <- L1cull_CM + L1dead_CM + L2cull_CM + L2dead_CM
    cull_SCM <- L1cull_SCM + L2cull_SCM
    culling <- cull_CM + cull_SCM

    # Materials and investments
    td_labour <- x[, "n_cows"] * 4 * 2 * 365 * 34.5/3600 * 2  # Eq. 28--29
    dct_labour <- x[, "n_cows"] * 0.8 * 2 * 34.5/60  # Eq. 30
    prev_cost <- x[, "premtd_cost"] + x[, "postmtd_cost"] + td_labour +
        x[, "dct_cost"] + dct_labour + x[, "gloves_cost"] + x[, "vacc_cost"]  # Eq. 31
    
    total_scm <- x[, "milkLoss_SCM"] + milkDisc_SCM + x[, "vetcost_scc"] + 
        quality_cost + x[, "scmsmp"] + cull_SCM
    total_scm5 <- x[, "milkLoss_SCM5"] + milkDisc_SCM + x[, "vetcost_scc"] + 
        quality_cost + x[, "scmsmp"] + cull_SCM
    total_scm10 <- x[, "milkLoss_SCM10"] + milkDisc_SCM + x[, "vetcost_scc"] + 
        quality_cost + x[, "scmsmp"] + cull_SCM

    total_cm <- milkLoss_CM + drug_spent + milkDisc_CM + vetcost_CM + labour_CM +
        x[, "cmsmp"] + cull_CM
    total_cm5 <- milkLoss_CM5 + drug_spent5 + milkDisc_CM5 + vetcost_CM5 + labour_CM5 +
        x[, "cmsmp"] + cull_CM
    total_cm10 <- milkLoss_CM10 + drug_spent10 + milkDisc_CM10 + vetcost_CM10 +
        labour_CM10 +
        x[, "cmsmp"] + cull_CM

    total_cost <- total_cm + total_scm + prev_cost
    total_costIR5 <- total_cm5 + total_scm + prev_cost
    total_costIR10 <- total_cm10 + total_scm + prev_cost
    total_costSCC5 <- total_cm + total_scm5 + prev_cost
    total_costSCC10 <- total_cm + total_scm10 + prev_cost

    cow_tot <- total_cost / x[, "n_cows"]
    cow_cm <- total_cm / x[, "n_cows"]
    cow_scm <- total_scm / x[, "n_cows"]

    return(c(sum(total_cost, na.rm = TRUE),
             sum(total_cm, na.rm = TRUE),
             sum(total_scm, na.rm = TRUE),
             sum(milkLoss_CM, na.rm = TRUE),
             sum(x[, "milkLoss_SCM"], na.rm = TRUE),
             sum(drug_spent, na.rm = TRUE),
             sum(milkDisc_CM, na.rm = TRUE),
             sum(milkDisc_SCM, na.rm = TRUE),
             sum(vetcost_CM+x[, "vetcost_scc"], na.rm = TRUE),
             sum(vetcost_CM, na.rm = TRUE),
             sum(x[, "vetcost_scc"], na.rm = TRUE),
             sum(labour_CM, na.rm = TRUE),
             sum(quality_cost, na.rm = TRUE),
             sum(x[, "cmsmp"]+x[, "scmsmp"], na.rm = TRUE),
             sum(x[, "cmsmp"], na.rm = TRUE),
             sum(x[, "scmsmp"], na.rm = TRUE),
             sum(culling, na.rm = TRUE),
             sum(cull_CM, na.rm = TRUE),
             sum(cull_SCM, na.rm = TRUE),
             sum(prev_cost, na.rm = TRUE),
             sum(total_costIR5, na.rm = TRUE),
             sum(total_costIR10, na.rm = TRUE),
             sum(total_costSCC5, na.rm = TRUE),
             sum(total_costSCC10, na.rm = TRUE),
             mean(cow_tot, na.rm = TRUE),
             mean(cow_cm, na.rm = TRUE),
             mean(cow_scm, na.rm = TRUE),
             mean(x[, "n_cows"], na.rm = TRUE),
             quantile(x[, "n_cows"], 0.25),
             quantile(x[, "n_cows"], 0.75)
             ))
}
