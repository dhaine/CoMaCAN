#################################################################################
## Computing cost of mastitis in Canadian dairies: loading data
## Denis Haine
#################################################################################

##----loading libraries---------------------------------------------------------
library(tidyverse)
library(readxl)
library(pbapply)

##----loading data---------------------------------------------------------------
ecomod <- read_csv("../../data/quest3.csv")
atlantic <- read_excel("../../data/DHI/SDufour_Lactations_Atlantic_2014.xlsx")
atlantic <- dplyr::rename(atlantic, lact_nbr = `Lact#`,
                   EndDate = End_date,
                   SCC_TestCnt = `SCC Test Count`,
                   avg_scc = `Lact AVG SCC`,
                   LS = `Lact Linear Score`,
                   milk_305 = `Milk 305`,
                   milk_total = `Milk Total`,
                   fat_305 = `Fat 305`,
                   fat_total = `Fat Total`,
                   protein_305 = `Protein 305`,
                   protein_total = `Protein Total`)
atlantic$Province <- with(atlantic, ifelse(Province == "NB", 1,
                                    ifelse(Province == "NF", 2,
                                    ifelse(Province == "NS", 3, 4))))
atlantic$L1 <- with(atlantic, ifelse(lact_nbr == 1, 1, NA))

quebec <- read_excel("../../data/DHI/SDufour_Lactations_Quebec_2014.xlsx")
quebec <- dplyr::rename(quebec, lact_nbr = `Lact#`,
                   EndDate = End_date,
                   SCC_TestCnt = `SCC Test Count`,
                   avg_scc = `Lact AVG SCC`,
                   LS = `Lact Linear Score`,
                   milk_305 = `Milk 305`,
                   milk_total = `Milk Total`,
                   fat_305 = `Fat 305`,
                   fat_total = `Fat Total`,
                   protein_305 = `Protein 305`,
                   protein_total = `Protein Total`)
quebec$Province <- 5
quebec$L1 <- with(quebec, ifelse(lact_nbr == 1, 1, NA))

ontario <- read_excel("../../data/DHI/SDufour_Lactations_Ontario_2014.xlsx")
ontario <- dplyr::rename(ontario, Breed = `ANB_CD`, AnimalID = `ANM_ID`,
                         lact_nbr = `Lact#`,
                         EndDate = End_date,
                         SCC_TestCnt = `SCC_TEST_CNT`,
                         avg_scc = `Lact AVG SCC`,
                         LS = `SCC_LINEAR_SCORE_AVG`,
                         milk_305 = `Milk 305`,
                         milk_total = `Milk Total`,
                         fat_305 = `Fat 305`,
                         fat_total = `Fat Total`,
                         protein_305 = `Protein 305`,
                         protein_total = `Protein Total`)
ontario$Province <- 6
ontario$L1 <- with(ontario, ifelse(lact_nbr == 1, 1, NA))

west <- read_excel("../../data/DHI/SDufour_Lactations_West_2014.xlsx")
west <- dplyr::rename(west, lact_nbr = `Lact#`,
                   EndDate = End_date,
                   SCC_TestCnt = `SCC Test Count`,
                   avg_scc = `Lact AVG SCC`,
                   LS = `Lact Linear Score`,
                   milk_305 = `Milk 305`,
                   milk_total = `Milk Total`,
                   fat_305 = `Fat 305`,
                   fat_total = `Fat Total`,
                   protein_305 = `Protein 305`,
                   protein_total = `Protein Total`)
west$Province <- with(west, ifelse(Province == "AB", 9,
                            ifelse(Province == "BC", 10,
                            ifelse(Province == "MB", 7, 8))))
west$L1 <- with(west, ifelse(lact_nbr == 1, 1, NA))

canada <- rbind(atlantic, quebec, ontario, west)
canada <- canada %>% distinct(AnimalID, .keep_all = TRUE)

avg_atlantic <- atlantic %>%
    group_by(HerdID) %>%
    dplyr::summarise(prod_tot = mean(milk_total, na.rm = FALSE),
                     prod_305 = mean(milk_305, na.rm = TRUE),
                     prod_day = mean(milk_305/305, na.rm = TRUE),
                     n_cows = n(),
                     ls_avg = mean(LS, na.rm = TRUE),
                     milkLoss_SCM = n_cows * 190 * (ls_avg - 1) * 0.78,
                     btscc = 10^(0.24*ls_avg + 1.6),
                     province = first(Province),
                     L1 = sum(L1, na.rm = TRUE)) %>%
    mutate(L1_prop = L1 / n_cows)

avg_quebec <- quebec %>%
    group_by(HerdID) %>%
    dplyr::summarise(prod_tot = mean(milk_total, na.rm = FALSE),
                     prod_305 = mean(milk_305, na.rm = TRUE),
                     prod_day = mean(milk_305/305, na.rm = TRUE),
                     n_cows = length(AnimalID),
                     ls_avg = mean(LS, na.rm = TRUE),
                     milkLoss_SCM = n_cows * 190 * (ls_avg - 1) * 0.78,
                     btscc = 10^(0.24*ls_avg + 1.6),
                     province = first(Province),
                     L1 = sum(L1, na.rm = TRUE)) %>%
    mutate(L1_prop = L1 / n_cows)

avg_ontario <- ontario %>%
    group_by(HerdID) %>%
    dplyr::summarise(prod_tot = mean(milk_total, na.rm = FALSE),
                     prod_305 = mean(milk_305, na.rm = TRUE),
                     prod_day = mean(milk_305/305, na.rm = TRUE),
                     n_cows = length(AnimalID),
                     ls_avg = mean(LS, na.rm = TRUE),
                     milkLoss_SCM = n_cows * 190 * (ls_avg - 1) * 0.78,
                     btscc = 10^(0.24*ls_avg + 1.6),
                     province = first(Province),
                     L1 = sum(L1, na.rm = TRUE)) %>%
    mutate(L1_prop = L1 / n_cows)

avg_west <- west %>%
    group_by(HerdID) %>%
    dplyr::summarise(prod_tot = mean(milk_total, na.rm = FALSE),
                     prod_305 = mean(milk_305, na.rm = TRUE),
                     prod_day = mean(milk_305/305, na.rm = TRUE),
                     n_cows = length(AnimalID),
                     ls_avg = mean(LS, na.rm = TRUE),
                     milkLoss_SCM = n_cows * 190 * (ls_avg - 1) * 0.78,
                     btscc = 10^(0.24*ls_avg + 1.6),
                     province = first(Province),
                     L1 = sum(L1, na.rm = TRUE)) %>%
    mutate(L1_prop = L1 / n_cows)

avg_canada <- canada %>%
    group_by(HerdID) %>%
    dplyr::summarise(prod_tot = mean(milk_total, na.rm = FALSE),
                     prod_305 = mean(milk_305, na.rm = TRUE),
                     prod_day = mean(milk_305/305, na.rm = TRUE),
                     n_cows = n(),
                     ls_avg = mean(LS, na.rm = TRUE),
                     milkLoss_SCM = n_cows * 190 * (ls_avg - 1) * 0.78,  # Eq. 3--5
                     milkLoss_SCM5 = n_cows * 190 * ((ls_avg - (0.07*ls_avg)) - 1) *
                         0.78,
                     milkLoss_SCM10 = n_cows * 190 * ((ls_avg - (0.16*ls_avg)) - 1) *
                         0.78,
                     btscc = 10^(0.24*ls_avg + 1.6),
                     btscc5 = btscc - (0.1*btscc),
                     btscc10 = btscc - (0.2*btscc),
                     province = first(Province),
                     L1 = sum(L1, na.rm = TRUE)) %>%
    mutate(L1_prop = L1 / n_cows)

saveRDS(avg_canada, "avg_canada.rds")

##----eof------------------------------------------------------------------------
