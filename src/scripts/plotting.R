## Plots for paper
## - N herds
## - N cows/herd (+ IQR)
## - Total cost (+ IQR)
## - Breakdown by cost components
## - Cost per animal (+ IQR)

####################################################################
## Data/libraries load

library(tidyverse)
library(gridExtra)
library(grid)
library(scatterpie)
library(viridis)
library(raster)
library(rgeos)

canada_cost <- readRDS("canada_cost.rds")
canada_cost$province <- with(canada_cost,
                             ifelse(province == "Quebec", "Québec", province))
n_herd <- readRDS("n_herd.rds")

## theme for maps
theme_map <- function(...) {
        theme_minimal() +
            theme(text = element_text(family = "Ubuntu Regular", color = "#22211d"),
                  axis.line = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
                  panel.grid.minor = element_blank(),
                  plot.background = element_rect(fill = "#f5f5f2", color = NA), 
                  panel.background = element_rect(fill = "#f5f5f2", color = NA), 
                  legend.background = element_rect(fill = "#f5f5f2", color = NA),
                  panel.border = element_blank(),
                  ...)
}
## theme for added table to plot
thm <- gridExtra::ttheme_minimal(core = list(fg_params = list(cex = .55)),
                                 colhead = list(fg_params = list(cex = .6)))

## load level 2 Canadian data from gadm.org
canada <- getData('GADM', country = 'CAN', level = 1)
provinces <- c("Nova Scotia", "New Brunswick", "Prince Edward Island",
               "Newfoundland and Labrador", "Ontario", "Québec",
               "Saskatchewan", "Alberta", "British Columbia", "Manitoba")
## keep provinces of interest
ca.provinces <- canada[canada$NAME_1 %in% provinces, ]
## merge provinces together to get West and Atlantic
ca.provinces$NAME_1[c(1:3, 10)] <- "West"
ca.provinces$NAME_1[c(4:6, 8)] <- "Atlantic"
ca.provinces <- rgeos::gUnaryUnion(ca.provinces, id = ca.provinces$NAME_1)
## make a useful data frame for plotting
ca.fortified <- fortify(ca.provinces, province = "NAME_1")

#############################################################
## Costs per animal

cow_cost <- canada_cost %>%
    filter(variable == "Cow cost (total)")
no_classes <- 5
quantiles <- quantile(cow_cost$median, 
                      probs = seq(0, 1, length.out = no_classes + 1))
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 0), 
                             " – ", 
                             round(quantiles[idx + 1], 0)))
}
labels <- labels[1:length(labels)-1]
cow_cost$cost_qtls <- cut(cow_cost$median, 
                          breaks = quantiles, 
                          labels = labels, 
                          include.lowest = TRUE)
brks_scale <- levels(cow_cost$cost_qtls)
labels_scale <- rev(brks_scale)

pnames <- aggregate(cbind(long, lat) ~ id,
                    ca.fortified,
                    FUN = function(x) mean(range(x)))
pnames[3, 2:3] <- c(-70, 50)

sum_table <- cow_cost[, c(1, 4, 3, 5)]
names(sum_table) <- c("Province", "Median", "p2.5", "p97.5")
sum_table[, 2:4] <- round(sum_table[, 2:4], 0)

p <- cow_cost %>%
    left_join(ca.fortified, ., by = c("id" = "province")) %>%
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(fill = cost_qtls,
                     x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25) +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 3) +
    coord_map() +
    theme_map() +
    theme(legend.position = c(0.5, 0.03),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          legend.title = element_text(size = 8),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Costs per cow",
         caption = "p2.5 and p97.5: 2.5th and 97.5th percentiles.") +
    scale_fill_manual(
        values = rev(viridis(8, alpha = 0.8)[3:7]),
        breaks = rev(brks_scale),
        name = "$/cow",
        drop = FALSE,
        labels = labels_scale,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(80/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = TRUE,
            reverse = TRUE,
            label.position = "bottom"))

png("cow_cost.png", width = 6, height = 4, units = 'in', res = 300)
p
vp <- viewport(x = 1.15, y = 2.6, width = 0.5, height = 0.75, default.units = 'in')
pushViewport(vp)
grid.draw(tableGrob(sum_table, rows = NULL, theme = thm))
dev.off()

###############################################################
## Total costs

tot_cost <- canada_cost %>%
    filter(variable == "Total cost")
no_classes <- 5
quantiles <- quantile(tot_cost$median, 
                      probs = seq(0, 1, length.out = no_classes + 1))
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(format(quantiles[idx], scientific = TRUE, digits = 2), 
                             " – ", 
                             format(quantiles[idx + 1], scientific = TRUE, digits = 2)))
}
labels <- labels[1:length(labels)-1]
tot_cost$cost_qtls <- cut(tot_cost$median, 
                          breaks = quantiles, 
                          labels = labels, 
                          include.lowest = TRUE)
brks_scale <- levels(tot_cost$cost_qtls)
labels_scale <- rev(brks_scale)

sum_table <- tot_cost[, c(1, 4, 3, 5)]
names(sum_table) <- c("Province", "Median", "p2.5", "p97.5")
sum_table[, 2:4] <- round(sum_table[, 2:4], 0)

p <- tot_cost %>%
    left_join(ca.fortified, ., by = c("id" = "province")) %>%
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(fill = cost_qtls,
                     x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25) +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 3) +
    coord_map() +
    theme_map() +
    theme(legend.position = c(0.25, 0.03),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          legend.title = element_text(size = 8),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Total Costs",
         caption = "p2.5 and p97.5: 2.5th and 97.5th percentiles.") +
    scale_fill_manual(
        values = rev(viridis(8, alpha = 0.8)[3:7]),
        breaks = rev(brks_scale),
        name = "$",
        drop = FALSE,
        labels = labels_scale,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(90/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 2,
            byrow = TRUE,
            reverse = TRUE,
            label.position = "bottom"))

png("total_cost.png", width = 6, height = 4, units = 'in', res = 300)
p
vp <- viewport(x = 1.35, y = 2.6, width = 0.5, height = 0.75, default.units = 'in')
pushViewport(vp)
grid.draw(tableGrob(sum_table, rows = NULL, theme = thm))
dev.off()

#####################################################################
## Costs components

overall_cost <- c("CM cost", "SCM cost", "Prevention")
cm_cost <- c("Culling (CM)", "Diagnostic (CM)", "Discarded milk (CM)",
             "Milk yield reduction (CM)", "Vet services (CM)", "Drug",
             "Labour")
scm_cost <- c("Culling (SCM)", "Diagnostic (SCM)", "Discarded milk (SCM)",
              "Milk yield reduction (SCM)", "Vet services (SCM)",
              "Product quality")

comp_cost <- canada_cost %>%
    filter(variable %in% overall_cost)

comp_cost <- comp_cost %>%
    dplyr::select(province, variable, median) %>%
    spread(key = variable, value = median) %>%
    mutate(long = c(pnames$long[1], pnames$long[3], pnames$long[2], pnames$long[4] + 7),
           lat = c(pnames$lat[1] + 5, pnames$lat[3] + 5, pnames$lat[2] + 5,
                   pnames$lat[4])) %>%
    rename(A = `CM cost`,
           B = `SCM cost`,
           C = Prevention)
comp_cost <- comp_cost[, c("province", "A", "B", "C", "long", "lat")]

ca.fortified %>% 
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25, fill = "gray") +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 4) +
    geom_scatterpie(data = comp_cost, aes(x = long, y = lat, r = 3),
                    cols = c("A", "B", "C")) +
    coord_equal() +
    theme_map() +
    theme(legend.position = c(0.5, 0.03),
          legend.title = element_blank(),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Costs components") +
    scale_fill_manual(
        values = viridis(3, alpha = 0.6),
        drop = FALSE,
        labels = c("Clinical mastitis", "Subclinical mastitis", "Prevention"),
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(90/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = TRUE,
            reverse = FALSE,
            label.position = "bottom"))
ggsave("comp_cost.png", width = 6, height = 4)

compcm_cost <- canada_cost %>%
    filter(variable %in% cm_cost)

compcm_cost <- compcm_cost %>%
    dplyr::select(province, variable, median) %>%
    spread(key = variable, value = median) %>%
    mutate(long = c(pnames$long[1], pnames$long[3], pnames$long[2], pnames$long[4] + 7),
           lat = c(pnames$lat[1] + 5, pnames$lat[3] + 5, pnames$lat[2] + 5,
                   pnames$lat[4])) %>%
    rename(A = `Culling (CM)`,
           B = `Diagnostic (CM)`,
           C = `Discarded milk (CM)`,
           D = `Drug`,
           E = `Labour`,
           F = `Milk yield reduction (CM)`,
           G = `Vet services (CM)`)

ca.fortified %>% 
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25, fill = "gray") +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 4) +
    geom_scatterpie(data = compcm_cost, aes(x = long, y = lat, r = 3),
                    cols = c("A", "B", "C", "D", "E", "F", "G")) +
    coord_equal() +
    theme_map() +
    theme(legend.position = c(0.5, 0.03),
          legend.title = element_blank(),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Costs components",
         subtitle = "Clinical mastitis") +
    scale_fill_manual(
        values = viridis(7, alpha = 0.6),
        drop = FALSE,
        labels = c("Culling", "Diagnostic", "Discarded milk", "Drug", "Labour",
                   "Milk yield reduction", "Vet services"),
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(90/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = TRUE,
            reverse = FALSE,
            label.position = "bottom"))
ggsave("compCM_cost.png", width = 6, height = 4)

compscm_cost <- canada_cost %>%
    filter(variable %in% scm_cost)

compscm_cost <- compscm_cost %>%
    dplyr::select(province, variable, median) %>%
    spread(key = variable, value = median) %>%
    mutate(long = c(pnames$long[1], pnames$long[3], pnames$long[2], pnames$long[4] + 7),
           lat = c(pnames$lat[1] + 5, pnames$lat[3] + 5, pnames$lat[2] + 5,
                   pnames$lat[4])) %>%
    rename(A = `Culling (SCM)`,
           B = `Diagnostic (SCM)`,
           C = `Discarded milk (SCM)`,
           D = `Milk yield reduction (SCM)`,
           E = `Vet services (SCM)`)

ca.fortified %>% 
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25, fill = "gray") +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 4) +
    geom_scatterpie(data = compscm_cost, aes(x = long, y = lat, r = 3),
                    cols = c("A", "B", "C", "D", "E")) +
    coord_equal() +
    theme_map() +
    theme(legend.position = c(0.5, 0.03),
          legend.title = element_blank(),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Costs components",
         subtitle = "Sub-clinical mastitis") +
    scale_fill_manual(
        values = viridis(5, alpha = 0.6),
        drop = FALSE,
        labels = c("Culling", "Diagnostic", "Discarded milk", "Milk yield reduction",
                   "Vet services"),
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(90/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 0.5,
            nrow = 1,
            byrow = TRUE,
            reverse = FALSE,
            label.position = "bottom"))
ggsave("compSCM_cost.png", width = 6, height = 4)

##########################################################################
## n_cow/herd

cow_n <- canada_cost %>%
    filter(variable == "N cows (median)")
no_classes <- 5
quantiles <- quantile(cow_n$median, 
                      probs = seq(0, 1, length.out = no_classes + 1))
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 0), 
                             " – ", 
                             round(quantiles[idx + 1], 0)))
}
labels <- labels[1:length(labels)-1]
cow_n$cost_qtls <- cut(cow_n$median, 
                          breaks = quantiles, 
                          labels = labels, 
                          include.lowest = TRUE)
brks_scale <- levels(cow_n$cost_qtls)
labels_scale <- rev(brks_scale)

pnames <- aggregate(cbind(long, lat) ~ id,
                    ca.fortified,
                    FUN = function(x) mean(range(x)))
pnames[3, 2:3] <- c(-70, 50)

sum_table <- cow_n[, c(1, 4, 3, 5)]
names(sum_table) <- c("Province", "Median", "p2.5", "p97.5")
sum_table[, 2:4] <- round(sum_table[, 2:4], 1)

p <- cow_n %>%
    left_join(ca.fortified, ., by = c("id" = "province")) %>%
    arrange(order) %>% 
    ggplot() +
    geom_polygon(aes(fill = cost_qtls,
                     x = long,
                     y = lat,
                     group = group),
                 colour = "black", size = 0.25) +
    geom_path(aes(x = long,
                  y = lat,
                  group = group),
              colour = "white", size = 0.1) +
    geom_text(data = pnames, aes(x = long, y = lat, label = id), size = 3) +
    coord_map() +
    theme_map() +
    theme(legend.position = c(0.5, 0.03),
          legend.text.align = 0,
          legend.background = element_rect(fill = alpha('white', 0.0)),
          legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
          plot.title = element_text(color = "#4e4d47"),
          legend.title = element_text(size = 8),
          plot.margin = unit(c(.5, .5, .2, .5), "cm"),
          panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
          panel.border = element_blank(),
          plot.caption = element_text(size = 6, 
                                      hjust = 0.92, 
                                      margin = margin(t = 0.2, 
                                                      b = 0, 
                                                      unit = "cm"), 
                                      color = "#939184")) +
    labs(x = NULL, 
         y = NULL, 
         title = "Cows per herd",
         caption = "p2.5 and p97.5: 2.5th and 97.5th percentiles.") +
    scale_fill_manual(
        values = rev(viridis(8, alpha = 0.8)[3:7]),
        breaks = rev(brks_scale),
        name = "Cows/herd",
        drop = FALSE,
        labels = labels_scale,
        guide = guide_legend(
            direction = "horizontal",
            keyheight = unit(2, units = "mm"),
            keywidth = unit(80/length(labels), units = "mm"),
            title.position = 'top',
            title.hjust = 0.5,
            label.hjust = 1,
            nrow = 1,
            byrow = TRUE,
            reverse = TRUE,
            label.position = "bottom"))

png("n_cow.png", width = 6, height = 4, units = 'in', res = 300)
p
vp <- viewport(x = 1.15, y = 2.6, width = 0.5, height = 0.75, default.units = 'in')
pushViewport(vp)
grid.draw(tableGrob(sum_table, rows = NULL, theme = thm))
dev.off()


#---eof---------------------------------------------------------------------
