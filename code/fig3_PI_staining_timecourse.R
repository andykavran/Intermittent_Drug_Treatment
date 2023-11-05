# Figure 3 Panel C
# Propidium Iodide staining over continuous or intermittent treatment

## Environment
library(tidyverse)
library(magrittr)
library(cowplot)
source("code/Helper_Functions/theme_int_dosing.R")

## Ingest Data
load_data = read_csv("data/fig3_propidum_iodide_staining.csv")
supp_data = read_csv("data/fig3_convenience_file_for_PI_staining.csv")

## Plot PI staining data
basesize = 10
linesize = .5
pointsize = 1.1
errorsize = .3
errorwidth = .5

this_plot <- ggplot(data = load_data, aes(x = Days, y = pi_positive, group = `Treatment Schedule`, color = `Treatment Schedule`,  shape = `Treatment Schedule`))+
  stat_summary(show.legend = TRUE, fun = mean, geom="point", size = pointsize)+
  stat_summary(show.legend = TRUE, fun= mean, geom="line", aes(linetype = `Treatment Schedule`), size = linesize)+
  stat_summary(show.legend = FALSE, fun.max = max, fun.min = min, geom="errorbar", width = .7)+
  coord_cartesian(ylim = c(0, 65))+
  scale_x_continuous(breaks = c(0, 3, 7, 10, 14, 17, 21, 24, 28, 31, 34), expand = c(0,.1))+
  scale_y_continuous(name = "Propidium Iodide Staining\n(% of Events)", breaks = c(0, 10, 20, 30, 40, 50, 60), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%"))+
  scale_color_manual(values = c("#222222", "#B11F30"))

this_plot +  geom_segment(aes(x = x, xend = xend, y=y, yend = yend, linetype = lines, color = `Treatment Schedule`),
                          data =supp_data, inherit.aes = FALSE, size = linesize) +  
  scale_linetype_manual(guide = FALSE, values = c(1,2,2,1))+  
  theme_int_dosing(base_size = 9, legend = c(.3, .9), aspect = 0.5278)+
  theme(legend.key.width = unit(.05, "npc"),
        legend.key.height = unit(0.05, "npc"),
        plot.margin = margin(t = 3, r = 0, b = 0, l = 0, unit = "pt"))
  ggsave("results/fig3_c_PI_staining.pdf", width = 3.6, height = 1.9, dpi = 600)