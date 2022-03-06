# Figure 2 B and C Growth Curves
# This script plots the growth curves for V600E and Vector

## Enrivonment 
library(tidyverse)
library(magrittr)
library(cowplot)
source("code/Helper_Functions/theme_int_dosing.R")

## Ingest Data
growth_data <- read_csv(file = "data/fig2_growth_curve_data.csv")
growth_data_long <- growth_data %>% pivot_longer(values_to = "cell_number", cols = c("0", "7", "14", "21", "28"), names_to = "Days")

## Wrangle Data
vector_data <- growth_data_long %>% filter(sample == "Vector Control")
vector_data$Days %<>% as.numeric
vec_baseline <- vector_data %>% filter(Days == 0) %>% summarize(mean_number = mean(cell_number)) %>% as.numeric
vector_data <- vector_data %>% mutate(normalized_cell_number = (cell_number/vec_baseline))

v600e_data <- growth_data_long %>% filter(sample == "V600E")
v600e_data$Days %<>% as.numeric
v600e_baseline <- v600e_data %>% filter(Days == 0) %>% summarize(mean_number = mean(cell_number)) %>% as.numeric
v600e_data <- v600e_data %>% mutate(normalized_cell_number = (cell_number/v600e_baseline))
subset_v600e_data <- v600e_data %>% filter(normalized_cell_number < 25) # remove outlier

## Plot Vector - panel c
ggplot(vector_data, aes(x = Days, y = normalized_cell_number, group = schedule, color = schedule,
                        linetype = schedule, shape = schedule))+
  #geom_point(alpha = 0.2)+
  stat_summary(geom = "line")+
  stat_summary(geom = "point", size = 2)+
  stat_summary(geom = "errorbar", width = 1.2, linetype = "solid")+
  # scale_y_continuous(breaks = c(0, .25, .5, .75, 1))+
  scale_color_manual(values = c("#92c5de", "#2767AB"))+
  scale_x_continuous(breaks = c(0,7, 14, 21, 28))+
  labs(y = "Normalized Cell Number")+
  scale_y_continuous(limits = c(0, 7))+
  ggtitle("WM239A Vector")+ 
  #coord_cartesian(ylim = c(0, 25))+
  theme_int_dosing(legend = c(.5, .8), base_size = 9, aspect = 1.2)+
  theme(legend.key.width = unit(.1, "npc"), 
        legend.key.height = unit(.05, "npc"))
ggsave("results/fig2_vector_growth_curve.pdf", height = 2, width = 1.6, units = "in", dpi = 600)

## Plot V600E - panel B
ggplot(subset_v600e_data, aes(x = Days, y = normalized_cell_number, group = schedule, color = schedule,linetype = schedule, shape = schedule))+
  #geom_point(alpha = 0.2)+
  stat_summary(geom = "line")+
  stat_summary(geom = "point", size = 2)+
  stat_summary(geom = "errorbar", width = 1.2, linetype = "solid")+
  scale_color_manual(values = c("#EF8A63","#B11F30"))+
  scale_y_continuous(breaks = c(0,2, 4, 6, 8, 10, 12, 14))+
  scale_x_continuous(breaks = c(0,7, 14, 21, 28))+
  ggtitle("WM239A V600E")+
  labs(y = "Normalized Cell Number")+
  theme_int_dosing(legend = c(.35, .8), base_size = 9, aspect = 1.2)+
  theme(legend.key.width = unit(.1, "npc"), 
        legend.key.height = unit(.05, "npc"))
ggsave("results/fig2_v600e_vector_growth_curve.pdf", dpi = 600, width = 1.6, height = 2, units = "in")

