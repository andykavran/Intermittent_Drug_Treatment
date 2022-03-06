# Figure 1 C LGX818 Dose Response
# This script creates the dose response curve in figure 1 c.

## Environment
library(tidyverse)
library(magrittr)
library(cowplot)
source("code/Helper_Functions/theme_int_dosing.R")
## Ingest Data
dr_data <- read_csv("data/lgx_mutant_dose_response.csv")
dr_data_long <- dr_data %>% pivot_longer(!c(sample, rep_number), values_to = "cell_number", names_to = "LGX818")
## Subset for only V600E and Vector
subset_dr_data <- dr_data_long %>% filter(sample == "Vector" | sample == "V600E")
subset_dr_data$LGX818 %<>% as.numeric()
## Plot Dose Reponse Curves
ggplot(subset_dr_data, aes(x = LGX818, y = cell_number, group = sample, color = sample))+
  geom_point(alpha = 0.2)+
  stat_summary(geom = "point")+
  stat_summary(geom = "errorbar", width = .5)+
  geom_smooth(span = 0.65, se = FALSE, size = 0.8)+
  scale_x_log10("LGX818, nM", breaks= c(0.01, .1, 1, 10, 100, 1000, 10000),
                labels = c(0, .1, 1, 10, 100, "1,000", "10,000"))+
  scale_color_manual("",values = c("#B11F30", "#2767AB"))+
  labs(y = "Normalized\nCell Number")+
  theme_int_dosing(legend = c(.9, .9), base_size = 10, aspect = 0.6)+
  theme(legend.key.height = unit(0.05, "npc"),
        legend.key.width = unit(0.1, "npc"),
        plot.margin = margin())
ggsave("results/fig1_LGX818_mutant_dose_response.pdf", dpi = 600, width = 5.5, height = 3.3, units = "cm")