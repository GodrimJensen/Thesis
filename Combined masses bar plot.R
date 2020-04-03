library(ggplot2)
library(tidyverse)
library(ggpubr)


Combined_mass <- left_join(Mean_absorbance, Mean_abs_supernatant, by = c("Solvent", "PCL_concentration", "Coating_time"))
Combined_mass <- Combined_mass %>% select(-c(Mean.x, Mean.y, Adj_mean.x, Adj_mean.y)) %>% 
  mutate(Comb_mass = Mass_ab.x + Mass_ab.y, Comb_Std = sqrt((4*(Std.x)^2+4*(Std.y)^2)/8)) %>% 
  select(-c(Std.x, Std.y, Mass_ab.x, Mass_ab.y))

Combined_mass$Sol_PCL <- paste(Combined_mass$Solvent, Combined_mass$PCL_concentration, sep = "_")

Mass_plot <- ggplot(data = Combined_mass,
                    aes(x = factor(Coating_time), 
                        y = Comb_mass, 
                        ymin = Comb_mass - Comb_Std, 
                        ymax = Comb_mass + Comb_Std, 
                        fill = Sol_PCL)) +
  geom_bar(position = position_dodge(), stat = 'identity') +
  scale_fill_manual(
  name = "Production parameter", 
  labels = c("CF_12_d", "CF_17.5_w", "DCM_12_d", "DCM_17.5_w"),
  values = c("dodgerblue4", "firebrick4", "limegreen", "turquoise")) +
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.5) + 
  theme_classic() +
  labs(x = "Coating time (hours)", y = "Mass of antibody (ng)")

Mass_plot

ggsave("Combined masses.png")
