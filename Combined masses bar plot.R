# tidyverse for dplyr and ggplot2, ggpubr for 
library(tidyverse)
library(ggpubr)

# Combines two dataframes with means from absorbance and supernatants by parameters: 
# solvent, PCL concentration and coating time.
Combined_mass <- left_join(Mean_absorbance, Mean_abs_supernatant, by = c("Solvent", "PCL_concentration", "Coating_time"))

# Cleans up the dataframe by deselecting unnecessary info, mutate creates new combined mass and combined std., last select
# removes now unnecessary variables, Std and Masses for individual elements.
Combined_mass <- Combined_mass %>% select(-c(Mean.x, Mean.y, Adj_mean.x, Adj_mean.y)) %>% 
  mutate(Comb_mass = Mass_ab.x + Mass_ab.y, Comb_Std = sqrt((4*(Std.x)^2+4*(Std.y)^2)/8)) %>% 
  select(-c(Std.x, Std.y, Mass_ab.x, Mass_ab.y))

# Creates a common variable based on solvent and PCL conc.
Combined_mass$Sol_PCL <- paste(Combined_mass$Solvent, Combined_mass$PCL_concentration, sep = "_")


# Creates a plot of x = coating time, y = combined mass, with fill/color = solution and PCL conc.
Mass_plot <- ggplot(data = Combined_mass,
                    aes(x = factor(Coating_time), 
                        y = Comb_mass, 
                        ymin = Comb_mass - Comb_Std, 
                        ymax = Comb_mass + Comb_Std, 
                        fill = Sol_PCL)) +
  # Uses geom_bar with stat = 'identity' to allow mapping y as something different than bin (or number of cases)
  # position = position_dodge() to unstack the bars.
  geom_bar(position = position_dodge(), stat = 'identity') +
  
  # Manually labels legend & x-axis and choses colors for bars
  scale_fill_manual(
  name = "Production parameter", 
  labels = c("CF_12_d", "CF_17.5_w", "DCM_12_d", "DCM_17.5_w"),
  values = c("dodgerblue4", "firebrick4", "limegreen", "turquoise")) +
  
  # geom_errorbar adds error bars to the plot, again with position dodge
  geom_errorbar(position = position_dodge(width = 0.9), width = 0.5) + 
  
  # changes the theme of the plot to classic ("publication ready")
  theme_classic() +
  
  # Adds x-axis and y-axis titles
  labs(x = "Coating time (hours)", y = "Mass of antibody (ng)")

# shows plot
Mass_plot

# saves plot to working directory
ggsave("Combined masses.png")
