## Week 4 HW: Calculating and Plotting Summary Statistics for penguin data ##
## Mei Iwamoto ##
## September 15th, 2026 ##

#install packages

#load libraries 
library(palmerpenguins)
library(dplyr)
library(tidyverse)


#view data
view(penguins)

#calculate the mean and variance of body mass by species, island, and sex without any NAs
penguins |> 
  drop_na(species, island, sex) |> 
  group_by(species, island, sex) |> 
  summarize(mean_body_mass = mean(body_mass_g), body_mass_variance = var(body_mass_g))

#filters out (i.e. excludes) male penguins, then calculates the log body mass, then selects only the columns for species, island, sex, and log body mass, then use these data to make any plot. Make sure the plot has clean and clear labels and follows best practices. Save the plot in the correct output folder.

penguins |> 
  filter(sex=="female") |> 
  mutate(log_body_mass = log(body_mass_g)) |> 
  select(species, island, sex, log_body_mass) |> 
  ggplot(aes(x=species, y=log_body_mass)) +
  geom_boxplot() +
  labs(title = "Log Body Mass of Penguin Species", x="Species", y="Log Body Mass")

#save plot in outputs folder 
  ggsave(here("Week_04","Outputs","LogBody.png"), width = 7, height = 5)


  