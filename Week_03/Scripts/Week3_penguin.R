## Week 3 Penguin data ##
## Mei Iwamoto ##
## September 14th, 2026 ##

#install packages
#install.packages("palmerpenguins") 
#install.packages("praise")
#install.packages("devtools") #to download packages in development
#install.packages("pak") #same as above 

library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(devtools)
library(pak)
library(beyonce) #pak::pkg_install("dill/beyonce") in console

#look at data 
glimpse(penguins)


#make plot (from lecture)
ggplot(data=penguins,
       mapping = aes(x=bill_depth_mm, 
                     y=bill_length_mm,
       color=species)) +
  geom_point() +
  labs(title = "Bill depth and length", subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x="Bill Depth (mm)", y="Bill Length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()

#make new plot for HW, cannot be the same plot!
view(penguins)

#plot for body mass of diff penguin species 

plot1<- ggplot(data=penguins,
       mapping = aes(x=species,
                     y=body_mass_g,
                     color=species
                     )) +
  geom_boxplot() +
  labs(title="Measured Body Mass for Adelie, Chinstrap, and Gentoo Penguins",
       x="Species", y="Body Mass (g)") +
  scale_color_manual(values = beyonce_palette(5))

#save plot as png
ggsave(here("Week_03","Outputs","penguin.png"), 
       width = 7, height = 5)
