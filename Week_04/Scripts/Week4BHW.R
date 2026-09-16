## Week 4B HW ##
## Mei Iwamoto ##
## September 16th, 2026 ##

#load libraries
library(tidyverse)
library(here)

#load in data
chemdata <- read.csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
head(chemdata)

#clean data, remove NAs and separate Time_time column into two columns 
chemdata_clean <- chemdata |> 
  filter(complete.cases(chemdata)) |> 
  separate_wider_delim(cols  = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time"))

#use pivot longer function to pivot it from a wide to long dataset 
chemdata_long <- chemdata_clean |> 
  pivot_longer(cols = Temp_in:percent_sgd, # select columns to pivot
               names_to  = "Variables",         # new column for old column names
               values_to = "Values")            # new column for the values)

view(chemdata_long)

#filter out site W, and summarize the mean values and variance of variables based on Season and Tide
chemdata_long |> 
  filter(!Site=="W") |> 
  group_by(Variables, Season, Tide) |> 
  summarize(Value_means = mean(Values, na.rm = TRUE),
            Value_vars  = var(Values,  na.rm = TRUE)) |> 
  write_csv(here("Week_04", "Outputs","HWsummary.csv")) #export the summary statistics as a csv
  

#make a plot showing salinity measured at Site BP in the Fall for high vs low tide 
chemdata_long |> 
  filter(!Site=="W", !Season=="Spring", Variables=="Salinity") |> 
  ggplot(aes(Values, color=Tide)) +
  geom_density() +
  labs(title = "Salinity Measured at Site BP for High and Low Tide", subtitle = "At Maunalua Bay", x="Salinity", y="Number of Measurements") 

#save plot as png
  ggsave(here("Week_04","Outputs","4BHWplot.png"), width = 7, height = 5)



