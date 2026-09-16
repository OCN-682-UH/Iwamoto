## Week 4B lecture on TidyR ##
## Mei Iwamoto ##
## September 15, 2026 ##

#load libraries
library(tidyverse)
library(here)

#load data
chemdata <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
glimpse(chemdata)

#another way to remove the NAs
chemdata_clean <- chemdata |> 
  filter(complete.cases(ChemData)) # filters out everything that is not a complete row, but drop na is preferred? 

#should separate the column Tide_time into two separate columns to tidy the data better 
?separate_wider_delim #to look at different options 

chemdata_clean <- chemdata |> 
  filter(complete.cases(chemdata)) |> 
  separate_wider_delim(cols  = Tide_time,
                     delim = "_",
                     names = c("Tide", "Time")) #note this does delete the original column, to keep it we would do cols_remove = FALSE

head(chemdata_clean)

#to combine two columns together, use paste function
#paste("Maunalua", "Fringing", sep = "."), joins x and y with a . between them

#use paste inside mutate function to create a new combined column 
chemdata_clean <- chemdata |> 
  filter(complete.cases(chemdata)) |> 
  separate_wider_delim(cols  = Tide_time,
                       delim = "_",
                       names = c("Tide", "Time")) |>  
  mutate(Site_Zone = paste(Site, Zone, sep = "."))

head(chemdata_clean)


## Pivoting the dataset between wide and long ##
#where wide data has one observation per row and all the columns are the variables
#long datasets have one unique measurement per row and all the unique info about that measurement in the same row 

#long format is easier to summarize by groupby, and easier to use facet wrap with 

#use pivot longer function to pivot it from a wide to long dataset 
chemdata_long <- chemdata_clean |> 
  pivot_longer(cols = Temp_in:percent_sgd, # select columns to pivot
                names_to  = "Variables",         # new column for old column names
                values_to = "Values")            # new column for the values)

#use the long dataset to calculate the mean and variance for all variables at each site 
chemdata_long |>
  group_by(Variables, Site) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE))

#Calculate mean, variance, and standard deviation for all variables by site, zone, and tide
chemdata_long |>
  group_by(Site, Zone, Tide) |>
  summarise(Param_means = mean(Values, na.rm = TRUE),
            Param_vars  = var(Values,  na.rm = TRUE),
            Param_stdev = sd(Values, na.rm = TRUE))

## Using facet_wrap with a long dataset ##
#create boxplots of every parameter for each site
chemdata_long |> 
  ggplot(aes(x=Site, y=Values)) +
  geom_boxplot() +
  facet_wrap(~Variables)

#fix the axes so the plot looks better by adding scales=free, which releases both axes 
chemdata_long |> 
  ggplot(aes(x=Site, y=Values)) +
  geom_boxplot() +
  facet_wrap(~Variables, scales = "free")

#to export as csv, do write_csv() with a |> 
#write_csv(here("Week_04", "output", "summary.csv"))
