## Week 4 lecture
## Mei Iwamoto
## September 15, 2026

#load libraries 
library(palmerpenguins)
library(tidyverse)
library(here)
library(dplyr)

#look at penguin data 
glimpse(penguins)
head(penguins)

#use the filter function to filter for female penguins in the dataset, use == which reads as 'is exactly equal to'. one equal sign just sets an arguement in the function 
filter(penguins, sex == "female")

#how to filter dataset to get penguins measured in year 2008?
filter(penguins, year=="2008")


#penguins that have a body mass greater than 5000
filter(penguins, body_mass_g > 5000)

#filter with multiple conditions
filter(penguins, sex=="female", body_mass_g > 5000)

#penguins collected in either 2008 or 2009
filter(penguins, year=="2008"|year=="2009")

#penguins not from island Dream
filter(penguins, !island=="Dream")

#penguins in the species adelie and gentoo
filter(penguins, !species=="Chinstrap")

#can use the in operator %in% for multiple conditions! add example here later 

#mutate function to add new column to data set, can make multiple columns at once
mutate(penguins, body_mass_kg = body_mass_g/1000) #here body mass kg is the new column, which you get by calculated with preexisting column body mass g 

#can use the across() function to apply the same function to many columns within mutate but we will come back to this later

#ifelse function - to do conditional tests within mutate, where ifelse(test, if true this value, if false this value)
mutate(penguins, 
       after_2008 = if_else(year>2008, "After 2008", "Before 2008"))

#create new column to add flipper length and body mass together
mutate(penguins, bodyflip = flipper_length_mm + body_mass_g)

#create a new column where body mass greater than 4000 is labeled as big, everything else is small
mutate(penguins,
       chonk = if_else(body_mass_g > 4000, "big", "small"))

#|> #do command shift M to enter this pipe, which is built into the native r instead of the %>% pipe, which is a part of the tidyverse package

#select function - to select certain columns to remain in the dataset, can also rename columns with this function 
penguins |>
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(Species = species, island, sex, log_mass)

#arrange function to sort rows by a column, ascending by default, can do descending by adding desc() inside the arrange function 
penguins |>
  arrange(desc(body_mass_g))

#summarise function calculates a table of summarized data, can keep on adding multiple summarized data thingies (like mean, min, max, etc)
penguins |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper  = min(flipper_length_mm, na.rm = TRUE))

#group by function, can use with summarize function, where you group the data by diff groups before calculating a value for each group. can also group by more than one thing 
penguins |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE))

#can use the count function to count the number of rows per group, helpful to know the number of observations 
penguins |>
  count(species)

#to remove NAs from a specific column, use drop_na()
penguins |>
  drop_na(sex)

penguins |>
  drop_na(sex) |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#to pipe straight into a ggplot
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()


