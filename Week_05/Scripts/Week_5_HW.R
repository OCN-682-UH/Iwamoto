## Week 5 Homework: Conductivity and depth data ##
## Mei Iwamoto ##
## September 22nd, 2026 ##

#install.packages("remotes")

#new packages from github 
remotes::install_github("R-CoderDotCom/ggcats@main") #cat plot
devtools::install_github('Mikata-Project/ggthemr') #cool color themes for ggplot

#load libaries
library(tidyverse)
library(here)
library(ggcats)
library(remotes)
library(devtools)
library(ggthemr)

#set new theme for plots
ggthemr("dust")

#read in conductivity and depth data
cond <- read.csv(here("Week_05", "Data", "CondData.csv"))
depth <- read.csv(here("Week_05", "Data", "DepthData.csv"))

view(cond)
view(depth)

#convert date columns, round date time column to 10 seconds to match depth data 
newcond <- cond |> 
  mutate(datetime = mdy_hms(date), 
         newdate = round_date(datetime, "10 seconds")) #round_date should be inside mutate

newdepth <- depth |> 
  mutate(newdate = ymd_hms(date)) 

#join the two datasets together by newdate column 
joindata <- inner_join(newcond, newdepth, join_by(newdate))

#Calculate averages of date, depth, temperature, and salinity by minute, do by rounddate again 
avgdata <- joindata |> mutate(avgmin=round_date(newdate, "minute")) |> 
  group_by(avgmin) |> 
  summarise(mean_depth=mean(Depth, na.rm=TRUE), mean_temp=mean(Temperature, na.rm=TRUE), mean_sal=mean(Salinity, na.rm=TRUE), mean_date=(mean(newdate))) #not sure what average of dates means, so just took the mean of the newdate column 

#Plot showing Mean temperature recorded per minute over time 
p <- ggplot(data=avgdata,
       mapping = aes(x=avgmin, y=mean_temp))+
  geom_point() +
  labs(title="Mean Temperature Recorded per minute", x="Time of Day", y="Temperature (C)") 

ggsave(here("Week_05", "Outputs", "HWplot.png"), width = 5, height = 7)

#cat plot (for fun)
ggplot(data=avgdata) +
  geom_cat(aes(avgmin, mean_temp), cat="nyancat", size=5) +
  labs(title = "Mean Temperature Recorded per min", x="Time of Day", y="Temperature (C)")
