## Week 5 Lecture ##
## Mei Iwamoto ##
## September 22nd, 2026 ##

#libaries
library(tidyverse)
library(here)

#load data in
topt <- read.csv(here("Week_05", "Data", "Topt_data.csv"))
site_ch <- read.csv(here("Week_05", "Data", "site.characteristics.data.csv"))

## joining data sets example ##

#create first tibble
T1 <- tibble(
  Site.ID = c("A", "B", "C", "D"),
  Temperature = c(14.1, 16.7, 15.3, 12.8)
)

T1

#create second tibble
T2 <- tibble(
  Site.ID = c("A", "B", "D", "E"),
  pH = c(7.3, 7.8, 8.1, 7.9)
)

T2

#do a left join, which keeps all rows from the left/first dataframe and adds matching rows from the right 
left_join(T1, T2) #here Site C stays from T1, but E is excluded because it's only in T2 


#right join does the opposite
right_join(T1, T2) #so now E is included and C is excluded

#inner join keeps only rows that exists in both dataframes, so it's a good way to drop NAs
inner_join(T1, T2) #here sites C and E are dropped 

#full join to keep all rows from both dataframes
full_join(T1, T2) #so here all sites are kept, and NA is filling in missing values 

#semi join keeps rows from first dataframe that matches from the second, but only returns columns from the first 
semi_join(T1, T2) #here only ABD, and the temp column remain 

#anti join returns rows from the first dataframe that don't match the second, can be useful when you have a big dataset for finding missing data!
anti_join(T1, T2) #here you only get site c, which has no match in T2 

#if you join by different column names, you can use the by argument to still join them together! ideally you could go and change column names to match first 
T3 <- tibble(
  SiteID = c("A", "B", "C", "D"),  # Note: different name!
  Chlorophyll = c(2.3, 3.1, 1.9, 2.8)
)

T3

left_join(T1, T3, by = c("Site.ID" = "SiteID")) #The syntax is: by = c("left_table_col" = "right_table_col")

#to join by multiple columns, specify all the columns 
T4 <- tibble(
  Site.ID = c("A", "A", "B", "B"),
  Year = c(2020, 2021, 2020, 2021),
  Biomass = c(12.5, 15.3, 18.2, 16.9)
)

T5 <- tibble(
  SiteID = c("A", "A", "B"),
  Year = c(2020, 2021, 2021)
)
  
left_join(T4, T5, by = c("Site.ID" = "SiteID", "Year" = "Year")) 

#handling naming conflicts, where they have columns with the same name but you're not joining by them you'll get .x and .y suffixes 
T6 <- tibble(
  Site.ID = c("A", "B", "C"),
  Notes = c("pristine", "degraded", "moderately impaired")
  )

T7 <- tibble(
  Site.ID = c("A", "B", "D"),
  Notes = c("sunny", "shaded", "partially shaded"),
  Quality = c("good", "fair", "poor")
  )

# Don't specify how to join — creates ambiguity with 'Notes'
left_join(T6, T7, by = "Site.ID")

#to avoid naming conflicts in the first place, you should rename columns before joining to avoid confusion 
T6_renamed <- T6 |> 
  rename(Condition_Notes = Notes)

T7_renamed <- T7 |> 
  rename(Habitat_Notes = Notes)

left_join(T6_renamed, T7_renamed, by = "Site.ID")


## Dates and Times with lubridate ##
#lubridate is already loaded with tidyverse package 
#can use now function to get current time
now()

#can get current time in different timezones
now(tzone="GMT")

#just the date would be, and can also do with diff timezones 
today()

#time checks
am(now())        # Is it morning?

leap_year(now()) # Is it a leap year?

#to use lubridate function, your date format has to be a character string, use as.character() as needed #to convert dates: iso format (yyyy-mm-dd)
ymd("2021-02-24")

#us format
mdy("02/24/2021")

#written months
mdy("February 24 2021")

#european format
dmy("24/02/2021")

#for date and time: you can add _hms() or _hm() after
ymd_hms("2021-02-24 10:22:20 PM")
mdy_hms("02/24/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")

#vector of date times must all have the same format, all in quotes because they have to be characters 
datetimes <- c(
  "02/24/2021 22:22:20",
  "02/25/2021 11:21:10",
  "02/26/2021 8:01:52"
)

datetimes

#convert vector to convert all of them 
datetimes <- mdy_hms(datetimes)
datetimes

#to extract the month as a number, if you wanna plot just by month for example 
month(datetimes)
#to extract names of month abbreviated, can do label=true
month(datetimes, label=TRUE)
#for full name, aka non abbreviated
month(datetimes, label = TRUE, abbr = FALSE)

#to extract days of month
day(datetimes)
#day of the week
wday(datetimes, label = TRUE)
#hour, min, sec
hour(datetimes)
minute(datetimes)
second(datetimes)

#to add time intervals, like 4 hours, like if timezones were different?
datetimes + hours(4) #when you add something, you make it plural - hours vs hour 
#to add two days
datetimes + days(2) #can add any time component! 

#you can also round up, like to the nearest minute
round_date(datetimes, "minute") 
round_date(datetimes, "5 mins") #or to 5 mins 

#if your data comes from diff timezones 
# Create a datetime WITHOUT timezone info
datetime_naive <- mdy_hms("02/24/2021 10:22:20")
datetime_naive

#with_tz - funtion to view same moment in diff timezone
# Assume the naive time is in Hawaii
hawaii_time <- with_tz(datetime_naive, tzone = "US/Hawaii")
hawaii_time
# Same moment, viewed from EST
est_time <- with_tz(hawaii_time, tzone = "EST")
est_time

#to change the timezone label instead of CONVERTING it, using force_tz 
# Claim this was collected in Hawaii (though it was naive)
force_hawaii <- force_tz(datetime_naive, tzone = "US/Hawaii")
force_hawaii
# Now convert to EST (this changes the clock time!)
with_tz(force_hawaii, tzone = "EST")

#read in cond data and convert the date column to a datetime using a pipeline
cond <- read.csv(here("Week_05", "Data","CondData.csv"))
head(cond)

cond |> 
  mutate(datetime=mdy_hms(date))

#to join topt and site characteristics data, pivot site characteristics to wide and then join it 
view(topt)
view(site_ch)

#pivot site ch to wide 
wide_site <- pivot_wider(site_ch, names_from = "parameter.measured",
                         values_from = "values")

#to join it 
full <- full_join(wide_site, topt)



