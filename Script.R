## Getting packages ready
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
library(readr)


##Setting work directory
getwd() #displays your working directory
setwd("/Users/vic/Google_analytics_capstone_projects/Case_study_1/Datasets")

## uploading data
ds_2022_06 <- read_csv("202206-divvy-tripdata.csv")
ds_2022_07 <- read_csv("202207-divvy-tripdata.csv")
ds_2022_08 <- read_csv("202208-divvy-tripdata.csv")
ds_2022_09 <- read_csv("202209-divvy-tripdata.csv")
ds_2022_10 <- read_csv("202210-divvy-tripdata.csv")
ds_2022_11 <- read_csv("202211-divvy-tripdata.csv")
ds_2022_12 <- read_csv("202212-divvy-tripdata.csv")
ds_2023_01 <- read_csv("202301-divvy-tripdata.csv")
ds_2023_02 <- read_csv("202302-divvy-tripdata.csv")
ds_2023_03 <- read_csv("202303-divvy-tripdata.csv")
ds_2023_04 <- read_csv("202304-divvy-tripdata.csv")
ds_2023_05 <- read_csv("202305-divvy-tripdata.csv")
ds_2023_06 <- read_csv("202306-divvy-tripdata.csv")

## wrangling data and combine to a single file
#The datasets have the same column names and are consistent with eachother.
str(ds_2022_06)
str(ds_2022_07)
as_tibble(ds_2022_06)


#Combining to a single dataset:
all_trips_ds <- bind_rows(ds_2022_06,ds_2022_07,ds_2022_08,ds_2022_09,ds_2022_10,
                          ds_2022_11,ds_2022_12,ds_2023_01,ds_2023_02,ds_2023_03,
                          ds_2023_04,ds_2023_05,ds_2023_06)

##write_csv(all_trips_ds, file="all_trips_ds.csv") #make a backup
##all_trips_ds <- read_csv("all_trips_ds.csv") #bring it up

##Cleaning up data and prepare for Analysis
str(all_trips_ds)
summary(all_trips_ds)

#Making sure we only have two kinds of labels in the member_casual column
members_casual_ds <- all_trips_ds %>% select(member_casual)
unique(members_casual_ds, incomparables = FALSE)
##counting trips made by these categories. 
table(all_trips_ds$member_casual)

##In order to aggregate data by the date, we can create new columns
##such as month, day, year, days of the week, etc.

all_trips_ds$date <- as.Date(all_trips_ds$started_at) 
all_trips_ds$month <- format(as.Date(all_trips_ds$date), "%m")
all_trips_ds$day <- format(as.Date(all_trips_ds$date), "%d")
all_trips_ds$year <- format(as.Date(all_trips_ds$date), "%Y")
all_trips_ds$day_of_week <- format(as.Date(all_trips_ds$date), "%A")

#Now we add a "ride_length" in minutes
all_trips_ds$ride_length <- difftime(all_trips_ds$ended_at,all_trips_ds$started_at,
                                     units = "mins")
#Checking
str(all_trips_ds)
print(order(all_trips_ds$ride_length>0))

# Convert "ride_length"  to numeric so we can run calculations on the data
all_trips_ds$ride_length <- as.numeric(as.character(all_trips_ds$ride_length))
is.numeric(all_trips_ds$ride_length) #just to check

## Descriptive analysis
#Descriptive analysis on ride_length
mean(all_trips_ds$ride_length > 0) #straight average 
sd(all_trips_ds$ride_length > 0)
median(all_trips_ds$ride_length > 0) #midpoint number 
max(all_trips_ds$ride_length) #longest ride
min(all_trips_ds$ride_length > 0) #shortest ride

 
##Comparing members and casual users
aggregate(all_trips_ds$ride_length>0 ~ all_trips_ds$member_casual, FUN = mean)
aggregate(all_trips_ds$ride_length>0 ~ all_trips_ds$member_casual, FUN = sd)
aggregate(all_trips_ds$ride_length~ all_trips_ds$member_casual, FUN = median)
aggregate(all_trips_ds$ride_length ~ all_trips_ds$member_casual, FUN = max)
aggregate(all_trips_ds$ride_length>0 ~ all_trips_ds$member_casual, FUN = min)


all_trips_ds$day_of_week <-ordered(all_trips_ds$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday","Saturday"))

## Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_ds$ride_length ~ all_trips_ds$member_casual + 
            all_trips_ds$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_ds %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_ds %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title="Number of rides by rider type")+
  ylab("Number of rides")+
  xlab("Weekday")

all_trips_ds %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")+
  labs(title="Average ride duration per weekday")+
  ylab("Average duration (min)")+
  xlab("Weekday")

