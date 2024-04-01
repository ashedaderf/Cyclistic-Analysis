install.packages("tidyverse")
install.packages("purrr")
library(tidyverse)
library(ggplot2)
library(maps)
library(ggmap)
library(ggmaps)
library(ggthemes)
library(lubridate)
library(forcats)

Jan2024 <- read_csv("202401-divvy-tripdata.csv")

register_stadiamaps("bf4fda8f-becd-49b9-8421-4dc6bcfd41f1", write = TRUE)

#we know that there can't be data that have NA as a time or member so we cleaned the data such that we don't have any NA data for those metrics.
Jan2024 <- Jan2024 %>%
  drop_na(started_at, member_casual, end_lat, end_lng)


#make a new column that takes the hour 
Jan2024$hour <- hour(Jan2024$started_at)

#hourly usage for members
hourly_usage_member <- Jan2024 %>%
  filter(member_casual == "member")%>%
  group_by(hour, member_casual) %>%
  summarize(count = n())

ggplot(data = hourly_usage_member)+
  geom_line(mapping = aes(x = hour, y = count))+
  scale_x_continuous(breaks = seq(0, 23, by = 1))+
  labs(x = "Hour of the Day", y = "Number of Bike Usage During Each Hour", title = "Usage of Bikes Throughout the Day by Members January 2024")+
  annotate("text", x = 8, y = 11000, label = "Peak at 8AM")+
  annotate("text", x = 17, y = 14000, label = "Peak at 5PM")

#hourly usage for casuals
hourly_usage_casual <- Jan2024 %>%
  filter(member_casual == "casual")%>%
  group_by(hour, member_casual) %>%
  summarize(count = n())

ggplot(data = hourly_usage_casual)+
  geom_line(mapping = aes(x = hour, y = count))+
  scale_x_continuous(breaks = seq(0,23, by = 1))+
  labs(x = "Hour of the Day", y = "Number of Bike Usage During Each Hour", title = "Usage of Bikes Throughout the Day by Casuals January 2024")+
  annotate("text", x = 8, y = 1500, label = "Peak at 8AM")+
  annotate("text", x = 16, y = 2500, label = "Peak at 4PM")

#see that peaks are highest at 4-5pm for casuals and 5-6pm for members and 8am for both so maybe we should look at the location of the peaks at 4pm and 8am for casuals and members individually
#members
five_pm_coordinates <- Jan2024 %>%
  filter(hour == "17") %>%
  group_by(end_lat, end_lng, hour)

five_pm_coordinates_member <- five_pm_coordinates %>%
  filter(member_casual == "member") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = five_pm_coordinates_member, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Member's End location at 5PM")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())


#casuals
four_pm_coordinates <- Jan2024 %>%
  filter(hour == "16") %>%
  group_by(end_lat, end_lng, hour)

four_pm_coordinates_casual <- four_pm_coordinates %>%
  filter(member_casual == "casual") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = four_pm_coordinates_casual, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Casual's End location at 4PM")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())

#filter coordinates at 8am
#member
eight_am_coordinates <- Jan2024 %>%
  filter(hour == "8") %>%
  group_by(end_lat, end_lng, hour)

eight_am_coordinates_member <- eight_am_coordinates %>%
  filter(member_casual == "member") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = eight_am_coordinates_member, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Member's End location at 8AM")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())

#casual
eight_am_coordinates <- Jan2024 %>%
  filter(hour == "8") %>%
  group_by(end_lat, end_lng, hour)

eight_am_coordinates_casual <- eight_am_coordinates %>%
  filter(member_casual == "casual") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = eight_am_coordinates_casual, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Casual's End location at 8AM")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())



#changing all the dates to just dates so we can do a histogram and see the time for just member or casual later.
Jan2024$started_at <- format(Jan2024$started_at, "%m-%d-%y")


#separated members and casuals so we can determine count for each individual one (MEMBER)
member_summary <- Jan2024 %>%
  group_by(started_at, member_casual)%>%
  filter(member_casual=="member")%>%
  summarize(count= n())

#separated members and casuals so we can determine count for each individual one (CASUAL)
casual_summary <- Jan2024 %>%
  group_by(started_at, member_casual)%>%
  filter(member_casual=="casual")%>%
  summarize(count = n())

#bargraph showing just the amount of member riders for each date
#note that 1/1 is new years so maybe no work
#note that 1/15 is mlk day
ggplot(data = member_summary)+
  geom_col(mapping = aes(x = started_at, y = count,), fill = "turquoise3")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  annotate("text", x = 15, y = 9000, label = "New Years Day 1/1")+
  annotate("text", x = 15, y = 9500, label = "Martin Luther King Jr. day 1/15")+
  labs(title = "Amount of Member Riders Throughout January 2024", y = "Number of Riders", x = "Date")


#bargraph showing just the amount of casual member riders for each date
ggplot(data = casual_summary)+
  geom_col(mapping = aes(x = started_at, y = count,), fill = "indianred1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  annotate("text", x = 15, y = 1750, label = "New Years Day 1/1")+
  annotate("text", x = 15, y = 2000, label = "Martin Luther King Jr. day 1/15")+
  labs(title = "Amount of Casual Riders Throughout January 2024", y = "Number of Riders", x = "Date")

#find out which day of the week is used most often throughout February and reorganize so monday is on the left and sunday is on the right
Jan2024$started_at <- as.Date(Jan2024$started_at, format = "%m-%d-%y")
Jan2024$day_of_week <- weekdays(Jan2024$started_at)
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
Jan2024$day_of_week <-fct_relevel(Jan2024$day_of_week, day_order)


#keep track of the records for all users
day_counts <- table(Jan2024$day_of_week)
day_counts_df <- as.data.frame(day_counts)
colnames(day_counts_df) <- c("day_of_week", "count")

#keep track of the records for members
member_records_dates <- Jan2024 %>%
  filter(member_casual == "member")%>%
  group_by(day_of_week, member_casual) %>%
  summarize(count = n())

#keep track of the records for casuals
casual_records_dates <- Jan2024 %>%
  filter(member_casual == "casual")%>%
  group_by(day_of_week, member_casual) %>%
  summarize(count = n())


#make bargraph for day of week vs count and have member/casual on top of eachother
ggplot(data = Jan2024)+
  geom_bar(mapping=aes(x = day_of_week, fill=member_casual))+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week", fill = "Type of Member")

#bargraph for day of week vs count (members only)
ggplot(data = member_records_dates)+
  geom_col(mapping=aes(x = day_of_week, y = count), fill = "turquoise3")+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week for Members")

#bargraph for day of week vs count (casuals only)
ggplot(data = casual_records_dates)+
  geom_col(mapping=aes(x = day_of_week, y = count), fill = "indianred1")+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week for Casuals")
