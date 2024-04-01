install.packages("tidyverse")
install.packages("ggmap")
install.packages("ggthemes")
install.packages("maps")
library(tidyverse)
library(ggplot2)
library(maps)
library(ggmap)
library(ggmaps)
library(ggthemes)
library(lubridate)
library(forcats)
Nov2023 <- read_csv("202311-divvy-tripdata.csv")

register_stadiamaps("bf4fda8f-becd-49b9-8421-4dc6bcfd41f1", write = TRUE)

#we know that there can't be data that have NA as a time or member so we cleaned the data such that we don't have any NA data for those metrics.
Nov2023 <- Nov2023 %>%
  drop_na(started_at, member_casual, end_lat, end_lng)


#make a new column that takes the hour 
Nov2023$hour <- hour(Nov2023$started_at)

#hourly usage for members
hourly_usage_member <- Nov2023 %>%
  filter(member_casual == "member")%>%
  group_by(hour, member_casual) %>%
  summarize(count = n())

ggplot(data = hourly_usage_member)+
  geom_line(mapping = aes(x = hour, y = count))+
  scale_x_continuous(breaks = seq(0, 23, by = 1))+
  labs(x = "Hour of the Day", y = "Number of Bike Usage During Each Hour", title = "Usage of Bikes Throughout the Day by Members November 2023")+
  annotate("text", x = 8, y = 21000, label = "Peak at 8AM")+
  annotate("text", x = 17, y = 28000, label = "Peak at 5PM")

#hourly usage for casuals
hourly_usage_casual <- Nov2023 %>%
  filter(member_casual == "casual")%>%
  group_by(hour, member_casual) %>%
  summarize(count = n())

ggplot(data = hourly_usage_casual)+
  geom_line(mapping = aes(x = hour, y = count))+
  scale_x_continuous(breaks = seq(0,23, by = 1))+
  labs(x = "Hour of the Day", y = "Number of Bike Usage During Each Hour", title = "Usage of Bikes Throughout the Day by Casuals November 2023")+
  annotate("text", x = 8, y = 5000, label = "Peak at 8AM")+
  annotate("text", x = 16, y = 9500, label = "Peak at 4PM")

#see that peaks are highest at 4-5pm and 8am so maybe we should look at the location of the peaks at 4pm and 8am for casuals and members individually
five_pm_coordinates <- Nov2023 %>%
  filter(hour == "17") %>%
  group_by(end_lat, end_lng, hour)

five_pm_coordinates_member <- five_pm_coordinates %>%
  filter(member_casual == "member") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = five_pm_coordinates_member, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Member's End location at 5PM November 2023")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())

four_pm_coordinates <- Nov2023 %>%
  filter(hour == "16") %>%
  group_by(end_lat, end_lng, hour)

four_pm_coordinates_casual <- four_pm_coordinates %>%
  filter(member_casual == "casual") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = four_pm_coordinates_casual, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Casual's End location at 4PM November 2023")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())

#filter coordinates at 8am
eight_am_coordinates <- Nov2023 %>%
  filter(hour == "8") %>%
  group_by(end_lat, end_lng, hour)

eight_am_coordinates_member <- eight_am_coordinates %>%
  filter(member_casual == "member") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = eight_am_coordinates_member, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Member's End location at 8AM November 2023")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())

eight_am_coordinates_casual <- eight_am_coordinates %>%
  filter(member_casual == "casual") %>%
  group_by(end_lat, end_lng, hour)

qmplot(end_lng, end_lat, data = eight_am_coordinates_casual, maptype = "stamen_terrain", alpha = "ended_at")+
  labs(title = "Casual's End location at 8AM November 2023")+
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.key = element_blank())



#changing all the dates to just dates so we can do a histogram and see the time for just member or casual later.
Nov2023$started_at <- format(Nov2023$started_at, "%m-%d-%y")


#separated members and casuals so we can determine count for each individual one (MEMBER)

member_summary <- Nov2023 %>%
  group_by(started_at, member_casual)%>%
  filter(member_casual=="member")%>%
  summarize(count= n())

#separated members and casuals so we can determine count for each individual one (CASUAL)
casual_summary <- Nov2023 %>%
  group_by(started_at, member_casual)%>%
  filter(member_casual=="casual")%>%
  summarize(count = n())

#bargraph showing just the amount of member riders for each date
ggplot(data = member_summary)+
  geom_col(mapping = aes(x = started_at, y = count,), fill = "turquoise3")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  annotate("text", x = 15, y = 17000, label = "Veterans Day 11/10")+
  annotate("text", x = 15, y = 16000, label = "Thanksgiving 11/23")+
  labs(title = "Amount of Member Riders Throughout November 2023", y = "Number of Riders", x = "Date")


#bargraph showing just the amount of casual member riders for each date
ggplot(data = casual_summary)+
  geom_col(mapping = aes(x = started_at, y = count,), fill = "indianred1")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0))+
  annotate("text", x = 15, y = 7500, label = "Veterans Day 11/10")+
  annotate("text", x = 15, y = 6500, label = "Thanksgiving 11/23")+
  labs(title = "Amount of Casual Riders Throughout November 2023", y = "Number of Riders", x = "Date")

#find out which day of the week is used most often throughout February and reorganize so monday is on the left and sunday is on the right
Nov2023$started_at <- as.Date(Nov2023$started_at, format = "%m-%d-%y")
Nov2023$day_of_week <- weekdays(Nov2023$started_at)
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
Nov2023$day_of_week <-fct_relevel(Nov2023$day_of_week, day_order)


#keep track of the records for all users
day_counts <- table(Nov2023$day_of_week)
day_counts_df <- as.data.frame(day_counts)
colnames(day_counts_df) <- c("day_of_week", "count")

#keep track of the records for members
member_records_dates <- Nov2023 %>%
  filter(member_casual == "member")%>%
  group_by(day_of_week, member_casual) %>%
  summarize(count = n())

#keep track of the records for casuals
casual_records_dates <- Nov2023 %>%
  filter(member_casual == "casual")%>%
  group_by(day_of_week, member_casual) %>%
  summarize(count = n())


#make bargraph for day of week vs count and have member/casual on top of eachother
ggplot(data = Nov2023)+
  geom_bar(mapping=aes(x = day_of_week, fill=member_casual))+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week November 2023", fill = "Type of Member")

#bargraph for day of week vs count (members only)
ggplot(data = member_records_dates)+
  geom_col(mapping=aes(x = day_of_week, y = count), fill = "turquoise3")+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week for Members November 2023")

#bargraph for day of week vs count (casuals only)
ggplot(data = casual_records_dates)+
  geom_col(mapping=aes(x = day_of_week, y = count), fill = "indianred1")+
  labs(x = "Days of the Week", y = "Count", title = "Counts of Each Day of the Week for Casuals November 2023")

