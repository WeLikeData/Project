data = read.csv("311_Call_Center_Tracking_Data.csv")
service_request <- read.csv("MyLA311_Service_Request_Data_2016.csv")
str(data)

library(tidyverse)
library(lubridate)
library(ggmap)
library(gridExtra)
data$Date <- mdy(data$Date)
data$Time <- hms(data$Time)
service_request$CreatedDate <- mdy_hms(service_request$CreatedDate)
service_request$UpdatedDate <- mdy_hms(service_request$UpdatedDate)
service_request$year_created <- year(service_request$CreatedDate)
data$year <- year(data$Date)
data$month <- month(data$Date, label = T)
service_request$month <- month(service_request$CreatedDate, label = T)


############### Percent of calls handled vs referred

unique(data$Call.Resolution)
table(data$Call.Resolution)
unique(service_request$ActionTaken)
table(service_request$ActionTaken)


data3 <- data %>%
  group_by(Call.Resolution) %>%
  summarise(count = n()) %>%
  filter(Call.Resolution != "N/A") %>%
  arrange(-count) %>%
  mutate(percent = round(count/sum(count),2)) %>%
  slice(1:4) %>%
  droplevels()

ggplot(data3, aes(reorder(Call.Resolution,percent), percent*100)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Percentage") +
  xlab("Call Resolution Action") +
  ggtitle("A Snapshot of calls Handled vs Referred") +
  theme_bw() +
  geom_text(aes(label = percent*100), hjust = -0.2) 

## Note: As evident from above analysis, 41% of the calls resulted in Transfer(City). However, the service
## requests processed + Caller Information account for combined 51%


############### Percent of calls handled vs referred (OVER TIME DELIVERY OF SERVICE)
### By Year

data4 <- data %>%
  group_by(year, Call.Resolution) %>%
  summarise(count = n()) %>%
  filter(Call.Resolution != "N/A" & year != "NA" & year != "2015") %>%
  mutate(percent = round(count/sum(count),2))

ggplot(data4, aes(reorder(Call.Resolution, percent), percent*100)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Percentage") +
  xlab("Call Resolution Action") +
  ggtitle("A Snapshot of calls Handled vs Referred") +
  theme_bw() +
  facet_wrap(~year) +
  geom_text(aes(label = percent*100), hjust = -0.2, size = 3)

## Not a valid comparison as 2015 year data is limited so removed 2015

###########

### TREND LINE OVER YEARS - (from months jan to may) as 2015 year has data only till may

data6 <- data %>%
  filter(Call.Resolution != "N/A" & year != "NA") %>%
  filter(month < 6) %>%
  group_by(year, Call.Resolution) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count),2)) %>%
  arrange(-percent) %>%
  slice(1:3)

ggplot(data6, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
  geom_line() +
  ylab("Percentage") +
  xlab("Year") +
  ggtitle("Trend of Call Resolution over Years (Months Jan-May)") +
  theme_bw() 

### TREND LINE OVER YEARS - (from months june to Dec) for 2011-2014

data7 <- data %>%
  filter(Call.Resolution != "N/A" & year != "NA") %>%
  filter(month > 5 & month < 13) %>%
  group_by(year, Call.Resolution) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count),2)) %>%
  arrange(-percent) %>%
  slice(1:3)

ggplot(data7, aes(factor(year), percent*100, group = Call.Resolution, color = Call.Resolution)) +
  geom_line() +
  ylab("Percentage") +
  xlab("Year") +
  ggtitle("Trend of Call Resolution over Years (June-Dec months)") +
  theme_bw()

### SEASONALITY --- TREND LINE OVER MONTHS - (from months june to Dec) for 2011-2014

data8 <- data %>%
  filter(Call.Resolution != "N/A" & year != "NA") %>%
  filter(year != "2015") %>%
  group_by(month, Call.Resolution) %>%
  summarise(count = n()) %>%
  mutate(percent = round(count/sum(count),2))

ggplot(data8, aes(factor(month), percent*100, group = Call.Resolution, color = Call.Resolution)) +
  geom_line() +
  ylab("Percentage") +
  xlab("Month") +
  ggtitle("Trend of Call Resolution (years considered 2011-2014)") +
  theme_classic()



##################### Change in volume of requests and input type (app/call) over time

##### CHANGE IN VOLUME OF REQUESTS - DATASET 1

## - YEARLY

data %>%
  filter(year != "2015") %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  ggplot(aes(year, count)) +
  geom_line() +
  geom_point(size = 1) +
  scale_y_continuous(limits = c(0,910000)) +
  ggtitle("Trend in volume of requests from 2011-2014")

## - MONTHLY

# For year 2011
data_2011 <- data %>%
  filter(year == "2011")

m1 <- ggplot(data_2011, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2011)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000))

# For year 2012
data_2012 <- data %>%
  filter(year == "2012")

m2 <- ggplot(data_2012, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2012)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000))

# For year 2013
data_2013 <- data %>%
  filter(year == "2013")

m3 <- ggplot(data_2013, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2013)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000))


# For year 2014
data_2014 <- data %>%
  filter(year == "2014")

m4 <- ggplot(data_2014, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2014)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000))


# For year 2015
data_2015 <- data %>%
  filter(year == "2015")

m5 <- ggplot(data_2015, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2015 - Jan-May)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,90000), breaks = seq(0,90000,10000))

grid.arrange(m1,m2,m3,m4)

## CHANGE IN VOLUME OF REQUESTS - DATASET 2

unique(service_request$RequestSource)
table(service_request$RequestSource)
range(service_request$CreatedDate)

service_request_2015 <- service_request %>%
  filter(year_created == "2015")

service_request_2016 <- service_request %>%
  filter(year_created == "2016")

p1 <- ggplot(service_request_2015, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2015 - Aug-Dec)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,95000), breaks = seq(0,95000,10000))
  
p2 <- ggplot(service_request_2016, aes(factor(month))) +
  geom_bar() +
  ggtitle("No. of service requests (Year 2016)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,95000), breaks = seq(0,95000,10000))

grid.arrange(p1, p2)


## UNDERSTAND INPUT TYPE - Call/App - DATASET 2 FOR YEAR 2016

unique(service_request$RequestSource)
table(service_request$RequestSource)

service_request_call_app_2016 <- service_request %>%
  filter(RequestSource == "Mobile App" | RequestSource == "Call" | RequestSource == "Driver Self Report") %>%
  filter(year_created == "2016")


ggplot(service_request_call_app_2016, aes(factor(month), fill = RequestSource)) +
  geom_bar(position = "dodge") +
  ggtitle("No. of service requests (Year 2016)") +
  xlab("Month") +
  ylab("Count") +
  scale_y_continuous(limits = c(0,40000), breaks = seq(0,40000,10000)) +
  facet_wrap(~RequestType)

