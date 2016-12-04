library(ggplot2)
library(dplyr)
library(lubridate)
library(ggmap)
library(gridExtra)

## App vs Phone call referrals, service_requests type question for each input channel
## Overall trends in calls / apps   sums and counts

service_requests <- read.csv("MyLA311_Service_Request_Data_2016.csv")

### investigating unique values for each
unique(service_requests$RequestSource)
unique(service_requests$RequestType)
unique(service_requests$ActionTaken)
unique(service_requests$Status)
unique(service_requests$AssignTo)
unique(service_requests$CD)  ### This is an important location variable 
unique(service_requests$APC) ### This is a good location variable - 8 levels
unique(service_requests$NCName) ### This one has 100 levels so it's tough to analyze, although Hunter said it could be useful
unique(service_requests$PolicePrecinct) ### 22 levels - might be relevant for certain kinds of requests

## Investigation 1
## Investigating trends in call vs app counts over 2016

phoneapp <- service_requests %>%
  mutate(type = ifelse(RequestSource %in% c("Call","Voicemail","Queue Initiated Customer Call"), "phone",
                       ifelse(RequestSource == "Mobile App", "app", 0)))
phoneapp <- phoneapp %>%
  filter(type %in% c("phone","app")) %>%
  droplevels()

### using lubridate
phoneapp$CreatedDate <- ymd_hms(phoneapp$CreatedDate)
phoneapp$UpdatedDate <- ymd_hms(phoneapp$UpdatedDate)
phoneapp$ClosedDate <- ymd_hms(phoneapp$ClosedDate)

### adding a month and year variable
phoneapp$month = month(phoneapp$CreatedDate)
phoneapp$year = year(phoneapp$CreatedDate)

### Looking at the time trend in year 2016 
timetrend_phonevsapp = phoneapp %>%
  group_by(year, month, type)%>%
  summarise(count = n())%>%
  filter(year == "2016")

### We see that Summer months have a slightly higher request count
timetrend_requestcount = ggplot(timetrend_phonevsapp, aes(x = factor(month), y = count))+
  geom_bar(stat = "identity", fill = "steelblue")+
  ggtitle("Service Request Count - 2016")+
  xlab("Month in 2016")+
  ylab("Count of Service Requests")

### Now looking at the percent of requests from app vs phone, over time
month_total = phoneapp %>%
  filter(year == "2016") %>%
  group_by(month)%>%
  summarise(count = n())
timetrend_phonevsapp_final = merge(timetrend_phonevsapp,month_total, by = "month") %>%
  mutate(percent = count.x/count.y)
phonevsapp_overtime = ggplot(timetrend_phonevsapp_final, aes(x = factor(month), y = percent, fill = type))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Percent Of Requests by Phone vs App Over 2016")+
  xlab("Month in 2016")+
  ylab("Percent of Requests")
### in general phone call usage has gone down about 5% in place of app usage 


####### 1 #########
grid.arrange(timetrend_requestcount, phonevsapp_overtime, nrow = 1)   #### can use this in presentation



### Investigation 2
### Looking at the top 4 categories of request types, accounting for 87% of requests.
phoneapp%>%
  group_by(RequestType)%>%
  summarise(count = n())%>%
  mutate(total = sum(count))%>%
  mutate(percent = count/total)%>%
  arrange(-percent)%>%
  slice(1:8)

service_requests%>%
  group_by(RequestType)%>%
  summarise(count = n())%>%
  mutate(total = sum(count))%>%
  mutate(percent = count/total)%>%
  arrange(-percent)


#### 60% of all graffiti removal requests tend to come in from non phone or app requests.
graffit_byrequestsource =
  service_requests%>%
  group_by(RequestType,RequestSource )%>%
  filter(RequestType == "Graffiti Removal")%>%
  summarise(count = n())%>%
  arrange(-count) %>%
  slice(1:5)%>%
  ggplot(aes(x=reorder(RequestSource, -count), y = count))+
  geom_bar(stat = "identity", fill = "darkred")+
  xlab("Request Source")+
  ggtitle("Count of Graffiti Removal Requests by Source")+
  ylab("Count of Graffiti Removal Requests")

### the above is very different than the trend for Bulky Item removal requests (50% of all requests are for bulky items)

bulkyitem_byrequestsource =
  service_requests%>%
  group_by(RequestType,RequestSource )%>%
  filter(RequestType == "Bulky Items")%>%
  summarise(count = n())%>%
  arrange(-count) %>%
  slice(1:5)%>%
  ggplot(aes(x=reorder(RequestSource, -count), y = count))+
  geom_bar(stat = "identity", fill = "darkred")+
  xlab("Request Source")+
  ggtitle("Count of Bulky Item Removal Requests by Source")+
  ylab("Count of Bulky Item Removal Requests")


##### 2 ######
grid.arrange(bulkyitem_byrequestsource,graffit_byrequestsource, nrow =1)



### Investigation 3


### The top 2 request types includes 70% of all phone or app requests
phoneapp%>%
  group_by(RequestType)%>%
  summarise(count = n())%>%
  mutate(total = sum(count))%>%
  mutate(percent = count/total)%>%
  arrange(-percent)%>%
  slice(1:2)

timetrend_requesttype = phoneapp %>%
  group_by(year, month, RequestType)%>%
  summarise(count = n())%>%
  filter(year == "2016")%>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal"))

a = ggplot(timetrend_requesttype, aes(x = factor(month), y = count, fill = RequestType))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Service Request Count By Request Type - 2016")+
  xlab("Month of 2016")+
  ylab("Count of Service Requests by Request Type - 2016")

#### 2 continued
month_total = phoneapp %>%
  filter(year == "2016")%>%
  group_by(month)%>%
  filter(RequestType %in% c("Bulky Items", "Graffiti Removal"))%>%
  summarise(total = n())

timetrend_requesttype_merged = merge(timetrend_requesttype,month_total, by = "month")%>%
  mutate(percent = count/total)

b = ggplot(timetrend_requesttype_merged, aes(x = factor(month), y = percent, fill = RequestType))+
  geom_bar(stat = "identity", position = "dodge")+
  ggtitle("Request Type Percentage Breakdown by Month - Bulky Items and Graffiti Removal")+
  xlab("Month of 2016")+
  ylab("Percent")

grid.arrange(a,b,nrow=1)

##### 3 ######

### We notice that bulky item removal peaks during August - this might be due to college move-in / move-out / (back-to-school)
### Thus could prepare additional staffing for those months for bulky removal, or create city-wide incentives not to
### We may also look at gegraphic breakdown to see whether these bulky item removal requests are close to college campuses




### Investigation 4 - Service request efficiency, phone vs app

View(phoneapp)
phoneapp$serviceefficiency = 
  (phoneapp$UpdatedDate - phoneapp$CreatedDate)/86400 ##(converting from seconds to days)

#### 4 ######
ggplot(phoneapp, aes(x=serviceefficiency, fill = type))+
  geom_histogram(position = "dodge", bins = 10)+
  xlim(c(0, 8))+
  xlab("Time to Service Request in Days")+
  ylab("Count of Requests")+
  ggtitle("Service Efficiency by Request Source")
  
#### Average Update time / Response Time

phoneapp%>%
  filter(type == "app")%>%
  mutate(averagetime = mean(serviceefficiency))
### 6.05 days (not seconds)
phoneapp%>%
  filter(type == "phone")%>%
  mutate(averagetime = mean(serviceefficiency))
### 4.13 days (not seconds)
#### we can go into what kind of requests take long to update




### Potential Next steps

### 1 - certain areas need more graffiti removal, maybe city
###     areas have more need for bulky item movement

### 2 Count of requests by app vs phone - plot it on a map (use cd, or apc variable or zipcodes/lat-lon)

### 3 Analyze open requests (~3% of total), what are they for - what type of request / phone or app or other type? / what location?

## 4 Service Request Type for Each Input Channel - Paige did this - graffiti removal coming in through the 
## app, and the bulky item request coming in through the phone mostly
## Combine into one chart, using grid.arrange