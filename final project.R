##Import 311 data
data <- read.csv("311_Call_Center_Tracking_Data.csv")

library(lubridate)
library(ggplot2)
library(dplyr)
library(ggmap)

##Import contextual data on ZIP code geography
zips = read.csv("2016_Gaz_zcta_national.txt", sep = "\t")
zips$GEOID = formatC(zips$GEOID, width = 5, flag = "0")

##add simplified factor columns for purposes of faceting
data$Date = mdy(data$Date)
data$Time = hms(data$Time)  
data$DOW = wday(data$Date, label = T, abbr = F)

##data set is large for my computer, so here's a random sample
set.seed(123)
quick = data[sample(nrow(data), 100000), ]

## heatmap of date by time
quick.heatmap = quick %>%
  group_by(DOW, hour(Time))%>%
  summarise(count = n())

quick.heatmap %>%
  ggplot(aes(DOW, `hour(Time)`, fill = count))+
  geom_tile()+
  scale_fill_gradient(high = "darkred", low = "white")


## Join ZIP data with sample
quick2 = merge(quick, zips, by.x = "Zip.Code", by.y = "GEOID", all.x = T)

##Adds `TOD` categorical time ranges to data set for purpose of faceting by TOD
quick2$TOD = cut(hour(quick2$Time), breaks = c(-Inf,4,8,12,16,20,Inf), labels = c("24to4","4to8","8to12","12to16","16to20","20to24"), ordered_result = T)

##Will want to plot on LA MAP over time
LA = qmap("Los Angeles", zoom = 10, maptype = "toner-lite")

##Saturday and Sunday look relatively dead 
##as do early morning and lated night. Focusing on AM and Afteroon
quick3 = quick2 %>%
  filter(!(DOW %in% c("Sunday","Saturday")))

##Plot on LA map faceted by DOW and TOD
LA+
  geom_point(data = quick3, 
             aes(INTPTLONG, INTPTLAT),
             size = 1/10,
             alpha = 1/10, 
             position = position_jitter(0.05,0.05))+
  facet_grid(DOW~TOD)


##working on full data set



data = merge(data, zips, by.x = "Zip.Code", by.y = "GEOID", all.x = T)

data$TOD = cut(hour(data$Time), breaks = c(-Inf,4,8,12,16,20,Inf), labels = c("24to4","4to8","8to12","12to16","16to20","20to24"), ordered_result = T)

data%>%
  filter(!is.na(DOW)) %>%
  ggplot(aes(as.numeric(Time))) +
  geom_line(stat = "density") +
  facet_wrap(~DOW)

# top 10 service names

data.servicesummary = data %>%
  group_by(Service.Name)%>%
  summarise(count = n())

top10 = data.servicesummary %>%
  filter(Service.Name!="") %>%
  arrange(desc(count)) %>%
  head(10)

data$top10 = data$Service.Name %in% top10$Service.Name

data%>%
  filter(top10==T)%>%
  ggplot(aes(as.numeric(Time))) +
  geom_line(stat = "density") +
  scale_x_continuous(breaks = c(0, (60*60*24)), labels = c('00', '24'))+
  theme(strip.text.x = element_text(size = 5, angle=90),
        strip.text.y = element_text(size = 5, angle=0)) + 
  facet_grid(Service.Name~DOW)

fulltop10 = data%>%
  filter(top10==T)

set.seed(123)
sampletop10 = fulltop10[sample(nrow(data), 200000), ]
  
unique(sampletop10$Service.Name)

sampletop10 = sampletop10 %>%
  filter(!is.na(DOW)) %>%
  filter(!is.na(Service.Name))

LA +
  geom_point(data = sampletop10, 
             aes(INTPTLONG, INTPTLAT),
             size = 1/10,
             alpha = 1/20, 
             position = position_jitter(0.05,0.05)) +
  theme(strip.text.x = element_text(size = 5, angle=90),
        strip.text.y = element_text(size = 5, angle=0)) +
  facet_grid(Service.Name~DOW)


#A closer look at Pool Noise Inspection

pool.complaints = data %>%
  subset(Service.Name == "Pool Noise Inspection")

ggplot(pool.complaints, aes(DOW, fill = Call.Resolution))+
  geom_bar(position = "stack")


## other data set

data2 <- read.csv("MyLA311_Service_Request_Data_2016.csv")

data2$CreatedDate = dmy_hms(data2$CreatedDate)
data2$UpdatedDate = dmy_hms(data2$UpdatedDate)
data2$ServiceDate = dmy_hms(data2$ServiceDate)
data2$ClosedDate = dmy_hms(data2$ClosedDate)
data2$Duration = data2$CreatedDate%--%data2$ClosedDate
data2$Duration = as.period(data2$Duration)


unique(data2$RequestSource)

data2%>%
  filter(!is.na(ClosedDate)) %>%
  filter(year(ServiceDate)>=2015)%>%
  ggplot(aes(as.numeric(Duration))) +
  geom_line(stat = "density")

data2 %>%
  filter(!is.na(Duration)) %>%
  ggplot(aes(days(Duration))) +
  geom_histogram()

refer = unique(data$Call.Resolution)[c(2,13,8,10)]
resolve = unique(data$Call.Resolution)[c(4,9,18)]
transfer = unique(data$Call.Resolution)[c(6,7,11,16,17)]

data$result = data$Call.Resolution
install.packages("rockchalk")
library(rockchalk)
data$result = combineLevels(data$result, c(10:13), newLabel = "Referred")
levels(data$result)
data$result = combineLevels(data$result, c(1,2,5,7,10), newLabel = "Resolved")
data$result = combineLevels(data$result, c(1:2,7:9), newLabel = "Transfer")

(nrow(subset(data, data$result =="Resolved")) / nrow(data)) * 100

data %>%
  filter(!is.na(Date))%>%
  ggplot(aes(month(Date, label = T, abbr = F), fill = result)) +
  geom_bar(stat = "count", position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))