library(zipcode)
data("zipcode")
USZipCodeData <- zipcode
USZipCodeData$zip <- factor(USZipCodeData$zip)
rm(zipcode)

data <- read.csv("311_Call_Center_Tracking_Data.csv")
data2 <- read.csv("MyLA311_Service_Request_Data_2016.csv")
library(lubridate)
library(dplyr)
library(ggplot2)

str(data)
data$ï..Date <- mdy(data$ï..Date)
colnames(data)[1] <- "Date"
data$Time  <- hms(data$Time)
str(data)
data <- mutate(data, Hour = hour(Time))
data <- mutate(data, Minute = minute(Time))
data$Zip.Code <- factor(data$Zip.Code)

colnames(data2)[1] <- "SRNumber"
save(data, data2, file="pass1.RData")

# Distribution of requests by department
data %>% group_by(Department.Name) %>% filter(Department.Name != "") %>% 
     summarize(n = n()) %>% mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>% mutate(cumul = cumsum(perc)) %>% slice(1: 10) %>%
     ggplot(aes(x = reorder(Department.Name, perc), y =perc)) + geom_bar(stat = "identity") +
     coord_flip()
## More than 90% of requests are from the 10 categories shown with building and safety, and sanitation generating highest call volume with 60% calls.


# Distribution of call resolutions
data %>% group_by(Call.Resolution) %>% summarize(n = n()) %>% mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>% mutate(cumul = cumsum(perc)) %>% slice(1: 6) %>%
     ggplot(aes(x = reorder(Call.Resolution, perc), y = perc)) + geom_bar(stat = "identity") + 
     coord_flip()

##Most calls end up being transferred to LA city services (probably city hall or council). Around 34% calls have requests processed while ~17% involve giving caller some information 

# Distribution of requests by department by year
data %>% mutate(year = year(Date)) %>% group_by(year, Department.Name) %>% 
     filter(Department.Name != "") %>% 
     summarize(n = n()) %>% mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>%  
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department")) %>%
     ggplot(aes(x = year, y =perc, color = Department.Name)) + 
     geom_line(size = 1.5) + theme_bw() + ylim(0, 0.5)
     

## We see that percentage of calls to LAPD and sanitation has declined while percentage of calls to department of building and safety has increased.

data %>% mutate(year = year(Date)) %>% group_by(year, Department.Name) %>% 
     filter(Department.Name != "") %>% 
     summarize(n = n()) %>% 
     arrange(-n) %>%  
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department")) %>%
     ggplot(aes(x = year, y = n, color = Department.Name)) + 
     geom_line(size = 1.5) + theme_bw() 

## We see that number of calls has remained more or less constant for LAPD and Sanitaiton while it has increased for building and safety. 2015 data is incomplete.

# Distribution of requests by department in 2015

data %>% mutate(year = year(Date)) %>% group_by(Department.Name) %>% 
     filter(Department.Name != "") %>% filter(year == 2015) %>%
     summarize(n = n()) %>% arrange(-n)  %>% mutate(perc = n / sum(n)) %>% 
     arrange(-n) %>% mutate(cumul = cumsum(perc)) %>% slice(1: 10) %>%
     ggplot(aes(x = reorder(Department.Name, perc), y =perc)) + geom_bar(stat = "identity") +
     coord_flip()

## In 2015, most requests have gone to building and safety, and sanitation 

# Distribution for service requests by department
data %>% filter(Service.Name != "") %>% mutate(year = year(Date)) %>% 
     group_by(Service.Name) %>% summarize(n = n()) %>%
     arrange(-n) %>% mutate(perc = n / sum(n))%>% mutate(cumul = cumsum(perc)) %>% slice(1:10) %>%
     ggplot(aes(x = reorder(Service.Name, perc), y = perc)) + geom_bar(stat = "identity") + 
     coord_flip()

## Most calls are requests for bulky item pick up or permit inspection

# Distribution by time
data %>% mutate(weekday = wday(Date, abbr = T, label = T)) %>%
     mutate(hour = factor(hour(Time))) %>% mutate(year = year(Date)) %>%
     filter(!is.na(hour) & !is.na(weekday)) %>%
     group_by(year, weekday, hour) %>% summarize(requests = n()) %>%
     ggplot(aes(x = weekday, y = hour, fill = requests)) + 
     geom_tile() + scale_fill_gradient(low = "white", high = "darkred") +
     facet_wrap(~year)

## Calls are almost equally distributed throughout the day

data %>% mutate(year = year(Date)) %>% 
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department",
                                   "PW/Board of Public Works", 
                                   "Department of Transportation", 
                                   "PW/Bureau of Street Services") & 
                 year(Date) != 2015) %>%
     group_by(year, Department.Name) %>% summarize(requests = n()) %>%
     ggplot(aes(x = year, y = Department.Name, fill = requests)) + 
     geom_tile() + scale_fill_gradient(low = "white", high = "darkred") 


## Department of building and safety sees a continuous increase in call volume

data %>% mutate(hour = hour(Time)) %>% 
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department",
                                   "PW/Board of Public Works", 
                                   "Department of Transportation", 
                                   "PW/Bureau of Street Services")) %>%
     filter(!is.na(hour)) %>%
     group_by(hour, Department.Name) %>% summarize(requests = n()) %>%
     ggplot(aes(x = hour, y = Department.Name, fill = requests)) + 
     geom_tile() + scale_fill_gradient(low = "white", high = "darkred") 

## Highest call volume for dpeartment of public and safety are early morning

data %>% mutate(month = month(Date)) %>% 
     filter(Department.Name %in% c("Department of Building and Safety", 
                                   "PW/Bureau of Sanitation", 
                                   "Los Angeles Police Department",
                                   "PW/Board of Public Works", 
                                   "Department of Transportation", 
                                   "PW/Bureau of Street Services")) %>%
     mutate(year = year(Date)) %>%
     filter(!is.na(month)) %>%
     group_by(year, month, Department.Name) %>% summarize(requests = n()) %>%
     ggplot(aes(x = factor(month), y = Department.Name, fill = requests)) + 
     geom_tile() + scale_fill_gradient(low = "white", high = "darkred") +
     facet_wrap(~year)

# Distribution of Service request processed by department name
department <- data %>% group_by(Department.Name) %>% summarize(calls = n()) 
departmentbyresolution <- data %>% group_by(Department.Name, Call.Resolution) %>% summarize(calls = n()) 
combined <- merge(departmentbyresolution, department, 
                  by.x = "Department.Name", by.y = "Department.Name")

combined %>% mutate(perc = calls.x / calls.y) %>% 
     filter(Call.Resolution == "Service Request Processed") %>% arrange(-perc) %>%
     ggplot(aes(x = reorder(Department.Name, perc), y = perc)) + geom_bar(stat="identity") +
     coord_flip()

## PW/Board of public works, lighting and sanitation process the highest percentage of their requests

combined %>% mutate(perc = calls.x / calls.y) %>% 
     filter(Call.Resolution == "Service Request Processed" & calls.y > 10000 & Department.Name != "") %>% 
     arrange(-perc) %>%
     ggplot(aes(x = reorder(Department.Name, perc), y = perc)) + geom_bar(stat="identity") +
     coord_flip()

rm(department, departmentbyresolution, combined)

# Distribution of Service request processed by zip code
zipcode <- data %>% group_by(Zip.Code) %>% summarize(calls = n()) 
zipbyresolution <- data %>% group_by(Zip.Code, Call.Resolution) %>% summarize(calls = n()) 
combined2 <- merge(zipbyresolution, zipcode, 
                  by.x = "Zip.Code", by.y = "Zip.Code")


x <- combined2 %>% mutate(perc = calls.x / calls.y) %>% 
     filter(Call.Resolution == "Service Request Processed" & calls.y > 10000) %>% 
     arrange(-perc) %>% filter (Zip.Code != 99999 & Zip.Code != 0)

x %>% arrange(-perc) %>%
     ggplot(aes(x = reorder(Zip.Code, perc), y = perc)) + geom_bar(stat="identity") +
     coord_flip()

clusterbyzip <- kmeans(x$perc, 4)
x$cluster <- factor(clusterbyzip$cluster)

ggplot(data = x, aes(x = cluster, y = perc, fill = cluster)) + geom_boxplot()
table(x$cluster)


zippositioncluster <- 
     merge(select(x, Zip.Code, perc ,cluster), USZipCodeData, by.x = "Zip.Code", by.y = "zip")

LA + 
     stat_bin_2d(data = zippositioncluster, 
                 aes(x = longitude, y = latitude, fill = cluster))

tapply(zippositioncluster$perc, zippositioncluster$cluster, mean)

## There are clusters of zip codes where service request processing percentage is lower.


rm(zipcode, zipbyresolution, combined2, clusterbyzip, x)

##########################################
data2$CreatedDate <- mdy_hms(data2$CreatedDate)
data2$UpdatedDate <- mdy_hms(data2$UpdatedDate)
data2 %>% mutate(ye = year(CreatedDate)) %>% group_by(ye, RequestType) %>% 
     summarize(request = n()) %>% 
     ggplot(aes(x = reorder(RequestType, request), y = request, fill = RequestType)) + 
     geom_bar(stat = "identity") +
     facet_wrap(~ye) + 
     coord_flip()
min(data2$CreatedDate)

##############################################

library(ggmap)

LA <- qmap("Los Angeles", zoom = 10, color = "bw")
mapdat <- select(data2, Latitude, Longitude)
mapdat <- filter(mapdat, !is.na(Latitude))
unique(round(mapdat$Longitude, 2))

mapdat$Lat2 <- round(mapdat$Latitude,2)
mapdat$Lon2 <- round(mapdat$Longitude,2)

mapdat2 <- mapdat %>% group_by(Lat2, Lon2) %>% summarize(requests = n()) 

LA +
     geom_tile(data = mapdat2, aes(Lon2, Lat2, fill = requests), alpha = 1) + 
     scale_fill_continuous(low = "white", high = "darkred")

rm(mapdat, mapdat2, x)

####################################
# Service requested by zipcode
data %>% group_by(Zip.Code, Department.Name) %>% summarize(requests = n()) %>%
     filter(requests > 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code) & Department.Name != "") %>%
     ggplot(aes(x = Zip.Code, y =requests)) + geom_bar(stat = "identity") +
     facet_wrap(~Department.Name) 

requestdeptzip <- data %>% group_by(Zip.Code, Department.Name) %>% summarize(requests = n()) %>%
     filter(requests > 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code) & Department.Name != "")

requestzip <- data %>% group_by(Zip.Code) %>% summarize(requests = n()) %>%
     filter(requests > 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code))

merged <- merge(x = requestdeptzip, y = requestzip, by.x = "Zip.Code", by.y = "Zip.Code")

merged %>% mutate(perc = requests.x / requests.y) %>% 
     ggplot(aes(x = Department.Name, y = Zip.Code, fill = perc)) + geom_tile() + 
     scale_fill_gradient(low = "white", high = "darkred")

rm(merged, requestdeptzip, requestzip)


requestdeptzip1 <- data %>% group_by(Zip.Code, Department.Name) %>% summarize(requests = n()) %>%
     filter(requests <= 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code) & Department.Name != "")

requestzip1 <- data %>% group_by(Zip.Code) %>% summarize(requests = n()) %>%
     filter(requests <= 10000) %>% arrange(-requests) %>% 
     filter(!is.na(Zip.Code))

merged1 <- merge(x = requestdeptzip1, y = requestzip1, by.x = "Zip.Code", by.y = "Zip.Code")

merged1 %>% mutate(perc = requests.x / requests.y) %>% filter(requests.x > 500) %>%
     ggplot(aes(x = Department.Name, y = Zip.Code, fill = perc)) + geom_tile() + 
     scale_fill_gradient(low = "white", high = "darkred") + theme_bw()

rm(requestdeptzip1, merged1, requestzip1)

###########################################################

unique(data2$ActionTaken)
data2 %>% group_by(RequestSource, RequestType) %>% summarize(n = n()) %>% arrange(-n) %>% 
     filter(RequestType != "Bulky Items") %>%
     ggplot(aes(x = RequestSource, y =RequestType, fill = n)) + geom_tile() + 
     scale_fill_gradient(low = "white", high = "darkred")

data2 %>% group_by(PolicePrecinct, RequestType) %>% summarize(n = n()) %>% 
     filter(PolicePrecinct != "") %>% filter(n > 5000) %>% 
     ggplot(aes(x = RequestType, y = PolicePrecinct, fill = n)) + geom_tile() +
     scale_fill_continuous(low = "white", high = "darkred")

data2 %>% filter(PolicePrecinct == "NEWTON" & RequestType == "Graffiti Removal") %>%
     group_by(ZipCode) %>% summarize(n = n()) %>% arrange(-n)

data2 %>% filter(PolicePrecinct == "NEWTON") %>%
     group_by(ZipCode) %>% summarize(n = n()) %>% arrange(-n)

