library(dplyr)
library(ggplot2)
library(ggmap)
library(lubridate)
library(stringr)



tracking <- read.csv("311_Call_Center_Tracking_Data.csv")

service <- read.csv("MyLA311_Service_Request_Data_2016.csv")
 
str(tracking)
str(service)

tracking$Date <- mdy(tracking$Date)
tracking$Time <- hms(tracking$Time)

service$CreatedDate <- mdy_hms(service$CreatedDate)
service$UpdatedDate <- mdy_hms(service$UpdatedDate)
service$ServiceDate <- mdy_hms(service$ServiceDate)
service$ClosedDate <- mdy_hms(service$ClosedDate)


unique(service$RequestSource)

service <- service %>%
    mutate(type = ifelse(RequestSource %in% c("Call",
                                              "Voicemail",
                                              "Queue Initiated Customer Call"), "phone",
                         ifelse(RequestSource == "Mobile App", "app", 0)))

phoneapp <- service %>%
    filter(type %in% c("phone","app")) %>%
    droplevels()

percent_part <- phoneapp %>%
    group_by(type, RequestType) %>%
    summarise(count = n()) %>%
    mutate(total = sum(count)) %>%
    mutate(percent = count/total)


## Percent of Service Requests compaired to Same Request Source

ggplot(percent_part, aes(x = RequestType, y = percent, fill = type)) +
    geom_bar(position = "dodge", stat = "identity") + 
    ggtitle("Percent of Service Requests compaired to Same Request Source") +
    ylab("Frequency") +
    xlab("Request Type") +
    theme_light() +
    theme(axis.text.x=element_text(angle=45, hjust=1, size = 12))
    

nrow(phoneapp)

percent_whole <- phoneapp %>%
    group_by(type, RequestType) %>%
    summarise(count = n()) %>%
    mutate(total = nrow(phoneapp)) %>%
    mutate(percent = count/total)


## Percent of Service Requests compaired to Total Requests
ggplot(percent_whole, aes(x = RequestType, y = percent, fill = type)) +
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Percent of Service Requests compaired to Total Requests") +
    ylab("Frequency") +
    xlab("Request Type") +
    theme_light() +
    theme(axis.text.x=element_text(angle=45, hjust=1, size = 12))


phoneapp <- phoneapp %>%
    mutate(daycreated = wday(CreatedDate, label = T, abbr = F), 
           timecreated = hour(CreatedDate))


appmap <- phoneapp %>%
    filter(type == "app") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
        geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "steelblue") +
    ggtitle("Heatmap of Service Requests by App") +
    theme_light() +
    ylab("Time of Day") +
    xlab("Day of Week")
    
    

phonemap <- phoneapp %>%
    filter(type == "phone") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "steelblue") + 
    ggtitle("Heatmap of Service requests by Phone") +
    ylab("Time of Day") +
    xlab("Day of Week") +
    theme_light()

appgraffitimap <- phoneapp %>%
    filter(type == "app", RequestType == "Graffiti Removal") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "darkred") +
    ggtitle("Heatmap of Graffiti Removal Requests by App") +
    ylab("Time of Day") +
    xlab("Day of Week") +
    theme_light()

phonegraffitimap <- phoneapp %>%
    filter(type == "phone", RequestType == "Graffiti Removal") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "darkred") +
    ggtitle("Heatmap of Graffiti Removal Requests by Phone") +
    ylab("Time of Day") +
    xlab("Day of Week") +
    theme_light()

appnograffitimap <- phoneapp %>%
    filter(type == "app", RequestType != "Graffiti Removal") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "green") +
    ggtitle("Heatmap of Non-Graffiti Removal Requests by App") +
    ylab("Time of Day") +
    xlab("Day of Week") +
    theme_light()

phonenograffitimap <- phoneapp %>%
    filter(type == "phone", RequestType != "Graffiti Removal") %>%
    group_by(daycreated, timecreated) %>%
    summarise(count = n()) %>%
    ggplot(aes(x = daycreated, y = factor(timecreated))) + 
    geom_tile(aes(fill = count)) +
    scale_fill_gradient(low = "white",
                        high = "green") +
    ggtitle("Heatmap of Non-Graffiti Removal Requests by Phone") +
    ylab("Time of Day") +
    xlab("Day of Week") +
    theme_light()



grid.arrange(appmap, phonemap, appgraffitimap,
             phonegraffitimap, appnograffitimap,
             phonenograffitimap, ncol = 2)
