library(lubridate)
library(dplyr)
library(ggplot2)
library(leaflet)
library(reshape2)

#Load data
##Available here: https://data.cityofmadison.com/
police <- read.csv("Madison_Police_Calls_for_Service.csv", stringsAsFactors = FALSE)
police$Date.Time <- mdy_hms(police$Date.Time, tz = "America/Chicago")

#Split up the "Address" column into address, latitude, and longitude,
#then bind everything back together with nice col names
temp <- colsplit(police$Address, "\\(", c("address", "latlon"))
temp2 <- colsplit(temp$latlon, ", ", c("lat", "lon"))
temp2$lon <- as.numeric(gsub("\\)", "", temp2$lon))
policegeo <- cbind(select(police, -Address), select(temp, address), temp2)
rm(temp,temp2)
#Some addresses don't include a block number, and are therefore missing lat/lon
#could code all of these as the 100 block, or just remove any missing data


#Grab noise complaints
noise <- filter(policegeo, Type == 49)
noise15 <- filter(noise, year(noise$Date.Time) == 2015)

#Group by year to make a selectable map
##Remove rows that are missing Latitude/Longitude, 
##Some addresses have inconsistent location data, use mean
noisecount <- noise %>% 
  filter(!is.na(noise$lat)) %>% 
  mutate(year = year(Date.Time)) %>% 
  group_by(year, address) %>% 
  summarize(count = n(), lat = mean(lat), lon = mean(lon)) %>% 
  arrange(-year, -count)



####################
#Graphs


#week of the year
noise15 %>% mutate(year = year(Date.Time), 
                 week = week(Date.Time)) %>% 
  filter(week < 53) %>% 
  group_by(week) %>% 
  ggplot(aes(x = week), data = .) + geom_bar(fill = "#F8766D") + 
  scale_x_continuous(labels = waiver(), breaks = 1:52) + 
  ggtitle("Noise Complaints Per Week, 2015") + 
  xlab("") + ylab("Number of Complaints") + 
  theme_bw(base_size = 16) +  
  theme(axis.text.x = element_text(angle = -90)) 

#day of the week
noise15 %>% 
  mutate(day = factor(wday(Date.Time, label = TRUE), 
                      levels =c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun"))) %>% 
  group_by(day) %>% 
  ggplot(aes(x=day), data = .) +
  geom_bar(fill = "#F8766D") +
  ggtitle("Noise Complaints Per Day, 2015") +
  xlab("") + ylab("Number of Complaints") +
  theme_bw(base_size = 16)


#hour of the day
#create axis labels
timelabs <- c(paste(6:11, " am"), "12 pm", paste(1:11, " pm"), 
              "12 am", paste(1:5, " am"))
#create graph
noise15 %>% 
  mutate(hour = factor(hour(Date.Time), 
                                 levels = c(6:23, 0:5))) %>% 
  group_by(hour) %>% 
  ggplot(aes(x = hour), data = .) + geom_bar(fill = "#F8766D") + 
  scale_x_discrete(labels = timelabs, breaks = c(6:23, 
                                                 0:5), expand = c(0.05, 0.01)) + 
  ggtitle("Noise Complaints Per Hour, 2015") + 
  scale_y_continuous(labels = waiver(), breaks = c(100, 200, 300, 400, 500)) +
  xlab("") + ylab("Number of Complaints") + 
  theme_bw(base_size = 16) +  
  theme(axis.text.x = element_text(vjust = 1, hjust = -0.05,  angle = -40))

#Loudest blocks of the year
##Need to manually iterate over years
toGraph <- noisecount %>% 
  filter(year == 2015) %>% #change this line to pick year
  slice(1:5) 

#create ordered factor for plotting
toGraph$address <- factor(toGraph$address, levels = toGraph$address)

ggplot(aes(address, count), data = toGraph) + 
  geom_bar(stat = "identity", fill = "#F8766D") +
  ggtitle(paste("Loudest Blocks, ", toGraph$year[1])) +
  xlab("") + ylab("Number of Complaints") + 
  theme_bw(base_size = 16) 


####################
#Leaflet mapping

noisemap <- leaflet(noisecount) %>% 
  # addTiles() %>% 
  addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png',
           attribution = paste(
             '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
             '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>') 
  ) %>% 
  setView(-89.4120894, 43.074521, zoom = 12) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, color = "#f66",
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(noisecount, year == 2015), group ="2015") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, color = "#f30",
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(noisecount, year == 2014), group ="2014") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, color = "#F8766D",
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(noisecount, year == 2013), group ="2013") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, 
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(noisecount, year == 2012), group ="2012") %>% 
  addLayersControl(baseGroups = c("2015", "2014", "2013", "2012"), 
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE))
noisemap


##############
#Region separation

#Define regions by lat, lon
area <- noisecount
area$region <- "Downtown"
area$region[area$lat < 43.066892 | area$lon < -89.413507] <- "West"
area$region[area$lon > -89.376986 | area$lat > 43.088866] <- "East"

#View regions spatially to check for definition errors
leaflet(area) %>% 
  addTiles() %>% 
  setView(-89.4120894, 43.074521, zoom = 12) %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, color = "#f66",
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(area, count >=4, region == "West"), group ="West") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, color = "#f30",
                   popup=~paste(address, " complaints", sep=" "),
                   data = filter(area, region=="East"), group ="East") %>% 
  addCircleMarkers(lng = ~lon, lat = ~lat, radius = ~count, 
                   popup=~paste(address, count, " complaints", sep=" "),
                   data = filter(area, region=="Downtown"), group ="Downtown") %>% 
  addLayersControl(baseGroups = c("East", "West", "Downtown"), 
                   position = "bottomright",
                   options = layersControlOptions(collapsed = FALSE))

#Find and graph mean complaints per block by region
area %>% 
  filter(year != 2016) %>% 
  group_by(region, year) %>% 
  summarize(total = sum(count), locations = n()) %>% 
  mutate(ratio = total/locations) %>% 
  #could use mean, but define explicitly for the sake of viewable totals
  ggplot(aes(region, ratio), data = .) + 
  geom_bar(stat = "identity", fill = "#F8766D") + 
  facet_grid(. ~ year) + 
  ggtitle("Average Complaints per Block by Region") + 
  xlab("") + ylab("Mean Complaints Per Block") + 
  theme_bw(base_size = 16)