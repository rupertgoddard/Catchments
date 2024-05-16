library(dplyr)
library(ggplot2)
library(tseries)
library(tidyr)
library(dbplyr)
library(RSQLite)
#library(maptools)
#library(rgdal)
#library(rgeos)
library(ggmap)
library(OpenStreetMap)
library(stringr)
library(hydroTSM) # One I've added to the mix

setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments")

HeliganRain <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Mevagissey/Heligan-Gardens-rainfall-15min-Qualified26Nov18to26Nov23.csv")
###

#water_all<-read.csv("all_years_water.csv")
#
View(HeliganRain)

# The combined data frame can be saved in different formats

# as a csv file

#write.csv(water_all,"all_years_water.csv",row.names = FALSE)

# This can be read back into R using the code above, just changing the file name within the " ".


###### Using the dataframe

# We can view the names of the columns

names(HeliganRain)

# We can rename the columns of the data frame

HeliganRain <- HeliganRain %>% rename(Rainfall_mm = value.mm.)

# WHY DOESN'T 'RAINFALL_mm' STAY??
# We can view the first few rows

head(HeliganRain)

# and the last

tail(HeliganRain)

# We can look at the structure of the data

str(HeliganRain)

# We can filter and save the results
# Need to convert date to as.Date
# Convert the 'Date' column to a common date format (e.g., 'YYYY-MM-DD')
HeliganRain$date <- as.Date(HeliganRain$date, format='%d/%m/%Y')
HeliganRain$dateTime <- gsub("T", " ",HeliganRain$dateTime)
HeliganRain$POSIXCT <- as.POSIXct(HeliganRain$dateTime, format="%Y-%m-%d %H:%M:%S")

# Checks no days are missing
date_range <-seq.POSIXt(min(HeliganRain$POSIXCT,na.rm=TRUE), max(HeliganRain$POSIXCT,na.rm=TRUE), by = "900 sec")
#Below reports which  15min intervals are missing.
Missing_intervals <- date_range[!date_range %in% HeliganRain$POSIXCT]
write.csv(Missing_intervals, "C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R/Missing_intervals.csv")

sum(is.na(HeliganRain$POSIXCT)) # checks for NA entries


Rain2018 <- HeliganRain %>% filter(substr(date, 1, 4) == "2018")
print(Rain2018)
# How can I generate a data set for annual data through a loop, 
# then subset that to generate a data set season i.e W:DJF,S:MAM,S:JJA,A:JON?
# I will need a table output of totals for 10yr, 5yr, 1 yr, Seasons with Means
# and St.Dev and plots.

#In the plot below, how does R know which data set I want it to use?

#Then it goes wrong........

Forplotting <- subset(HeliganRain, select = c("date","value.mm."))
Rain2018 <- Forplotting %>% filter(substr(date, 1, 4) == "2018")
TestPlot <- ggplot(data=Rain2018, # the data frame
          aes(x=date, y=value.mm.)) +   # the variables of interest
  geom_bar(stat="identity", position="dodge") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation - Heligan precipitation  Nov 2018 to Dec 2018")+
  theme(plot.title = element_text(size = 15, face = "bold")) # add a title 
TestPlot









# The results are saved in a separate dataframe

# This can be repeated for all the different columns

Ammonia<-water_all%>%filter(determinand.label=="Ammonia(N)")

# This dataframe contains all the Ammonia results for all the locations

# We may need to change the format of the data in certain columns, e.g. the reuslt data is currently not
# numerical

str(Ammonia)

Ammonia$result<-as.numeric(levels(Ammonia$result))[Ammonia$result]


# We may be interested the mean level of ammonia for all the different sites over the past 4 years

test<-Ammonia%>%
  group_by(Location) %>%
  summarise(ave = mean(result))

write.csv(test,"file_name.csv",row.names = FALSE)

# We can create extra columns from the data

Ammonia<-Ammonia%>%mutate(Date=as.Date(sample.sampleDateTime)) # This reformats the date
Ammonia<-Ammonia%>%mutate(Year=as.numeric(format(Date,"%Y"))) # This adds a column containing just the year

# We may be interested the mean level of ammonia for all the different sites for each of the past 4 years

Ammonia%>%
  group_by(Location, Year) %>%
  summarise(ave = mean(result),sd=sd(result))


# Can also produce other summary statistics such as the standard devation

# We can produce plots of the data

Subset<-Ammonia%>%filter(Location=="1-12 HOUGH ROAD BRANDON STW")

ggplot(Subset,aes(x=Date,y=result))+geom_line()
ggplot(Subset,aes(x=Date,y=result))+geom_point()

# If we want more than one location

Location_Group <- c("1-12 HOUGH ROAD BRANDON STW", "APOLLO OFFICE UNITS RADCLIVE RD GAWCOTT")
Subset_1<-filter(Ammonia, Location %in% Location_Group) 

ggplot(Subset_1,aes(x=Date,y=result,col=Location))+geom_line()

ggplot(Subset_1,aes(x=Date,y=result,col=Location))+geom_line()+facet_grid(Location~.)+
  theme(legend.position = "none")+labs(title = "Level of Ammonia from two locations in the UK",
                                       caption = "Data Source: Environment Agency")+
  ylab("Result (mg/l)")

# Other plots also available

ggplot(Subset_1,aes(x=Location, y=result,col=Location))+geom_boxplot()+
  theme(legend.position = "none")+labs(title = "Boxplot of the level of Ammonia from two locations in the UK",
                                       caption = "Data Source: Environment Agency")+
  ylab("Result (mg/l)")

# Spatial plotting

# rename the columns

Ammonia<-Ammonia%>%rename(Easting="sample.samplingPoint.easting", Northing="sample.samplingPoint.northing")

# Check that is has worked

names(Ammonia)


ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

# Create coordinates variable

Ammonia$Easting<-as.numeric(levels(Ammonia$Easting))[Ammonia$Easting]
Ammonia$Northing<-as.numeric(levels(Ammonia$Northing))[Ammonia$Northing]
coords <- cbind(Ammonia$Easting,Ammonia$Northing)

# Create the SpatialPointsDataFrame
dat_SP <- SpatialPointsDataFrame(coords,
                                 data = Ammonia,
                                 proj4string = CRS("+init=epsg:27700"))

#Convert
dat_SP_LL <- spTransform(dat_SP, CRS(latlong))


# replace Lat, Long
dat_SP_LL@data$Long <- coordinates(dat_SP_LL)[, 1]
dat_SP_LL@data$Lat <- coordinates(dat_SP_LL)[, 2]

# This has created a spatial object which can be exported from R

coord<-as.data.frame(dat_SP_LL@coords)

# join these to the ammonia data frame

Ammonia<-cbind(Ammonia,coord)

# check the names of our new columns

names(Ammonia)

Ammonia<-Ammonia%>%rename(Longitude="coords.x1",Latitude="coords.x2")

names(Ammonia)

Ammonia_sub<-Ammonia%>%filter(Year=="2019")
Ammonia_sub1<-Ammonia%>%filter(Year=="2018")

LAT1 =  49.5 ; LAT2 = 58
LON1 = -7.5 ; LON2 = 2

map <- openmap(c(LAT2,LON1), c(LAT1,LON2), zoom = NULL,
               type = "osm")

## OSM CRS :: "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"

map.latlon <- openproj(map, projection = "+epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

#LSOA<- spTransform(LSOA, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

mytheme <- theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                 panel.background = element_rect(colour = NA),
                 plot.background = element_rect(colour = NA),
                 axis.title = element_text(face = "bold",size = rel(1)),
                 axis.title.y = element_text(angle=90,vjust =2),
                 axis.title.x = element_text(vjust = -0.2))

OSMap <- autoplot(map.latlon)  +mytheme
OSMap

OSMap+geom_point(data = Ammonia_sub,aes(x=Longitude,y=Latitude,col=result),size=0.5)+
  scale_color_gradient(low = "blue",high = "red")
OSMap+geom_point(data = Ammonia_sub1,aes(x=Longitude,y=Latitude,col=result),size=0.5)+
  scale_color_gradient(low = "black",high = "red")
