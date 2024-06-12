setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R")
install.packages("ggplot2")
library (ggplot2)

precip.duchy <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Duchy College/Stoke-Climsland-rainfall-15min-Qualified.csv")

head(precip.duchy)
str(precip.duchy)
precip.duchy$date <- as.Date(precip.duchy$date, format = "%Y-%m-%d") # beaware of yr mth separator - or /
names(precip.duchy)[names(precip.duchy)=="value"] <- "Rainfall_mm"
str(precip.duchy)

#double check structure
str(precip.duchy$date)

hist(precip.duchy$Rainfall_mm)

View(precip.duchy)
sum(is.na(precip.duchy))

str(precip.duchy)

sum(is.na(precip.duchy$date)) # checks for NA entries

# aggregate the Precipitation (PRECIP) data by DATE
precip.duchy_daily <-aggregate(precip.duchy$Rainfall_mm,   # data to aggregate
                                 by=list(precip.duchy$date),  # variable to aggregate by
                                 FUN=sum,   # take the sum (total) of the precip
                                 na.rm=TRUE)  # if the are NA values ignore them
# if this is FALSE any NA value will prevent a value be totalled

head(precip.duchy_daily)
# rename the columns
names(precip.duchy_daily)[names(precip.duchy_daily)=="Group.1"] <- "DATE"
names(precip.duchy_daily)[names(precip.duchy_daily)=="x"] <- "PRECIP_mm"
str(precip.duchy_daily)
# plot daily aggregated hourly to daily data
precPlot_daily <- ggplot(data=precip.duchy_daily,  # the data frame
                         aes(DATE, PRECIP_mm)) +   # the variables of interest
  geom_bar(stat="identity") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation - Stoke Climsland precipitation 26 Nov2018 to 26 Nov 2023") + # add a title
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +  # add vertical axis line
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)    # add horizontal axis line

precPlot_daily

#Minimum and maximum values
max_index <- which.max(precip.duchy_daily$PRECIP_mm)
max_date <- precip.duchy_daily$DATE[max_index]
max_value <- precip.duchy_daily$PRECIP_mm[max_index]

# Print or use the results as needed
print(paste("Date:", max_date, "Precipitation (mm):", max_value, collapse = " "))

