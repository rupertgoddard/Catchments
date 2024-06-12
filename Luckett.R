setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R")

#install.packages("ggplot2")
#install.packages("devtools")
#install_version("EcoHydRology", repos = "http://cran.us.r-project.org")


library (ggplot2)
library (devtools)
library (hydroTSM)
library (EcoHydRology)
library (dplyr)

#Luckett Rainfall
precip.luckett <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Duchy College/WRT Luckett data/Luckett Data for Review/Stoke Climsland 15min rainfall 01.01.2020-01.06.csv")
#Need to remove metadata and other non useful columns
#i.e remove the first 20 rows and columns 5 to 132
cleaned.precip.luckett <- precip.luckett %>%
  slice(21:n()) %>%
  select(-c(5:132))

names(cleaned.precip.luckett)[names(cleaned.precip.luckett)=="STOKE"] <- "Rainfall_mm"
names(cleaned.precip.luckett)[names(cleaned.precip.luckett)=="Station"] <- "Date"
names(cleaned.precip.luckett)[names(cleaned.precip.luckett)=="name"] <- "Time"
names(cleaned.precip.luckett)[names(cleaned.precip.luckett)=="X"] <- "DateTime"


str(cleaned.precip.luckett)
head(precip.luckett)
head(cleaned.precip.luckett)


cleaned.precip.luckett$Date <- as.Date(cleaned.precip.luckett$Date, format = "%d/%m/%Y") # messes up date format
cleaned.precip.luckett$Rainfall_mm <- as.numeric(cleaned.precip.luckett$Rainfall_mm) # messes up date format

#cleaned.precip.luckett$Time <- as.Date(cleaned.precip.luckett$Time, format = "%H:/%M:/%S") # messes up date format

#cleaned.precip.luckett (DateTime=c (as.POSIXct(DateTime2,format="%m/%d/%Y %H:%M:%S")
                                    


hist(cleaned.precip.luckett$Rainfall_mm, breaks=10)

View(cleaned.precip.luckett)



sum(is.na(cleaned.precip.luckett$Rainfall_mm)) # checks for NA entries

# aggregate the Precipitation (PRECIP) data by DATE
precip_df <-aggregate(cleaned.precip.luckett$Rainfall_mm,   # data to aggregate
                      by=list(cleaned.precip.luckett$Date),  # variable to aggregate by
                      FUN=sum,   # take the sum (total) of the precip
                      na.rm=TRUE)  # if the are NA values ignore them
# if this is FALSE any NA value will prevent a value be totalled

head(precip_df)
# rename the columns
names(precip_df)[names(precip_df)=="Group.1"] <- "date"
names(precip_df)[names(precip_df)=="x"] <- "PRECIP_mm"
str(precip_df)

min(cleaned.precip.luckett$Date)
max(cleaned.precip.luckett$Date)
# plot daily aggregated rainfall hourly to daily data
precPlot_daily <- ggplot(data=precip_df, # the data frame
                         aes(x=date, y=PRECIP_mm)) +   # the variables of interest
  geom_bar(stat="identity", position="dodge") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation - Stoke Climsland precipitation 01 Jan 2020 to 01 June 2022")+
  theme(plot.title = element_text(size = 15, face = "bold")) # add a title 
precPlot_daily
#Minimum and maximum values
max_index <- which.max(precip_df$PRECIP_mm)
max_date <- precip_df$date[max_index]
max_value <- precip_df$PRECIP_mm[max_index]

min_index <- which.min(precip_df$PRECIP_mm)
min_date <- precip_df$date[min_index]
min_value <- precip_df$PRECIP_mm[min_index]

# Print or use the results as needed
print(paste("Date:", max_date, "Precipitation (mm):", max_value, collapse = " "))
print(paste("Date:", min_date, "Precipitation (mm):", min_value, collapse = " "))
View(precip_df)

###Continue from here....


##STAGE
stage_df <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Mevagissey/Mevagissey-all-measures_stage.csv")
head(stage_df)
str(stage_df)
stage_df$date <- as.Date(stage_df$date, format = "%d/%m/%Y") # messes up date format
hist(stage_df$stage_m,breaks=50)
View(stage_df)
subs_stage_df <- subset(stage_df, select=c("date", "stage_m"))
#Minimum and maximum values
Meva_max_index <- which.max(stage_df$stage_m)
Meva_max_date <- stage_df$date[Meva_max_index]
print(paste("Date:",Meva_max_date, "Stage (m):", Meva_max_value, collapse = " "))

#Need to bind stage.meva with precip.heligan_daily

View(subs_stage_df)

# Convert the 'Date' column to a common date format (e.g., 'YYYY-MM-DD')
precip_df$date <- as.Date(precip_df$date, format='%d/%m/%Y')
stage_df$date <- as.Date(stage_df$date, format='%d/%m/%Y')

# Ensure that the dates in both dataframes are unique
precip_df <- precip_df[!duplicated(precip_df$date), ]
subs_stage_df <- subs_stage_df[!duplicated(subs_stage_df$date), ]

# Merge the two dataframes on the 'Date' column
merged_df <- merge(precip_df, subs_stage_df, by='date', all=TRUE)
head(merged_df)
# Rename the columns to 'Date', 'Precip', and 'Discharge'
#colnames(merged_df) <- c('Date', 'Precip', 'Discharge')

str(merged_df)

#rename value col as rainfall(mm)
names(merged_df)[names(merged_df)=="PRECIP_mm"] <- "Rainfall_mm"

#Tidy up the df to just include cols of interest.
subset_df <- subset(merged_df, select=c("date", "Rainfall_mm", "stage_m"))

# Sort the dataframe by the 'Date' column
subset_df <- subset_df[order(subset_df$date), ]

# Reset the row names
rownames(subset_df) <- NULL


# Print the resulting dataframe
print(subset_df)
head(subset_df)
#Remove null values and print  to new dataframe
cleaned_df <- na.omit(subset_df)
#print(cleaned_df)

hydrograph (input = cleaned_df, streamflow = cleaned_df$stage_m, 
            timeSeries = cleaned_df$date, precip = cleaned_df$Rainfall_mm, 
            P.units = "mm", S.units = "m",
            S1.col = "black", S2.col = "red", stream.label = "Stage", 
            streamflow3 = NULL, streamflow4 = NULL, precip2 = NULL)

# Suggest I need to cut data series in to yearly chunks to view relationship 
yearly_subsets <- subset_df %>%
  group_split(year = lubridate::year(date))
View(yearly_subsets)
subset_2019 <- yearly_subsets[[which(yearly_subsets$year == 2019)]]
str(yearly_subsets)
unique_years <- unique(unlist(lapply(yearly_subsets, function(x) unique(lubridate::year(x$date)))))
print(unique_years)


# Read the 'Precip' and 'Discharge' CSV files into separate dataframes
precip_df <- read.csv('Precip.csv')
discharge_df <- read.csv('Discharge.csv')

# Merge the two dataframes on the 'Date' column
merged_df <- merge(precip_df, discharge_df, by='Date', all=TRUE)

# Rename the columns to 'Date', 'Precip', and 'Discharge'
colnames(merged_df) <- c('Date', 'Precip', 'Discharge')

# Sort the dataframe by the 'Date' column
merged_df <- merged_df[order(merged_df$Date), ]

# Reset the row names
rownames(merged_df) <- NULL

# Print the resulting dataframe
print(merged_df)