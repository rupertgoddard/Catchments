setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R")

#install.packages("ggplot2")
#install.packages("devtools")
#install_version("EcoHydRology", repos = "http://cran.us.r-project.org")


library (ggplot2)
library (devtools)
library (hydroTSM)
library (EcoHydRology)
library (dplyr)

#Heligan Rainfall
precip.heligan <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Mevagissey/Heligan-Gardens-rainfall-15min-Qualified26Nov18to26Nov23.csv")

head(precip.heligan)

str(precip.heligan)

precip.heligan$date <- as.Date(precip.heligan$date, format = "%d/%m/%Y") # messes up date format

names(precip.heligan)[names(precip.heligan)=="value.mm."] <- "Rainfall_mm"
str(precip.heligan)

#double check structure
str(precip.heligan$date)

hist(precip.heligan$Rainfall_mm, breaks=10)

View(precip.heligan)

sum(is.na(precip.heligan))

str(precip.heligan)

sum(is.na(precip.heligan$Rainfall_mm)) # checks for NA entries

# aggregate the Precipitation (PRECIP) data by DATE
precip_df <-aggregate(precip.heligan$Rainfall_mm,   # data to aggregate
                                 by=list(precip.heligan$date),  # variable to aggregate by
                                 FUN=sum,   # take the sum (total) of the precip
                                 na.rm=TRUE)  # if the are NA values ignore them
# if this is FALSE any NA value will prevent a value be totalled

head(precip_df)
# rename the columns
names(precip_df)[names(precip_df)=="Group.1"] <- "date"
names(precip_df)[names(precip_df)=="x"] <- "PRECIP_mm"
str(precip_df)

# plot daily aggregated rainfall hourly to daily data
precPlot_daily <- ggplot(data=precip_df, # the data frame
                         aes(x=date, y=PRECIP_mm)) +   # the variables of interest
  geom_bar(stat="identity", position="dodge") +   # create a bar graph
  xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  ggtitle("Daily Precipitation - Heligan precipitation 26 Nov 2018 to 26 Nov 2023")+
  theme(plot.title = element_text(size = 15, face = "bold")) # add a title 
precPlot_daily
#Minimum and maximum values
max_index <- which.max(precip_df$PRECIP_mm)
max_date <- precip_df$date[max_index]
max_value <- precip_df$PRECIP_mm[max_index]

# Print or use the results as needed
print(paste("Date:", max_date, "Precipitation (mm):", max_value, collapse = " "))
View(precip_df)

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
