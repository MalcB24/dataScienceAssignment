library(utf8)
library(pillar)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape)

# import csv
data <- read.csv("C:/Users/Malcolm/Downloads/SeoulBikeData.csv", header=FALSE)
# remove 1st row
data <- data[-c(1), ]
# rename columns
colnames(data) <- c("date","RentCount", "Hour", "Temp", "Hum", "Wind", 'Vis', "Dew","SolRad","Rain", "Snow","Season", "Holiday","Function")
data <- type.convert(data, as.is= TRUE, na.strings = "NA")

data$Function[data$Function == 'No'] <- FALSE
data$Function[data$Function == 'Yes'] <- TRUE

data$Holiday[data$Holiday == 'No Holiday'] <- FALSE
data$Holiday[data$Holiday == 'Holiday'] <- TRUE

data$date <- dmy(data$date)
as.numeric(as.character(data$RentCount))
as.numeric(as.character(data$Hour))
as.numeric(as.character(data$Temp))
as.numeric(as.character(data$Hum))
as.numeric(as.character(data$Vis))
as.numeric(as.character(data$Dew))
as.numeric(as.character(data$Wind))
as.numeric(as.character(data$SolRad))
as.numeric(as.character(data$Rain))
as.numeric(as.character(data$Snow))

# finds the null values
null_data <- data[!complete.cases(data),]
# list rows that contain the null values
View(null_data)

for(i in 1:nrow(null_data)) 
{
  
  for(j in 1:ncol(null_data))
  {
    # if there is a null in the column we replace it with the seasons hourly average
    if(is.na(null_data[i,j]))
    {
      # gets column name of missing value
      col_name = (colnames(null_data)[j])
      #gets the hour where the missing value is
      hour = null_data[i, "Hour"]
      #gets all data where the season and hour match that of the missing value
      season_hour_data = filter(data, Season == null_data[i, "Season"] & Hour == hour) 
      #gets average value
      hourly_average = mean(season_hour_data[,col_name], na.rm = TRUE)
      
      #replaces the missing value with the average value
      data[rownames(null_data)[i],col_name] <- hourly_average
    }
  }
}
 
# selecting only required columns
weekday_data <- data %>% select(date, RentCount, Hour)
# convert date to day of week
weekday_data$date <- weekdays(weekday_data$date)
# remove saturday and sunday
weekday_data <- filter(weekday_data, date != "Saturday" & date != "Sunday")

# get the different days of week in the data
days_of_week <- unique(weekday_data$date)

# create a new empty dataframe
grouped_weekday_data = data.frame()

# loops through the different days
for(i in days_of_week)
{
  # filters data to show only of the day i
  x <- filter(weekday_data, date == i) %>%
    # groups data by the hour
    group_by(Hour) %>% 
    # gets mean for each hour
    summarise(average=mean(RentCount))
  
  # transposition of x
  y <-transpose(x)
  # adds the day that corresponds to the date
  y <- cbind(y,i)
  # renames columns
  colnames(y) <- c(0:23, "Day")
  # removes the first row which held the column names
  y <-y[-c(1),]
  # adds the day row to the grouped weekday data
  grouped_weekday_data = rbind(grouped_weekday_data, y)
}
#moves day column to front (not necessary but easier to understand when viewing)
grouped_weekday_data <- grouped_weekday_data %>% relocate(Day)
# melts the grouped_weekday_data so as to be able to plot
melted_grouped_weekday_data <- melt(grouped_weekday_data, id.vars="Day")
View(melted_grouped_weekday_data)

# plots line for each day of week
melted_grouped_weekday_data %>% ggplot(aes(x=variable, y=value, color=Day, group=Day)) +
  xlab("Hour") + ylab("Average Rent Count") + ggtitle("Rented Bike Count by Hour of the day across weekdays") + 
  geom_line(linewidth=1.2)