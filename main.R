library(utf8)
library(pillar)
library(lubridate)
library(dplyr)
library(ggplot2)

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
}# group data by month
monthly <- data %>%  group_by(month = lubridate::floor_date(date, "month")) %>%
  summarize(average = mean(RentCount), sd = sd(RentCount))

# convert from date to month name
monthly$month <- months(as.Date(monthly$month)) 
# order by month
monthly <- monthly %>% mutate(month = factor(month, levels=month.name)) %>% arrange(month)

View(monthly)

# plot average and sd per month
ggplot(monthly, aes(x=month, y=average)) + ylab("Rent Count") + xlab("Month") + ggtitle("Average and Standard Deviation of Rented Bike Count for each month") +
  geom_bar(stat="identity",fill="blue") +
  geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.3) +
  geom_point(size=2)
