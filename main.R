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
# gets list of the different Seasons in the dataset
seasons <- unique(data$Season)

# creates empty dataframe
seasonal_hourly = data.frame()

# loops through the seasons
for(i in seasons)
{
  
  # filters the data to show only of season i
  x <- filter(data, Season == i) %>% 
    # groups the data by the hour
    group_by(Hour) %>% 
    # gets the mean RentCount for each hour
    summarise(average = mean(RentCount))
  
  #transposition of x 
  y <- transpose(x)
  # adds the season that corresponds to the data
  y <- cbind(y, i)
  # names the columns
  colnames(y) <- c(0:23, "Season")
  # removes the first row which held the column names
  y <- y[-c(1), ]
  # adds the season row to the seasonal hourly
  seasonal_hourly = rbind(seasonal_hourly, y)

}
#moves season column to front (not necessary but easier to understand when viewing)
seasonal_hourly <- seasonal_hourly %>% relocate(Season)
# melts the seasonal_hourly dataframe so as to be able to plot
melted <- melt(seasonal_hourly, id.vars="Season")
View(seasonal_hourly)
# plots a line for each Season
melted %>% ggplot(aes(x=variable,y=value, color = Season, group=Season)) + xlab("Hour") + ylab("Average Rent Count") + ggtitle("Rented Bike Count by Hour of the day across season") + 
  geom_line(linewidth=1.2)