library(utf8)
library(pillar)
library(lubridate)
library(dplyr)
library(ggplot2)
library(reshape2)
library(data.table)

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

# Task 2
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


# copy of data
temp_dat <- data

# changing data so as to be numerical to calculate correlation
temp_dat$Season[temp_dat$Season == 'Winter'] <- 0
temp_dat$Season[temp_dat$Season == 'Spring'] <- 1
temp_dat$Season[temp_dat$Season == 'Summer'] <- 2
temp_dat$Season[temp_dat$Season == 'Autumn'] <- 3
temp_dat$Holiday[temp_dat$Holiday == 'FALSE'] <- 0
temp_dat$Holiday[temp_dat$Holiday == 'TRUE'] <- 1
temp_dat$Function[temp_dat$Function == 'FALSE'] <- 0
temp_dat$Function[temp_dat$Function == 'TRUE'] <- 1

# changing types
temp_dat$Season <- as.numeric(temp_dat$Season)
temp_dat$Holiday <- as.numeric(temp_dat$Holiday)
temp_dat$Function <- as.numeric(temp_dat$Function)

# removing date column
dataNoDate <- temp_dat[, !colnames(temp_dat) %in% "date"]

str(dataNoDate)

cor_matrix <- cor(dataNoDate)
print(cor_matrix)

histogram_data <- hist(data$RentCount, breaks=33, col="blue") 




# Task3

# candle 3


# group data by month
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


# candle 4

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


# candle 5

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

# candle 6

summer_winter_data <- data %>% filter(Season == "Summer" | Season =="Winter") %>% group_by(Season) %>% summarise(Total=sum(RentCount), "Average\n(multiplied by 1000 for clearer view)"=mean(RentCount)*1000)

summer_winter_data <- melt(summer_winter_data, id.vars="Season")
View(summer_winter_data)

ggplot(summer_winter_data, aes(x=Season, y=value, group=variable, fill=variable)) + 
  ylab("Rent Count") + xlab("Season") + ggtitle("Average and Total of Rented Bike Count for Summer and Winter") +
  geom_bar( position = "dodge", stat = "identity")

#filters and only keeps required columns
summer_winter_data <- data %>% filter(Season == "Summer" | Season =="Winter") %>% select(date, Hour, RentCount, Season)

# gets hourly averages
summer_winter_hourly_average <- summer_winter_data %>% group_by(Season) %>% summarise(average=mean(RentCount))

# gets totals for day
summer_winter_daily_average <- summer_winter_data %>% group_by(date) %>% summarise(Season=Season, total=sum(RentCount)) %>%
# gets daily averages
  group_by(Season) %>% summarise(average=mean(total))

# plots daily averages
ggplot(summer_winter_daily_average, aes(x=Season, y=average)) + 
  ylab("Average Rent Count") + xlab("Season") + ggtitle("Daily Averages of Rented Bike Count for Summer and Winter") +
  geom_bar(position = "dodge", stat = "identity")

# plots hourly averages
ggplot(summer_winter_hourly_average, aes(x=Season, y=average)) + 
  ylab("Average Rent Count") + xlab("Season") + ggtitle("Hourly Averages of Rented Bike Count for Summer and Winter") +
  geom_bar(position = "dodge", stat = "identity")