# Cyclistic-Bike-Share-Coursera-2023-2024

### Scenario

In this case study, we are a junior data analyst working on the marketing analyst team at Cyclistic, a bike-sharing company in Chicago. The marketing director believes that the company's future success depends on maximizing the number of annual memberships.

Therefore, your team wants to understand what differences exist in the use of Cyclistic bicycles between casual cyclists and annual members. Using these insights, your team will design a new marketing strategy to convert casual riders into annual members.

Before that, however, Cyclistic executives must approve your recommendations; Therefore, you must support your proposal with a compelling vision of the data and professional visualizations of the data.

# 1.ASK

### Business Task

-   How are annual members and casual riders different when it comes to using Cyclistic bikes?

-   Why would casual cyclists purchase Cyclistic annual memberships?

-   How can Cyclistic use digital media to influence casual cyclists to become members?

    Understand the differences in bicycle usage patterns between casual cyclists and annual members -Analyze historical records of trips made by users to identify trends. -Use these insights from data to design a new marketing strategy aimed at converting casual riders into annual members.

# 2. PREPARE

### Library Used📕

```{r}
library("tidyverse")
library('dplyr')
library("ggplot2")
library("lubridate")
library("gridExtra") 
library("readr")
library("geosphere")
library("gridExtra") 
library("ggmap")
```

-   Data used: ***04/2023 to 03/2024***

The data has been made available by Motivate International Inc. under this [**license.**](https://divvybikes.com/data-license-agreement)

```{r}
tripdata_2023_04 <- read.csv("C:/....../Nueva carpeta/202304-divvy-tripdata.csv")
tripdata_2023_05 <- read.csv("C:/....../Nueva carpeta/202305-divvy-tripdata.csv")
tripdata_2023_06 <- read.csv("C:/....../Nueva carpeta/202306-divvy-tripdata.csv")
tripdata_2023_07 <- read.csv("C:/....../Nueva carpeta/202307-divvy-tripdata.csv")
tripdata_2023_08 <- read.csv("C:/....../Nueva carpeta/202308-divvy-tripdata.csv")
tripdata_2023_09 <- read.csv("C:/....../Nueva carpeta/202309-divvy-tripdata.csv")
tripdata_2023_10 <- read.csv("C:/....../Nueva carpeta/202310-divvy-tripdata.csv")
tripdata_2023_11 <- read.csv("C:/....../Nueva carpeta/202311-divvy-tripdata.csv")
tripdata_2023_12 <- read.csv("C:/....../Nueva carpeta/202312-divvy-tripdata.csv")
tripdata_2024_01 <- read.csv("C:/....../Nueva carpeta/202401-divvy-tripdata.csv")
tripdata_2024_02 <- read.csv("C:/....../Nueva carpeta/202402-divvy-tripdata.csv")
tripdata_2024_03 <- read.csv("C:/....../Nueva carpeta/202403-divvy-tripdata.csv")

```

Now we must check each of the files with the "glimpse" function. Perhaps there are different types of values and we cannot join all the files into one.

After checking it we will join the files:

```{r}
alltripdata = bind_rows(tripdata_2023_04,tripdata_2023_05,tripdata_2023_06,tripdata_2023_07,tripdata_2023_08,tripdata_2023_09,tripdata_2023_10,tripdata_2023_11,tripdata_2023_12,tripdata_2024_01,tripdata_2024_02,tripdata_2024_03)
```

And check again:

```{r}
glimpse(alltripdata)
```

# 3. PROCESS

### Lets clean

Now lets clean the data to be able to properly work with it: First we will check the number of NAS that exists.

```{r}
colSums(is.na(alltripdata))
(100/nrow(alltripdata))*sum(is.na(alltripdata$end_lat))
(100/nrow(alltripdata))*sum(is.na(alltripdata$end_lng))
```

The number of missing values is very small, it does not matter if we delete those rows.

```{r}
alldataclean <- drop_na(alltripdata)
```

### Modify and add some ideas

Let's modify the value type that has "started_at" and "ended_at" to a value type of "date". We need "lubridate" for this function.

```{r}
alldataclean$started_at <- ymd_hms(alldataclean$started_at)
alldataclean$ended_at <- ymd_hms(alldataclean$ended_at)
```

The more broken down our data is, the easier it will be to analyze later. Therefore, it is better to make new columns indicating day, month, year and day of the week.

```{r}
alldataclean$date <- as.Date(alldataclean$started_at)
alldataclean$month <- format(as.Date(alldataclean$date), "%m")
alldataclean$day <- format(as.Date(alldataclean$date), "%d")
alldataclean$year <- format(as.Date(alldataclean$date), "%Y")
alldataclean$day_of_week <- format(as.Date(alldataclean$date), "%A")
```

It also looks clearer if we convert "ride-length" to a column where it would be represented in minutes.

```{r}
alldataclean$ride_length <- difftime(alldataclean$ended_at,alldataclean$started_at, units = "mins")
```

Por supuesto, sería muy importante que además de saber la duración de los viajes saber cuantos kilometros hacen los usuarios.

```{r}
alldataclean$ride_distance <- distGeo(matrix(c(alldataclean$start_lng,alldataclean$start_lat),ncol = 2), matrix(c(alldataclean$end_lng, alldataclean$end_lat),ncol = 2))
alldataclean$ride_distance <- alldataclean$ride_distance/1000
```

And the speed per km/h is not very relevant but it can contribute to our analysis.

```{r}
alldataclean$ride_speed = c(alldataclean$ride_distance)/as.numeric(c(alldataclean$ride_length), units = "hours")
```

Finally, we will do a final cleaning for those data that are not relevant. I have verified that there is data where the bicycles supposedly leave a station but they are verifications from the company.

```{r}
alldataclean <- alldataclean[!(alldataclean$start_station_name == "HQ QR" | alldataclean$ride_length<0),]
```

# 4. ANALYZE

We will start by analyzing the average distance that our two types of users make. In addition, we will calculate the number of rides they make in the week

## Average Distance and Number of Rides

```{r}
userType_means <- alldataclean %>% group_by(member_casual) %>% summarise(mean_time = mean(ride_length), mean_distance = mean(ride_distance))

membervstime <- ggplot(userType_means) +
                geom_col(mapping = aes(x=member_casual,y=mean_time,fill=member_casual), show.legend = FALSE) +
                labs(title = "Travels times by User type", x="User Type", y="Time in Mins")

membervsdistance <- ggplot(userType_means) + 
  geom_col(mapping=aes(x=member_casual,y=mean_distance,fill=member_casual), show.legend = FALSE)+
  labs(title = "Mean travel distance by User type",x="User Type",y="Mean distance In Km",caption = "Motivate International Inc")

  grid.arrange(membervstime, membervsdistance, ncol = 2)
```

![TRAVELS TIMES BY USER TYPE](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/fce73b8f-68bd-4219-a9d2-720c9777e12c)



| Member_Casual | Mean_time | Mean_distance |
|---------------|-----------|---------------|
| Casual        | 21 mins   | 2.120603 KM   |
| Member        | 12 mins   | 2.109597 KM   |

```{r}
  alldataclean %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n()
              ,average_duration = mean(ride_length),.groups = 'drop') %>% 
    arrange(member_casual, weekday)  %>% 
    ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Number of rides by User type during the week",x="Days of the week",y="Number of rides",caption = "Data by Motivate International Inc", fill="User type") +
    theme(legend.position="top")
```

![Rides distance](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/b2cc1075-c60c-436f-8e11-e61351cefcc1)


-   After seeing the graphs we can say that members use the service daily as their main of transport. On the other hand, casual users use the service for leisure on weekends. Because of this, they spend more time with the bikes than the members, but in terms of kilometers, it is quite tight although it may be a data collection error.

## Type of bikes

Now we move on to analyze which bicycles our users use.

```{r}
with_bike_type <- alldataclean %>% filter(rideable_type=="classic_bike" | rideable_type=="electric_bike")

with_bike_type %>%
  group_by(member_casual,rideable_type) %>%
  summarise(totals=n(), .groups = "drop") %>%
  
ggplot()+
  geom_col(aes(x=member_casual,y=totals,fill=rideable_type), position = "dodge") +
  labs(title = "Bike type usage by user type", x= "User type", y=NULL, fill="Bike type") + 
  theme_minimal() +
  theme(legend.position = "top")
```

![Bikes by user type](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/57b8dced-0571-4131-81d7-727490d167a1)


-   According to the data collected, we can see that users with annual subscriptions use both electric and classic bikes with a small difference of greater use of classic bicycles. While casual users there is a greater difference and they use electric ones more.

```{r}
with_bike_type %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual,rideable_type,weekday) %>%
  summarise(totals=n(), .groups = "drop") %>%
  
ggplot() +
  geom_col(aes(x=weekday,y=totals,fill= rideable_type), position = "dodge") +
  facet_wrap(~member_casual) +
  labs(title = "Bikes usage during a week", x="User type", y=NULL,caption = "Motivate International INC") +
  theme_minimal() +
  theme(legend.position = "top")
```
![Usage during week](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/d084cd98-38e6-4634-ab7b-5de80bdee93f)


-   Looking at the graph of "Bicycle use during the week" we can draw the conclusion that perhaps casual users use this type of bicycle because their reason for use is leisure. We can also see that members use electric bikes on Fridays, perhaps due to fatigue from the entire week since it is their main of transport.

## 2019 Data Compare

-   I previously carried out this same investigation but with data from 2019. It may be interesting to compare the data from different years to find out if something has changed in the company.

```{r}
ggplot(alldataclean) +
  geom_bar(mapping = aes(x = month, fill = member_casual)) +
  labs(title = "2024: Usertype vs Month", x = "Month", y = "Number of trips", fill = "Usertype", caption = "Motivate International Inc") + 
  scale_fill_hue(labels = c("Casual","Member"))
```

![2024  usertype vs month](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/e2bf625d-8ade-431f-8757-09c58d3adea5)


![2019 USERTYPE VS MONTH](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/bd7b3eb5-4756-4d61-a021-b50b015cefbc)


-   We can see that there has been an increase in casual users in some months such as January and February. But the summer months are still the important ones for the company.


![USERTYPE VS DAYOFWEEK](https://github.com/Andreamgz/Cyclistic-Bike-Share-Coursera-2023-2024/assets/173884816/1f00324a-5350-4942-9922-d8f395c496e8)


-   We can also see how casual users differ from members by using the service on weekends.
-   Finally we can say that despite the years that have passed there is not a big difference in terms of dates of use of the service.

# 5.SHARE

Taking into account the key questions

1.  How are annual members and casual riders different when it comes to using *Cyclistic* bikes?
2.  Why would casual cyclists purchase *Cyclistic* annual memberships?
3.  How can *Cyclistic* use digital media to influence casual cyclists to become members?

-   The main difference is that members use this service on a daily basis, we could say that it is their main transportation to go to work or simply to get around the city. While occasional users use bicycles mostly on weekends, as leisure activities.

-   This data is important for the marketing department to carry out strategies to encourage these occasional users to join the annual subscription to the service.

What type of strategies could be worked on? We could pay attention to the environment. The environment is a concern that has been increasing year after year in society. Like people's physical and mental health, a strategy could be applied where advertising shows the benefits of using bikes on a daily basis.

-   This advertising could be worked especially on social networks. People are glued to their mobile phones and their social networks like Instagram, X, Facebook... So you could invest in this type of advertising on the company profiles, in addition to looking for new clients through influencers.

-   A series of offers could also be planned for both types of users to encourage the use of this service.

#### Thanks For Reading!
