
# read data into dataframe using the read.csv function
data <- read.csv('Uber Request Data.csv')

# using lubridate library for easy manipulation of date and time
library(lubridate)

# getting request and drop dates and times(only hours in 24 hour format)

temp1 <- date(dmy_hms(data$Request.timestamp))
temp2 <- date(dmy_hm(data$Request.timestamp))
temp1[is.na(temp1)] <- temp2[is.na(temp1)]
data$request_date <- temp1

temp3 <- hour(dmy_hm(data$Request.timestamp))
temp4 <- hour(dmy_hms(data$Request.timestamp))
temp3[is.na(temp3)] <- temp4[is.na(temp3)]
data$request_time <- temp3

temp5 <- date(dmy_hms(data$Drop.timestamp))
temp6 <- date(dmy_hm(data$Drop.timestamp))
temp5[is.na(temp5)] <- temp6[is.na(temp5)]
data$drop_date <- temp5

temp7 <- hour(dmy_hm(data$Drop.timestamp))
temp8 <- hour(dmy_hms(data$Drop.timestamp))
temp7[is.na(temp7)] <- temp8[is.na(temp7)]
data$drop_time <- temp7


# create time periods based on time(hour) of the day

# 12 AM to 4 AM - late night
# 4 AM to 8 AM - early morning
# 8 AM to 12 PM - morning
# 12 PM to 4 PM - noon
# 4 PM to 8 PM - evening
# 8 PM to 12 AM - night


for(row in 1:nrow(data)){
  dum <- data[row, 'request_time']
  
  if(dum >= 0 & dum <= 4){
    data[row, 'time_period'] <- 'late-night'
  }
  if(dum >= 5 & dum <= 8){
    data[row, 'time_period'] <- 'early-morning'
  }
  if(dum >= 9 & dum <= 12){
    data[row, 'time_period'] <- 'morning'
  }
  if(dum >= 13 & dum <= 16){
    data[row, 'time_period'] <- 'noon'
  }
  if(dum >= 17 & dum <= 20){
    data[row, 'time_period'] <- 'evening'
  }
  if(dum >= 21 & dum <= 24){
    data[row, 'time_period'] <- 'night'
  }
}

# creating success labels based on trip status

# Trip Completed -> Successful
# No Cars Available/Cancelled -> Unsuccessful

for(row in 1:nrow(data)){
  dum <- data[row, 'Status']
  
  if(dum == 'Trip Completed'){
    data[row, 'success_status'] <- 'successful'
  }
  else{
    data[row, 'success_status'] <- 'unsuccessful'
  }
}


library(ggplot2)

#visualizations

#how many cases of each category of status

ggplot(data = data, aes(x = Status, fill = Status)) + geom_bar(width = 0.5) + geom_text(stat = 'count', aes(label = ..count..), vjust=2)

# no of requests vs time

ggplot(data = data, aes(x = data$request_time)) + geom_histogram(fill="black", col="grey") + stat_bin(aes(y = ..count.., label = ..count..), geom = 'text', vjust = -1, drop = TRUE)


# pick-up point based request-times and status based stacked bar chart

ggplot(data = data, aes(x = data$time_period, fill = Status)) + geom_bar() + facet_wrap(~data$Pickup.point) + scale_x_discrete(limits=c('late-night', 'early-morning', 'morning', 'noon', 'evening', 'night')) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# success_status vs time period based on pick up point

ggplot(data = data, aes(x = data$time_period, fill = success_status)) + geom_bar() + facet_wrap(~data$Pickup.point)+ facet_wrap(~data$Pickup.point) + scale_x_discrete(limits=c('late-night', 'early-morning', 'morning', 'noon', 'evening', 'night')) + theme(axis.text.x = element_text(angle = 90, hjust = 1))



