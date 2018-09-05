setwd("C:/Users/AP041200/Downloads/Upgrad & IIIT B")
Uber_data<- read.csv("Uber Request Data.csv",stringsAsFactors = F,header = T)

View(Uber_data)
library(sqldf)

str(Uber_data)
names(Uber_data)

names(Uber_data)<- c('Request_id','Pickup_point','Driver_id','Status','Request_timestamp','Drop_timestamp')
names(Uber_data)


install.packages("lubridate")
library(lubridate)
Uber_data$Request_timestamp<- parse_date_time(Uber_data$Request_timestamp,
                                              orders = c("%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M"),
                                              locale = "eng")
Uber_data$Drop_timestamp<- parse_date_time(Uber_data$Drop_timestamp,
                                           orders = c("%d-%m-%Y %H:%M:%S","%d-%m-%Y %H:%M"),
                                           locale = "eng")
Uber_data$Request_date<-as.Date(Uber_data$Request_timestamp)

Uber_data$Request_time <- format(Uber_data$Request_timestamp,"%H:%M:%S")

Uber_data$Drop_date<-as.Date(Uber_data$Drop_timestamp)
Uber_data$Drop_time<-format(Uber_data$Drop_timestamp,"%H:%M:%S")
Uber_data$Request_time_hour<-as.numeric(format(strptime(Uber_data$Request_time,"%H:%M:%S"),'%H'))
library(xlsx)
library(dplyr)
write.xlsx(Uber_data,"C:/Users/AP041200/Downloads/Upgrad & IIIT B/Uber_data.xlsx")
Uber_data<-Uber_data1

library(chron)

View(Uber_data)

library("sqldf")

#Trip cancelled  in Early Morning
data_cancelled_Early_Morning_time<-sqldf("select count(*) from Uber_data where Stats='Early Morning' and (status='Cancelled' OR status='No Cars Available')")
#Trip completed in Early Morning
data_tripcompleted_Early_morning_time<-sqldf("select count(*) from Uber_data where Stats='Early Morning' and status='Trip Completed'")
data<-sqldf("select count(*) from Uber_data GROUP_BY Pickup_point")

#Trip cancelled  Morning
data_cancelled_Morning_time<-sqldf("select count(*) from Uber_data where Stats='Morning' and (status='Cancelled' OR status='No Cars Available')")
#Trip completed in Morning
data_tripcompleted_Morning_time<-sqldf("select count(*) from Uber_data where Stats='Morning' and status='Trip Completed'")


#Trip cancelled in Afternoon
data_cancelled_Afternoon_time<-sqldf("select count(*) from Uber_data where Parts_of_the_day='Afternoon' and (status='Cancelled' OR status='No Cars Available')")
#Trip completed in Afternoon 
data_tripcompleted_Afternoon_time<-sqldf("select count(*) from Uber_data where Parts_of_the_day='Afternoon' and status='Trip Completed'")


#Trip cacelled in Evening
data_cancelled_Evening_time<-sqldf("select count(*) from Uber_data where Stats='Evening' and (status='Cancelled' OR status='No Cars Available')")
#Trip completed in Evening
data_tripcompleted_Evening_time<-sqldf("select count(*) from Uber_data where Stats='Evening' and status='Trip Completed'")

#Trip cacelled in Night
data_cancelled_Night_time<-sqldf("select count(*) from Uber_data where Stats='Night' and (status='Cancelled' OR status='No Cars Available')")
#Trip completed in Night
data_tripcompleted_Night_time<-sqldf("select count(*) from Uber_data where Stats='Night' and status='Trip Completed'")





#Request from Pickup(airport)  and Destination(city)
data_AirportCity_completed_earlymorning<-sqldf("select count(*) from Uber_data where Stats='Early Morning' and status='Trip Completed' and Pickup_point='Airport'")
data_AirportCity_cancelled_earlymorning<-sqldf("select count(*) from Uber_data where Stats='Early Morning' and (status='Cancelled' OR status='No Cars Available') and Pickup_point='Airport'")
#Demand is the requested trips  and supply is where the requested trips gets fulfilled 
# so from the total trips if the trip completed is more than the cancelled trip or no available cabs then the supply is high and demand is low 
#if the trip completed is less than the cancelled trips or no available cabs then the supply is low and demand is more 
# we have to see these scenario w.r.t the plots 

