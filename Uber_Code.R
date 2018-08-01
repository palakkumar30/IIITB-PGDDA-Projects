library(stringr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

#Read the file in R
uber<-read.csv("Uber Request Data.csv", stringsAsFactors = F, header = TRUE)

#Make the date seperator consistent and substitute it in a new derived column for request/drop dates:
uber$request_date <- str_replace_all(uber$Request.timestamp, "[/]", "-")
uber$drop_date <- str_replace_all(uber$Drop.timestamp, "[/]", "-")

#Convert date and time in date time format
uber$request_date <- as.POSIXlt(uber$request_date, format = "%d-%m-%Y %H:%M")
uber$drop_date <- as.POSIXlt(uber$drop_date, format = "%d-%m-%Y %H:%M")

#Create new derived column seperating only hour using request_date column
uber$request_hour <- format(uber$request_date, "%H")

#Create new derived column seperating only date using request_date column
uber$Date <- format(uber$request_date,"%d")

#Convert column "request_hour" to numeric for numerical analysis
uber$request_hour<-as.numeric(uber$request_hour)

#make separate Time slots
for (i in 1:nrow(uber)) {
      record <- uber[i, ]
      record
      if(record$request_hour<=5) {
      uber[i,"Time_slot"]<-"Early_Morning"
      } else if(record$request_hour>5 && record$request_hour<=10) {
          uber[i,"Time_slot"]<-"Morning_Peak_Time"
      } else if(record$request_hour>10 && record$request_hour<=17) {
          uber[i,"Time_slot"]<-"Day_Time"
      } else if(record$request_hour>17 && record$request_hour<=22) {
          uber[i,"Time_slot"]<-"Evening_Peak_Time"
      } else {
          uber[i,"Time_slot"]<-"Late_Evenings"
      }
}

# 1. Visually identify the most pressing problems for Uber: 
#Plots to dentify the most problematic types of requests (city to airport / airport to city etc.) and the time slots (early mornings, late evenings etc.) 
#Plot the number of cabs requested in a particular hour
hourly_request_freq <- ggplot(uber,aes(x=factor(request_hour),fill=factor(Pickup.point)))
plot1 <- hourly_request_freq +geom_bar(stat='count',position = "dodge")+ ggtitle("Fig. 1. Demand of Uber Cabs on Hourly Basis") + labs(x="Time(Hours)", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Plot the number of cabs requested for each Time_slot
Timeslot_freq <- ggplot(uber,aes(x=factor(Time_slot),fill=factor(Pickup.point)))
plot2 <- Timeslot_freq +geom_bar(stat='count',position = "dodge")+ggtitle("Fig. 2. Demand of Uber Cabs for all Time Slots(Peak and Off-Peak hours)")+labs(x="Time_Slots", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Combining all the plots thats shows demand of Uber cabs (hourly basis and during the Time slots) on a single page 
grid.arrange(plot1, plot2, ncol=2, top=textGrob("Creation of Time Slots as per the frequency of Requested Cabs", gp=gpar(fontsize=12, font = 2)))


#2. Identifying Gap for Demand and Supply for pickup point 'Airport'
#Create a subset for pickup point 'Airport'
airport_trip<-subset(uber,uber$Pickup.point=="Airport")

#Plot for this subset for all time slots reflecting the cab status
airport_trip_plot <- ggplot(airport_trip,aes(x=Time_slot,fill=factor(Status)))
plot11<- airport_trip_plot +geom_bar(stat='count',position = "dodge")+ggtitle("Fig. 1. Gap for Demand and Supply for pickup point 'Airport'")+labs(x="Time Slots", y="Requested no. of Cabs")+ labs(fill="Cab Status")+ annotate("text", x=-Inf,y=Inf,label="72%", hjust=-8.0,vjust=1)


# Identifying Gap for Demand and Supply for pickup point 'City'
#Create a subset for pickup point 'City'
city_trip<-subset(uber,uber$Pickup.point=="City")

#Plot for this subset for all time slots reflecting the cab status
city_trip_plot <- ggplot(city_trip,aes(x=Time_slot,fill=factor(Status)))
plot12<- city_trip_plot +geom_bar(stat='count',position = "dodge")+ggtitle("Fig. 2. Gap for Demand and Supply for pickup point 'City'")+labs(x="Time Slots", y="Requested no. of Cabs")+ labs(fill="Cab Status") + annotate("text", x=-Inf,y=Inf,label="47%", hjust=-13.9,vjust=1)

#Combining all the plots thats shows demand of Uber cabs (hourly basis and during the Time slots) on a single page 
grid.arrange(plot11, plot12, ncol=2, top=textGrob("Gap in Demand vs Supply of Uber Cabs", gp=gpar(fontsize=12, font = 2)))

#Plot to visualise the frequency of requests that get cancelled or show 'no cars available'
#Plot the number of cabs requested for every Trip Status
Trip_status<-ggplot(uber,aes(x=factor(Status),fill=factor(Pickup.point)))
plot3 <- Trip_status +geom_bar(stat='count',position = "dodge")+ggtitle("Frequency of requests that get 'Cancelled' or show 'No cars available'")+labs(x="Time_Slots", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Combining all the above plots on a single page 
grid.arrange(plot1, plot2, plot3,ncol=2, top=textGrob("Demand of Uber Cabs", gp=gpar(fontsize=12, font = 2)))

#Find the Status of Cabs during "Morning_Peak_Time"
Problem1_Morning_Peak <- subset(uber,uber$Time_slot=="Morning_Peak_Time")
Problem1_Freq <- ggplot(Problem1_Morning_Peak,aes(x=factor(Status),fill=factor(Pickup.point)))
#Plot the Status of Cabs during "Morning_Peak_Time"
plot4<- Problem1_Freq +geom_bar(stat='count',position = "stack")+ggtitle("Fig. 1. Status of Cabs during Morning_Peak_Time")+labs(x="Cab Status", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Find the Status of Cabs during "Early_Morning"
Problem2_Early_Morning<- subset(uber,uber$Time_slot=="Early_Morning")
Problem2_Freq <- ggplot(Problem2_Early_Morning,aes(x=factor(Status),fill=factor(Pickup.point)))
#Plot the Status of Cabs during "Early_Morning"
plot5<- Problem2_Freq +geom_bar(stat='count',position = "stack")+ggtitle("Fig. 2. Status of Cabs during Early Morning")+labs(x="Cab Status", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Find the Status of Cabs during "Day Time"
Problem3_Day_Time<- subset(uber,uber$Time_slot=="Day_Time")
Problem3_Freq <- ggplot(Problem3_Day_Time,aes(x=factor(Status),fill=factor(Pickup.point)))
#Plot the Status of Cabs during "Day_Time"
plot6<- Problem3_Freq +geom_bar(stat='count',position = "stack")+ggtitle("Fig. 3. Status of Cabs in Day Time")+labs(x="Cab Status", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Find the Status of Cabs during "Evening_Peak_Time"
Problem4_Evening_Peak_Time<- subset(uber,uber$Time_slot=="Evening_Peak_Time")
Problem4_Freq <- ggplot(Problem4_Evening_Peak_Time,aes(x=factor(Status),fill=factor(Pickup.point)))
#Plot the Status of Cabs during "Evening_Peak_Time"
plot7<- Problem4_Freq +geom_bar(stat='count',position = "stack")+ggtitle("Fig. 4. Status of Cabs during Evening Peak Time")+labs(x="Cab Status", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Find the Status of Cabs during "Late Evenings"
Problem5_Late_Evenings<- subset(uber,uber$Time_slot=="Late_Evenings")
Problem5_Freq <- ggplot(Problem5_Late_Evenings,aes(x=factor(Status),fill=factor(Pickup.point)))
#Plot the Status of Cabs during "Late Evenings"
plot8<- Problem5_Freq +geom_bar(stat='count',position = "stack")+ggtitle("Fig. 5. Status of Cabs during Late Evenings")+labs(x="Cab Status", y="Requested no. of Cabs")+ labs(fill="Pickup Point")

#Combining all the above plots on a single page 
grid.arrange(plot4,plot5,plot6,plot7,plot8,ncol=2, top=textGrob("Plots highlighting the Cab Status vs Number of Cabs Requested for various Time Slots from Airport/City ", gp=gpar(fontsize=12, font = 2)))

# demand vs supply analysis for pick up point "City" during Morning_Peak_time
Morning_Peak_subset <- subset(uber,uber$Time_slot=="Morning_Peak_Time")
total_cabs_req_Morning_Peak_city <- length(which(Morning_Peak_subset$Pickup.point=="City"))
city_cancelled_trip_freq <- length(which((Morning_Peak_subset$Pickup.point=="City") & (Morning_Peak_subset$Status == "Cancelled")))
city_cancelled_trip_freq_perc <-round((city_cancelled_trip_freq /total_cabs_req_Morning_Peak_city*100),1)

city_completed_trip_freq<-length(which((Morning_Peak_subset$Pickup.point=="City") & (Morning_Peak_subset$Status == "Trip Completed")))

# demand vs supply analysis for pick up point "Airport" during Evening_Peak_time
Evening_Peak_subset <- subset(uber,uber$Time_slot=="Evening_Peak_Time")
total_cabs_req_Evening_Peak_Airport<-length(which(Evening_Peak_subset$Pickup.point=="Airport"))
Airport_no_car_trip_freq <- length(which((Evening_Peak_subset$Pickup.point=="Airport") & (Evening_Peak_subset$Status == "No Cars Available")))
Airport_no_car_trip_freq_perc <-round((Airport_no_car_trip_freq /total_cabs_req_Evening_Peak_Airport*100),1)

Airport_completed_trip_freq<-length(which((Morning_Peak_subset$Pickup.point=="Airport") & (Morning_Peak_subset$Status == "Trip Completed")))
