#Set working Directory to Uber Case Study Folder
setwd("C:/Users/ansroy/Desktop/Personal/IIIT B/Assignment & Case Study/Case study 2 _UBER/")
#Read Uber Request File in Uber Variable
Uber <- read.csv("Uber request data.csv",header= T,stringsAsFactors = FALSE)
library("lubridate")
library(scales)
#Converting Character Date to Date Format
Uber$Date <- as.Date(gsub("/","-",Uber$Date),"%d-%m-%Y")
#Finding the Hour of Cab Request
Uber$Cab_Request_Hour <- as.character(gsub(":","",substr(Uber$Request.time,1,2)))
#Finding the Time_Slot
#Variables for THe different Time Slot
Pre_Morning <- c(1,2,3,4)
Morning_Rush <- c(5,6,7,8,9)
Day_Time <- c(10,11,12,13,14,15,16)
Evening_Rush <- c(17,18,19,20,21)
Late_Night<- c(22,23,0)
#Allocating the TIme Slot value based on Hour Using Loop & IfElse Command
for(i in 1:length(Uber$Cab_Request_Hour))
{
  Uber$Time_Slot[i] <- ifelse(Uber$Cab_Request_Hour[i] %in% Late_Night,  c("Late_Night") ,
                              ifelse(Uber$Cab_Request_Hour[i] %in% Pre_Morning,  c("Pre_Morning"),
                                     ifelse(Uber$Cab_Request_Hour[i] %in% Morning_Rush, c("Morning_Rush"),
                                            ifelse(Uber$Cab_Request_Hour[i] %in% Day_Time, c("Day_Time"),
                                                   ifelse(Uber$Cab_Request_Hour[i] %in% Evening_Rush, c("Evening_Rush"),c("NA"))))))}
#Altername Code of Doing the same thing as above
# Uber$Time_Slot[Uber$Cab_Request_Hour %in% Late_Night]<- "Late_Night"
# Uber$Time_Slot[Uber$Cab_Request_Hour %in% Pre_Morning]<- "Pre_Morning"
# Uber$Time_Slot[Uber$Cab_Request_Hour %in% Morning_Rush]<- "Morning_Rush"
# Uber$Time_Slot[Uber$Cab_Request_Hour %in% Day_Time]<- "Day_Time"
# Uber$Time_Slot[Uber$Cab_Request_Hour %in% Evening_Rush]<- "Evening_Rush"

#Adding 0 to Cab_request_Hour where the number of char is 1 so as to order the graph
for (i in 1:length(Uber$Cab_Request_Hour))
Uber$Cab_Request_Hour[i]<- ifelse(nchar(Uber$Cab_Request_Hour[i]) == 1,  paste("0",Uber$Cab_Request_Hour[i],sep="") , Uber$Cab_Request_Hour[i])
library("ggplot2")
Uber_Graph <- as.data.frame(table(Uber$Cab_Request_Hour,Uber$Pickup.point))
names(Uber_Graph) <- c("Hour","Pickup_Point", "Count")
#Creating Stacked Graph
Hour_Graph <- ggplot(Uber_Graph,aes(Hour,Count,fill = Pickup_Point))
Hour_Graph +   geom_bar(stat="identity", position = "stack") + labs(title="Hourly Count of Cabs Requested from City & Airport",y = "Number of Trips")

# PRoblem :- Bar chart for number of trips made for each time slot. Status = "Trip Completed"
Uber_graph_doc <- Uber[Uber$Status == "Trip Completed",]
Uber_graph_doc <- as.data.frame(table(Uber_graph_doc$Time_Slot))
names(Uber_graph_doc) <- c("Time_Slot","Count")
prob_defn_doc <- ggplot(Uber_graph_doc,aes(Time_Slot,Count,fill=Time_Slot,label=Count))
prob_defn_doc +   geom_bar(stat="identity") + labs(title="Number of Completed trips made for each time slots",y = "Number of Trips",x="Time Slot")


#PROBLEM IDENTIFICATION: 
#1 :- stacked bar chart where each bar represents a time slot and the y-axis shows the frequency of requests. 
#Different proportions of bars should represent the completed, cancelled and no cars available out of the total 
#customer requests.
Uber_prob_defn <- as.data.frame(table(Uber$Time_Slot,Uber$Status))
names(Uber_prob_defn) <- c("Time_Slot","Status","Count")
prob_defn_graph <- ggplot(Uber_prob_defn,aes(Time_Slot,Count,fill = Status))
prob_defn_graph +   geom_bar(stat="identity", position = "stack") + 
  labs(title="Number of trips made for different time slots with different trip status",y = "Number of Trips",x="Time Slot")

#2 :- Visually identify the 2 most pressing problems for Uber, out of the 15 possible scenarios (5 slots * 3 trip status).
# Soln - From the above graph, Evening Rush Time Slot(Between 17 to 21 Hours) Problem of No Cars Available, In Morning Rush Hour
#(Between 5 to 9 Hours) Cancelled cabs

#Problem 1: Morning Rush, Cancelled Cars
#1. For the time slot when problem 1 exists, plot a stacked bar chart to find out if the problem is more severe 
#for pick-up requests made at the airport or the city. As a next step, you have to determine the number of 
#times this issue exists in that time slot. Also find the percentage breakup for the total number of issues 
#in this time slot based on the pick-up point?
Uber_prob1 <- subset(Uber,(Uber$Time_Slot == "Morning_Rush" &  Uber$Status == "Cancelled") )
Uber_prob1_freq <- as.data.frame(table(Uber_prob1$Time_Slot,Uber_prob1$Pickup.point))
library(scales)
b <- aggregate(Uber_prob1_freq$Freq,by = list(Uber_prob1_freq$Var1),FUN = sum)
Uber_prob1_freq <- merge(Uber_prob1_freq,b,by.x = c("Var1"),by.y = c("Group.1"),all.x = TRUE)
b <- as.data.frame(c("1"))
names(b) <- c("Percent")
Uber_prob1_freq <- cbind(Uber_prob1_freq,b)
Uber_prob1_freq$Percent <- as.character(paste(round((Uber_prob1_freq$Freq*100)/Uber_prob1_freq$x),"%",sep=""))
remove(b)
prob1_graph <- ggplot(Uber_prob1_freq,aes(Var1,Freq,fill = Var2))
prob1_graph +   geom_bar(stat="identity", position = "stack") +  geom_text(aes(label=Percent)) +
   labs(title="Stack Bar Chart for the Morning Rush - Cancelled Car Status",y = "Number of Trips",x="Time Slot - Problem 1")
#Alternate Code - Without Manually Calculating the Percentage
ggplot(Uber_prob1_freq,aes(x = Var1, y = Freq,fill = Var2)) + 
  geom_bar(position = "stack",stat = "identity") +  scale_y_continuous(labels = percent_format()) + 
  labs(title="Stack Bar Chart for the Morning Rush - Cancelled Car Status",y = "Percentage of Trips",x="Time Slot - Problem 1") 


#2. Now let's find out the gap between supply and demand. For this case, the demand is the number of trip 
#requests made at the city, whereas the supply is the number of trips completed from city to the airport?
demand_city <- subset(Uber,Uber$Pickup.point == "City")
demand1 <- nrow(demand_city)
supply1 <- nrow(demand_city[demand_city$Status == "Trip Completed",])
gap1 <- demand1 - supply1
#demand1 -> total demand; supply1 -> otal supply;gap1 -> total gap. Not wrt the Problem timeslot
demand <- nrow(demand_city[demand_city$Time_Slot=="Morning_Rush",])
supply <- nrow(demand_city[demand_city$Status == "Trip Completed" & demand_city$Time_Slot=="Morning_Rush",])
gap <- demand - supply
#demand, supply & gap are for the time slot of the problem

#3. What do you think is the reason for this issue for the supply-demand gap? (Write the answer in less than 100 words).?
#Issue :-
demand_graph <- as.data.frame(table(demand_city$Status,demand_city$Time_Slot))
names(demand_graph) <- c("Status","Var2","Freq")
prob3_graph <- ggplot(demand_graph,aes(Var2,Freq,fill= Status))
prob3_graph +   geom_bar(stat="identity", position = "stack") +labs(title="City Pickup Demand Graph for different time slots",x="Time Slots",y="Number of Requests")

#Problem 2: Evening Rush, No Cars Available
#1. For the time slot when problem 2 exists, plot the stacked bar chart to find out if the issue is for 
#pick-up request made at the airport or the city. Just like problem 1, find the percentage breakup for issue 
#based on the pick-up point for the time slot in which problem 2 exists.
Uber_prob2 <- subset(Uber, (Uber$Time_Slot == "Evening_Rush" &  Uber$Status == "No Cars Available"))
Uber_prob2_freq <- as.data.frame(table(Uber_prob2$Time_Slot,Uber_prob2$Pickup.point))
b <- aggregate(Uber_prob2_freq$Freq,by = list(Uber_prob2_freq$Var1),FUN = sum)
Uber_prob2_freq <- merge(Uber_prob2_freq,b,by.x = c("Var1"),by.y = c("Group.1"),all.x = TRUE)
b <- as.data.frame(c("1"))
names(b) <- c("Percent")
Uber_prob2_freq <- cbind(Uber_prob2_freq,b)
Uber_prob2_freq$Percent <- as.character(paste(round((Uber_prob2_freq$Freq*100)/Uber_prob2_freq$x),"%",sep=""))
remove(b)
names(Uber_prob2_freq) <- c("Time_Slot","Pickup","Count","Tot_Count","Percent")
prob2_graph <- ggplot(Uber_prob2_freq,aes(Time_Slot,Count,fill = Pickup))
prob2_graph +   geom_bar(stat="identity", position = "stack") + 
  labs(title="Stack Bar Chart for the Evening Rush - No Cars Available Trip",y = "Number of Trips",x="Time Slot - Problem 2")
#Alternate Code :- Without Manually calculating the Percentage
ggplot(Uber_prob2_freq,aes(x = Time_Slot, y = Count,fill = Pickup)) + 
  geom_bar(position = "stack",stat = "identity") +  scale_y_continuous(labels = percent_format()) + 
  labs(title="Stack Bar Chart for the Evening Rush - No Cars Available Trip",y = "Number of Trips",x="Time Slot - Problem 2")

#2. Now let's find out the gap between supply and demand. For this case, the demand is the number of trip 
#requests made at the airport, whereas the supply is the number of trips completed from airport to the city.
demand_airport <- subset(Uber,Uber$Pickup.point == "Airport")
demand2 <- nrow(demand_airport[demand_airport$Time_Slot == "Evening_Rush",])
supply2 <- nrow(demand_airport[demand_airport$Status == "Trip Completed" & demand_airport$Time_Slot == "Evening_Rush",])
gap2 <- demand2 - supply2
#demand2,supply2 & gap2 -> the demand,supply & gap for the timeslot Evening Rush
demand2_1 <- nrow(demand_airport)
supply2_1 <- nrow(demand_airport[demand_airport$Status == "Trip Completed",])
gap2_1 <- demand2_1 - supply2_1
#demand2_1, suppl2_1, gap2_1 -> total demand, supply & gap for the complete day.


#3. What do you think is the reason for this issue for this supply-demand gap. (Not more than 100 words)?
demand_graph2 <- as.data.frame(table(demand_airport$Status,demand_airport$Time_Slot))
names(demand_graph2) <- c("Status","Var2","Freq")
prob3_graph2 <- ggplot(demand_graph2,aes(Var2,Freq,fill= Status))
prob3_graph2 +   geom_bar(stat="identity", position = "stack")  +labs(title="Airport Pickup Demand Graph for different time slots",x="Time Slots",y="Number of Requests")


#Save the Loan Data Frame in R to be opened by Tableau - For Tableau Practice
#save(Uber,file="Uber.RData")