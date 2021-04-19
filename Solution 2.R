# Selecting the working Directory

setwd("D:/Simplilearn/Project Data Sets/2/1567503160_comcasttelecomcomplaintsdata")

getwd()

# Importing the dataset

Comcast <- read.csv('Complaints.csv',head = TRUE,sep = ',')

View(Comcast)

str(Comcast)

# Let us check if there is any missing data

Comcastna <- is.na('Comcast')

length(Comcastna[Comcastna == T])

# As per the results there is no missing values in the dataset

library(lubridate)

Comcast$Date <- dmy(Comcast$Date)

# Let us extract the monthly and daily count tickets

library(dplyr)

monthly_tickets <- summarise(group_by(Comcast,month = as.integer(month(Date))), count = n())

# Let us remove the NA values

monthly_tickets <- na.omit(monthly_tickets)

daily_tickets <- summarise(group_by(Comcast,Date),count = n())

daily_tickets <- na.omit(daily_tickets)

monthly_tickets <- arrange(monthly_tickets,month)

# Let us plot the monthly and daily complaints to perform a comparison

library(ggplot2)

# Monthly tickets

ggplot(data = monthly_tickets,aes(month,count,label = count))+
  geom_line()+
  geom_point(size = 0.5)+
  geom_text()+
  scale_x_continuous(breaks = monthly_tickets$month)+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
  theme(plot.title = element_text(hjust = 0.5))

# Daily Tickets

ggplot(data = daily_tickets,aes(as.POSIXct(Date),count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))
# Complaint Type Processing
# To find which types of Complaints are maximum

network_issues <- contains(Comcast$Customer.Complaint,match="network",ignore.case = T)

internet_issues <- contains(Comcast$Customer.Complaint,match = "internet",ignore.case = T)

billing_issues <- contains(Comcast$Customer.Complaint,match = "billing",ignore.case = T)

charges_issues <- contains(Comcast$Customer.Complaint,match = "charge", ignore.case = T)

email_issues <- contains(Comcast$Customer.Complaint,match = "email", ignore.case = T)

Comcast$ComplaintType[internet_issues]<- "Internet"

Comcast$ComplaintType[network_issues] <- "Network"

Comcast$ComplaintType[billing_issues] <- "billing"

Comcast$ComplaintType[charges_issues] <- "Charges"

Comcast$ComplaintType[email_issues] <- "Email"

Comcast$ComplaintType[-c(internet_issues,network_issues,billing_issues,charges_issues,email_issues)] <- "Others"
table(Comcast$ComplaintType)

# Let us create a new categorical variable with value as Open and Closed

open_complaints <- (Comcast$Status == "Open"| Comcast$Status=="Pending")

closed_complaints <- (Comcast$Status=="Closed"| Comcast$Status=="Solved")

Comcast$ComplaintStatus[open_complaints] <- "Open"

Comcast$ComplaintStatus[closed_complaints] <- "Closed"

# Even after changing it finding for NA values

na_vector <- is.na(Comcast)

length(na_vector[na_vector==T])

Comcast <- subset(Comcast,!is.na(Comcast$ComplaintStatus))

# State wise Complaints in a stacked bar chart

library(stringi)

library(ggpubr)

Comcast <- group_by(Comcast,State,ComplaintStatus)

chart_data <- summarise(Comcast,Count = n())

ggplot(as.data.frame(chart_data), mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "Red"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status",
       x = "States",y = "No of Tickets",
       fill= "Status")

# To Provide the percentage of complaints resolved till date,
 # which were received through the Internet and customer care calls
 # To know Which state has the maximum complaints
 # Which state has the highest percentage of unresolved complaints

resolved <- group_by(Comcast, ComplaintStatus)

total_resolved <- summarise(resolved,percentage=(n()/nrow(resolved)))

resolved <- group_by(Comcast,Received.Via,ComplaintStatus)

Category_resloved<- summarise(resolved ,percentage =(n()/nrow(resolved))) 

# To visualize the results 
# Let's plot this in a pie chart

par(mfrow = c(1,2))
total <- ggplot(total_resolved,
              aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())

total

# To get the visualized result of categorized wise Ticket status

category<-ggplot(Category_resloved,
                 aes(x= "",y =percentage,fill = ComplaintStatus))+
  geom_bar(stat = "identity",width = 1)+
  coord_polar("y",start = 0)+
  geom_text(aes(label = paste0(Received.Via,"-",round(percentage*100),"%")),
            position = position_stack(vjust = 0.5))+
  labs(x = NULL,y = NULL,fill = NULL)+
  theme_classic()+theme(axis.line = element_blank(),
                        axis.text = element_blank(),
                        axis.ticks = element_blank())
ggarrange(total,category,nrow = 1, ncol = 2)

category