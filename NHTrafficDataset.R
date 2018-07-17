#Jeremy's code about number of stops for the time of day
library(tidyverse)
library(lubridate)
library(plotly)
getwd()
setwd("C:/Users/jerem.DESKTOP-GGM6Q2I/Documents/UNH Data Analytics/csv excel files")
nh=read.csv("NH_cleaned.csv")
nh$hour<-hour(hm(nh$stop_time))
l<-ggplot(nh,aes(x=hour,fill=county_name))+geom_bar()#bar chart#fill color by sex column
l<-l+ggtitle("Bar Chart of Hours")
l

###################################################################

#Jen's code about stop outcomes and days of the month
setwd("C:\\Users\\leger\\Desktop\\intro to R")

library(dplyr)
library(ggplot2)
library(plotly)
my_NHdata <- read.csv("NH_cleaned.csv")

#making a new column specific to the end of the month
mutate(my_NHdata$day_of_month <- substr(my_NHdata$stop_date, 9, 10))

#filtering out the last 3 days of the month since they dont happen every month
stopsfilter <- filter(my_NHdata, day_of_month < 29)

#this is the ggplot for the bar plot that shows number of stops for every single day of the month 
stopsfilter <- filter(my_NHdata, day_of_month < 29)
y <- ggplot(stopsfilter, aes(x=day_of_month, fill=stop_outcome)) +geom_bar()
y <- y + ggtitle("Number of Stops vs. Day of the Month")
y <- y+xlab("Day Of Month")+ylab("Count")
y <- y + theme_bw()
y

#table of stops vs. day of the month
stopstable <- table(stopsfilter$day_of_month)

#table of tickets vs. day of the month
ticketfilter <- filter(my_NHdata, stop_outcome == "Ticket", day_of_month <29)
tickettable <- table(ticketfilter$day_of_month)

#combining the tickettable with the stopstable
tickstoptable <- cbind(tickettable,stopstable)

#turning the tickstoptable into a dataframe so i can add columns to it
tickstopdataframe <- as.data.frame.matrix(tickstoptable) 

#new column with the percentage of tickets
tickstopdataframe1 <- mutate(tickstopdataframe, percent = tickettable / stopstable)
#new column with day of the month
tickstopdataframe2 <- mutate(tickstopdataframe1, day_of_month = 1:28)

#making a scatter plot of the percentages of tickets given over the month
plot(x=tickstopdataframe2$day_of_month, y=tickstopdataframe2$percent, xlab = "Day Of The Month", ylab = "Percentage Of Tickets Written", main = "Percent Of Tickets Written Per Day Of The Month", col = "Purple", type = "l", ylim=c(0,.6))

###################################################################

# Question: Do Out-of-State Residents get more tickets?
# Name: Yi-Ming Chang (Amy)

#check and setup the working directoy
getwd()
setwd("/Users/amy/Desktop")
getwd()
NHdata <- read.csv("NH_cleaned.csv")

#call the libraries that might use
library(dplyr) #using select()
library(Hmisc) #using describe()
library(tidyverse) #for final barchart

#view the raw data
View(NHdata)
class(NHdata)
describe(NHdata)

#select, only keep the column of "stop_outcome" and "out_of_state"
NHdata_select <- select(NHdata, stop_outcome, out_of_state)

#select, remove NA from the data
NHdata_keep <- NHdata_select[!is.na(NHdata_select$stop_outcome) & !is.na(NHdata_select$out_of_state),]

#view the going-to-be-analyzed data
describe(NHdata_keep)
View(NHdata_keep)

#create a new column called "Residency" by referring to the column of "out_of_state"
NHdata_keep$Residency <- ifelse(NHdata_keep$out_of_state,
                                "Out of State","In State")

#create the barchart: Residency vs Stop Outcome
getOption("scipen")
opt <- options("scipen" = 20)
x <- table(NHdata_keep$stop_outcome, NHdata_keep$Residency)
barplot(x, xlab = "Residency", ylab = "Stop Outcome",
        main = "Residency vs Stop Outcome",
        col = c("Orange", "Grey", "Pink", "LightBlue"), beside = TRUE)

#include the legend in the chart
legend("topright", legend=c("Checkup", "Summons", "Ticket", "Warning"),
       col=c("Orange", "Grey", "Pink", "LightBlue"),
       lty=c(1),lwd=c(4),pch=c(NA))

###################################################################

#Maz's code about the gender vs. stop outcome 
library(dplyr)
library(ggplot2)

#assigning our data set to variable called mynhdata
mynhdata <- read.csv("NH_cleaned.csv")

#filtering all rows that the driver_gender is either M or F and assigning to new variable
genderfilter <- filter(mynhdata, driver_gender == "F" | driver_gender =="M"  )

#filtering the stop_outcome (people who got ticket)
genderticketfilter <- filter(genderfilter, stop_outcome == "Ticket")


m <- ggplot(genderticketfilter, aes(x=driver_gender,fill=driver_gender)) +geom_bar()

m <- m + ggtitle("Gender vs. Ticket")
m <- m+xlab("Gender")+ylab("Number of tickets")
m <- m + theme_bw()
m

n <- ggplot(genderfilter, aes(x=driver_gender,fill=stop_outcome)) +geom_bar(aes(y =(..count..)/sum(..count..) ))
n <- m + ggtitle("Gender vs.  stop outcome")
n <- m+xlab("Gender")+ylab("Number of tickets")
n <- m + theme_bw()
n

###################################################################

#Frawley's code about percentage of warnings for males and females 
library(dplyr)
NHdata=read.csv("NH_cleaned.csv")
NHdatatibble<- tbl_df(traffic)

traffic[traffic==""] = NA

traffic <- filter(traffic,
                  !is.na(driver_gender),
                  stop_outcome == "Warning")
warningtable <- table(traffic$driver_gender)
warningtable

stopstable <- table(NHdata$driver_gender)
stopstable

totaltabledataframe<- as.data.frame.matrix(totaltable)

totaltabledataframe[-c(1),]

ihavenoidea <- newtotaltable$warningtable/newtotaltable$stopstable
ihavenoidea

barplot(ihavenoidea)
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued")
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued")
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued (%)", main="Warning Issued When Stopped", names.arg=c("Female", "Male"), col= c("hotpink", "darkslateblue"), ylim=c(0,1))
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued (%)", main="Percentage of Warnings Issued", names.arg=c("Female", "Male"), col= c("hotpink", "darkslateblue"), ylim=c(0,1))
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued (%)", main="Percentage of Warnings Issued", names.arg=c("Female", "Male"), col= c("hotpink", "darkslateblue"), ylim=c(0,1), legend("topright", legend=c("Female", "Male"), col=c("hotpink", "darkslateblue"), lty=c(1),lwd=c(4),pch=c(NA))
barplot(ihavenoidea, xlab = "Gender", ylab="Warning Issued (%)", main="Percentage of Warnings Issued", names.arg=c("Female", "Male"), col= c("hotpink", "darkslateblue"), ylim=c(0,1))
legend("topright",fill=c("hotpink", "darkslateblue") ,c("Female", "Male") )