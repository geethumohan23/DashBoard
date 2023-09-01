setwd('C:/Users/geeth/Desktop/Visualization/')

library(dplyr)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(mice)
library(GGally)
library(lubridate)
library(xts)
library(ggforce)
library(reshape)

library(rgdal)
library(Rcpp)
library(sf)
library(tidyverse)
library(ggmap)
library(leaflet)
library(maps)
library(shinyWidgets)
library(echarts4r)

data <- read.csv('37-00049_UOF-P_2016_prepped.csv')
head(data)
data<-data[-1,]
#sample_n(data,3)
str(data)
ncol(data)
nrow(data)
is.na(data)
apply(is.na(data),2,which)
#md.pattern(data)
sapply(data, function(x) sum(is.na(x)))


data[data=='']<-NA
new_data <- subset(data, select=c(0:39,47))
sapply(new_data, function(x) sum(is.na(x)))

new_data$STREET_DIRECTION[new_data$STREET_DIRECTION=='NULL']<-NA

#Treating Time and Date fields

new_data$INCIDENT_DATE<-mdy(new_data$INCIDENT_DATE)
new_data$OFFICER_HIRE_DATE <- mdy(new_data$OFFICER_HIRE_DATE)

#cONVERTING TO NUMERIC
new_data$UOF_NUMBER <- as.numeric(factor(new_data$UOF_NUMBER))
new_data$OFFICER_ID <- as.numeric(factor(new_data$OFFICER_ID))
new_data$OFFICER_GENDER <- as.numeric(factor(new_data$OFFICER_GENDER))
new_data$OFFICER_RACE <- as.numeric(factor(new_data$OFFICER_RACE))
new_data$OFFICER_YEARS_ON_FORCE <- as.numeric(factor(new_data$OFFICER_YEARS_ON_FORCE))
new_data$OFFICER_INJURY<-as.numeric(as.factor(new_data$OFFICER_INJURY))
new_data$OFFICER_INJURY_TYPE<-as.numeric(as.factor(new_data$OFFICER_INJURY_TYPE))
new_data$OFFICER_HOSPITALIZATION<-as.numeric(as.factor(new_data$OFFICER_HOSPITALIZATION))
new_data$SUBJECT_ID <- as.numeric(factor(new_data$SUBJECT_ID))
new_data$SUBJECT_GENDER <- as.numeric(factor(new_data$SUBJECT_GENDER))
new_data$SUBJECT_RACE <- as.numeric(factor(new_data$SUBJECT_RACE))
new_data$SUBJECT_INJURY <- as.numeric(factor(new_data$SUBJECT_INJURY))
new_data$SUBJECT_INJURY_TYPE <- as.numeric(factor(new_data$SUBJECT_INJURY_TYPE))
new_data$SUBJECT_WAS_ARRESTED <- as.numeric(factor(new_data$SUBJECT_WAS_ARRESTED))
new_data$SUBJECT_DESCRIPTION <- as.numeric(factor(new_data$SUBJECT_DESCRIPTION))
new_data$SUBJECT_OFFENSE <- as.numeric(factor(new_data$SUBJECT_OFFENSE))

new_data[, c("REPORTING_AREA", "BEAT", "SECTOR")] <- lapply(new_data[, c("REPORTING_AREA", "BEAT", "SECTOR")], function(x) type.convert(x, na.strings = "NA", as.is = FALSE))
new_data[, c("DIVISION", "LOCATION_DISTRICT", "STREET_NUMBER", "STREET_NAME", "STREET_DIRECTION", "STREET_TYPE","LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION", "LOCATION_CITY", "LOCATION_STATE" )] <- lapply(new_data[, c("DIVISION", "LOCATION_DISTRICT", "STREET_NUMBER", "STREET_NAME", "STREET_DIRECTION", "STREET_TYPE","LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION", "LOCATION_CITY", "LOCATION_STATE")], function(x) type.convert(x, na.strings = "NA", as.is = FALSE))

#new_data[, c("LOCATION_LATITUDE", "LOCATION_LONGITUDE")] <- lapply(new_data[, c("LOCATION_LATITUDE", "LOCATION_LONGITUDE")], function(x) type.convert(x, na.strings = "NA", as.is = FALSE))
new_data$OFFICER_YEARS_ON_FORCE <- as.numeric(factor(new_data$OFFICER_YEARS_ON_FORCE))
new_data$TYPE_OF_FORCE_USED2 <- as.numeric(factor(new_data$TYPE_OF_FORCE_USED2))
new_data$TYPE_OF_FORCE_USED3 <- as.numeric(factor(new_data$TYPE_OF_FORCE_USED3))
new_data$TYPE_OF_FORCE_USED4 <- as.numeric(factor(new_data$TYPE_OF_FORCE_USED4))

str(new_data)

#Imputation on numeric data
# imputed_data<-mice(new_data,m=6,method="pmm")
# summary(imputed_data)
# new_data<-complete(imputed_data)

#Mean value imputation
new_data$TYPE_OF_FORCE_USED2[is.na(new_data$TYPE_OF_FORCE_USED2)] <- mean(new_data$TYPE_OF_FORCE_USED2, na.rm = TRUE)
new_data$TYPE_OF_FORCE_USED3[is.na(new_data$TYPE_OF_FORCE_USED3)] <- mean(new_data$TYPE_OF_FORCE_USED3, na.rm = TRUE)
new_data$TYPE_OF_FORCE_USED4[is.na(new_data$TYPE_OF_FORCE_USED4)] <- mean(new_data$TYPE_OF_FORCE_USED4, na.rm = TRUE)


# 2 way column

two_table<-table(new_data$UOF_NUMBER, new_data$SUBJECT_GENDER)
head(two_table)

#Bar Plot (used force)
par(mar=c(1,1,1,1))

Male<-sum(new_data$SUBJECT_GENDER=='Male')
Female<-sum(new_data$SUBJECT_GENDER=='Female')
sex<-as.data.frame(c("Male", "Female")) 
counts<-as.data.frame(c(Male,Female)) 
barplot(counts,
        names.arg = c("Male", "Female"),
        xlab="Sex",
        tlab="Count",
        col="blue",
        density=1500,
        main="Number of Male and Female involved in the crime")

# Create a barplot using ggplot2
ggplot(counts, aes(x = sex, y = count, fill = sex)) +
  geom_bar(stat = "identity") +
  xlab("sex") +
  ylab("Male") +
  ggtitle("Number of Males and Females Involved in Crime")

ggplot2.barplot(data=new_data, xName="sex", yName='counts')


#Pie chart
ggplot(new_data, aes(x="", y=DIVISION, fill=SUBJECT_GENDER)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  
  theme_void()
#Dot Plot




#Histogram

Female
# Create a vector of counts
counts <- c(Male, Female)
counts

# Create a vector of labels
labels <- c("Male", "Female")

# Create the histogram plot
plot(density(new_data$REPORTING_AREA, from = 0), 
     xlim=c(0,800),main="Density Plot of Reporting Area",
     xlab="Area Code",
     )

# Create the density plot
# Basic density
pd <- ggplot(new_data, aes(x=REPORTING_AREA, fill=REPORTING_AREA)) + 
  geom_density()
pd
# Add mean line
pd+ geom_vline(aes(xintercept=mean(REPORTING_AREA)),
              color="blue", linetype="dashed", size=1)


# Basic box plot
pb <- ggplot(new_data, aes(x=REPORTING_AREA, y=SECTOR)) + 
  geom_boxplot()
pb
# Rotate the box plot
pb + coord_flip() 
# Notched box plot
ggplot(new_data, aes(x=REPORTING_AREA, y=SECTOR)) + 
  geom_boxplot(notch=TRUE) 
# Change outlier, color, shape and size
ggplot(new_data, aes(x=REPORTING_AREA, y=SECTOR)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)



# Create a data frame with some sample data
# Create the violin plot
pv<-ggplot(new_data, aes(x = STREET_DIRECTION, y = SUBJECT_RACE)) +
  geom_violin()

pv+stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  stat_summary(fun.y=median, geom="point", size=2, color="red")
  

# Create the Sina plot
#str(new_data$STREET_DIRECTION)
ps<-ggplot(new_data, aes(x=STREET_DIRECTION, y=SUBJECT_RACE))+
  scale_y_log10()
ps+geom_sina()


#Pair plot
ggpairs(new_data[,5:10])

# Create a scatter plot using ggplot2
ggplot(new_data, aes(x = SUBJECT_RACE, y = TYPE_OF_FORCE_USED3, color=SUBJECT_RACE)) + 
  geom_point(alpha = 0.3, size = 1) +
  labs(x = "Subject Race", y = "Force Used") +
  theme_classic()


#Corelation Analysis
corr_dataset <- subset(new_data, select=c(3:6,8:22))
corr_dataset<- data.frame(corr_dataset)
corr_matrix <- cor(corr_dataset)
corr_matrix
str(corr_dataset)

ggcorr(corr_dataset, label= T, hjust = 1, layout.exp = 5)


#Time series plot

is.finite(new_data$OFFICER_ID)
is.na(new_data$OFFICER_ID)

date_var<-new_data$UOF_NUMBER
tsplot<-xts(data.frame(date_var=date_var),new_data$INCIDENT_DATE)
autoplot(tsplot,FACET=NULL)+theme_bw()

#Time series colored
new_data<-new_data[order(new_data$INCIDENT_DATE),]
wd <-seq(as.Date("2016-01-01"),by="week",length=nrow(new_data))
head(wd)
ccts<- xts(new_data[,c("OFFICER_RACE","SUBJECT_RACE")],wd)
gts <- autoplot(ccts,facet=NULL) + xlab("OFFICER_RACE") + ylab("SUBJECT_RACE")
gts

gts +
  geom_vline(xintercept=as.numeric(as.Date("2040/01/01")),linetype=2,size=1)



#Interactive plot
theme_set(theme_classic())
# Plot

gk= ggplot(new_data, aes( x=TYPE_OF_FORCE_USED2, y=UOF_NUMBER))+ geom_line(aes(color=UOF_NUMBER))
gk

ggplot(new_data, aes(x=STREET_NUMBER, y=STREET_DIRECTION, color=STREET_DIRECTION))+ 
  geom_point(shape=19) + xlab ("STREET_NUMBER")+ ylab ("UOF_NUMBER")+ 
  labs (color="STREET_DIRECTION") + ggtitle("scatter plot of cars weight")


#Map
str(new_data)
map_plot <- map_data("LOCATION_STATE", "LOCATION_LATITUDE", "LOCATION_LONGITUDE") %>% 
  select(state = LOCATION_STATE, lat=LOCATION_LATITUDE, long=LOCATION_LONGITUDE)
head(map_plot)


new_data$id <- seq_len(nrow(new_data))
ggplot(new_data, aes(LOCATION_LONGITUDE, LOCATION_LATITUDE, group=id)) + 
  geom_point(size = .50, show.legend = FALSE, color="red") +
  geom_polygon(fill = "white", colour = "grey50")
  coord_quickmap()
  
  
e_gauge(91, min = 0, max = 100, symbol = '%', gaugeSectors(
    success = c(80, 100), warning = c(40, 79), danger = c(0, 39)
  ))
