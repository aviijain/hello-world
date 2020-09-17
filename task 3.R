install.packages("lubridate")
library(lubridate) # For year() 
library(dplyr)# To filter, group_by and summarize
library(ggplot2)# To plot using ggplot()
library(ggthemes)# For  Wall street journal theme "theme_wsj()"

#Reading data set aushealth.csv
bigstock <- read.csv(file="C:/Users/DEll/Desktop/LTU'/SEM 4/VA/assg 2/Big_stock_data.csv", header=TRUE, sep=",")
head(bigstock)

#Converting 'date' into Date format
bigstock$date<-as.Date(bigstock$date,"%d/%m/%Y")

#Extracting 'Year' form the Date
bigstock$st_year<-year(bigstock$date)

#Subsetting data by aggregating(mean) close price by year for Viz1
bg1<-bigstock %>% 
  filter(company %in% c("Apple","Amazon","Google","Facebook")) %>%
  group_by(year=st_year,Company=company) %>%
  summarize(Averagecp=mean(close_price))

myarrow=arrow(angle = 15, ends = "both", type = "closed")

#For dollar sign on y-axis
require(scales) 

#Visualization 1 - Average close price for Apple, Microsoft,Intel & SAP
ggplot(bg1, aes(x = year, y = Averagecp)) + 
  geom_line(aes(color = Company),arrow=myarrow) + 
  geom_point() +
  ggtitle("Average Close price for Stocks(2000-2017)") +
  theme_wsj()+ scale_colour_wsj("colors6") +
  scale_x_continuous(name="year",breaks=seq(2000,2017,1)) +
  scale_y_continuous(labels = dollar)

#Big 4 shared volume distribution
big4<-bigstock %>% 
  filter(company %in% c("Apple","Amazon","Google","Facebook"),st_year>=2013 & st_year<=2015) %>%
  group_by(year=st_year,Company=company) %>%
  summarize(Averagevol=median(volume))


#Visualization 2 - Average volume for Apple,Amazon,Facebook & Google

big4$Averagevol<-format(round(big4$Averagevol/ 1e6, 1), trim = TRUE)
big4$Averagevol<-as.numeric(big4$Averagevol)

ggplot(big4, aes(x = year, y = Averagevol, paste(y,"M"))) + 
  geom_line(aes(color = Company),arrow=myarrow) + 
  geom_point() +
  ggtitle("Highest Median volume share for Stocks('13-'15)") + 
  scale_colour_wsj("colors6") +
  theme_wsj() +
  scale_y_continuous(labels=function(x) paste0(x,"M")) +
  scale_x_continuous(name="year",breaks=seq(2013,2015,1))  +gghighlight(Company=='Apple')


#Subsetting data by aggregating(mean) close price by year for Viz2
bg2<-bigstock %>% 
  filter(company %in% c("Amazon","Google"), st_year>2004) %>%
  group_by(year=st_year,Company=company) %>%
  summarize(Averagecp=mean(close_price))

#Visualization 3 - Average close price for Amazon & Google
ggplot(bg2, aes(x = year, y = Averagecp)) + 
  geom_line(aes(color = Company),arrow=myarrow) + 
  geom_point() +
  ggtitle("Average Close price for Stocks(2005-2017)") +
  theme_wsj()+ scale_colour_wsj("colors6") +
  scale_x_continuous(name="year",breaks=seq(2005,2017,1)) +
  scale_y_continuous(labels = dollar)

#Subsetting data by aggregating(mean) close price by year for Viz3
bg3<-bigstock %>% 
  filter(company %in% c("Alibaba","Facebook"), st_year>2013) %>%
  group_by(year=st_year,Company=company) %>% 
  summarize(Averagecp=mean(close_price))

#Visualization 4 - Average close price for Alibaba & Facebook
ggplot(bg3, aes(x = year, y = Averagecp)) + 
  geom_line(aes(color = Company),arrow=myarrow) + 
  geom_point() +
  ggtitle("Average Close price for Stocks(2014-2017)") +
  theme_wsj()+ scale_colour_wsj("colors6") +
  scale_x_continuous(name="year",breaks=seq(2014,2017,1)) +
  scale_y_continuous(labels = dollar)

