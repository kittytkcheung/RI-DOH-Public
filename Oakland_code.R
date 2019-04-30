# required libraries
library(tidyverse)
library(psych)
library(trend)
library(broom)

# set working directory 
setwd("/Users/kittytcheung/Documents/MSAR/Spring/RI DOH/RI-DOH")

# read in excel data
raw_data = read.csv("Oakland Beach Data to Kitty Cheung_2-8-19_sp.csv")
raw_data08 = read.csv("Oakland Beach 2008 Data to Kitty Cheung_2-12-19_sp.csv", header=TRUE)
colnames(raw_data08)[1] = "Monitoring.Station"
colnames(raw_data08)[11] = "SWResults"

####################################################
####################################################
# 2008 data

# format 2008 data
raw_data08 = raw_data08 %>%
  mutate(StationNum = case_when(Monitoring.Station == "                Oakland Beach-Center  " ~ "1",
                                Monitoring.Station == "                Oakland Beach-East  " ~ "2",
                                Monitoring.Station == "                Oakland Beach-West  " ~ "3"))

# create new columns for month & year
raw_data08 = raw_data08 %>%
  mutate(Year = as.character(Start.Date))

for (i in 1:length(raw_data08$Year)) {
  x = str_split(raw_data08$Year[i], "/")
  raw_data08$Year[i] = str_trim(x[[1]][3], "both")
}

raw_data08 = raw_data08 %>%
  mutate(Month = as.character(Start.Date))

for (i in 1:length(raw_data08$Month)) {
  x = str_split(raw_data08$Month[i][1], "/")
  raw_data08$Month[i] =  str_trim(x[[1]][1], "both")
}

raw_data08 = raw_data08 %>%
  mutate(Day = as.character(Start.Date))

for (i in 1:length(raw_data08$Day)) {
  x = str_split(raw_data08$Day[i][1], "/")
  raw_data08$Day[i] =  str_trim(x[[1]][2], "both")
}

####################################################
####################################################
# main data

# create new columns for Station ID, Year, Month
raw_data = raw_data %>%
  mutate(StationNum = substr(StationID, 11, 11)) %>%
  mutate(Year = str_split(CollectionTime, " ")) %>%
  mutate(Month = str_split(CollectionTime, " ")) %>%
  mutate(Day = str_split(CollectionTime, " "))

for (i in 1:length(raw_data$Year)) {
  x = str_split(raw_data$Year[[i]][1], "/")
  x = x[[1]][3]
  raw_data$Year[i] = paste(20,x, sep="")
}

for (i in 1:length(raw_data$Month)) {
  x = str_split(raw_data$Month[[i]][1], "/")
  raw_data$Month[i] =  x[[1]][1]
}

for (i in 1:length(raw_data$Day)) {
  x = str_split(raw_data$Day[[i]][1], "/")
  raw_data$Day[i] =  x[[1]][2]
}

# remove 2006 test type 2 observations
raw_data = raw_data %>%
  filter(!(Year == 2006 & Test_Type == 2))

####################################################
####################################################
# coerce everything to numeric & merge
data = raw_data %>%
  select(SWResults, StationNum, Year, Month, Day)

data = data %>%
  mutate_all(function(x) as.numeric(as.character(x)))

data08 = raw_data08 %>%
  select(SWResults, StationNum, Year, Month, Day)

data08 = data08 %>%
  mutate_all(function(x) as.numeric(x))

# merge 2008 data with rest
df = merge(data, data08, by = c("SWResults", "StationNum", "Year", "Month", "Day"),
             all = TRUE)

####################################################
####################################################
# analysis
geomean_output = data.frame(matrix(ncol = 5, nrow = 0))
colnames(geomean_output) = c("Year", "Month", "StationNum", "geo_mean", "total_obsv")

for (i in c(unique(df$StationNum))) {
  station_output = data.frame()
  x = df %>% filter(StationNum == i)
  for (j in c(unique(x$Year))) {
    test = x %>%
      filter(Year == j) %>%
      filter(Month!= 5) %>%
      group_by(Month) %>%
      summarize(geo_mean = geometric.mean(SWResults), total_obsv = n()) 
    
    test$Year = rep(j, nrow(test))
    test$StationNum = rep(i, nrow(test))
    station_output = rbind(station_output, test)
  }
  geomean_output = rbind(geomean_output, station_output)
}

geomean_output = 
  geomean_output[c("Year", "Month", "StationNum", "geo_mean", "total_obsv")]

####################################################
####################################################
# stv, geomean > 30, geomean > 60

# count geomeans over 30 in a given year per station (excluding May)
geomeans30 = data.frame()
for (i in unique(geomean_output$StationNum)) {
  x = geomean_output %>%
    filter(StationNum == i) %>%
    group_by(Year) %>%
    summarize(geomean_30 = sum(geo_mean > 30))
  
  x$StationNum = rep(i, nrow(x))
  geomeans30 = rbind(geomeans30, x)
}

# count SWResults over 60 in a given year per station (includes May)
swresults60 = data.frame()
for (i in unique(df$StationNum)) {
  x = df %>%
    filter(StationNum == i) %>%
    group_by(Year) %>%
    summarize(swresults_60 = sum(SWResults > 60)) 
  
  x$StationNum = rep(i, nrow(x)) 
  swresults60 = rbind(swresults60, x)
}


# calculate stv (include May)
stv = data.frame()
colnames(stv) = c("Year", "Month")

for (i in unique(df$StationNum)) {
  station_output = data.frame()
  x = df %>% filter(StationNum == i)
  for (j in c(unique(x$Year))) {
    test = x %>%
      filter(Year == j) %>%
      group_by(Month) %>%
      summarize(stv_exceed = sum(SWResults > 110) > (n() * .1))
    
    test$Year = rep(j, nrow(test))
    test$StationNum= rep(i, nrow(test))
    station_output = rbind(station_output, test)
  }
  stv = rbind(stv, station_output)
}


stv2 = data.frame()
for (i in unique(stv$StationNum)) {
  station_output = data.frame()
  x = stv %>% filter(StationNum == i)
  for (j in c(unique(x$Year))) {
    test = x %>%
      filter(Year == j) %>%
      summarize(stv_count = sum(stv_exceed))
    
    test$Year = rep(j, nrow(test))
    test$StationNum= rep(i, nrow(test))
    station_output = rbind(station_output, test)
  }
  stv2 = rbind(stv2, station_output)
}

# merge swresults60 and geomeans30
annual_by_month_output = merge(stv2, swresults60, by = c("Year", "StationNum"))
annual_by_month_output = merge(annual_by_month_output, geomeans30, by = c("Year", "StationNum"))

annual_by_month_output =
  annual_by_month_output[c("Year", "StationNum", "geomean_30", "swresults_60", "stv_count")]

####################################################
####################################################
# trend analysis: annual summaries
annual_geomean_output = data.frame(matrix(ncol = 5, nrow = 0))
colnames(annual_geomean_output) = c("Year", "Month", "StationNum", "geo_mean", "total_obsv", "stv_exceed")

for (i in c(unique(df$StationNum))) {
  station_output = data.frame()
  x = df %>% filter(StationNum == i)
  for (j in c(unique(x$Year))) {
    test = x %>%
      filter(Year == j) %>%
      summarize(geo_mean = geometric.mean(SWResults), total_obsv = n(),
                stv_exceed = sum(SWResults > 110) > (n() * .1)) 
    
    
    test$Year = rep(j, nrow(test))
    test$StationNum = rep(i, nrow(test))
    station_output = rbind(station_output, test)
  }
  annual_geomean_output = rbind(annual_geomean_output, station_output)
}

annual_geomean_output = 
  annual_geomean_output[c("Year", "StationNum", "geo_mean", "total_obsv", "stv_exceed")]

####################################################
####################################################
# trend analysis: mann kendall

mk_output = data.frame(matrix(ncol = 3, nrow = 0))

for (i in c(unique(annual_geomean_output$StationNum))) {
  x = annual_geomean_output %>%
    filter(StationNum == i) 
  
  mk_output = rbind(mk_output, tidy(mk.test(x$geo_mean))[1:3])
}

mk_output$StationNum = c(1,2,3)
mk_output = mk_output[c("StationNum", "statistic", "p.value", "parameter")]


####################################################
####################################################
# save outputs as csv
write.csv(geomean_output, file = "Oakland_geomeans.csv")
write.csv(annual_by_month_output, file = "Oakland_annual_by_month.csv")
write.csv(annual_geomean_output, file = "Oakland_annual_geomean.csv")
write.csv(mk_output, file = "Oakland_mk.csv")
