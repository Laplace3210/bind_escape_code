
rm(list=ls(all=TRUE))
options(scipen = 100)
##loading the package
library(tidyverse)
library(lubridate)

global_data <- read.csv("D:/all_project/Italy_project/0_data_process/input/owid-covid-data.csv")
names(global_data)

UK_reported_cases <- global_data %>% 
  filter(location == "United Kingdom") %>% 
  select(date, new_cases) %>% 
  mutate(date = as.Date(date)) #%>% 
#filter(date>= as.Date("2020-02-01") & date <= as.Date("2020-02-01")+89)

Germany_reported_cases <- global_data %>% 
  filter(location == "Germany") %>% 
  select(date, new_cases) %>% 
  mutate(date = as.Date(date))

Japan_reported_cases <- global_data %>% 
  filter(location == "Japan") %>% 
  select(date, new_cases) %>% 
  mutate(date = as.Date(date))

Italy_reported_cases <- global_data %>% 
  filter(location == "Italy") %>% 
  select(date, new_cases) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2020-02-21") & date <= as.Date("2022-03-10"))
Italy_reported_cases[120,2] <- round(mean(Italy_reported_cases$new_cases[c(119,121)]))


Italy_google_mob_data <- read.csv("D:/all_project/Italy_project/0_data_process/input/Italy_google_mobility_data.csv") %>% 
  select(date, transit_stations_mov_ave, retail_and_recreation_mov_ave, grocery_and_pharmacy_mov_ave,
         workplaces_mov_ave ) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2020-01-31") & date <= as.Date("2022-03-05"))
names(Italy_google_mob_data )

Italy_apple_mob_data <- read.csv("D:/all_project/Italy_project/0_data_process/input/Italy_apple_mobility_data.csv") %>% 
  select(date, driving_mov_ave,transit_mov_ave,walking_mov_ave) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date >= as.Date("2020-01-31") & date <= as.Date("2022-03-05"))
names(Italy_apple_mob_data)

merged_mob <- merge(Italy_google_mob_data, Italy_apple_mob_data, by = "date", all = T)
which(is.na(merged_mob)==T)

#################mobility data########################## 
merged_mob_ave <- apply(merged_mob[,-1], 1, mean,na.rm = T)
merged_mob_ave <- merged_mob_ave/100
plot(merged_mob_ave,type = "l")

#################escape data############################ 
escape_data <- read.csv("D:/all_project/Italy_project/0_data_process/input/bind_escape.csv")

#################vaccine data ##########################
vaccine_data_Italy_merged <- read.csv("D:/all_project/Italy_project/0_data_process/input/vaccine_data_Italy_merged.csv")

##################gamma data############################ 
index_non_voc <- which(Italy_reported_cases$date[1:765] %in% as.Date("2021-02-04") )
gamma_non_voc <- rep(1/8.9, index_non_voc)

# gamma for Alpha period 
index_alpha <- which(Italy_reported_cases$date[1:765] %in% c(as.Date("2021-02-05"), as.Date("2021-06-23")) )
gamma_alpha <- rep(1/4.4, length(index_alpha[1]:index_alpha[2]))

# gamma for delta period
index_delta <- which(Italy_reported_cases$date[1:765] %in% c(as.Date("2021-06-24"), as.Date("2021-10-01")) )
gamma_delta <- rep(1/5.2, length(index_delta[1]:index_delta[2]))

# gamma for omicron period
index_omicron <- which(Italy_reported_cases$date[1:765] %in% c(as.Date("2021-10-02"), as.Date("2022-03-05")) )
gamma_omicron <- rep(1/3, length(index_omicron[1]:index_omicron[2]))

gamma_data <- c(gamma_non_voc, gamma_alpha,gamma_delta,gamma_omicron)

# Italy_pop_immunity <- read.csv("D:/Zhaojun/all_project_ding/Italy_project/0_data_process/input/pop_immunity.csv")[,-1]
# Italy_pop_immunity[765] <- Italy_pop_immunity[764]  
# plot((1-Italy_pop_immunity))
# plot((1-Italy_pop_immunity)*merged_mob_ave)
# plot(Italy_pop_immunity*merged_mob_ave)
# 
# merged_mob$transit_mov_ave[102:103] <- mean(c(merged_mob$transit_mov_ave[101], merged_mob$transit_mov_ave[104]))
# merged_mob$transit_mov_ave[407] <- mean(c(merged_mob$transit_mov_ave[406], merged_mob$transit_mov_ave[408]))
# plot(merged_mob$transit_stations_mov_ave/100,type = "l")
# plot(merged_mob$transit_mov_ave/100,type = "l")

##################protection data############################ 
adj_protection <- read.csv("D:/all_project/Italy_project/0_data_process/input/ajusted_protection_Italy.csv")
