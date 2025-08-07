library(shiny)
library(tidyverse)
rm(list = ls())
parameters <- c("PM2.5", "PM10", "SO2", "NO2", "CO", "O3", "TEMP", "PRES", "DEWP", "RAIN", "WSPM")


china_air <- read.table("china_air.csv", sep = ",", header = TRUE) %>%
  mutate(date = as.Date(paste0(year, "-", month, "-", day))) %>%
  group_by(station, date) %>%
  mutate(across(parameters, ~mean(., na.rm = TRUE))) %>%
  ungroup()

min_max_date <- c(
  china_air %>% filter(date == min(date, na.rm = TRUE)) %>% distinct(date) %>% pull(),
  china_air %>% filter(date == max(date, na.rm = TRUE)) %>% distinct(date) %>% pull())

unique_stations <- china_air %>%
  distinct(station) %>%
  arrange(station) %>%
  pull()

wart <- china_air %>% filter(PM2.5 > 10)

print(length(wart))