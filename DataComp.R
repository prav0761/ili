library(tidyverse)
library(cowplot)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)


###TO DO - make mode function to get 7-day mode values for oxCGRT


# For better plotting
theme_set(theme_minimal_grid())
mytext <- element_text(angle=90, hjust=0.5, vjust=0.5)


# Source in functions -----------------------------------------------------

source("R/av7fun.R")

# State FIPS,  population data --------------------------------------------

fips <- read_csv("data/fips-codes.csv")
uspop <- read_csv("data/uspop.csv")

colnames(fips) <- c("fips", "state_full", "state", "alphacount")
colnames(uspop) <- c("state_full", "pop")


# Install necessary packages ----------------------------------------------

## Socrata
# install.packages("RSocrata")
library(RSocrata)

# cdcfluview
# See:  
# install.packages("cdcfluview")
library(cdcfluview)

# covidcast
# For data descriptions, see: 
# https://cmu-delphi.github.io/delphi-epidata/
# install.packages("covidcast")
library(covidcast)

# HHS hospital data ("ground truth") ---------------------------------------

# Socrata API token for this project (do not share)
token <- "llBMOEMWQQKKIg7XEAwuNVri4" 

hhs <- read.socrata("https://healthdata.gov/resource/g62h-syeh.csv",
                    app_token = token)

# Add in FIPS and population data, and calculate per-capita admissions
hhs <- hhs %>% 
  mutate(date = date(date)) %>%
  left_join(fips) %>% 
  left_join(uspop) %>%
  filter(!is.na(state_full)) %>% # For now, only 50 states + DC
  # Create more convenient names for admissions
  mutate(flu = previous_day_admission_influenza_confirmed,
         covid = previous_day_admission_adult_covid_confirmed) %>%
  # Per-capita
  mutate(flu_per_cap = flu / pop * 1e5) %>% 
  mutate(covid_per_cap = covid / pop * 1e5) %>% 
  # 7-day average for admissions
  arrange(state, date) %>% 
  group_by(state) %>%
  mutate(flu_av7 = av7fun(flu, date),
         covid_av7 = av7fun(covid, date),
         flu_per_cap_av7 = av7fun(flu_per_cap, date),
         covid_per_cap_av7 = av7fun(covid_per_cap, date)) %>% 
  mutate(ratio_log = log(flu_per_cap_av7) - log(covid_per_cap_av7)) %>%
  ungroup()



# ILI (1997-present) ------------------------------------------------------

usflu <- ilinet(region = "state",  years = 2019:2022)
usflu$state<- state.abb[match(usflu$region,state.name)]
usflu <- usflu %>% rename(date=week_start) %>%  filter(!is.na(state))

# Mask wearing data -------------------------------------------------------

mask <- covidcast_signal("fb-survey", "smoothed_wearing_mask_7d", 
                         geo_type = "state",
                         start_day = "2021-01-01",
                         end_day = today())

mask <- mask %>% 
  rename(state = geo_value) %>% 
  rename(date = time_value) %>% 
  mutate(state = toupper(state)) %>%
  mutate(mask_av7 = av7fun(value, date)) %>%
  filter(state %in% state.abb) # For now, only 50 states 
#Google mobility ---------------------------------------------------------
Google <- list.files(path="google", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
Google$state<- state.abb[match(Google$sub_region_1,state.name)]

#sorts data for state level analysis by filtering out county and national level rows, calc avg for weekly analysis
Google <- Google %>%  mutate(rr_av7 = av7fun(retail_and_recreation_percent_change_from_baseline, date),
                             gp_av7 = av7fun(grocery_and_pharmacy_percent_change_from_baseline, date),
                             park_av7 = av7fun(parks_percent_change_from_baseline, date),
                             transit_av7 = av7fun(transit_stations_percent_change_from_baseline, date),
                             work_av7 = av7fun(workplaces_percent_change_from_baseline, date),
                             resid_av7 = av7fun(residential_percent_change_from_baseline, date)) %>%
                             filter(!is.na(iso_3166_2_code)) 

#oxCGRT ------------------------------------------------------------


# urlox<- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/United%20States/OxCGRT_USA_latest.csv"
# ox<-read_csv(url(urlox))
# write.csv(ox, 'oxCGRT/ox', row.names=FALSE)

oxCGRT <- list.files(path="oxCGRT", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

oxCGRT$state<- state.abb[match(oxCGRT$RegionName,state.name)]
oxCGRT<- oxCGRT%>% mutate(date = ymd(Date)) %>% mutate(school_av7 = av7fun(`C1M_School closing`, date), 
                                                       elderly_av7 = av7fun(`H8M_Protection of elderly people`, date),
                                                       mask_policy_av7 = av7fun(`H6M_Facial Coverings`, date))

#____________________________

ILIjoindf <- usflu[, c('state', 'date', 'unweighted_ili')]
HHSjoindf <- hhs[, c('state', 'date',"flu_av7" ,"flu_per_cap_av7")]
Maskjoindf<- mask[,c("state", "date", "mask_av7")]
Googlejoindf <- Google[,c('state','date','rr_av7', 'gp_av7','park_av7','transit_av7', 'work_av7','resid_av7')]
oxCGRTjoindf <- oxCGRT[,c('state','date','school_av7','elderly_av7','mask_policy_av7')]

finaldf<- list(ILIjoindf,HHSjoindf,Maskjoindf,Googlejoindf,oxCGRTjoindf) %>% 
  reduce(left_join, by = c('state','date'))
View(finaldf)
write.csv(finaldf,"data/finaldf.csv",row.names = FALSE)
