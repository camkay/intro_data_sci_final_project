###############################
###############################
###data preparation script
###############################
###############################

###############################
###set up
###############################

###load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(here)
library(rio)
library(lubridate)

###import data
df <- import(here("data", "dataSPSS.sav"), setclass = "tibble") %>%
  characterize() %>%
  janitor::clean_names()

###############################
###data tidying
###############################

###select only variables of interest
df <- df %>%
  select(-comp, 
         -state:-qs1,
         -intmob,
         -bbhome1:-smart2,
         -device1b:-web1h,
         -pial5a:-pial5d,
         -pial11a:-pial11_igbm,
         -marital:-racem4,
         -hh1:-cellweight)
  
###parse date for interview date column
df <- df %>%
  mutate(int_date = ymd(int_date))


