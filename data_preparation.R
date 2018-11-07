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

###create function to find column numbers given column names
.rcol <- function(column.name.as.string = NULL, data = df) {
  grep(column.name.as.string,colnames(data))
}

###import data
df <- import(here::here("data", "dataSPSS.sav"), setclass = "tibble") %>%
  janitor::clean_names()

###characterize all columns except for age and books1 (the spss labels were causing the numeric values to be recorded as NAs for those columns)
df[, -c(.rcol("age"), 
        .rcol("books1"))] <- characterize(df[, -c(.rcol("age"), 
                                                  .rcol("books1"))])

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

###remove participants who do not occasionally use the internet or email
df <- df %>% 
  filter(eminuse == "Yes")

###

