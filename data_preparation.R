###############################
###############################
###data preparation script
###############################
###############################

###############################
###set up
###############################

#load required packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(here)
library(rio)
library(lubridate)

#create function to find column numbers given column names
.rcol <- function(column.name.as.string = NULL, data = df) {
  grep(column.name.as.string,colnames(data))
}

#import data
df <- import(here::here("data", "dataSPSS.sav"), setclass = "tibble") %>%
  janitor::clean_names()

#characterize all columns except for age and books1 (the spss labels were causing the numeric values to be recorded as NAs for those columns)
df[, -c(.rcol("age"), 
        .rcol("books1"))] <- characterize(df[, -c(.rcol("age"), 
                                                  .rcol("books1"))])

###############################
###data tidying
###############################

#select only variables of interest
df <- df %>%
  select(-comp, 
         -state:-qs1,
         -intmob,
         -home4nw:-device1d,
         -web1f:-web1h, #dropped because we don't have frequency values
         -pial5a:-pial5d,
         -pial11a:-pial11_igbm,
         -marital:-racem4,
         -birth_hisp,
         -partyln:-cellweight)
  
#parse date for interview date column
df <- df %>%
  mutate(int_date = ymd(int_date))

#remove participants who do not occasionally use the internet or email
df <- df %>% 
  filter(eminuse == "Yes") %>%
  select(-eminuse)

#move never-use social media sites from the columns starting web to the frequency columns

##gather names for never-use columns and for frequency-use columns
never_use <- grep("^web1", colnames(df))
freq_use <- grep("^sns2", colnames(df))

##change "no, do not do this" in the web columns to "Rarely if ever" in the sns columns
for (i in 1:length(never_use)) {
  df[which(df[ , never_use[i]] == "No, do not do this"), freq_use[i]] <- "Rarely if ever"
}

##drop web columns
df <- df %>%
  select(-starts_with("web1"))

##drop unneeded values
rm(freq_use, never_use)

#tidy the data according to the SNS columns
df <- df %>%
  gather(key = "sns", value = "freq_usage", starts_with("sns2")) %>%
  mutate(sns = recode(sns, sns2a = "Twitter",
                           sns2b = "Instagram",
                           sns2c = "Facebook",
                           sns2d = "Snapchat",
                           sns2e = "YouTube"))

#rename poorly named columns for sanity
df <- df %>%
  rename(id = respid,
         date = int_date,
         int_use_freq = intfreq,
         int_good_society = pial11,
         int_good_self = pial12,
         total_books_read = books1,
         books_print = books2a,
         books_audio = books2b,
         books_elect = books2c,
         race = racecmb,
         income = inc)




