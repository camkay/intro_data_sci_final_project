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

###import data
df <- import(here("data", "dataSPSS.sav"), setclass = "tibble") %>%
  characterize() %>%
  janitor::clean_names()

###############################
###data tidying
###############################


