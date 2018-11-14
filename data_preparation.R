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
library(cowplot)
library(wesanderson)
library(forcats)

#create function to find column numbers given column names
.rcol <- function(column.name.as.string = NULL, data = df) {
  grep(column.name.as.string, colnames(data))
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

#over gather the sns_use and sns_freq_use columns, and spread them back out
df <- df %>%
  gather(key = "websites", value = "value", starts_with("sns"), starts_with("web")) %>%
  separate(websites, into = c("temp", "website"), sep = "[[:digit:]]") %>%
  spread(key = "temp", value = "value")

#change "no, do not do this" in the sns_use column to "Rarely if ever" in the sns_freq_use columns
df[which(df[, "web"] == "No, do not do this"), "sns"] <- "Rarely if ever"

#drop now unneeded sns_use column
df <- df %>%
  select(-web)

#rename the values in the website column
df <- df %>%
  mutate(website = recode(website, a = "Twitter",
                                   b = "Instagram",
                                   c = "Facebook",
                                   d = "Snapchat",
                                   e = "YouTube"))

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
         income = inc,
         sns_freq_use = sns,
         sns = website)


#################################################
########### Analyses and Data Viz ###############
#################################################

# Examine whether age trends in reading differs as a function of book format 
# (e.g. do older readers spend more time with paper copy books whereas younger users may spend more time with audiobooks or digital print?). 

# Subseet the original data and tidy for plotting
plot_data <- df %>% 
  select(age, total_books_read, books_print, books_audio, books_elect) %>% 
  mutate(books_print = factor(books_print),
         books_audio = factor(books_audio),
         books_elect = factor(books_elect)) %>% 
  gather(book_format, yn, -age, -total_books_read) %>% 
  separate(book_format, c("dis", "book_format"), sep = "_", convert = TRUE) %>% 
  select(-dis) %>% 
  filter(!is.na(yn) & !str_detect(yn, "VOL")) %>% 
  mutate(book_format = factor(book_format),
         yn = factor(yn)) 

# Bar Chart: looking at average books read across age.
bar_plot <- plot_data %>% 
  group_by(age) %>% 
  summarize(mean_books = mean(total_books_read)) %>% 
  ggplot(aes(x = age, y = mean_books, fill = age)) + 
  geom_col()

bar_plot

## Scatter Plots: 
point_plot <- plot_data %>% 
  filter(yn == 'Yes') %>% 
  group_by(age, book_format) %>% 
  count(yn) %>% 
  ggplot(aes(x = age, y = n)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~book_format, nrow = 3, ncol = 1)

point_plot

point_plot2 <- plot_data %>% 
  group_by(age, book_format) %>% 
  summarize(mean_books = mean(total_books_read)) %>% 
  ggplot(aes(x = age, y = mean_books, color = book_format)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~book_format, nrow = 3, ncol = 1)

point_plot2

# Regression model to see how age and format of books relates to average number of books read. 
reg_data <- plot_data %>%
  group_by(age, book_format) %>% 
  summarize(mean_books = mean(total_books_read))

model <- lm(mean_books ~ age * book_format, data = reg_data)
model
anova(model)


###############################################
######### Ash's Data Visualizations ###########
###############################################


#Examine whether perceptions of social media use vary as age, political party, and social media site

#Prep data

plot_data_ash <- df %>% 
  select(age, sex, race, cregion, party, sns, int_good_society, int_good_self, int_use_freq) %>% 
  mutate(sex = factor(sex),
         race = factor(race),
         party = factor(party),
         sns = factor(sns),
         int_good_society = factor(int_good_society),
         int_good_self = factor(int_good_self),
         int_use_freq = factor(int_use_freq)) 

plot_data_ash <- plot_data_ash %>%
  mutate(int_good_society = fct_recode(int_good_society,
      "Bad" = "Bad thing",
      "Good" = "Good thing",
      "Some of both" = "(VOL) Some of both",
      "Other" = "(VOL) Don't know",
      "Other" = "(VOL) Refused")) %>%
  mutate(int_good_self = fct_recode(int_good_self,
      "Bad" = "Bad thing",
      "Good" = "Good thing",
      "Some of both" = "(VOL) Some of both",
      "Other" = "(VOL) Don't know",
      "Other" = "(VOL) Refused")) %>%
  mutate(party = fct_recode(party,
      "Other" = "(VOL) Other party",
      "Other" = "(VOL) Don't know",
      "Other" = "(VOL) No preference",
      "Refused" = "(VOL) Refused"))

levels(plot_data_ash$int_good_society) <- c("Other", "Bad", "Some of both", "Good")

#FINALLY ready for the first graph:

#Graphing ratings of how good vs bad the internet is for society as a function of political party

plot_ash1 <- plot_data_ash %>%
  mutate(int_good_self = as.numeric(int_good_self),
         int_good_society = as.numeric(int_good_society)) %>%
  filter(party != "Refused") %>%
  group_by(party) %>%
  summarize(m_age = mean(age),
            m_self = mean(int_good_self),
            m_society = mean(int_good_society)) %>%
  ggplot(aes(x = party, y = m_society)) +
  geom_col(alpha = 0.5, fill = "turquoise3", color = "turquoise4") +
  geom_hline(yintercept = 3.5) + #Note that this line represents the overall mean
  theme_bw() +
  labs(title = "Ash's Plot 1.",
       subtitle = "Ratings on a 4 point scale of how good or bad the internet is for society as a function of political party",
       x = "Political Party", 
       y = "Mean Rating") +
  coord_cartesian(ylim = c(2.5, 4)) 

plot_ash1

#Graph number 2 data prep:

levels(plot_ash2$int_use_freq) <- c("(VOL) Don't know", "Less often?", "Several times a week, OR", "About once a day", "Several times a day", "Almost constantly")

plot_ash2 <- plot_data_ash %>%
  mutate(int_good_self = as.numeric(int_good_self),
         int_good_society = as.numeric(int_good_society)) %>%
  select(-party, -age) %>%
  filter(int_use_freq != "(VOL) Don't know",
         race != "Don't know/Refused (VOL.)") %>%
  group_by(cregion, race) %>%
  summarize(m_self = mean(int_good_self),
            m_society = mean(int_good_society))

plot_ash2 <- plot_ash2 %>%
  mutate(self_vs_society = m_self - m_society) %>%
  mutate(race = fct_recode(race,
          "Asian" = "Asian or Asian-American",
          "Black" = "Black or African-American",
          "Other" = "Or some other race",
          "Mixed" = "Mixed Race"))

#Plot comparing ratings between how good the internet is for the self.. 
#relative to how good the internet is for society.. 
#as a funtion of race and region in the US

ggplot(plot_ash2, aes(x = race, y = self_vs_society, fill = race)) +
  geom_col(alpha = 0.8) +
  facet_wrap(~cregion, ncol = 4) +
  scale_fill_viridis_d() +
  theme_bw() +
  theme(legend.position = "") +
  labs(title = "Ash's Plot 2.",
       subtitle = "Mean difference in ratings between how good the internet is for the self relative to how good the internet is for society as a funtion of race and region in the US",
       x = "Race", 
       y = "Mean rating for self - Mean rating for society")


