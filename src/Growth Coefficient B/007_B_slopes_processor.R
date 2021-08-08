## This document is to combine all processed tables
## and combine it with Ohio county profile data from CDC
## Author:  Ziyan Zhu, Cheyenne Ehman ## Updated on 08/01/2021

source("Data_processor/005_GetAll_table_combiner.R")

# Notes:

#The 1st derivative of log new deaths+1 we observed on date T corresponds to the death growth rate B happened on date T-24
#We shift the slope data to B(t-24) cause established research suggests that on average it took 24 days from COVID infection to death
#The mobility date has been shifted one week forward as well. Assuming mobility a week before date T change the infections on date T

############ B(t) shift to B(t-24) ###########
library(dplyr)
# splines and slopes added
cases_slope <- read.csv("Data/Cleaned_Data/county_splines_slopes.csv", header = T)%>%
  select(COUNTY,DATE,POPULATION,CUMDEATHS,log_tot_deaths,tot.slope,NEWDEATHS,rev_NEWDEATHS,log_new_deaths,new.slope)

# SHIFT THE DATE 24 days FORWARD !!!!!!!!!!!!
cases_slope$DATE <- as.Date(cases_slope$DATE)-24

########### Combine with teachingmethod data ###########
deaths_teaching_county_slope <- deaths_teaching%>%
  select(-DATE,-POPULATION,-CUMDEATHS,-NEWDEATHS)%>%
  distinct()%>%
  right_join(cases_slope,by=c("COUNTY"))%>%
  filter(DATE>as.Date("2020-01-23"))

# ordering the teaching method factor to ensure the color order
deaths_teaching_county_slope$major_teaching <- factor(deaths_teaching_county_slope$major_teaching,levels = c("On Premises","Hybrid","Online Only"))
deaths_teaching_county_slope$DATE <- as.Date(deaths_teachingcounty__slope$DATE)


############# Aggregate counties by teaching method then compute B* ############
deaths_agg_teaching_slope <- deaths_teaching_county_slope %>%
  drop_na(major_teaching)%>%
  group_by(DATE, major_teaching) %>%
  summarise(total_new_deaths = sum(rev_NEWDEATHS), .groups = "drop") %>%
  mutate(log_new_deaths = log(total_new_deaths + 1)) %>%
  group_by(major_teaching) %>%
  mutate(smooth.spline = smooth.spline(DATE,log_new_deaths,df = 398/28)$y,
         B = predict(smooth.spline(DATE,log_new_deaths,df = 398/28),deriv = 1)$y)






