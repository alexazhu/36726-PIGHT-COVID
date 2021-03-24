# libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

################### OH_K12 Group By Open Dates ###################
source("step2_data_wrangle.R")

# read in OH_K12 data
OH_K12 <- read.csv("OH_K12_clean.csv")
OH_K12$opendategrouped <- as.Date(OH_K12$opendategrouped)

# group the school open date for each county (get the most common one)
county_w_dates <- OH_K12 %>%
  distinct(county,opendategrouped,leaid,district_enroll,county_enroll)%>%
  group_by(county,opendategrouped,county_enroll)%>%
  summarise(n_opendategrouped = sum(district_enroll),prop_opendategrouped = round(sum(district_enroll/county_enroll),2),.groups = "drop")%>%
  group_by(county,county_enroll) %>%
  slice(which.max(prop_opendategrouped))%>%
  mutate(enddate = '12/15/2020')

# read in OHIO_CASES_DATA
cases <- read_excel("COVID_CASES_OH_CNTY_20210223_pop.xlsx")
# convert dates
cases$DATE <- as.Date(cases$DATE, format = "%m/%d/%Y")
county_w_dates$enddate <- as.Date(county_w_dates$enddate, format = "%m/%d/%Y")

################### Bracket Covid Data ###################
# get the cumulative cases and deaths from the covid data on both begin and end date, get the difference between the dates as total_deaths_s, total_cases_s
county_covid <- left_join(county_w_dates, cases, by = c("opendategrouped" = "DATE", "county" = "COUNTY"))

county_covid <- county_covid %>%
  select(county, opendategrouped, CUMCONFIRMED, CUMDEATHS,enddate)

names(county_covid)[3:5] <- c("opendatecommon","opencases","opendeaths")

county_covid <- left_join(county_covid, cases, by = c("enddate" = "DATE", "county" = "COUNTY"))

county_covid <- county_covid %>%
  mutate(deaths_increase = CUMDEATHS - opendeaths,cases_increase = CUMCONFIRMED - opencases)%>%
  select(county,POPULATION,opendatecommon, enddate,deaths_increase,cases_increase)%>%
  mutate(deaths_increase_prop = deaths_increase/POPULATION,cases_increase_prop = cases_increase/POPULATION)%>%
  mutate(deaths_increase_per_1000 = deaths_increase_prop*1000,cases_increase_per_1000 = cases_increase_prop*1000)

################### long table ################### 

bracket_county_policy_wide <- wide_teaching_enroll%>%
  full_join(county_covid, by = c("county",'county_enroll')) %>%
  full_join(wide_studentmask_enroll, by = c("county"))%>%
  full_join(wide_staffmask_enroll, by = c("county"))

write.csv(bracket_county_policy_wide,"bracket_county_policy_aggregate_wide.csv")

################### long table ################### 

long_bracket_teaching <- teachingmethod_enroll%>%
  left_join(county_covid,by = c("county",'county_enroll'))

long_bracket_studentmask <- studentmask_enroll%>%
  left_join(county_covid,by = c("county",'county_enroll'))

long_bracket_staff <- staffmask_enroll%>%
  left_join(county_covid,by = c("county",'county_enroll'))

long_bracket_isonline <- isonline_enroll%>%
  left_join(county_covid,by=c('county'))

long_bracket_isonline_mask <- long_bracket_studentmask%>%
  filter(!studentmaskpolicy%in%c('Pending','Unknown'))%>%
  group_by(county)%>%
  slice(which.max(prop_student_mask))%>%
  left_join(isonline_enroll,by=c('county'))

long_bracket_teaching_mask <- teachingmethod_enroll%>%
  filter(teachingmethod != 'Other'&teachingmethod != 'Pending'&teachingmethod != 'Unknown')%>%
  group_by(county)%>%
  slice(which.max(prop_teachingmethod))%>%
  left_join(long_bracket_studentmask%>%
              filter(!studentmaskpolicy%in%c('Pending','Unknown'))%>%
              group_by(county)%>%
              slice(which.max(prop_student_mask)),by='county','county_enroll')

