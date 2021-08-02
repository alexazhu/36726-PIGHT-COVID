## This document is to aggregate county-level school posture data for future analysis
## Author: Ziyan Zhu ## Updated on 08/01/2021


################### Load libraries ########################
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

########################### OH_K12 #############################
# read in OH_K12 data
OH_K12 <- read.csv("Cleaned_Data/OH_K12_clean.csv")
OH_K12$opendategrouped <- as.Date(OH_K12$opendategrouped)
OH_K12$date <- as.Date(OH_K12$date)

# total number of students enrolled in Ohio State
OH_K12 <- OH_K12%>%
  left_join(OH_K12%>%
              distinct(county,county_enroll)%>%
              mutate(state_enroll = sum(county_enroll)))

#### Aggregate number of students by adopted teaching posture #############

# compute number/percent of enrolled students in each school district by their adopted teaching posture 
enroll_by_teaching <- OH_K12 %>%
  distinct(county,teachingmethod,leaid,district_enroll,county_enroll,state_enroll)%>%
  group_by(county,teachingmethod, county_enroll)%>%
  summarise(enroll_by_teaching = sum(district_enroll),.groups = "drop")%>%
  mutate(prop_by_teaching = round(enroll_by_teaching/county_enroll,2))

# reshape the data frame to wide
enroll_by_teaching_wide <- enroll_by_teaching%>%
  dcast(county + county_enroll ~ teachingmethod,value.var ='prop_by_teaching')%>%
  replace(is.na(.),0)

# drop school districts whose teaching posture are UNKNOWN & PENDING & OTHER
#enroll_by_teaching_wide <- enroll_by_teaching_wide%>%
 # select(-Unknown,-Other,-Pending)

# then find the teaching posture adopted by the majority of school districts in each county
# create a new column named 'major_teaching'
enroll_by_teaching_wide[,'major_teaching'] <- apply(enroll_by_teaching_wide[,3:5], 
                                                    1, function(x){names(which.max(x))})
# replace spaces in columns names with '_'
colnames(enroll_by_teaching_wide) <- gsub(' ','_',colnames(enroll_by_teaching_wide))


#### Aggregate number of students by reported school reopen date #############
enroll_by_reopendate <- OH_K12%>%
  distinct(county,teachingmethod,leaid,opendategrouped,district_enroll,county_enroll,state_enroll)%>%
  group_by(county,county_enroll,opendategrouped)%>%
  summarise(enroll_by_opendate = sum(district_enroll),.groups = "drop")%>%
  mutate(prop_by_opendate = round(enroll_by_opendate/county_enroll,2) )

# find the 2020 Fall semester reopen date adopted by the majority of school districts in each county
major_reopendate <- enroll_by_reopendate%>%
  group_by(county)%>%
  slice(which.max(prop_by_opendate))%>%
  rename(COUNTY=county,major_opendate=opendategrouped)%>%
  distinct(COUNTY,major_opendate,prop_by_opendate)



