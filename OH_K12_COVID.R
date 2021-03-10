library(tidyverse)

#load data clean ver.
OH_K12_clean <- read_excel("~/Desktop/PHIGHT/OH_K12_clean.xlsx")

#load cases & deaths data county level
COVID_CASES_OH_CNTY <- read_excel("~/Desktop/PHIGHT_COVID_DATA/Ohio_Data/COVID_CASES_OH_CNTY_20210223_pop.xlsx")


#check if schoolname is unique
OH_K12_clean[OH_K12_clean$schnam > 1,]

#we have enrolment for each school and for each district 
#(i would assume enrollment_1 is for distric but there is slightly some gaps)

#calculate total enrolment for each county
try_1 <- OH_K12_clean %>% group_by(county) %>%
  filter(!duplicated(schnam)) %>% #note we have duplicated schoolnanmes (change intervention for more than once)
  summarise(total_enrol_county = sum(enrollment, na.rm=T))

#calculate total enrolment for each district
try_2 <- OH_K12_clean %>% group_by(districtname) %>%
  filter(!duplicated(schnam)) %>% #note we have duplicated schoolnanmes (change intervention for more than once)
  summarise(total_enrol_district = sum(enrollment, na.rm=T))

#join the datasets
OH_K12_COVID <- left_join(OH_K12_clean, try_1, by = 'county')
OH_K12_COVID <- left_join(OH_K12_COVID, try_2, by = 'districtname')

#check
#try_3 <- OH_K12_COVID %>% filter(county == 'ADAMS')
#seems like we have small issues to make sure sum of district = total county, should be okay


#calculate the proportion of each district's enrolment to the total enrolment (county)
#based on exploration, the teaching mode is followed in the district level

OH_K12_COVID <- OH_K12_COVID %>% mutate(prop = total_enrol_district/total_enrol_county)


#now we add in cases & deaths based on the proportion

#we fix the date first into the same format
library(lubridate)

COVID_CASES_OH_CNTY$DATE <- as.POSIXct(COVID_CASES_OH_CNTY$DATE, format = '%m/%d/%Y')
COVID_CASES_OH_CNTY$DATE <- format(COVID_CASES_OH_CNTY$DATE, format = '%Y-%m-%d')
COVID_CASES_OH_CNTY$DATE <- as.Date(COVID_CASES_OH_CNTY$DATE)

#covid sub data for only variables of interest
COVID_sub <- COVID_CASES_OH_CNTY %>% 
  select(COUNTY, DATE, NEWCONFIRMED, CUMCONFIRMED, NEWDEATHS, CUMDEATHS)

#join the tables
OH_K12_COVID <- left_join(OH_K12_COVID, COVID_sub, by = c("date" = "DATE", "county" = "COUNTY"))

#times prop
OH_K12_COVID <- OH_K12_COVID %>% 
  mutate(newconfirmed_p = round(prop*NEWCONFIRMED), cumconfirmed_p = round(prop*CUMCONFIRMED),
         newdeaths_p = round(prop*NEWDEATHS), cumdeaths_p = round(prop*CUMDEATHS))

#export the data
library(xlsx)
write.xlsx(OH_K12_COVID, "~/Desktop/OH_K12_COVID.xlsx")

