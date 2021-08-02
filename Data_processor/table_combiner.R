## This document is to combine all processed tables
## and combine it with Ohio county profile data from CDC
## Author: Ziyan Zhu ## Updated on 08/01/2021


### loading processor
source("Data_processor/OHK12_data_processor.R")
source("Data_processor/mobility_collecter.R")
source("Ohio_case_profile_data_processor.R")


################# County-level mobility & case data ################
## calculate death per 1000 people
deaths_teaching <- cases%>%
  left_join(enroll_by_teaching_wide,by=c('COUNTY'='county'))%>%
  drop_na(major_teaching)%>%
  mutate(death_prop = 1000*round(CUMDEATHS/POPULATION,5))%>%
  mutate(death_per_1000 = death_prop)

deaths_teaching$major_teaching <- factor(deaths_teaching$major_teaching,
                                                    levels = c("On Premises","Hybrid","Online Only"))


# SHIFT the DATE for mobility: mobility a week ago may impact the infections number now
deaths_teaching_mobility <- deaths_teaching %>%
      inner_join((mobility%>%
                  rename(FIPS = geo_value,DATE = time_value)%>%
                  mutate(DATE = DATE -7)),by=c("FIPS","DATE"))

deaths_teaching_mobility$major_teaching <- factor(deaths_teaching_mobility$major_teaching,
                                            levels = c("On Premises","Hybrid","Online Only"))


#write.csv(deaths_teaching_mobility,"Cleaned_Data/deaths_teaching_mobility.csv")

