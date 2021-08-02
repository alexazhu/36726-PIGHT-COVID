## This document is to combine all processed tables
## and combine it with Ohio county profile data from CDC
## Author:  Ziyan Zhu, Cheyenne Ehman ## Updated on 08/01/2021

source("Data_processor/005_GetAll_table_combiner.R")

ß############ B(t) shift to B(t-24) ###########

# splines and slopes added
cases_slope <- read.csv("Cleaned_Data/county_splines_slopes_B.csv", header = T)%>%
  select(COUNTY,DATE,POPULATION,CUMDEATHS,log_tot_deaths,tot.slope,NEWDEATHS,rev_NEWDEATHS,log_new_deaths,new.slope)

# SHIFT THE DATE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
cases_slope$DATE <- as.Date(cases_slope$DATE)-24

########### Combine with deaths, teachingmethod data ###########

deaths_teaching_slope <- deaths_teaching%>%
  select(-DATE,-POPULATION,-CUMDEATHS,-NEWDEATHS)%>%
  distinct()%>%
  right_join(cases_slope,by=c("COUNTY"))%>%
  filter(DATE>as.Date("2020-01-23"))

# ordering the teaching method factor to ensure the color order
deaths_teaching_slope$major_teaching <- factor(deaths_teaching_slope$major_teaching,levels = c("On Premises","Hybrid","Online Only"))
deaths_teaching_slope $DATE <- as.Date(deaths_teaching_slope$DATE)


########### Select Max B & Avg B ###########

maxB1 <- deaths_teaching_slope%>%
  group_by(COUNTY)%>%
  filter(DATE >= as.Date("2020-08-18") & DATE<=as.Date("2020-12-15"))%>%
  summarise(max_B1 = max(new.slope))

avgB1 <- deaths_teaching_slope%>%
  group_by(COUNTY)%>%
  filter(DATE >= as.Date("2020-08-18") & DATE<=as.Date("2020-12-15"))%>%
  summarise(avg_B1 = mean(new.slope))

## avg3w_B0 ## average B0 of the first 3 weeks of school reopening 
## avg1w_2w_B0 ## OR average B0s between  2020-08-18 -7days and +14days [before the rate bounce back around the dashed line]
## avg3w_bf_B0 ## OR average B0s between  2020-08-18 -21days and 2020-08-18 [before the rate bounce back around the dashed line]
avgB0 <- deaths_teaching_slope%>%
  group_by(COUNTY)%>%
  filter(DATE > as.Date("2020-08-18") & DATE<as.Date(major_opendate)+21)%>%
  summarise(avg3w_B0 = mean(new.slope))%>%
  left_join(deaths_teaching_slope%>%
              group_by(COUNTY)%>%
              filter(DATE > as.Date("2020-08-18")-7 & DATE<as.Date("2020-08-18")+14)%>%
              summarise(avg1w_2w_B0 = mean(new.slope)),by="COUNTY")%>%
  left_join(deaths_teaching_slope%>%
              group_by(COUNTY)%>%
              filter(DATE < as.Date("2020-08-18") & DATE>=as.Date("2020-08-18")-21)%>%
              summarise(avg3w_bf_B0 = mean(new.slope)),by="COUNTY")



########### compute avg mobility

avg_mobility <- deaths_teaching_mobility %>%
  left_join(major_reopendate,by=c("COUNTY"))%>%
  group_by(COUNTY)%>%
  #filter(DATE >= as.Date("2020-08-18")& DATE <as.Date("2020-08-18") + 21)%>%
  filter(DATE >= as.Date("2020-08-18")& DATE <= as.Date("2020-12-15"))%>%
  summarise(avg_full_work_prob = mean(full_work_prop_7d))%>%
  left_join(case_mobility%>%
              left_join(major_reopening,by=c("COUNTY"))%>%
              group_by(COUNTY)%>%
              filter(DATE >= as.Date("2020-08-18")+ 21 & DATE <=as.Date("2020-08-18") + 42)%>%
              summarise(avg2_full_work_prob = mean(full_work_prop_7d)),
            by="COUNTY")

#  B0 and B1
B0B1 <- deaths_teaching%>%
  distinct(COUNTY,POPULATION,NCHS.Urban.Rural.Status,Metropolitan.Status,Population.density)%>%
  left_join(maxB1,by="COUNTY")%>%
  left_join(wide_teaching_enroll, by = c("COUNTY" = "county"))%>%
  left_join(avgB1,by="COUNTY")%>%
  left_join(avgB0,by="COUNTY")%>%
  left_join(avg_mobility,by="COUNTY")

## ordering the teaching method factor to ensure the color order
B0B1$major_teaching <- factor(B0B1$major_teaching,levels = c("On Premises","Hybrid","Online Only"))







