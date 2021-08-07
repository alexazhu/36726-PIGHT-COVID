
## This document is to generate average mobility for each county in Ohio
## Author: Ziyan Zhu 04/27/2021; Updatde on 08/06/2021


########### compute avg mobility
# We generalize the start date and end date of the Fall semester is between 

fall_end = as.Date("2020-12-15")
fall_start = as.Date("2020-08-18")


avg_mobility <- deaths_teaching_mobility %>%
  group_by(COUNTY)%>%
  #filter(DATE >= as.Date("2020-08-18")& DATE <as.Date("2020-08-18") + 21)%>%
  filter(DATE >= as.Date("2020-08-18")& DATE <= as.Date("2020-12-15"))%>%
  summarise(avg_full_work_prob = mean(full_work_prop_7d))%>%
  left_join(deaths_teaching_mobility%>%
              left_join(major_reopendate,by=c("COUNTY"))%>%
              group_by(COUNTY)%>%
              filter(DATE >= as.Date("2020-08-18")+ 21 & DATE <=as.Date("2020-08-18") + 42)%>%
              summarise(avg2_full_work_prob = mean(full_work_prop_7d)),
            by="COUNTY")
