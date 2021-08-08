
## This document is to generate the maximum and average of growth coefficients B for each county in Ohio
## Author: Ziyan Zhu 04/27/2021; Updatde on 08/06/2021


source("Data_processor/007_B_slopes_processor.R")
# Noted that B has already been shifted 24 days forward

########### Generate Max B & Avg B in 2020 Fall Semester ###########
# We generalize the start date and end date of the Fall semester is between 

fall_end = as.Date("2020-12-15")
fall_start = as.Date("2020-08-18")


maxB <- deaths_teaching_county_slope%>%
  group_by(COUNTY)%>%
  filter(DATE >= fall_start & DATE<= fall_end)%>%
  summarise(maxB_fall = max(new.slope))

avgB <- deaths_teaching_county_slope%>%
  group_by(COUNTY)%>%
  ilter(DATE >= fall_start & DATE<= fall_end)%>%
  summarise(avgB_fall = mean(new.slope))


############### Now let's narrow the time window and compute the average B values for 

## average B0 of the first 3 weeks of school reopening by the most popular 
## grouped reopen date -- but to simplify the process, we fixed the start and end date of the
## Fall semester for all counties
## noted that some district has reopen date as late as in November as reported


## avg3w_bf_B0 ## OR average B0s between  2020-08-18 -21days and 2020-08-18 [before the rate bounce back around the dashed line]

avgB_m3w <- deaths_teaching_county_slope%>%
              group_by(COUNTY)%>%
              filter(DATE >= fall_start -21 & DATE < fall_start)%>%
              summarise(avgB_m3w = mean(new.slope))

## 1 week before school starts to 2 weeks after [before the rate bounce back around the dashed line]
## m1w => start date minus 1 week; p2w => start date plus 2 weeks
avgB_m1w_p2w <- deaths_teaching_county_slope%>%
  group_by(COUNTY)%>%
  filter(DATE >= fall_start-7 & DATE < fall_start+14)%>%
  summarise(avgB_m1w_p2w = mean(new.slope))


# p3w => start date plus 3 weeks
avgB_p3w <- deaths_teaching_county_slope%>%
  left_join(major_reopendate,'COUNTY')%>%
  group_by(COUNTY)%>%
  #filter((DATE >= major_opendate) & (DATE < major_opendate+21))%>%
  filter((DATE >= fall_start) & (DATE < fall_start+21))%>%
  summarise(avgB_p3w = mean(new.slope))

###### join the avg Bs

avgB0 <- avgB_m3w%>%
  left_join(avgB_m1w_p2w,'COUNTY')%>%
  left_join(avgB_p3w,'COUNTY')

#  B0 and B1
B0B1 <- deaths_teaching%>%
  distinct(COUNTY,POPULATION,NCHS.Urban.Rural.Status,Metropolitan.Status,Population.density)%>%
  left_join(maxB,by="COUNTY")%>%
  left_join(enroll_by_teaching_wide, by = c("COUNTY" = "county"))%>%
  left_join(avgB,by="COUNTY")%>%
  left_join(avgB0,by="COUNTY")%>%
  left_join(avg_mobility,by="COUNTY")

## ordering the teaching method factor to ensure the color order
B0B1$major_teaching <- factor(B0B1$major_teaching,levels = c("On Premises","Hybrid","Online Only"))






