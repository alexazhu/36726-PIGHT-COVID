## This document is to collect mobility data from CMU Delphi's 'covid_cast' API
## and combine it with Ohio county profile data from CDC
## Author: Cheyenne Ehman, Ziyan Zhu ## Updated on 08/01/2021


## load libraries
library(tidyverse)
library(readxl)


################## Ohio CASES & DEATHS data ######################
# read in OHIO_CASES_DATA
cases <- read_excel("../Data/RawData/COVID_CASES_OH_CNTY_20210223_pop.xlsx")

# convert dates
cases$DATE <- as.Date(cases$DATE, "%m/%d/%Y")

# remove UNASSIGNED and OUT OF OH data
cases <- cases%>%
  filter( (COUNTY != 'UNASSIGNED') & (COUNTY !='OUT OF OH'))%>%
  mutate(FIPS = str_sub(UID,start = 4,end = 8),
         NEWDEATHS = ifelse(is.na(NEWDEATHS),0,NEWDEATHS))%>%
  select(COUNTY,FIPS,DATE,CNTY_LAT,CNTY_LONG,POPULATION,CUMCONFIRMED,CUMDEATHS,NEWDEATHS,NEWCONFIRMED)%>%
  arrange(DATE)


### Fix negative values in NEW DEATHS

distri_neg <- function(newdeaths){
  for (i in 1:length(newdeaths)) {
    if(newdeaths[i] < 0){
      if(i == 1){
        stop("problem")
      }
      j = i-1
      while (newdeaths[i]<0 && j>=1) {
        if(newdeaths[j]>0){
          newdeaths[j] = newdeaths[j] - 1
          newdeaths[i] = newdeaths[i] + 1
        }
        j = j - 1
      }
    }
    if(newdeaths[i]<0){print("Still negative, need to double check")}
  }
  return(newdeaths)
}

## apply to each county
cases <- cases%>%
  group_by(COUNTY)%>%
  mutate(rev_NEWDEATHS = distri_neg(NEWDEATHS))

# double check
#cases[cases$rev_NEWDEATHS<0,]
# two counties have negative new deaths at the beginning



################### Ohio profile data from CDC website ###################
ohio_profile <- read.csv("../Data/RawData/county_level_latest_data_for_ohio.csv")
ohio_profile <- ohio_profile[,c(1,14:20,38:50)]
ohio_profile$County <- toupper(ohio_profile$County)

### append profile data to cases
cases <- cases%>%
  left_join(ohio_profile%>%distinct(County,Metropolitan.Status,NCHS.Urban.Rural.Status,Population.density),
            by=c("COUNTY"="County"))


