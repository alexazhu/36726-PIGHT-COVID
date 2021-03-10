library(tidyverse)
library(mice)
library(VIM)
library(visdat)
library(naniar)
library(corrplot)
library(simputation)

#import data (please change the code locally)

OH_K12 <- read_excel("Desktop/PHIGHT_COVID_DATA/Ohio_Data/OH_K12.xlsx")


#data cleaning

#have a look at the data

summary(OH_K12)

#make all lower cases

names(OH_K12) <- tolower(names(OH_K12))
dim(OH_K12)

#select variables of interest

#srcname is incomplete
#ncessch is not useful (uniform)
#city (do not have relevant data, only county/state level)
#level_ is seemed to be correlated to level (1: elementary, 2: middle, 3:high)
#schoolyear is not useful (uniform)
#lastverified date is not useful, but will keep for time-related analysis
#district_nces is duplicate with leaid

OH_K12 <- OH_K12 %>% select(-srcname, -ncessch, -city, -level_,-schoolyear, -district_nces)
glimpse(OH_K12)

#missingness

#The frequency distribution of the missing cases per variable can be obtained as:
ini <- mice(OH_K12, maxit = 0) 
table(ini$nmis)

vis_miss(OH_K12)

#county and enrollment may be fixed manually  by researching online
#sum(is.na(OH_K12$county)) #50 entries missing, some wrong entries, fixed in excel manually and import again, so no code here

#sum(enrollment) = enrollment_1 (not accurate, tbc)
#missingness regarding date, verifieddate and opendate is hard to handle (tbc)


#exclude observation with so many missing data
which (is.na(OH_K12$control)) 
which (is.na(OH_K12$sportsparticipation)) 

#we exclude 1883, 1884, 1885, 2786
OH_K12 <- OH_K12[-c(1883, 1884, 1885, 2786),]

#detect any weried entries
summary(OH_K12)

#enrollment got -999, replace negative values with NA
OH_K12$enrollment <- replace(OH_K12$enrollment, which(OH_K12$enrollment < 0), NA)

#make date variable
OH_K12$date <- as.Date(OH_K12$date)
OH_K12$lastverifieddate <- as.Date(OH_K12$lastverifieddate)
OH_K12$opendategrouped <- as.Date(OH_K12$opendategrouped)

#export the data
library(xlsx)
write.xlsx(OH_K12, "~/Desktop/OH_K12_clean.xlsx")

