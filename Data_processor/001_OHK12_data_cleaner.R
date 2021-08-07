## This document is used to clean Ohio K12 data
## Author: Ziyan Zhu, 03/15/2021 ## Updated on 08/01/2021


## Objectives in data cleaning:
#### 1. Drop redundant columns
#### 2. Spot ill-posed data and potential wrong data entries
#### 3. Clean the structure for easier manipulation
#### 4. Remove observations with too much missing values

## Features we can drop:
#### - srcname: most school names is missing
#### - ncessch: only one single value
#### - city & physicalcity: we concern about county level data but we will keep physicalcity for impute missing values in county
#### - level_ is seemed to be correlated to level (1: elementary, 2: middle, 3:high)
#### - stusab/physicalstate: single value of OH
#### - schoolyear: single value of the same academic year 2020-2021
#### - district_nces & leaid are the same identifiers of school district, keep only leaid


## load libraries
library(tidyverse)
library(mice) #for missing value visualization
library(visdat)
library(readxl)

## Raw data
oh_k12 <- read_excel("Data/RawData/OH_K12.xlsx")

# Rename the columns with lower cases for easier reference
colnames(oh_k12) <- tolower(colnames(oh_k12))

# deselect redundant columns
oh_k12 <- oh_k12 %>% 
  select(-stusab, -srcname, -ncessch, -city, -level_,-schoolyear, -district_nces)

### transform data typ
oh_k12$enrollment <- as.numeric(oh_k12$enrollment)
oh_k12$enrollment_1 <- as.numeric(oh_k12$enrollment_1)
oh_k12$enrollment[oh_k12$enrollment==-999] <- NA
#oh_k12[1:10,c('enrollment','enrollment_1')]

#__Notes:__
#Enrollment marks the number of students in a school
#Enrollment_1 is the total number of students in its school district

# columns with dates
any_date <- function(col){
  any(str_detect(col, "20..-"),na.rm = T)
}

# which are date columns?
#oh_k12[ ,apply(oh_k12,2,any_date)]

# transform into Date obj 
oh_k12$date <- as.Date(oh_k12$date)
oh_k12$lastverifieddate <- as.Date(oh_k12$lastverifieddate)
oh_k12$opendategrouped <- as.Date(oh_k12$opendategrouped)


# check the entries of columns storing strings
#char_cols <- oh_k12[ ,apply(oh_k12,2,is.character)]
#colnames(char_cols)
#apply(char_cols[,-c(1:2,9,10,14,15,27,28,8,12)], 2, table)


########## missing values
# convert N to NA
oh_k12$gslo[oh_k12$gslo=='N'] <- NA
oh_k12$gshi[oh_k12$gshi=='N'] <- NA
oh_k12$openenroll[oh_k12$openenroll==2] <-NA
oh_k12$level[oh_k12$level=='N'] <- NA


ini <- mice(oh_k12,maxit = 0)
# how many missing values for each column
#ini$nmis
# check if there is patterns in missing values
#vis_miss(oh_k12)

#### 1. Impute missing values in county

# create pairs of physical city and county names for reference
county_city <- oh_k12%>%
  group_by(county)%>%
  select(county,physicalcity)%>%
  unique()%>%
  na.omit(county)

#county_city

# create id for rows with missing values in county
miss_county_id <- oh_k12%>%
  mutate(id = row_number())%>%
  filter(is.na(county))%>%
  select(id, schnam,county,physicalcity,multibdy)

##miss_county_id 
# 58 rows with missing in county
# dim(impute_county_id)

# merge missing value df with  reference
impute_county <- miss_county_id%>%
  left_join(county_city,by = 'physicalcity')%>%
  rename(county = county.y)%>%
  select(id,schnam,county,physicalcity,multibdy)

#impute_county
#dim(impute_county)

# row number does not match -> one city matches with two county
impute_county%>%
  filter(duplicated(id))

##__NOTES__:  Research shows that Dublin is a city in Franklin, Delaware, and Union counties in the U.S. state of Ohio, we will note impute the county for this city.
## Clermont and Hamilton makes up Cincinnati city, will impute that part manually.

# not impute dublin and cincinnati
impute_county_nam <- miss_county_id%>%
  filter(!physicalcity %in% c('Dublin','Cincinnati'))%>%
  left_join(county_city,by = 'physicalcity')%>%
  rename(county = county.y)%>%
  select(id,schnam,county,physicalcity,multibdy)

oh_k12[impute_county_nam$id,'county'] <- impute_county_nam$county
oh_k12[is.na(oh_k12$county),'county'] <- c(rep('CLERMONT',2),rep('FRANKLIN',3))

#### 2. handle missing values

#oh_k12%>%filter(is.na(oh_k12$teachingmethod))

oh_k12 <- oh_k12[!is.na(oh_k12$teachingmethod),]

#oh_k12[is.na(oh_k12$enrollment),]%>%
 # filter(studentmaskpolicy %in% c('Required for high school students only',"Required for middle/high school students only"))


#### 3. aggregate enrollment

oh_k12%>%
  group_by(leaid)%>%
  mutate(total_enrollment = sum(enrollment,na.rm = T))%>%
  select(county,leaid,enrollment,total_enrollment,enrollment_1)%>%
  filter(total_enrollment>enrollment_1)


check_enroll <- oh_k12%>%
  select(county,leaid,enrollment,enrollment_1)%>%
  group_by(county,leaid,enrollment_1)%>%
  summarise(leaid_enrollment = sum(enrollment,na.rm = T))

check_enroll <- check_enroll%>%
  mutate(district_enroll = ifelse(leaid_enrollment>enrollment_1,leaid_enrollment, enrollment_1))%>%
  select(-enrollment_1)


oh_k12 <- check_enroll%>%
  group_by(county)%>%
  mutate(county_enroll = sum(district_enroll))%>%
  full_join(oh_k12,by = c('county','leaid'))%>%
  select(-leaid_enrollment,enrollment_1)


#export the data
oh_k12$lastverifieddate <- as.character(oh_k12$lastverifieddate)
oh_k12$opendategrouped <- as.character(oh_k12$opendategrouped)

#write_excel_csv(oh_k12,"Cleaned_Data/OH_K12_clean.csv")


