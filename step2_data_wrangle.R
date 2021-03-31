# libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

################### OH_K12 ###################

# read in OH_K12 data
OH_K12 <- read.csv("OH_K12_clean.csv")
OH_K12$opendategrouped <- as.Date(OH_K12$opendategrouped)

################## teaching method ######################

# aggregate OH_K12 data by #enrollments in each districts 
# number of students & prop of students by different teaching methods
teachingmethod_enroll <- OH_K12 %>%
  distinct(county,teachingmethod,leaid,district_enroll,county_enroll)%>%
  group_by(county,teachingmethod, county_enroll)%>%
  summarise(total_teachingmethod = sum(district_enroll),prop_teachingmethod = round(sum(district_enroll/county_enroll),2),.groups = "drop")

# reshape the data frame
wide_teaching_enroll <- teachingmethod_enroll%>%
  dcast(county+county_enroll~teachingmethod,value.var ='prop_teachingmethod')
wide_teaching_enroll[is.na(wide_teaching_enroll)] <- 0

# remove unknown,pending, other
wide_teaching_enroll <- wide_teaching_enroll%>%
  select(-Unknown,-Other,-Pending)

# majority teaching method
wide_teaching_enroll[,'major_teaching']<- apply(wide_teaching_enroll[,3:5], 1, function(x){names(which.max(x))})


################## student mask ######################

# number of students & prop of students required to wear mask 
# at some point between 2020-2021
studentmask_enroll <- OH_K12 %>%
  distinct(county,leaid,studentmaskpolicy,district_enroll,county_enroll)%>%
  mutate(studentmaskpolicy = ifelse(studentmaskpolicy %in%c('Required for high school students only',"Required for middle/high school students only"),'Required for all students', studentmaskpolicy))%>%
  group_by(county,studentmaskpolicy,county_enroll)%>%
  summarise(n_studentmask = sum(district_enroll))

# #student in elementary schools from school districts where only require high/middle school students to wear mask
elementary_school_enroll <- OH_K12 %>%
  filter(studentmaskpolicy %in% c('Required for high school students only',"Required for middle/high school students only"))%>%
  filter(level == 1)%>%
  select(county,leaid,schnam,enrollment,studentmaskpolicy)%>%
  group_by(county)%>%
  summarise(elementary_enroll = sum(enrollment,na.rm=T),.groups = "drop")

# subtract above number
studentmask_enroll[(studentmask_enroll$studentmaskpolicy == 'Required for all students')&(studentmask_enroll$county %in% elementary_school_enroll$county),'n_studentmask'] <- studentmask_enroll[(studentmask_enroll$studentmaskpolicy == 'Required for all students')&(studentmask_enroll$county %in% elementary_school_enroll$county),'n_studentmask']-elementary_school_enroll$elementary_enroll

# calculate proportion
studentmask_enroll <- studentmask_enroll%>% 
  group_by(county,studentmaskpolicy,county_enroll)%>%
  mutate(prop_student_mask = round(n_studentmask/county_enroll,2))

# reshape the data frame
wide_studentmask_enroll <- studentmask_enroll%>%
  dcast(county~studentmaskpolicy,value.var ='prop_student_mask')
wide_studentmask_enroll [is.na(wide_studentmask_enroll )] <- 0

# remove unknown and pending
wide_studentmask_enroll <- wide_studentmask_enroll%>%
  select(-Unknown,-Pending)

colnames(wide_studentmask_enroll)[2] <- "Not required student"

# majority mask wearing
wide_studentmask_enroll[,'student_mask']<- apply(wide_studentmask_enroll[,2:3], 1, function(x){names(which.max(x))})


################## staff mask ######################
# we assume the number of staffs are proportionate to the number of students enrolled

# number of staff & prop of staff required to wear mask 
# at some point between 2020-2021

staffmask_enroll <- OH_K12 %>%
  distinct(county,staffmaskpolicy,leaid,district_enroll,county_enroll)%>%
  group_by(county,staffmaskpolicy,county_enroll)%>%
  summarise(n_staffmask = sum(district_enroll),prop_staff_mask = round(sum(district_enroll/county_enroll),2),.groups = "drop")

# reshape the data frame
wide_staffmask_enroll <- staffmask_enroll%>%
  dcast(county~staffmaskpolicy,value.var ='prop_staff_mask')
wide_staffmask_enroll[is.na(wide_staffmask_enroll )] <- 0

# remove unknown and pending
wide_staffmask_enroll <- wide_staffmask_enroll%>%
  select(-Unknown,-Pending)

colnames(wide_staffmask_enroll)[2] <- "Not required staff"

# majority staff mask
wide_staffmask_enroll[,'staff_mask']<- apply(wide_staffmask_enroll[,2:3], 1, function(x){names(which.max(x))})

################## open vs. reopen dates ######################
open_reopen_enroll <- OH_K12%>%
  distinct(county,county_enroll,district_enroll,leaid,date)%>%
  group_by(county,date)%>%
  summarise(date_enroll = sum(district_enroll))%>%
  left_join(OH_K12%>%
              distinct(county,county_enroll,district_enroll,leaid,opendategrouped)%>%
              group_by(county,opendategrouped)%>%
              summarise(reopen_enroll = sum(district_enroll)), by = 'county' )

reopen_teaching_enroll <- OH_K12%>%
  filter(teachingmethod%in%c('Online Only','On Premises','Hybrid'))%>%
  distinct(county,county_enroll,district_enroll,leaid,opendategrouped,teachingmethod)%>%
  group_by(county,opendategrouped,teachingmethod)%>%
  summarise(reopen_prop = sum(district_enroll/county_enroll))

reopen_teaching_enroll%>%
  group_by(county,teachingmethod)%>%
  mutate(teaching_prop = sum(reopen_prop))%>%
  group_by(county)%>%
  slice(which.max(teaching_prop))%>%
  ggplot(aes(x = opendategrouped,fill= teachingmethod))+facet_wrap(~teachingmethod)+geom_bar(stat = "count")+theme_minimal()+theme(axis.text = element_text(size = 10),title=element_text(size=13),legend.text = element_text(size=13))

################## OH CASES ######################

# read in OHIO_CASES_DATA
cases <- read_excel("COVID_CASES_OH_CNTY_20210223_pop.xlsx")
# convert dates
cases$DATE <- as.Date(cases$DATE, format = "%m/%d/%Y")
# remove UNASSIGNED and OUT OF OH data
cases <- cases%>%
  filter( (COUNTY != 'UNASSIGNED') & (COUNTY !='OUT OF OH'))%>%
  mutate(FIPS = str_sub(UID,start = 4,end = 8))%>%
  select(COUNTY,FIPS,DATE,CNTY_LAT,CNTY_LONG,POPULATION,CUMCONFIRMED,CUMDEATHS)

##################### get mobility data ########################


library(covidcast, character.only = TRUE)

work <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "part_time_work_prop_7dav",
                   start_day = "2020-01-22", end_day = "2021-02-22",
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175"))
)

restaurant <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "restaurants_visit_prop",
                   start_day = "2020-01-22", end_day = "2021-02-22",
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175"))
)

bar <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "bars_visit_prop",
                   start_day = "2020-01-22", end_day = "2021-02-22",
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175"))
)

mobility <- work%>%
  rename(work_prop_7d=value,work_std = stderr,work_sample_size=sample_size)%>%
  left_join(restaurant%>%
              rename(res_visit_prop = value)%>%
              select(-stderr,-sample_size),by = c("geo_value","time_value"))%>%
  left_join(bar%>%
              rename(bar_visit_prop = value)%>%
              select(-stderr,-sample_size),by = c("geo_value","time_value"))%>%
  select(geo_value,time_value,work_prop_7d,work_std,work_sample_size,res_visit_prop,bar_visit_prop)
  

case_mobility <- mobility%>%
  right_join(cases,by=c("geo_value"="FIPS","time_value"="DATE"))%>%
  rename(FIPS = geo_value,DATE = time_value,)

##################### calculate death prop ########################

# county-wise death proportions = cum deaths/population on 2021-02-22
death_prop <- case_mobility %>%
  filter(DATE == '2021-02-22')%>%
  mutate(death_prop = round(CUMDEATHS/POPULATION,5))%>%
  mutate(death_per_1000 = 1000*death_prop)

################### combine wide table ################### 
colnames(wide_teaching_enroll) <- gsub("\\ ","_",colnames(wide_teaching_enroll))
colnames(wide_studentmask_enroll) <- gsub("\\ ","_",colnames(wide_studentmask_enroll))
colnames(wide_staffmask_enroll) <- gsub("\\ ","_",colnames(wide_staffmask_enroll))

county_policy_wide <- wide_teaching_enroll%>%
  full_join(death_prop, by = c("county"= "COUNTY")) %>%
  full_join(wide_studentmask_enroll, by = c("county"))%>%
  full_join(wide_staffmask_enroll, by = c("county"))

write.csv(county_policy_wide,"county_policy_aggregate_wide.csv")

################### long table ################### 

long_teaching <- teachingmethod_enroll%>%
  left_join(death_prop,by = c("county"= "COUNTY"))

long_studentmask <- studentmask_enroll%>%
  left_join(death_prop,by = c("county"= "COUNTY"))

long_staff <- staffmask_enroll%>%
  left_join(death_prop,by = c("county"= "COUNTY"))

isonline_enroll <- teachingmethod_enroll%>%
  filter(teachingmethod != 'Other'&teachingmethod != 'Pending'&teachingmethod != 'Unknown')%>%
  mutate(is_online = ifelse(teachingmethod == "Online Only","Online Only","Not Online Only"))%>%
  group_by(county,is_online) %>%
  summarise(prop_online_only = sum(prop_teachingmethod))%>%
  group_by(county)%>%
  slice(which.max(prop_online_only))

long_isonline <- isonline_enroll%>%
  left_join(death_prop,by=c('county'='COUNTY'))

long_isonline_mask <- long_studentmask%>%
  filter(!studentmaskpolicy%in%c('Pending','Unknown'))%>%
  group_by(county)%>%
  slice(which.max(prop_student_mask))%>%
  left_join(isonline_enroll,by=c('county'))

long_teaching_mask <- teachingmethod_enroll%>%
  filter(teachingmethod != 'Other'&teachingmethod != 'Pending'&teachingmethod != 'Unknown')%>%
  group_by(county)%>%
  slice(which.max(prop_teachingmethod))%>%
  left_join( long_studentmask%>%
               filter(!studentmaskpolicy%in%c('Pending','Unknown'))%>%
               group_by(county)%>%
               slice(which.max(prop_student_mask)),by=c('county','county_enroll'))





