
library(tidyverse)
require(scales)


source("step2_data_wrangle.R")

county_policy_wide$major_teaching <- factor(county_policy_wide$major_teaching,
                                            levels = c("On Premises",
                                                       "Hybrid",
                                                       "Online Only"))
# see when the intesection happens
date.intercept <- as.Date("2020-11-24")

# add 95% confidence bans
confidence_level <- .95
z_cl <- qnorm(confidence_level)

# case_policy_wide
case_policy_wide <- cases %>%
  left_join(county_policy_wide[,c("county","major_teaching","Online_Only","Hybrid","On_Premises")],
            by = c("COUNTY" = "county")) %>%
  mutate(death_prop = CUMDEATHS/POPULATION)

# plot death prop over time by the majority teaching method
case_policy_wide%>%
  group_by(DATE, major_teaching) %>%
  drop_na(major_teaching)%>%
  summarise(total_deaths = sum(CUMDEATHS),
            total_pop = sum(POPULATION),
            death_prop = total_deaths/total_pop,
            death_prop_upper = death_prop + z_cl*sqrt(death_prop*(1 - death_prop)/total_pop),
            death_prop_lower = death_prop - z_cl*sqrt(death_prop*(1 - death_prop)/total_pop),
            .groups = "drop") %>%
  ggplot(aes(x = DATE, y = death_prop*1000, group = major_teaching))+
  geom_rect(data=case_policy_wide[1,],
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/12"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") + 
  geom_line(aes(color = major_teaching),size = 1, alpha = .8) + 
  geom_ribbon(aes(ymin = 1000*death_prop_lower, ymax = 1000*death_prop_upper,
                  fill= major_teaching), 
              alpha = .3, show.legend = F)+ 
  geom_vline(xintercept = date.intercept, linetype = "dashed") + 
  annotate("text",x = date.intercept,y = 1.5,
           label = date.intercept,
           hjust = 1.1,size=5) + 
  theme_bw() + 
  labs(x = "Date", y = "Cumulative Death Incidence / 1,000 people",
       color = "Majority Teaching Method") + 
  theme(legend.position = "")+
  theme(axis.title = element_text(size=16),axis.text = element_text(size=15))
  #theme(legend.position = "bottom")+
  #theme(legend.title = element_text(size=13),legend.text = element_text(size=13),legend.background = element_rect(fill = alpha("orange",0.0)),legend.key.size = unit(1.4,"lines"),title = element_text(size=12.9))


#ggsave("totaldeath.jpg", width = 5,height = 5)



######################### Boxplots ############

# need open date for each county 
district_policies <- OH_K12 %>%
  distinct(county,county_enroll,leaid,district_enroll,schooltemporaryshutdown,opendategrouped,teachingmethod)

# Calculate the proportion and generate date brackets
major_opendate <- district_policies%>%
  filter(!schooltemporaryshutdown %in% c('Closed indefinitely','Pending','Unknown'))%>%
  group_by(county,county_enroll,opendategrouped)%>%
  summarise(n_opendate = sum(district_enroll))%>% # number of students under certain date for each county
  mutate(prop_opendate = round(n_opendate/county_enroll,2))%>% # proportion
  group_by(county)%>%
  #filter(prop_opendate>0.6)%>% 
  slice(which.max(prop_opendate))%>% # filter large proportions of students with same reopen dates #can be replaced with # slice(which.max(prop_opendate))#
  mutate(reopen_3w_after = opendategrouped + 21)%>%
  select(-n_opendate)

# join most common open date for each county
opendate_cases <- case_policy_wide%>%
  inner_join(major_opendate[,c("county","opendategrouped")],by=c('COUNTY'='county'))

# Box Plots in Fall semester
library(PMCMRplus)
require(DescTools)

fall_cases <- opendate_cases %>%
  filter(DATE >= opendategrouped & DATE <= as.Date("2020/12/15")) %>%
  group_by(COUNTY) %>%
  arrange(DATE) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  mutate(death_incidence = diff(CUMDEATHS),
         death_incidence_per_1000 = death_incidence*1000/POPULATION) %>%
  distinct(COUNTY,POPULATION,major_teaching,
           death_incidence,death_incidence_per_1000) 

fall_major_teaching.aov <- aov(death_incidence_per_1000 ~ major_teaching,data = fall_cases)
summary(fall_major_teaching.aov) # p-value of .012
stat.test <- PostHocTest(fall_major_teaching.aov, method = "duncan")$major_teaching %>%
  as.data.frame()%>%
  rownames_to_column("group") %>%
  separate(group,"-", into = c("group1","group2")) %>%
  mutate(pval = round(pval,3),
         p = case_when(pval <= .01~ "**",
                       pval <= .05 ~ "*",
                       TRUE ~ "NS"))%>%
  select(group1, group2, pval, p)

library(ggpubr)

ggplot(fall_cases,aes(y = death_incidence_per_1000, x = major_teaching)) + 
  geom_boxplot(aes(fill = major_teaching))+
  stat_compare_means(method = "anova")+ 
  stat_pvalue_manual(stat.test, label = "p",y.position = 2.5, step.increase = 0.15)+ 
  ylim(c(0,3.5))+ 
  theme_bw()+ 
  labs(y = "Death Incidence / 1,000 people",
       fill = "Majority Teaching Method",
       title = "Death Incidence in the Fall Semester",
       caption = "Pairwise p-values come from Duncan pairwise comparison test") +
  theme(legend.position = "bottom",
        axis.text.x=element_blank())

#ggsave("fall_boxplots.jpg",width = 8.5, height = 5)


# Assign 3 key windows
interval_cases <- opendate_cases %>%
  mutate(interval = case_when(DATE >= opendategrouped &
                     DATE <= opendategrouped + 21 ~ "Start of School - 3 weeks after Start of School",
                   DATE >= as.Date("2020/11/24") - 10 & 
                     DATE<= as.Date("2020/11/24") + 11 ~ "11/14/2020 - 12/05/2020",
                   DATE >= as.Date("2020/12/15") + 21 &
                     DATE <= as.Date("2020/12/15") + 42 ~ "01/05/2021 - 01/26/2020"))%>%
  group_by(COUNTY, interval) %>%
  arrange(DATE) %>%
  filter(row_number()==1 | row_number()==n())%>%
  mutate(death_incidence = diff(CUMDEATHS),
         death_incidence_per_1000 = death_incidence*1000/POPULATION) %>%
  distinct(COUNTY,POPULATION,major_teaching,
           death_incidence,death_incidence_per_1000,interval) %>%
  filter(!is.na(interval)) 

interval_cases$interval <- factor(interval_cases$interval, 
                                  levels = c("Start of School - 3 weeks after Start of School",
                                             "11/14/2020 - 12/05/2020",
                                             "01/05/2021 - 01/26/2020"))

# Box Plots in 3 key Windows 
ggplot(interval_cases, aes(y = death_incidence_per_1000, 
                           x = major_teaching,fill = major_teaching)) + 
  geom_boxplot() +
  stat_compare_means(method = "anova")+ 
  facet_wrap(~interval) + 
  theme_bw() + 
  labs(y = "Death Incidence / 1,000 people",
       fill = "Majority Teaching Method",
       title = "Death Incidence in 3 Key Windows",
       subtitle = "3 Week Windows",
       caption = "The first window looks at the 3 weeks after the start of school, \nthe second is the widndow around the point of intersection, \nand the last window is 3 weeks after the end of the fall semester.") + 
  theme(legend.position = "bottom",
        axis.text.x=element_blank())


#ggsave("int_boxplots.jpg",width = 8.5, height = 5)





# Derivative Plot 

lag_cases <- case_mobility %>%
  left_join(county_policy_wide[,c("county","major_teaching")],
            by = c("COUNTY" = "county")) %>%
  drop_na(major_teaching)%>%
  select(COUNTY,DATE,CUMDEATHS,POPULATION,major_teaching)%>%
  group_by(COUNTY) %>%
  mutate(lag_total_deaths = lag(CUMDEATHS,21)) %>%
  ungroup()%>%
  group_by(DATE,major_teaching) %>%
  summarise(total_deaths = sum(CUMDEATHS),
            total_deaths_lag = sum(lag_total_deaths),
            total_pop = sum(POPULATION),
            death_prop = total_deaths/total_pop,
            lag_death_prop = total_deaths_lag/total_pop,
            death_prop_inc = (total_deaths-total_deaths_lag)/total_pop,
            .groups = "drop")

peak.date <- as.Date("2020-12-23")
ggplot(lag_cases,aes(x = DATE, y = death_prop_inc*1000, 
                     group = major_teaching)) + 
  geom_line(na.rm = T, aes(color = major_teaching)) +
  geom_rect(data = lag_cases[1,],
            aes(xmin=as.Date("2020/08/26"), xmax=as.Date("2020/12/12"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") + 
  geom_vline(xintercept = peak.date, linetype = "dashed")+ 
  annotate("text",x = peak.date,y = .0005,
           label = peak.date,
           hjust = 1.2)  + 
  theme_bw() + 
  labs(x = "Date",
       y = "Death Proportion / 1,000 people", 
       title = "Death Proportion Increase by Teaching Method",
       subtitle = "Yellow area represents Fall Semester",
       caption = "Increase compared to 3 Week Lag",
       color = "Majority Teaching Method") + 
  scale_y_continuous(labels = comma) + 
  theme(legend.position = "bottom")



# create map plots
wide_teaching_enroll%>%
  left_join(ohio_map,by='county')%>%
  mutate(Online_Only= Online_Only*100)%>%
  ggplot() + geom_polygon(aes(x = long, y = lat, group = group, fill = Online_Only), color = "gray") + coord_fixed(1.3) + theme_map() +
  scale_fill_distiller(palette = "Spectral")+labs(fill='% Online Only')+
  geom_text_repel(data=centroids2,aes(x = clong, y = clat,label=county), color = "black",size = 3)
