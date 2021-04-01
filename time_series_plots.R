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
case_policy_wide <- case_mobility %>%
  left_join(county_policy_wide[,c("county","major_teaching","Online_Only","Hybrid","On_Premises")],by = c("COUNTY" = "county")) %>%
  mutate(death_prop = CUMDEATHS/POPULATION)

# plot
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
            aes(xmin=as.Date("2020/08/26"), xmax=as.Date("2020/12/12"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") + 
  geom_line(aes(color = major_teaching),size = 1, alpha = .8) + 
  geom_ribbon(aes(ymin = 1000*death_prop_lower, ymax = 1000*death_prop_upper,
                  fill= major_teaching), 
              alpha = .3, show.legend = F)+ 
  geom_vline(xintercept = date.intercept, linetype = "dashed") + 
  annotate("text",x = date.intercept,y = 1.5,
           label = date.intercept,
           hjust = 1.1) + 
  theme_bw() + 
  labs(x = "Date", y = "Cumulative Death Proportion / 1,000 people", 
       title = "Cumulative Death Proportion by Teaching Method",
       subtitle = "Yellow area represents the fall semester",
       color = "Majority Teaching Method") + 
  theme(legend.position = "bottom")

#ggsave("totaldeath.jpg", width = 7,height = 5)


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
