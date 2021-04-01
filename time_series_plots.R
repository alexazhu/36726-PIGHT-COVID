
library(tidyverse)


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
  labs(x = "Date", y = "Death Proportion / 1,000 people", 
       title = "Cumulative Death Proportion by Teaching Method",
       subtitle = "Yellow area represents the fall semester",
       color = "Majority Teaching Method") + 
  theme(legend.position = "bottom")

#ggsave("totaldeath.jpg", width = 7,height = 5)