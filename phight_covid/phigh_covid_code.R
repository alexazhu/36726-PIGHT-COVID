
########### Load data ###########
library(dplyr)
library(ggplot2)
## Notes: cases & cumulative deaths, mobility data have  88 rows for all 88 counties

## All those read.csv steps to make sure the column names look fine

# NEW Deaths number
newdeaths <- read.csv("newdeaths_ohio.csv",header = F)
colnames(newdeaths) <- newdeaths[1,]
newdeaths <- newdeaths[-1,]

# CUMULATIVE Deaths number
cumdeaths <- read.csv("cumdeaths_ohio.csv",header = F)
colnames(cumdeaths) <- cumdeaths[1,]
cumdeaths <- cumdeaths[-1,]

# mobility data for proportion of devices stay 6 hours + away from home
# the original data was already averaged in 7 days window when pulled from covid_cast
sixhrs_away <- read.csv("sixhrs_away.csv",header = F)
colnames(sixhrs_away) <- sixhrs_away[1,]
sixhrs_away <- sixhrs_away[-1,]

## Notes: teaching method have only 86 rows for 86 counties
# teaching method proportions
teaching_method <- read.csv("teaching_method.csv")

############# deaths curve ###########

## mergeed cumulative deaths and teaching method tables
death_teaching <- read.csv("deaths_teaching.csv")
death_teaching$DATE <- as.Date(death_teaching$DATE)
death_teaching$major_teaching <- factor(death_teaching$major_teaching,
                                            levels = c("On Premises",
                                                       "Hybrid",
                                                       "Online Only"))
# see when the intersection happens
date.intercept <- as.Date("2020-11-24")

# add 95% confidence bans
confidence_level <- .95
z_cl <- qnorm(confidence_level)

## aggregate deaths prop by the majority teaching method
death_teaching_agg <- death_teaching%>%
  group_by(DATE, major_teaching) %>%
  summarise(total_deaths = sum(CUMDEATHS),
            total_pop = sum(POPULATION),
            death_prop = total_deaths/total_pop,
            death_prop_upper = death_prop + z_cl*sqrt(death_prop*(1 - death_prop)/total_pop),
            death_prop_lower = death_prop - z_cl*sqrt(death_prop*(1 - death_prop)/total_pop),
            .groups = "drop")

###### plot death prop over time by the majority teaching method
### grey line is those counties whose majority teaching method is unknown 
### (other than the three teaching methods we specify)

death_teaching_agg %>%
  #drop_na(major_teaching)%>%    ##  if you want to remove the grey line
  ggplot(aes(x = DATE, y = death_prop*1000, group = major_teaching))+
  geom_line(aes(color = major_teaching),size = 1, alpha = .8) + 
  geom_rect(data=death_teaching_agg[1,],  ## make the orange bock
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") + 
  geom_ribbon(aes(ymin = 1000*death_prop_lower, ymax = 1000*death_prop_upper,
                  fill= major_teaching),  # make CI band
              alpha = .3, show.legend = F)+ 
  geom_vline(xintercept = as.Date("2020-11-24"), linetype = "dashed") + 
  annotate("text",x = as.Date("2020-11-24"),y = 1.5, # for dashed line
           label = as.Date("2020-11-24"),
           hjust = 1.1) + 
  ggtitle("Death Incidences Increase Faster for Red Counties ")+
  labs(x = "Date", y = "Cumulative Death Incidence / 1,000 people",
       subtitle = "Yellow area represents Fall Semester",color = "Majority Teaching Method") +
  theme(legend.position = "bottom",legend.title = element_text(size=13),legend.text = element_text(size=13),axis.title = element_text(size=14),axis.text = element_text(size=15),legend.background = element_rect(fill = alpha("orange",0.0)),legend.key.size = unit(1.4,"lines"),title = element_text(size=12.9))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())

####################  B(t)  #################### 

#### load the B Cheyenne computed for each county, gonna take a few minutes
cases_slope_teach <- read.csv("cases_slope_teach.csv", header = T)
cases_slope_teach$DATE <- as.Date(cases_slope$DATE)
cases_slope_teach$major_teaching <- factor(cases_slope_teach$major_teaching,levels = c("On Premises","Hybrid","Online Only"))


