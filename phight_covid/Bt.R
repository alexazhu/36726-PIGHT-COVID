####################  B(t)  #################### 

#### load the B Cheyenne computed for each county, 
#merged with major teaching and major reopen date

cases_slope_teach <- read.csv("cases_slope_teach.csv", header = T)
cases_slope_teach$DATE <- as.Date(cases_slope_teach$DATE)
cases_slope_teach$major_teaching <- factor(cases_slope_teach$major_teaching,levels = c("On Premises","Hybrid","Online Only"))

## rev_NEWDEATHS is the NEWDEATHS number whose negative values have been distributed

#### aggregate the rev_NEWDEATHS of the counties by color
## then compute the smoother with 4 weeks window, and get the slopes B

cases_slope_teach_agg <- cases_slope_teach %>%
  drop_na(major_teaching)%>%
  group_by(DATE, major_teaching) %>%
  summarise(total_new_deaths = sum(rev_NEWDEATHS), .groups = "drop") %>%
  mutate(log_new_deaths = log(total_new_deaths + 1)) %>%
  group_by(major_teaching) %>%
  mutate(smooth.spline = smooth.spline(DATE,log_new_deaths,df = 398/28)$y,
         B = predict(smooth.spline(DATE,log_new_deaths,df = 398/28),deriv = 1)$y)

ggplot(cases_slope_teach_agg, aes(x = DATE, color = major_teaching)) +
  geom_point(aes(y = log_new_deaths), alpha = .3)+ 
  geom_line(aes(y = smooth.spline), size = 1) +
  geom_rect(data = cases_slope_teach_agg[1,],
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") + 
  theme_bw() + 
  labs(x = "Date", y = "Log ( New Deaths + 1 )", 
       color = "Majority Teaching Method",
       caption = "Smoothing window set to every 4 weeks",
       subtitle = "Yellow area represents the fall semester (08/18 - 12/15)") + 
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.text = element_text(size=15),axis.title=element_text(size=15),legend.text = element_text(size=13))
#ggsave("log_death_time_series.png",width = 7, height = 5)

week3_after_start <- as.Date("2020/08/18") + 21

ggplot(cases_slope_teach_agg, aes(x = DATE, color = major_teaching)) + 
  geom_line(aes(y = B), size = 1) +
  geom_rect(data = cases_slope_teach_agg[1,],
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") +
  geom_vline(xintercept = week3_after_start, lty = 2) + 
  annotate("text",label = week3_after_start,
           x = week3_after_start, y = .05, hjust = 1.1)+ 
  labs(x = "Date", y = "Exponential Growth Coefficient", 
       color = "Majority Teaching Method",
       caption = "Smoothing window set to every 4 weeks",
       subtitle = "Yellow area represents the fall semester (08/18 - 12/15)") + 
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.text = element_text(size=15),axis.title=element_text(size=15),legend.text = element_text(size=13))


week3_after_start <- as.Date("2020/08/18") + 21

ggplot(cases_slope_teach_agg, aes(x = DATE, color = major_teaching)) + 
  geom_line(aes(y = B), size = 1) +
  geom_rect(data = cases_slope_teach_agg[1,],
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "orange") +
  geom_vline(xintercept = week3_after_start, lty = 2) + 
  annotate("text",label = week3_after_start,
           x = week3_after_start, y = .05, hjust = 1.1)+ 
  labs(x = "Date", y = "Exponential Growth Coefficient", 
       color = "Majority Teaching Method",
       caption = "Smoothing window set to every 4 weeks",
       subtitle = "Yellow area represents the fall semester (08/18 - 12/15)") + 
  theme(legend.position = "bottom")+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.text = element_text(size=15),axis.title=element_text(size=15),legend.text = element_text(size=13))


########### max B1 and avg B0 
###########

# Select Max B1 & B0

maxB1 <- cases_slope_teach%>%
  group_by(COUNTY)%>%
  filter(DATE >= as.Date("2020-08-18") & DATE<=as.Date("2020-12-15"))%>%
  summarise(max_B1 = max(new.slope))

avgB1 <- cases_slope_teach%>%
  group_by(COUNTY)%>%
  filter(DATE >= as.Date("2020-08-18") & DATE<=as.Date("2020-12-15"))%>%
  summarise(avg_B1 = mean(new.slope))

## avg3w_B0 ## average B0 of the first 3 weeks of school reopening 
## avg1w_2w_B0 ## OR average B0s between  2020-08-18 -7days and +14days [before the rate bounce back around the dashed line]
## avg3w_bf_B0 ## OR average B0s between  2020-08-18 -21days and 2020-08-18 [before the rate bounce back around the dashed line]

avgB0 <- cases_slope_teach%>%
  group_by(COUNTY)%>%
  filter(DATE > as.Date("2020-08-18") & DATE<as.Date(major_opendate)+21)%>%
  summarise(avg3w_B0 = mean(new.slope))%>%
  left_join(cases_slope_teach%>%
              group_by(COUNTY)%>%
              filter(DATE > as.Date("2020-08-18")-7 & DATE<as.Date("2020-08-18")+14)%>%
              summarise(avg1w_2w_B0 = mean(new.slope)),by="COUNTY")%>%
  left_join(cases_slope_teach%>%
              group_by(COUNTY)%>%
              filter(DATE < as.Date("2020-08-18") & DATE>=as.Date("2020-08-18")-21)%>%
              summarise(avg3w_bf_B0 = mean(new.slope)),by="COUNTY")



# average full time work mobility in 6 weeks window
avg_mobility <- case_mobility%>%
  left_join(major_reopening,by=c("COUNTY"))%>%
  group_by(COUNTY)%>%
  filter(DATE >= as.Date(major_opendate) + 21 & DATE <= as.Date(major_opendate) + 63)%>%
  summarise(avg_full_work_prob = mean(full_work_prop_7d))

# made major teaching method prop
wide_teaching_enroll <- wide_teaching_enroll%>%
  mutate(major_teaching_prop = case_when(
    major_teaching=="Online Only" ~ Online_Only,
    major_teaching=="Hybrid" ~ Hybrid,
    major_teaching=="On Premises" ~On_Premises,
    TRUE~ 0
  ))

#  B0 and B1
B0B1 <- maxB1%>%
  left_join(wide_teaching_enroll, by = c("COUNTY" = "county"))%>%
  left_join(avgB1,by="COUNTY")%>%
  left_join(avgB0,by="COUNTY")%>%
  left_join(avg_mobility,by="COUNTY")
## ordering the teaching method factor to ensure the color order
B0B1$major_teaching <- factor(B0B1$major_teaching,levels = c("On Premises","Hybrid","Online Only"))



##### made graphs

# max B1 vs B0
B0B1%>%
  ggplot(aes(x=avg3w_B0,y=max_B1,group=major_teaching,color=major_teaching))+geom_point(aes(size=avg_full_work_prob,alpha=major_teaching_prop))+theme_minimal()+labs(color="Majority Teaching Method",size="Averaged \n%6hr+ Away Home",title="max B1 v.s. averaged B0 in 3 weeks after reopen",alpha="Size of Majority",y="Max B1", x= "Averaged B0 in 3 weeks")+geom_smooth(method = "lm", se=F, formula = y ~ x,alpha=0.1)+geom_vline(xintercept = 0, lty = 2)

# avgB1 vs avg3w_B0
B0B1%>%
  ggplot(aes(x=avg3w_B0,y=avg_B1,group=major_teaching,color=major_teaching))+geom_point(aes(size=avg_full_work_prob,alpha=major_teaching_prop))+geom_smooth(method = "lm", se=F, formula = y ~ x,alpha=0.1)+theme_minimal()+labs(color="Majority Teaching Method",size="Averaged \n%6hr+ Away Home",title="Averaged B1 in Fall v.s. averaged B0 in 3 weeks after reopen",alpha="Size of Majority",y="Averaged B1 in Fall", x= "Averaged B0 in 3 weeks")+
  team_theme+geom_vline(xintercept = 0, lty = 2)


# avgB1 vs avg1w_2w_B0
B0B1%>%
  ggplot(aes(x=avg1w_2w_B0,y=max_B1,group=major_teaching,color=major_teaching))+geom_point(aes(size=avg_full_work_prob,alpha=major_teaching_prop))+geom_smooth(method = "lm", se=F, formula = y ~ x,alpha=0.1)+theme_minimal()+labs(color="Majority Teaching Method",size="Averaged \n%6hr+ Away Home",title="Max B1 in Fall v.s.\n averaged B0 between 1 week before and 2 weeks after reopen",alpha="Size of Majority",y="Max B1 in Fall", x= "Averaged B0 in 3 weeks")+
  team_theme+geom_vline(xintercept = 0, lty = 2)
ggsave("maxB1vsAvgB01w2w.png",width = 7, height = 7)

# avgB1 vs avg1w_2w_B0
B0B1%>%
  ggplot(aes(x=avg1w_2w_B0,y=avg_B1,group=major_teaching,color=major_teaching))+geom_point(aes(size=avg_full_work_prob,alpha=major_teaching_prop))+geom_smooth(method = "lm", se=F, formula = y ~ x,alpha=0.1)+theme_minimal()+labs(color="Majority Teaching Method",size="Averaged \n%6hr+ Away Home",title="Averaged B1 in Fall v.s.\n averaged B0 1 week before and 2 weeks after reopen",alpha="Size of Majority",y="Averaged B1 in Fall", x= "Averaged B0 in 3 weeks")+
  team_theme+geom_vline(xintercept = 0, lty = 2)
