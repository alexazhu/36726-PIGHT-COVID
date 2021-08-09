## Set up aesthetic theme for all graphs generated in the report
Sys.setlocale("LC_TIME", "English")
library(ggrepel)
library(tidyverse)
library(lubridate)
require(scales)
library(readxl)
library(ggpubr)
library(PMCMRplus)
require(DescTools)
library(cowplot)
library(sp)
library(ggplot2)
library(grid)
library(gridExtra)
library(showtext)
font_import()


source("step2_data_wrangle.R")
# color blind friendly Palette
library(ggthemes)
col_theme <- c("Hybrid"="#009E73","On Premises"="#D55E00","Online Only"="#0072B2")
## plot theme
grid_theme <- theme(axis.line = element_line(colour = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    legend.key = element_blank(),
                    panel.background = element_blank(),
                    legend.box="vertical", legend.margin=margin())
team_theme <- grid_theme+
  theme(legend.text = element_text(size=18,family="Times New Roman"),
        legend.title = element_text(size=18,family="Times New Roman"),
        axis.text = element_text(size=20,family="Times New Roman"),
        title=element_text(size=20,family="Times New Roman"),
        strip.text.x = element_text(size = 20, face = "bold",family="Times New Roman"))

map_theme <-   theme(legend.position = "right",
                     legend.text = element_text(size=18,family="Times New Roman"),
                     legend.title = element_text(size=18,family="Times New Roman"),
                     legend.key = element_rect(size = 3),
                     legend.key.height = unit(1, "cm"),
                     legend.key.width = unit(1, "cm"))

ohio_map <- map_data("county") %>%subset(region=="ohio")%>%
  mutate(county=toupper(subregion))%>%select(long,lat,county,group)

# Map of proportion of students taking online-only classes
online_map <- wide_teaching_enroll%>%
  left_join(ohio_map,by='county')%>%
  mutate(Online_Only= Online_Only*100)%>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Online_Only), color = "gray") + 
  coord_fixed(1.3) + theme_map() +
  scale_fill_distiller(palette = "Blues",direction = 1)+
  labs(fill='% Online-only \nStudents')+map_theme

# Map of proportion of students taking on-premises classes
onpremise_map <- wide_teaching_enroll%>%
  left_join(ohio_map,by='county')%>%
  mutate(On_Premises= On_Premises*100)%>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = On_Premises), color = "gray") + 
  coord_fixed(1.3) + theme_map() +
  scale_fill_distiller(palette = "OrRd",direction = 1)+
  labs(fill='% On-premises \nStudents')+map_theme



# Map of proportion of students taking hybrid classes
hybrid_map <- wide_teaching_enroll%>%
  left_join(ohio_map,by='county')%>%
  mutate(Hybrid= Hybrid*100)%>%
  ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group, fill = Hybrid), color = "gray") + 
  coord_fixed(1.3) + 
  theme_map() +
  scale_fill_distiller(palette = "Greens",direction = 1)+
  labs(fill='% Hybrid \nStudents')+map_theme

# major teaching map
teach_map <- wide_teaching_enroll%>%
  left_join(ohio_map,by='county')%>%
  mutate(On_Premises= On_Premises*100)%>%
  ggplot() + geom_polygon(aes(x = long, y = lat, group = group, fill = as.factor(major_teaching)), color = "white",alpha=0.9) + 
  coord_fixed(1.3) + theme_map() + 
  scale_fill_manual(values=col_theme)+ 
  labs(fill='Majority teaching\nposture')+map_theme

pic12 <- arrangeGrob(online_map, 
                     top = textGrob("B.", x = unit(0, "npc")
                                               , y   = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(fontface="bold",col="black", fontsize=22, fontfamily="Times Roman")))

pic11 <- arrangeGrob(onpremise_map, 
                     top = textGrob("A.", x = unit(0, "npc")
                                               , y = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(fontface="bold",col="black", fontsize=22, fontfamily="Times Roman")))

pic13 <- arrangeGrob(hybrid_map, 
                     top = textGrob("C.", x = unit(0, "npc")
                                               , y  = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(fontface="bold",col="black", fontsize=22, fontfamily="Times Roman")))

pic14 <- arrangeGrob(teach_map, 
                     top = textGrob("D.", x = unit(0, "npc")
                                               , y = unit(1, "npc"), just=c("left","top"),
                                               gp=gpar(fontface="bold",col="black", fontsize=22, fontfamily="Times Roman")))

## graph 1
fig1 <- grid.arrange(pic11, pic12, pic13, pic14, ncol = 2)

ggsave("Figure1.png",fig1,height = 7,width = 13)

#################################################

# see when the intesection happens
date.intercept <- as.Date("2020-11-24")
# add 95% confidence bans
confidence_level <- .95
z_cl <- qnorm(confidence_level)
### death plots
death_plot <- case_policy_wide%>%
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
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "#E69F00") + 
  geom_line(aes(color = major_teaching),size = 1, alpha = .8) + 
  geom_ribbon(aes(ymin = 1000*death_prop_lower, ymax = 1000*death_prop_upper,
                  fill= major_teaching), 
              alpha = .3, show.legend = F)+ 
  geom_vline(xintercept = date.intercept, linetype = "dashed") + 
  annotate("text",x = date.intercept,y = 1.5,
           label = date.intercept,
           hjust = 1.1) + 
  team_theme + 
  #ggtitle("Death Incidences Increase Faster for Red Counties ")+
  labs(x = "Date", y = "Death Incidence / 1,000 people",
       color = "Majority teaching posture"
       #caption = "Yellow area represents fall semester (09/18/20 - 12/15/20)"
       )+
  scale_colour_manual(values=col_theme)+scale_fill_manual(values=col_theme)

library(lemon)
### B plots
cases_slope_teach_agg <- cases_slope_teach %>%
  drop_na(major_teaching)%>%
  group_by(DATE, major_teaching) %>%
  summarise(total_new_deaths = sum(rev_NEWDEATHS), .groups = "drop") %>%
  mutate(log_new_deaths = log(total_new_deaths + 1)) %>%
  group_by(major_teaching) %>%
  mutate(smooth.spline = smooth.spline(DATE,log_new_deaths,df = 398/28)$y,
         B = predict(smooth.spline(DATE,log_new_deaths,df = 398/28),deriv = 1)$y,B2 = predict(smooth.spline(DATE,log_new_deaths,df = 398/28),deriv = 2)$y)

week3_after_start <- as.Date("2020/08/18") + 21

####
b_plot <- ggplot(cases_slope_teach_agg, aes(x = DATE, color = major_teaching)) + 
  geom_line(aes(y = B), size = 1) +
  geom_rect(data = cases_slope_teach_agg[1,],
            aes(xmin=as.Date("2020/08/18"), xmax=as.Date("2020/12/15"),
                ymin=-Inf,ymax=Inf),
            color = NA,alpha=0.2, show.legend = F, fill = "#E69F00") +
  geom_hline(yintercept = 0, lty = 2) + 
  geom_vline(xintercept = week3_after_start, lty = 2) + 
  annotate("text",label = "3 Weeks After",
           x = week3_after_start, y = .05, hjust = 1.1)+ 
  geom_vline(xintercept = as.Date("2020/08/18")+42, lty = 2) + 
  annotate("text",label = "6 Weeks After",
           x = as.Date("2020/08/18")+130, y = .06, hjust = 1.3)+ 
  labs(x = "Date", y = "Exponential Growth Coefficient" 
       #color = "Majority teaching posture",
       #caption = "Yellow area represents fall semester (09/18/20 - 12/15/20)\nSmoothing window set to every 4 weeks"
       ) +team_theme+scale_color_manual(values=col_theme)

############

## Duncan test after significant ANOVA test
stat.test <- PostHocTest(fall_major_teaching.aov, method = "duncan")$major_teaching%>%
  as.data.frame()%>%
  rownames_to_column("group") %>%
  separate(group,"-", into = c("group1","group2")) %>%
  mutate(pval = round(pval,3),
         p = case_when(pval <= .01~ "**",
                       pval <= .05 ~ "*",
                       TRUE ~ "NS"))%>%
  select(group1, group2, pval, p)

# Box Plots with test statistics
death_box <- ggplot(fall_cases,aes(y = death_incidence_per_1000, x = major_teaching)) + 
  geom_boxplot(aes(fill = major_teaching),width=0.6)+
  stat_compare_means(method = "anova",size=6,geom="text",vjust=0.5,hjust=-0.2)+ 
  stat_pvalue_manual(stat.test, label = "p",y.position = 2, step.increase = 0.2,size=6)+
  ylim(c(0,3.5))+ 
  labs(y = "Death Incidence / 1,000 people",x = "Majority Teaching Posture")+
       #fill = "Majority teaching posture",
       #title = "Death Incidence in the Fall Semester",
       #subtitle = "Deaths (08/18/20 - 12/15/20)",
       #caption = "Pairwise p-values come from Duncan pairwise comparison test\nSignificance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘NS’ 1") +
  theme(legend.position = "hidden")+team_theme+ 
  scale_colour_manual(values=col_theme)+scale_fill_manual(values=col_theme)
#############

# one-way ANOVA
max_B.aov <- aov(max_B1~ major_teaching,data = B0B1)

summary(max_B.aov)

# Duncan test: p-value of .012
stat.test <- PostHocTest(max_B.aov, method = "duncan")$major_teaching %>%
  as.data.frame()%>%
  rownames_to_column("group") %>%
  separate(group,"-", into = c("group1","group2")) %>%
  mutate(pval = round(pval,3),
         p = case_when(pval <= .01~ "**",
                       pval <= .05 ~ "*",
                       TRUE ~ "NS"))%>%
  select(group1, group2, pval, p)
# boxplot
maxB_box <-na.omit(B0B1)%>%
  ggplot(aes(x=major_teaching,y=max_B1))+geom_boxplot(aes(fill=major_teaching),width=0.6)+
  ylim(c(0,0.08))+ 
  stat_compare_means(method = "anova",size=6,geom="text",vjust=0.5,hjust=-0.2)+ 
  stat_pvalue_manual(stat.test, label = "p",y.position = 0.04, step.increase = 0.2,size = 6)+
  labs(title="",x="Majority teaching posture",y="Max Growth",fill="Majority teaching posture")+
      # caption = "Pairwise p-values come from Duncan pairwise comparison test\nSignificance codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘NS’ 1")+
  team_theme+theme(legend.position = "hidden")+scale_fill_manual(values=col_theme)

#######

pic21 <- arrangeGrob(death_plot+
                       theme(legend.position = "hidden",
                             plot.margin=unit(c(1,1,1.5,1.5),"cm")),
                     top = textGrob("A.", x = unit(0, "npc")
                                    , y   = unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(fontface="bold",col="black", fontsize=22, 
                                            fontfamily="Times Roman")))

pic22 <- arrangeGrob(b_plot+
                       theme(legend.position = "hidden",
                             plot.margin=unit(c(1,1,1.5,1.5),"cm")),
                     top = textGrob("B.", x = unit(0, "npc")
                                    , y = unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(fontface="bold",col="black", fontsize=22, 
                                            fontfamily="Times Roman")))

pic23 <- arrangeGrob(death_box+
                       theme(
                         plot.margin=unit(c(1,1,1,1.5),"cm")), 
                     top = textGrob("C.", x = unit(0, "npc")
                                    , y = unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(fontface="bold",col="black", fontsize=22, 
                                            fontfamily="Times Roman")))

pic24 <- arrangeGrob(maxB_box+
                       theme(
                         plot.margin=unit(c(1,1,1,1.5),"cm")), 
                     top = textGrob("D.", x = unit(0, "npc")
                                    , y = unit(1, "npc"), just=c("left","top"),
                                    gp=gpar(fontface="bold",col="black", fontsize=22, 
                                            fontfamily="Times Roman")))



fig2 <- grid.arrange(pic21,pic22,pic23,pic24,ncol=2)

ggsave("Figure2.png",plot=fig2,width=15,height=10)

