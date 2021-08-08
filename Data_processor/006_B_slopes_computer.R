## This document is to compute the growth coefficients B for each county in Ohio
## Author: Cheyenne Ehman 04/27/2021

source("../Data_processor/004_Ohio_case_profile_data_processor.R")

# Fit Splines to Log( New Deaths + 1 )
log_deaths_county <- cases %>%
  group_by(COUNTY) %>%
  mutate(log_tot_deaths = log(CUMDEATHS + 1),
         log_new_deaths = log(rev_NEWDEATHS + 1),
         tot.smoothed.spline = smooth.spline(DATE,log_tot_deaths, df = 398/28)$y,
         new.smoothed.spline = smooth.spline(DATE,log_new_deaths, df = 398/28)$y,
         tot.slope = predict(smooth.spline(DATE,log_tot_deaths, df = 398/28),deriv = 1)$y,
         new.slope = predict(smooth.spline(DATE,log_new_deaths, df = 398/28),deriv = 1)$y)
#Want to order the counties by most Populous
log_deaths_county$COUNTY <- factor(log_deaths_county$COUNTY,
                                      levels = levels(fct_reorder(log_deaths_county_df$COUNTY,log_deaths_county_df$POPULATION,max,.desc = TRUE)))

#writes.csv(log_deaths_county,"../Data/Cleaned_Data/county_splines_slopes_B.csv")