## This document is to collect mobility data from CMU Delphi's 'covid_cast' API
## Author: Ziyan Zhu ## Updated on 08/01/2021


##################### get mobility cured data ########################
library(covidcast)

start_collect = "2020-01-22"
end_collect = "2021-02-22"


parttime_work <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "part_time_work_prop_7dav",
                   start_day = start_collect, end_day = end_collect,
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175"))
)

#Delphi receives data from SafeGraph, which collects anonymized location data from mobile phones. 
#Using this data, we calculate the fraction of mobile devices that spent more than 6 hours in one
#location other than their home during the daytime, and average it over a 7 day trailing window. 
#This indicator measures how mobile people are, and ought to reflect whether people are traveling 
#to work or school outside their homes. See also our "At Away Location 3-6hr" indicator.

fulltime_work <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "full_time_work_prop_7dav",
                   start_day = start_collect, end_day = end_collect,
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175"))
)


restaurant <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "restaurants_visit_prop",
                   start_day = start_collect, end_day = end_collect,
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175")))


bar <- suppressMessages(
  covidcast_signal(data_source = "safegraph", signal = "bars_visit_prop",
                   start_day = start_collect, end_day = end_collect,
                   geo_type = "county", geo_values = c("39001", "39003", "39005", "39007", "39009", "39011", "39013", "39015", "39017", "39019", "39021", "39023", "39025", "39027", "39029", "39031", "39033", "39035", "39037", "39039", "39041", "39043", "39045", "39047", "39049", "39051", "39053", "39055", "39057", "39059", "39061", "39063", "39065", "39067", "39069", "39071", "39073", "39075", "39077", "39079", "39081", "39083", "39085", "39087", "39089", "39091", "39093", "39095", "39097", "39099", "39101", "39103", "39105", "39107", "39109", "39111", "39113", "39115", "39117", "39119", "39121", "39123", "39125", "39127", "39129", "39131", "39133", "39135", "39137", "39139", "39141", "39143", "39145", "39147", "39149", "39151", "39153", "39155", "39157", "39159", "39161", "39163", "39165", "39167", "39169", "39171", "39173", "39175")))

#combine the data above into one mobility table

mobility <- parttime_work%>%
  rename(part_work_prop_7d=value,part_work_std = stderr,part_work_sample_size=sample_size)%>%
  left_join(fulltime_work%>%
              rename(full_work_prop_7d=value,full_work_std = stderr,full_work_sample_size=sample_size,full_work_std = stderr),
            by = c("geo_value","time_value"))%>%
  left_join(restaurant%>%
              rename(res_visit_by_pop = value),by = c("geo_value","time_value"))%>%
  left_join(bar%>%
              rename(bar_visit_by_pop = value),by = c("geo_value","time_value"))%>%
  select(geo_value,time_value,part_work_prop_7d,part_work_sample_size,
         full_work_prop_7d,full_work_sample_size,full_work_std,
         res_visit_by_pop,bar_visit_by_pop)

remove(bar)
remove(fulltime_work)
remove(parttime_work)
remove(restaurant)

#write.csv(mobility,"mobility_2021_08_01.csv")