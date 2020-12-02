library(RODER)
library(tidyverse)


plan_res_data <- RODER::get_system_plan(query_start = "2019-01-01 00:00:00", query_end = "2020-01-01 00:00:00")
real_res_data <- RODER::get_system_real(query_start = "2019-01-01 00:00:00", query_end = "2020-01-01 00:00:00")



comp_data <- plan_res_data[,c(1,6)] %>% 
  left_join(real_res_data[,c(1,6)], by = "datetime") %>% 
  mutate(dif = plan.production_renewable - real.production_renewable)


ggplot(comp_data, aes(x = datetime, y = dif))+
  geom_line()+
  geom_smooth()




