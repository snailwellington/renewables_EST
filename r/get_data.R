library(tidyverse)
library(RODER) ## R Open Data Elering library for Elering dashboard API
library(here)



## Create years to query and make the query, then unnest the query
renew_dataset <- data.frame(year = seq(2010,2019,1)) %>% 
  mutate(data = map2(.x = paste0(year,"-01-01 00:00:00"), .y = paste0(year+1,"-01-01 00:00:00"), .f = get_system_real)) %>% 
  unnest()
## Save the data to RDS file so it would be easier to use 

saveRDS(renew_dataset,here("data","renew_dataset.RDS"))

