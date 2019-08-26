library(tidyverse)
library(here)
library(extrafont)

## calendar heatmap example -> https://rpubs.com/haj3/calheatmap

# load fonts - every session
loadfonts(device = "win", quiet = TRUE)

## get hour of the year

yhour <- function(datetime){

  return(lubridate::yday(datetime)*24+lubridate::hour(datetime)-24)
}




## read in system data to get renewables and other production data
renew_data_raw <- readRDS(here("data","renew_dataset.RDS"))


## add the latest data without querying all the data from ER API
### find max date from past data
last_max_date <- max(renew_data_raw$datetime)

### query new data until today
additional_data <- RODER::get_system_real(query_start = last_max_date, query_end = Sys.Date()) %>% 
  mutate(year = lubridate::year(datetime))

### bind by rows all the data and clean duplicate datetimes
renew_data_raw <- rbind(renew_data_raw,additional_data) %>% 
  distinct(datetime, .keep_all = TRUE)


### Save the data
saveRDS(renew_data_raw,here("data","renew_dataset.RDS"))

## remove "real." from column names
colnames(renew_data_raw) <- gsub("real.","",colnames(renew_data_raw))


## mutate data to get day of year and the balance between renewables and other energy production

renew_data <- renew_data_raw %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         yhour = yhour(datetime),
         monthweek = ceiling(lubridate::day(datetime)/7)) %>% 
  group_by(year,yhour) %>%
  # group_by(year,doy) %>%
  summarise(production = sum(production),
            production_renewable = sum(production_renewable),
            consumption = sum(consumption)) %>% 
  mutate(non_renew = production - production_renewable,
         renew_balance = 100*round(production_renewable / production,3),
         other_balance = 100*round(non_renew / production,3),
         renew_of_con = 100*round(production_renewable / consumption,3)) %>% 
  # rename("yhour" = doy)
  rename()
  


## per hour plots
##Share of renewables 
ggplot(subset(renew_data, year <= 2019),aes(x = yhour, y = 1))+
  geom_col(aes(fill = renew_balance), width = 1, color = NA)+
  scale_fill_gradient2(high = "green", mid = "grey70", low = "grey10", midpoint = 25, limits = c(0,50), na.value = "darkblue")+
  facet_grid(year~.,switch = "y")+
  labs(fill = "Renewable share, %",
       title = "Hourly share of Estonia's power generation by renewable fuels",
       caption = paste0("Data from Elering API (",format(Sys.Date(),"%d.%m.%y"),") \n Inspiration from -> https://twitter.com/Jamrat_"))+ #/status/1132390396787613696
  theme_minimal()+
  theme(axis.title = element_blank(),
        # legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Trebuchet MS"), #
        plot.caption = element_text(color = "grey25", size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key.width = unit(2,"cm"),
        panel.spacing.y = unit(0.25, "cm"),
        panel.spacing.x = unit(0, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"))+
  coord_cartesian(expand = FALSE)
  
ggsave(here("output","renewable_balance.png"), dpi = 300, width = 16, height = 9)

## per hour plots
##Share of renewables to cover the consumption
ggplot(subset(renew_data, year <= 2019),aes(x = yhour, y = 1))+
  geom_col(aes(fill = renew_of_con), width = 1, color = NA)+
  scale_fill_gradient2(high = "green", mid = "grey70", low = "grey10", midpoint = 25, limits = c(0,50), na.value = "darkblue")+
  facet_grid(year~.,switch = "y")+
  labs(fill = "Renewable share, %",
       title = "Hourly share of renewables to cover consumption of Estonia",
       caption = paste0("Data from Elering API (",format(Sys.Date(),"%d.%m.%y"),") \n Inspiration from -> https://twitter.com/Jamrat_"))+ #/status/1132390396787613696
  theme_minimal()+
  theme(axis.title = element_blank(),
        # legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Trebuchet MS"), #
        plot.caption = element_text(color = "grey25", size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key.width = unit(2,"cm"),
        panel.spacing.y = unit(0.25, "cm"),
        panel.spacing.x = unit(0, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"))+
  coord_cartesian(expand = FALSE)

ggsave(here("output","renewable_of_con.png"), dpi = 300, width = 16, height = 9)




## Share of non renewables
ggplot(renew_data,aes(x = yhour, y = 1))+
  geom_col(aes(fill = other_balance), width = 1, color = NA)+
  scale_fill_gradient2(high = "grey15", mid = "grey90", low = "green", midpoint = 75, limits = c(50,100), na.value = "red")+
  facet_grid(year~., switch= "y", space = "free")+
  labs(fill = "Non-Renewable share, %",
       title = "Hourly share of Estonia's power generation by non-renewable fuels",
       caption = paste0("Data from Elering API (",format(Sys.Date(),"%d.%m.%y"),") \n Inspiration from -> https://twitter.com/Jamrat_"))+ #/status/1132390396787613696
  theme_minimal()+
  theme(axis.title = element_blank(),
        # legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Trebuchet MS"), #
        plot.caption = element_text(color = "grey25", size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.key.width = unit(2,"cm"),
        panel.spacing.y = unit(0.25, "cm"),
        panel.spacing.x = unit(0, "cm"),
        plot.margin = unit(c(1,1,1,1), "cm"))+
  coord_cartesian(expand = FALSE)



ggsave(here("output","non_renewable_balance.png"), dpi = 300, width = 16, height = 9)

## 

renew_col_plot <- renew_data_raw %>% 
  na.omit() %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         yhour = yhour(datetime)) %>% 
  mutate(non_renew = production - production_renewable) %>% 
  select(datetime,production_renewable,non_renew) %>% 
  gather(key = "type",value = "value", production_renewable:non_renew) %>% 
  mutate(year = lubridate::year(datetime),
         yhour = yhour(datetime))


renew_col_plot <- renew_data_raw %>% 
  na.omit() %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         yhour = yhour(datetime)) %>% 
  mutate(non_renew = production - production_renewable,
         renew_balance = 100*round(production_renewable / production,3),
         other_balance = 100*round(non_renew / production,3)) %>% 
  select(datetime, renew_balance, other_balance) %>% 
  gather(key = "type",value = "value", other_balance:renew_balance) %>% 
  mutate(year = lubridate::year(datetime),
         yhour = yhour(datetime),
         type = as.factor(type))
 
renew_col_plot$type <- factor(renew_col_plot$type, levels = rev(levels(renew_col_plot$type)))

 
ggplot(renew_col_plot,aes(x = yhour))+
  geom_area(aes(y = value, fill = type))+
  scale_fill_manual(values = c("green","grey60"),labels = c("Renewable","Non renewable"))+
  facet_grid(year~., switch= "y", space = "free")+ 
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Aino"), #
        plot.caption = element_text(color = "grey25", size = 10),
        legend.text = element_text(size = 8),
        legend.key.width = unit(2,"cm"),
        strip.text = element_text(vjust = -20))
  
  
renew_col_plot <- renew_data_raw %>% 
  na.omit() %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         yhour = yhour(datetime)) %>% 
  group_by(year,doy) %>% 
  summarise(production = sum(production),
            production_renewable = sum(production_renewable)) %>% 
  mutate(non_renew = production - production_renewable,
         renew_balance = 100*round(production_renewable / production,3),
         other_balance = 100*round(non_renew / production,3)) %>% 
  select(year,doy, renew_balance, other_balance) %>% 
  gather(key = "type",value = "value", other_balance:renew_balance) %>% 
  mutate(type = as.factor(type))
  

renew_col_plot$type <- factor(renew_col_plot$type, levels = rev(levels(renew_col_plot$type)))

ggplot(renew_col_plot,aes(x = doy))+
  geom_area(aes(y = value, fill = type),position = "stack", color = NA, alpha = 0.95)+
  scale_fill_manual(values = c("green","grey60"),labels = c("Renewable","Non renewable"))+#,guide = guide_legend(reverse=TRUE))+
  facet_grid(year~., switch= "y", space = "free")+ 
  theme_minimal()+
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        legend.position = "top",
        panel.grid = element_blank(),
        text = element_text(size = 20, family = "Aino"), #
        plot.caption = element_text(color = "grey25", size = 10),
        legend.text = element_text(size = 8),
        legend.key.width = unit(2,"cm"),
        strip.text = element_text(vjust = -20))



## calendar plot
calendar_data <- renew_data_raw %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         weekday = lubridate::wday(datetime, label = TRUE),
         month = lubridate::month(datetime),
         hour = lubridate::hour(datetime),
         yhour = yhour(datetime),
         monthweek = ceiling(lubridate::day(datetime)/7)) %>% 
  na.omit() %>% 
  group_by(year,weekday,monthweek,month) %>%
  summarise(production = sum(production),
            production_renewable = sum(production_renewable),
            consumption = sum(consumption)) %>% 
  mutate(non_renew = production - production_renewable,
         renew_balance = 100*round(production_renewable / production,3),
         other_balance = 100*round(non_renew / production,3),
         renew_of_con = 100*round(production_renewable / consumption,3))

ggplot(calendar_data,aes(monthweek,weekday, fill = renew_of_con))+
  geom_tile(color = "white")+
  facet_grid(year~month)+
  scale_fill_gradient2(high = "green", mid = "grey70", low = "grey10", midpoint =25, limits = c(0,50), na.value = "darkblue")
