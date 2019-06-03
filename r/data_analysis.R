library(tidyverse)
library(here)
library(extrafont)



# load fonts - every session
loadfonts(device = "win", quiet = TRUE)

## get hour of the year

yhour <- function(datetime){

  return(lubridate::yday(datetime)*24+lubridate::hour(datetime)-24)
}




## read in system data to get renewables and other production data
renew_data_raw <- readRDS(here("data","renew_dataset.RDS"))

## remove "real." from column names
colnames(renew_data_raw) <- gsub("real.","",colnames(renew_data_raw))


## mutate data to get day of year and the balance between renewables and other energy production

renew_data <- renew_data_raw %>% 
  na.omit() %>% 
  filter(production != 0) %>% 
  mutate(doy = lubridate::yday(datetime),
         week = lubridate::week(datetime),
         yhour = yhour(datetime)) %>% 
  group_by(year,yhour) %>% 
  summarise(production = sum(production),
            production_renewable = sum(production_renewable)) %>% 
  mutate(non_renew = production - production_renewable,
         renew_balance = 100*round(production_renewable / production,3),
         other_balance = 100*round(non_renew / production,3))


## per hour plots
##Share of renewables 
ggplot(renew_data,aes(x = yhour, y = 1))+
  geom_col(aes(fill = renew_balance), width = 1, color = NA)+
  scale_fill_gradient2(high = "green", mid = "grey90", low = "grey15", midpoint = 25, limits = c(0,50), na.value = "darkblue")+
  facet_grid(year~., switch= "y", space = "free")+
  labs(fill = "Renewable share, %",
       title = "Hourly share of Estonia's power generation by renewable fuels",
       caption = "Data from Elering API (02.06.19)")+
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

ggsave(here("output","renewable_balance.png"), dpi = 300, width = 16, height = 9)

## Share of non renewables
ggplot(renew_data,aes(x = yhour, y = 1))+
  geom_col(aes(fill = other_balance), width = 1, color = NA)+
  scale_fill_gradient2(high = "grey15", mid = "grey90", low = "green", midpoint = 75, limits = c(50,100), na.value = "red")+
  facet_grid(year~., switch= "y", space = "free")+
  labs(fill = "Non-Renewable share, %",
       title = "Hourly share of Estonia's power generation by non-renewable fuels",
       caption = "Data from Elering API (02.06.19)")+
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

