library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)

### example code from https://taraskaduk.com/2017/11/26/pixel-maps/

dmv = read.csv("github/covid_dotmap/dmv_covid.csv")%>%
  mutate(county_state = tolower(paste0(county, ", ", state)))%>%
  select(TotalCases, county_state)

# lets do one for DMV now
lat <- data_frame(lat = seq(36, 40, by = .12))
long <- data_frame(long = seq(-85, -74, by = .12))

dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  mutate(county = map.where('county', long, lat))%>%
  separate(county, c("state", "county"), sep = ",")%>%
  mutate(county_state = paste0(county, ", ", state))%>%
  filter(state %in% c("district of columbia", "virginia", "maryland"))

dots = left_join(dots, dmv, by = "county_state")
  
theme <- theme_void() +
  theme(panel.background = element_rect(fill="#212121"),
        plot.background = element_rect(fill="#212121"),
        plot.title=element_text(face="bold", colour="#3C3C3C",size=16),
        plot.subtitle=element_text(colour="#3C3C3C",size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none")


ggplot(data = dots) +   
  geom_point(
    aes(x=long, 
        y = lat, 
        color = state, 
        size = TotalCases),
    alpha = .8
    ) + 
  #scale_size_area(max_size = 10)+
  coord_map()+
  theme         


