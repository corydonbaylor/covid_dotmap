library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)

### example code from https://taraskaduk.com/2017/11/26/pixel-maps/

# lets do one for DMV now
lat <- data_frame(lat = seq(36, 40, by = .085))
long <- data_frame(long = seq(-85, -74, by = .085))

dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  mutate(county = map.where('county', long, lat))%>%
  separate(county, c("state", "county"), sep = ",")%>%
  filter(state %in% c("district of columbia", "virginia", "maryland"))
  
theme <- theme_void() +
  theme(panel.background = element_rect(fill="#212121"),
        plot.background = element_rect(fill="#212121"),
        plot.title=element_text(face="bold", colour="#3C3C3C",size=16),
        plot.subtitle=element_text(colour="#3C3C3C",size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


ggplot() +   
  geom_point(data = dots, aes(x=long, y = lat, color = state), size = 0.7) + 
  coord_map()+
  theme         

