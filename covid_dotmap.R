library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)

### example code from https://taraskaduk.com/2017/11/26/pixel-maps/

lat <- data_frame(lat = seq(-90, 90, by = 1))
long <- data_frame(long = seq(-180, 180, by = 1))
dots <- lat %>% 
  merge(long, all = TRUE)

## Only include dots that are within borders. Also, exclude lakes.
dots <- dots %>% 
  mutate(country = map.where('world', long, lat),
         lakes = map.where('lakes', long, lat)) %>% 
  filter(!is.na(country) & is.na(lakes)) %>% 
  select(-lakes)

head(dots)

theme <- theme_void() +
  theme(panel.background = element_rect(fill="#212121"),
        plot.background = element_rect(fill="#212121"),
        plot.title=element_text(face="bold", colour="#3C3C3C",size=16),
        plot.subtitle=element_text(colour="#3C3C3C",size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"))

ggplot() +   
  #base layer of map dots
  geom_point(data = dots, aes(x=long, y = lat), col = "grey45", size = 0.7) + 
  #plot all the places I've been to
  # geom_point(data = locations, aes(x=long_round, y=lat_round), color="grey80", size=0.8) + 
  #plot all the places I lived in, using red
  # geom_point(data = locations %>% filter(status == 'lived'), aes(x=long_round, y=lat_round), color="red", size=0.8) +
  #an extra layer of halo around the places I lived in
  # geom_point(data = locations %>% filter(status == 'lived'), aes(x=long_round, y=lat_round), color="red", size=6, alpha = 0.4) +
  #adding my theme
  theme


# lets do one for USA now
lat <- data_frame(lat = seq(19, 64, by = .5))
long <- data_frame(long = seq(-161, -68, by = .5))

dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  mutate(state = map.where('state', long, lat))%>%
  filter(!is.na(state))

ggplot() +   
  geom_point(data = dots, aes(x=long, y = lat), col = "grey45", size = 0.7) + 
  theme


# lets do one for DMV now
lat <- data_frame(lat = seq(35, 40, by = .085))
long <- data_frame(long = seq(-85, -72, by = .085))

dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  mutate(state = map.where('state', long, lat))%>%
  mutate(county = map.where('county', long, lat))%>%
  filter(state %in% c("maryland", "virginia:chesapeake", "district of columbia", "virginia:main"))

ggplot() +   
  geom_point(data = dots, aes(x=long, y = lat, color = state), size = 0.7) + 
  theme         

