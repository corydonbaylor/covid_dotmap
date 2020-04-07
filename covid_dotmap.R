library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
library(patchwork)

### example code from https://taraskaduk.com/2017/11/26/pixel-maps/

dmv = read.csv("github/covid_dotmap/dmv_covid.csv")%>%
  mutate(county_state = tolower(paste0(county, ", ", state)))%>%
  select(TotalCases, county_state)

# lets do one for DMV now
lat <- data_frame(lat = seq(36, 40, by = .1))
long <- data_frame(long = seq(-85, -74, by = .1))

dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  mutate(county = map.where('county', long, lat))%>%
  separate(county, c("state", "county"), sep = ",")%>%
  mutate(county_state = paste0(county, ", ", state))%>%
  mutate(county_state = gsub(":chincoteague", "",  county_state))%>%
  filter(state %in% c("district of columbia", "virginia", "maryland"))

dots = left_join(dots, dmv, by = "county_state")

missing = dots%>%
  filter(is.na(TotalCases))
  
theme <- theme_void() +
  theme(
    
        plot.title=element_text(face="bold", colour="#3C3C3C",size=16),
        plot.subtitle=element_text(colour="#3C3C3C",size=12),
        plot.caption = element_text(colour="#3C3C3C",size=10),  
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        legend.position = "none")


dot_map = ggplot(data = dots) +   
  geom_point(
    aes(x=long, 
        y = lat, 
        color = state, 
        size = TotalCases),
    alpha = .5
    ) + 
  #coord_map()+
  theme+
  scale_color_manual(values=c("#007a62", "#9999CC", "#7A0018"))

dot_map

# creating histograms

lats = dots%>%
  group_by(lat)%>%
  summarise(total = sum(TotalCases))

lat.histo <- ggplot(lats, aes(y = total, x = lat)) +
  geom_col(fill = "#7A0018") +
  theme_void() +
  coord_flip()

longs = dots%>%
  group_by(long)%>%
  summarise(total = sum(TotalCases))  

long.histo <- ggplot(longs, aes(y = total, x = long)) +
  geom_col(fill = "#7A0018") +
  theme_void() +
  scale_y_reverse()


(dot_map + lat.histo) + long.histo + plot_layout(ncol = 2, nrow = 2)

historatio <- max(lats$total)/max(longs$total)
histowidth <- 0.5

coord <- coord_quickmap(xlim = range(dots$long), 
                        ylim = range(dots$lat), expand = F)

coord

map.aspect <- coord$aspect(list(x.range = range(dots$long),
                                y.range = range(dots$lat)))



dot_map + lat.histo + long.histo + 
  plot_layout(ncol = 2, 
              nrow = 2, 
              widths = c(1/map.aspect, histowidth/historatio),
              heights = c(1,historatio*histowidth)
              )


outputfactor <- 10

ggsave("dmv.png", units = "cm", 
       height = outputfactor*(1 + histowidth), 
       width = outputfactor*(1/map.aspect + histowidth/historatio))
