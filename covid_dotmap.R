library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
library(plotly)
library(patchwork)
library(lubridate)
library(gganimate)
library(gifski)


### example code from https://taraskaduk.com/2017/11/26/pixel-maps/
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Creating COVID Time Series ----------------------------------------------


# cleaning up data
dmv = read.csv("covid_confirmed_usafacts.csv", stringsAsFactors = F)%>%
  filter(State %in% c("VA", "DC", "MD"))%>%
  select(-c(ï..countyFIPS, stateFIPS))
  
# creating a time series 
dmv_ts = dmv%>%
  mutate(county_state =  paste0(tolower(gsub(" County|\\.|'", "", County.Name)),", ",
                                tolower(
                                  ifelse(
                                    State == "DC", "district of columbia", #dc isnt in state.abb
                                    state.name[match(State, state.abb)]
                                  ) # ifelse
                                  )# to lower
                                ) # paste0
         )%>%
  select(-State, -County.Name)%>%
  gather(Date, Count, -county_state)%>%
  mutate(Date = mdy(gsub("X", "", Date)))%>%
  filter(Date >= '2020-03-01')

# there are a few edge case county names that need to be adjusted for
dmv_ts = dmv_ts%>%
  mutate(county_state = ifelse(
    county_state %in% c("baltimore city, maryland", 
                        "james city, virginia",
                        "charles city, virginia"),
    county_state,
    gsub(" city", "", county_state)
    ))
  

# Creating Lat Long Dot Dataframe -----------------------------------------



# DC, Maryland and Virginia sit between the 36th and 40th latitude
# and -85 and -74 longitude

lat <- data.frame(lat = seq(36, 40, by = .06))
long <- data.frame(long = seq(-85, -74, by = .06))

# create a lat long dataframe
dots = lat %>% 
  merge(long, all = TRUE)

# where we talking
dots = dots %>% 
  
  # the map.where function returns the county given a lat long
  
  mutate(county = map.where('county', long, lat))%>%
  separate(county, c("state", "county"), sep = ",")%>%
  mutate(county_state = paste0(county, ", ", state))%>%
  mutate(county_state = gsub(":chincoteague|:main", "",  county_state))%>%
  filter(state %in% c("district of columbia", "virginia", "maryland"))%>%
  
  # Next we need to join the time series to our dot matrix
  
  left_join(dmv_ts, by = "county_state")%>%
  group_by(Date)%>%
  mutate(day_total = sum(Count))%>%
  ungroup()

# We want to create total for each region as a caption

dots_final = dots%>%
  group_by(state, Date)%>%
  summarise(total = sum(Count))%>%
  spread(state, total)%>%
  right_join(dots, by = "Date")


# Creating gganimate ------------------------------------------------------



dot_map = ggplot(data = dots_final) +   
  geom_point(
    aes(x=long, 
        y = lat, 
        color = state, 
        size = Count),
    alpha = .5
    ) + 
  coord_map()+
  theme_void()+
  theme(
    
    plot.title=element_text(
                        face="bold", 
                        colour="#3C3C3C",
                        size=22,
                        hjust = .2,
                        vjust = -20
                        ),
    plot.subtitle=element_text(
                              colour="#3C3C3C",
                              size=13,
                              hjust = .225,
                              vjust = -28
                              ),
    plot.caption = element_text(colour="#3C3C3C",
                                size=13,
                                hjust = 0.1,
                                vjust = 5),  
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "none"
    
  )+
  scale_color_manual(values=c("#007a62", "#9999CC", "#7A0018"))+
  labs(
   title = "COVID-19",
   subtitle = "in DC, Maryland, and Virgina",
   
   # Using glue we can find the relevant total
   
   caption = "Date: {format(as.Date(closest_state), '%B %d')} | DC Cases: {format(dots_final[dots_final$Date == closest_state,]$`district of columbia`[1], big.mark = ',')} | Maryland Cases: {format(dots_final[dots_final$Date == closest_state,]$maryland[1], big.mark = ',')} | Virginia Cases: {format(dots_final[dots_final$Date == closest_state,]$virginia[1], big.mark = ',')} | Total Cases: {format(dots_final[dots_final$Date == closest_state,]$day_total[1], big.mark = ',')}"
   
   #caption = "{closest state}"
  )+
transition_states(
    Date,
    transition_length = 2,
    state_length = 1
  )


animate(dot_map, 
        nframes = 150, #more frames for make it smoother but longer to render
        fps = 10, #how many frames are shown per second
        height = 400,
        width = 800,
        end_pause = 30
)
anim_save("covid19_dot_map.gif")
