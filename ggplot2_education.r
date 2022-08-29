### Data visualisation
  # data
  # mapping (aesthetics)
  # geometric representation
  # statistics
  # facet
  # coordinate space
  # labels
  # theme

library(tidyverse)

?BOD

ggplot(data = BOD,
       mapping = aes(x = Time,
                     y = demand)) +
  geom_point(size = 3) +
  geom_line(colour = "red")


View(CO2)

CO2 %>%
  ggplot(mapping = aes(conc, uptake,
                       colour = Treatment)) +
  geom_point(size = 3, alpha = 0.5) + 
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~Type) + 
  labs(title = "Concentration of CO2") +
  theme_bw()

CO2 %>% 
  ggplot(aes(Treatment, uptake)) +
  geom_boxplot()+
  geom_point(aes(size = conc,
                 colour = Plant)) + # notice the mapping only applied to this geometry
  coord_flip() +
  theme_bw() +
  facet_wrap(~Type) +
  labs(title = "Chilled vs Non-chilled")

# Use this function to save
ggsave() 

View(mpg)

mpg %>% 
  ggplot(aes(displ, cty))+
  geom_point(aes(colour = drv,
                 size = trans),
             alpha = 0.5)+
  geom_smooth(method = lm)+
  facet_wrap(~year, nrow = 1)+
  labs(x = "Engine size",
       y = "MPG in the city",
       title = "Fuel efficiency")+
  theme_bw()
