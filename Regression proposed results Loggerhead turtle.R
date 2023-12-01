
#use ggplot2 to make a declarative graph(this has no data it's just a graph to illustrate an idea)
library(ggplot2)

ggplot() +

  #make a line segment with geom_segment. Set color, size, and direction of line

  geom_segment(aes(x=0, y = 1, xend = 1, yend=0), size = 2, colour = "red") +
  
  #label x and y axis using labs()
  labs(x = "Time since nest protection policies", y = "Egg poaching") +

  #make a second y axis using sec.axis() under scale_y_continuous()

  scale_y_continuous(
  
    #set transformation of y axis to what you need(1 in this case because we don't want to transform)
    name = "egg poaching", sec.axis = sec_axis(trans = ~.*1, name = "Hatchling to Ocean Mortality rate")
    )  +
  
  #theme minimal gets rid of background
  theme_minimal() +
  
  #use theme to set the size of axis text with axis.title, make axis lines, and in this case leave out numbers on axis using axis.text = element_blank()
  theme(axis.line = element_line(size = 1, colour = "black", linetype = "solid" ),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text = element_blank())
