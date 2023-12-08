library(ggplot2)

#make room on the bottom with par
par(mar = c(7, 4, 4, 6))
#make a plot
ggplot() +
  
  #make a black line for the graph using geom_segment for species 1 isocline I set the lines to go from -1 because
  #I am going to set the axis limits to zero using coord_cartesian. This will make the lines intersect the axis :)
  geom_segment(aes(x = -1, y = 2, xend = 4, yend = -1), size = 2, colour = "black") +
  
  #make a black dashed line for the grpah using geom_segment for species 2 isocline
  geom_segment(aes(x = -1, y = 4, xend = 2, yend = -1), size = 2, colour = "black", linetype = "dashed") +
  
  #label x and y axis using labs()
  labs(x = expression(N[1]), y = expression(N[2])) +
  
  #Use coord_cartesian to set limits from 0-4 that way the lines intersect
  coord_cartesian(xlim = c(0, 3), ylim = c(0, 3)) +
  
  #theme minimal gets rid of background
  theme_minimal() +
    
  #use theme to set the size of axis text with axis.title, make axis lines, and in this case leave out numbers on axis using axis.text = element_blank()
  theme(axis.line = element_line(size = 1, colour = "black", linetype = "solid" ),
          axis.title.x = element_text(size = 25),
          axis.title.y = element_text(size = 25),
          axis.text = element_blank())


