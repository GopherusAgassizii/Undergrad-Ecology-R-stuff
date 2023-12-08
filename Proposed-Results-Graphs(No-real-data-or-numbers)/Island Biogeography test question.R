#make a plot
ggplot() +
  
  #label x and y axis using labs()
  labs(x = "S", y = "I or E") +
  
  #make curves for far and close island using geomcurve() customize using aes().
  geom_curve(aes(x = 0, y = 4, xend = 4, yend = 0), curvature = 0.2, linewidth = 2, lty = "dashed") +
  geom_curve(aes(x = 0, y = 0, xend = 4, yend = 4), curvature = 0.2, linewidth = 2) +
  geom_curve(aes(x = 0, y = 3, xend = 4, yend = 0), curvature = 0.2, linewidth = 2, lty = "dotted") +
  geom_segment(aes(x = 1.75, y = 0, xend = 1.75, yend = 0.9), linetype = "dashed", linewidth = 2) +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 1.2), linetype = "dashed", linewidth = 2) +
  
  #make labels for the curves using annotate()
  annotate("text", x = 1, y = 2.5, label = "Close island Immigration", angle = 315, size = 10) +
  annotate("text", x = 1, y = 2, label = "Far island Immigration", angle = 315, size = 10) +
  annotate("text", x = 3, y = 2.5, label = "E", size = 10) +
  

  
  
  #theme minimal gets rid of background
  theme_minimal() +
  
  #use theme to set the size of axis text with axis.title, make axis lines, and in this case leave out numbers on axis using axis.text = element_blank()
  theme(axis.line = element_line(size = 1, colour = "black", linetype = "solid" ),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text = element_blank())



