#make a plot
ggplot() +
  
  #label x and y axis using labs()
  labs(x = "S", y = "I or E") +
  
  #make curves for far and close island using geomcurve() customize using aes().
  geom_curve(aes(x = 0, y = 4, xend = 4, yend = 0), curvature = 0.2, linewidth = 2, lty = "dashed") +
  geom_curve(aes(x = 0, y = 0, xend = 4, yend = 4), curvature = 0.2, linewidth = 2) +
  geom_curve(aes(x = 0, y = 0, xend = 3, yend = 4), curvature = 0.2, linewidth = 2, col = "red") +
  geom_curve(aes(x = 0, y = 3, xend = 4, yend = 0), curvature = 0.2, linewidth = 2, lty = "dotted") +
  geom_segment(aes(x = 1.45, y = 0, xend = 1.45, yend = 1.1), linetype = "dashed", linewidth = 2, col = "green") +
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 1.2), linetype = "dashed", linewidth = 2, col = "green") +
  
  #make labels for the curves using annotate() use expression() and hat() and [] to make a circumflex.
  annotate("text", x = 1.1, y = 2.5, label = "I close and large", angle = 320, size = 5) +
  annotate("text", x = 0.9, y = 2, label = "I far and small", angle = 320, size = 5) +
  annotate("text", x = 3, y = 2.5, label = "E large and close", size = 5, angle = 45) +
  annotate("text", x = 2.1, y = 2.5, label = "E small and far ", size = 5, angle = 45) +
  annotate("text", x =2, y = 0, label = expression(hat(S = "S"[LC])), col = "black", size = 7) +
  annotate("text", x =1.45, y = 0, label = expression(hat(S = "S"[SF])), col = "black", size = 7) +
  
  
  #theme minimal gets rid of background
  theme_minimal() +
  
  #use theme to set the size of axis text with axis.title, make axis lines, and in this case leave out numbers on axis using axis.text = element_blank()
  theme(axis.line = element_line(size = 1, colour = "black", linetype = "solid" ),
        axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text = element_blank())





