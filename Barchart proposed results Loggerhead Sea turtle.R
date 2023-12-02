
library(ggplot2)
  
  #use ggplot to make our graph
    
    #use par() to increase size of graph on left(2nd number normally 4 so we added 2 inches hence the 6)
    par(mar = c(5, 6, 4, 6))
    
    #make a vector to contain the values that will be used to construct bar chart
    turtle <- c(-0.5, 1)
    
    #make our bar chart use turtle vector
    barplot(turtle, 
              #Since this is for proposed results I want to eliminate numbers so we set yaxt = "n"
              yaxt="n",
              #set colors for bar charts to be red and green
              col=c("red", "darkgreen"), 
              #set width of bar chart to 0.2 and space between them to 0
              width = 0.2, 
              space = 0,
              #add a label for the y axis and set its font size to 3
              ylab = "Population Growth rate(r)",
              cex.lab = 3,
              #add a legend and set text to 3.5 and move to top right corner
              legend.text = c("Without Policies", "With Policies"), 
              args.legend=list(cex=3.5,x="topright"),
              #set the limits we can see in the graph for our y axis and our x axis
              ylim = c(-1, 1),
              xlim = c(0, 1.5))
    
    #make a line segment with geom_segment. We will use this for visualizing where zero is on the chart
    abline(h=0, lwd = 3, col = "black", lty = "solid")
  
    #make a line segment with geom_segment. We will use this for plotting current r for loggerheads
    abline(h=-0.5, lwd = 3, col = "red", lty = "dashed")
    
    #make a new y axis since we got rid of the old one. 
    #Make a vector for the lower bound, zero, and upper bound. Then label the vector using label. (we are leaving upper and lower bound blank since this is proposed results
    #set line width to 3 and axis size to 3
    axis(2, at = c(-1, 0, 1), labels = c("", "0", ""), lwd = 3, cex.axis = 3)
  
    
    
  
  