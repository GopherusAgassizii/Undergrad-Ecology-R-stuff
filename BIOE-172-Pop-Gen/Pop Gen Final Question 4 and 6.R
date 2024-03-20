#Question 4 part a
#create vectors to store our genotype frequencies
Pop_1 <- c(34, 51, 59)
Pop_2 <- c(24, 68, 49)

#calculate p and q for both pops
p_1 <- (Pop_1[1] + 0.5*Pop_1[2])/sum(Pop_1)
q_1 <- (Pop_1[3] + 0.5*Pop_1[2])/sum(Pop_1)

p_2 <- (Pop_2[1] + 0.5*Pop_2[2])/sum(Pop_2)
q_2 <- (Pop_2[3] + 0.5*Pop_2[2])/sum(Pop_2)

#calculate He assuming Hardy Weinberg Equilibrium
He_1 <- 1 - (p_1^2 + q_1^2)
He_2 <- 1 - (p_2^2 + q_2^2)

#Calculate Ho using the frequencies from the table
Ho_1 <- Pop_1[2]/sum(Pop_1)
Ho_2 <- Pop_2[2]/sum(Pop_2)

#Part B
#calculate inbreeding coefficient using FIS = He - Ho / He
FIS_1 <- (He_1 - Ho_1)/He_1
FIS_2 <- (He_2 - Ho_2)/He_2

#THIS SECTION IS NOT WORK FOR ANY REQUIRED CALCULATIONS YOU CAN SKIP OVER IT. I DONT WANT TO WASTE YOUR TIME FOR GRADING. 
#bonus FST calculation
#For this we are using FST where you subtract expected heterozygosity for subpopulation 
#from expected heterozygosity of total population.I know this is different from the one used in class but it is one I was taught in another class that I think
#is more appropriate given the situation
p_total <- (Pop_1[1] + Pop_2[1] + 0.5*(Pop_1[2] + Pop_2[2]))/(sum(Pop_1 + Pop_2))
q_total <- 1 - p_total

H_total <- 1 - (p_total^2 + q_total^2)

FST_1 <- (H_total - He_1)/H_total
FST_2 <- (H_total - He_2)/H_total

#END OF SKIPPABLE SECTION

#Question 6
#make sequence for different values of p and q
p <- (0:10)/10
#make equation for our line with mean fitness as our response variable
w <- (1-p^2)*p^2 + 2*p*(1-p)*(1-2*p*(1-p)) + (1-(1-p)^2)*(1-p)^2

#plot our line, make 0,0 the origin using axxs = i, yaxs = i use lwd to make a nicer line. 
plot(p, w, type = 'l', lwd = 5, xaxs = 'i', yaxs = 'i', ylab = "w")

#label each point on x axis so we can see where D is effectively zero
axis(side = 1, at = seq(0, 1, by = 0.1))
