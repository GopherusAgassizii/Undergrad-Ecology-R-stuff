pop1 <- c(0.2, 0.3, 0.5)
pop2 <- c(0.6, 0.3, 0.1)

I <- sum(pop1 * pop2)/(sqrt(sum(pop1^2) * sum(pop2^2)))

D = -log(I)

