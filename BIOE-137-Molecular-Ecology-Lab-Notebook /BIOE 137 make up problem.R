pop1A <- c(10, 5, 1)

pop2A <- c(2, 6, 8)

p1 <- (pop1A[1] + 0.5*pop1A[2])/sum(pop1A)

q1 <- 1 - p1

p2 <- (pop2A[1] + 0.5 * pop2A)/sum(pop2A)

q2 <- 1 - p2

p <- (pop1A[1] + pop2A[1] + 0.5*(pop1A[2] + pop2A[2]))/(sum(pop1A) + sum(pop2A))

q <- 1 - p


HSA <- (pop1A[2] + pop2A[2])/(sum(pop1A) + sum(pop2A))

HtA <- 2 * p * q

FSTA <- (HtA - HSA)/HtA


pop1B <- c(4, 8, 4)

pop2B <- c(6, 7, 3)

z1 <- (pop1B[1] + 0.5*pop1B[2])/sum(pop1B)

c1 <- 1 - z1

z2 <- (pop2B[1] + 0.5 * pop2B)/sum(pop2B)

c2 <- 1 - z2

z <- (pop1B[1] + pop2B[1] + 0.5*(pop1B[2] + pop2B[2]))/(sum(pop1B) + sum(pop2B))

c <- 1 - p


HSB <- (pop1B[2] + pop2B[2])/(sum(pop1B) + sum(pop2B))

HtB <- 2 * z * c

FSTB <- (HtB - HSB)/HtB
