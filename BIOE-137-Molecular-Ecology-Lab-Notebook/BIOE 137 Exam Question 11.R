#Part 1
#p = frequency dominant T, q = frequency of recessive t

#10% of the pop has double recessive so q should be sqrt(0.1)
q_0 <- sqrt(0.1)

#p + q add to 1 so 1 - q = p

p_0 <- 1 - q_0

#Part 2
#p = frequency dominant T, q = frequency of recessive t

#this year 31/250 are recessive so q should equal sqrt(31/250)
q_1 <- sqrt(31/250)

#p + q add to 1 so 1 - q = p
p_1 <- 1 - q_1

#p^2 + 2pq + q^2 = 1 under hardy weinberg. Therefore p^2 is TT, 2pq is Tt and q^2 is tt

TT <- p_1^2

Tt <- 2*p_1*q_1

tt <- q_1^2

#I want to check they add to 1 to make sure I did it right

HW <- TT + Tt + tt

#Part 3
#calculate the genotype frequencies of year zero

TT_0 <- p_0^2

Tt_0 <- 2*p_0*q_0

tt_0 <- q_0^2

#I am going to create vectors with the genotype frequencies to make doing the chi square easier. I am multiplying them by 100 to follow the directions in the question of showing frequencies out of 100 individuals
year_0 <- 100*c(TT_0, Tt_0, tt_0)

year_2 <- 100*c(TT, Tt, tt)

#I am using the chi square equation in the question and plugging vectors into it

Chi_test <- sum((year_2 - year_0)^2/year_0)

#degrees of freedom should be 2 because we have 2 columns and 3 rows df = (r-1)(c-1)
#it is inbetween 0.9 and 0.1 on the table so I will calculate what the p value should be based on the values on the table

p <- 0.9 - (0.9-0.1) * ((Chi_test - 0.211)/(4.605-0.211))