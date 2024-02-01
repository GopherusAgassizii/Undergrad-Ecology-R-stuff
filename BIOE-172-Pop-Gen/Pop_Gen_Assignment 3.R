
#Question 1
#(a)
#make matrices for the southern california and central british columbia data
southern_california_raw <- c(35, 79, 59, 5, 28, 6, 2, 3, 3)
matrix_southern_california <- matrix(data = southern_california_raw, nrow = 3, ncol =  3)
southern_california <- rbind(matrix_southern_california, colSums(matrix_southern_california))
southern_california_f <- cbind(southern_california, rowSums(southern_california))

Central_British_Columbia_raw <- c(27, 39, 25, 63, 52, 4, 35, 0, 0)
matrix_Central_British_Columbia <- matrix(data = Central_British_Columbia_raw, nrow = 3, ncol =  3)
Central_British_Columbia <- rbind(matrix_Central_British_Columbia, colSums(matrix_Central_British_Columbia))
Central_British_Columbia_f <- cbind(Central_British_Columbia, rowSums(Central_British_Columbia))

#estimate allele frequencies for socal(sc) and british columbia(bc) using p1 = A1A1 + 0.5A1A2, q1 = A2A2 + 0.5A1A2, .......
p_SC_A1 <- (southern_california_f[4] + 0.5 *southern_california_f[8])/southern_california_f[16]
q_SC_A2 <- (southern_california_f[12] + 0.5 *southern_california_f[8])/southern_california_f[16]
p_SC_B1 <- (southern_california_f[13] + 0.5 *southern_california_f[14])/southern_california_f[16]
q_SC_B2 <- (southern_california_f[15] + 0.5 *southern_california_f[14])/southern_california_f[16]

p_BC_A1 <- (Central_British_Columbia_f[4] + 0.5 *Central_British_Columbia_f[8])/Central_British_Columbia_f[16]
q_BC_A2 <- (Central_British_Columbia_f[12] + 0.5 *Central_British_Columbia_f[8])/Central_British_Columbia_f[16]
p_BC_B1 <- (Central_British_Columbia_f[13] + 0.5 *Central_British_Columbia_f[14])/Central_British_Columbia_f[16]
q_BC_B2 <- (Central_British_Columbia_f[15] + 0.5 *Central_British_Columbia_f[14])/Central_British_Columbia_f[16]

#make expected frequencies using homozygote = p^2 or q^2 and heterozygote = 2pq

SC_ex_A1A1 <- p_SC_A1^2
SC_ex_A1A2 <- 2*(p_SC_A1 * q_SC_A2)
SC_ex_A2A2 <- q_SC_A2^2

SC_ex_B1B1 <- p_SC_B1^2
SC_ex_B1B2 <- 2*(p_SC_B1 * q_SC_B2)
SC_ex_B2B2 <- q_SC_B2^2

BC_ex_A1A1 <- p_BC_A1^2
BC_ex_A1A2 <- 2*(p_BC_A1 * q_BC_A2)
BC_ex_A2A2 <- q_BC_A2^2

BC_ex_B1B1 <- p_BC_B1^2
BC_ex_B1B2 <- 2*(p_BC_B1 * q_BC_B2)
BC_ex_B2B2 <- q_BC_B2^2

#make vectors for expected and observed for SNPs
ex_SCA <- c(SC_ex_A1A1, SC_ex_A1A2, SC_ex_A2A2)
ex_SCB <- c(SC_ex_B1B1, SC_ex_B1B2, SC_ex_B2B2)

obs_SCA <- c(southern_california_f[4], southern_california_f[8], southern_california_f[12])
obs_SCB <- c(southern_california_f[13], southern_california_f[14], southern_california_f[15])

ex_BCA <- c(BC_ex_A1A1, BC_ex_A1A2, BC_ex_A2A2)
ex_BCB <- c(BC_ex_B1B1, BC_ex_B1B2, BC_ex_B2B2)

obs_BCA <- c(Central_British_Columbia_f[4], Central_British_Columbia_f[8], southern_california_f[12])
obs_BCB <- c(Central_British_Columbia_f[13], Central_British_Columbia_f[14], Central_British_Columbia_f[15])

#do chi square test to test for hardy Weinberg equilibrium. I checked the first one since it was saying it might not be accurate due to needing to be rounded to 6 decimals.
#To check it I made sure to use the formula X^2 = Sum(E-O)^2/E. Because it spit out the right value I assume it is correct for the other tests too given that I used the same formula for each one
check_chi <- sum((obs_SCA- 220*ex_SCA)^2/(220*ex_SCA))
chisq.test(obs_SCA, p = round(ex_SCA, digits = 6))
chisq.test(obs_SCB, p = round(ex_SCB, digits = 3))
chisq.test(obs_BCA, p = round(ex_BCA, digits = 2))
chisq.test(obs_BCB, p = round(ex_BCB, digits = 4))

#I also did Chi square tests for the populations using the genotype frequencies(A1A1B1B1, A1A1,B1B2, ....) rather than seperate frequencies for each locus. In both instances I got warnings that 
#the chisq.test function may not give the right answer, so I used the formula to check it with my chi_test_SC and chi_test_BC. 
ex_genotypes_SC <- c(SC_ex_A1A1*SC_ex_B1B1, SC_ex_A1A1*SC_ex_B1B2, SC_ex_A1A1*SC_ex_B2B2, SC_ex_A1A2*SC_ex_B1B1, SC_ex_A1A2*SC_ex_B1B2, SC_ex_A1A2*SC_ex_B2B2,
                     SC_ex_A2A2*SC_ex_B1B1, SC_ex_A2A2*SC_ex_B1B2, SC_ex_A2A2*SC_ex_B2B2)
ex_count_genotypes_SC <- 220*ex_genotypes_SC

genotype_observed_SC <- c(southern_california_f[1],southern_california_f[2],southern_california_f[3],southern_california_f[5],southern_california_f[6],
                          southern_california_f[7],southern_california_f[9],southern_california_f[10],southern_california_f[11])
chi_test_SC <- sum((ex_count_genotypes_SC - genotype_observed_SC)^2/ex_count_genotypes_SC)
chisq.test(genotype_observed_SC, p = round(ex_genotypes_SC, digits = 3))

ex_genotypes_BC <- c(BC_ex_A1A1*BC_ex_B1B1, BC_ex_A1A1*BC_ex_B1B2, BC_ex_A1A1*BC_ex_B2B2, BC_ex_A1A2*BC_ex_B1B1, BC_ex_A1A2*BC_ex_B1B2, BC_ex_A1A2*BC_ex_B2B2,
                     BC_ex_A2A2*BC_ex_B1B1, BC_ex_A2A2*BC_ex_B1B2, BC_ex_A2A2*BC_ex_B2B2)
ex_count_genotypes_BC <- 220*ex_genotypes_BC

genotype_observed_BC <- c(Central_British_Columbia_f[1],Central_British_Columbia_f[2],Central_British_Columbia_f[3],Central_British_Columbia_f[5],Central_British_Columbia_f[6],
                          Central_British_Columbia_f[7],Central_British_Columbia_f[9],Central_British_Columbia_f[10],Central_British_Columbia_f[11])
chi_test_BC <- sum((ex_count_genotypes_BC - genotype_observed_BC)^2/ex_count_genotypes_BC)
chisq.test(genotype_observed_BC, p = round(ex_genotypes_BC, digits = 6))

#estimate linkage disequilibrium using D = [2N11 + N12 + N21 + (N22/2)]/Ntotal - 2p1p2
D_SC <- ((2*southern_california_f[1] + southern_california_f[5] + 
           southern_california_f[2] + 0.5*southern_california_f[6])/southern_california_f[16])- 2*p_SC_A1*p_SC_B1

D_BC <- ((2*Central_British_Columbia_f[1] + Central_British_Columbia_f[5] + 
            Central_British_Columbia_f[2] + 0.5*Central_British_Columbia_f[6])/Central_British_Columbia_f[16])- 2*p_BC_A1*p_BC_B1

#B
#estimate D prime by doing D/Dmax for both populations

D_prime_SC <- D_SC/-0.06969
D_prime_BC <- D_BC/-0.11729

#C
#I think there is some natural selection going on. I want to see the expected counts so I can compare to the observed

exc_BC_raw <- c( 245*p_BC_A1^2*p_BC_B1^2, 245*p_BC_A1^2*p_BC_B1*q_BC_B2, 245*p_BC_A1^2*q_BC_B2^2,
              245*p_BC_A1*q_BC_A2*p_BC_B1^2, 245*p_BC_A1*q_BC_A2*p_BC_B1*q_BC_B2, 245*p_BC_A1*q_BC_A2*q_BC_B2^2,
              245*q_BC_A2^2*p_BC_B1^2, 245*q_BC_A2^2*p_BC_B1*q_BC_B2, 245*q_BC_A2^2*q_BC_B2^2)

matrix_exc_BC <- matrix(data = exc_BC_raw, nrow = 3, ncol = 3)
exc_BC <- rbind(matrix_exc_BC, colSums(matrix_exc_BC))
exc_BC_f <- cbind(exc_BC, rowSums(exc_BC))

#after poring over the data there seems to be selection for double heterozygotes(4x what we would expect!) and evidence of selection against double homozygotes in this population. 
