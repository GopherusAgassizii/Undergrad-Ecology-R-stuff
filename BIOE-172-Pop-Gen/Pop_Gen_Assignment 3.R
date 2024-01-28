
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

#estimate allele frequencies for socal(sc) and british columbia(bc)
p_SC_A1 <- (southern_california_f[4] + 0.5 *southern_california_f[8])/southern_california_f[16]
q_SC_A2 <- (southern_california_f[12] + 0.5 *southern_california_f[8])/southern_california_f[16]
p_SC_B1 <- (southern_california_f[13] + 0.5 *southern_california_f[14])/southern_california_f[16]
q_SC_B2 <- (southern_california_f[15] + 0.5 *southern_california_f[14])/southern_california_f[16]

p_BC_A1 <- (Central_British_Columbia_f[4] + 0.5 *Central_British_Columbia_f[8])/Central_British_Columbia_f[16]
q_BC_A2 <- (Central_British_Columbia_f[12] + 0.5 *Central_British_Columbia_f[8])/Central_British_Columbia_f[16]
p_BC_B1 <- (Central_British_Columbia_f[13] + 0.5 *Central_British_Columbia_f[14])/Central_British_Columbia_f[16]
q_BC_B2 <- (Central_British_Columbia_f[15] + 0.5 *Central_British_Columbia_f[14])/Central_British_Columbia_f[16]

#make expected frequencies

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
check_chi <- sum((obs_SCA- 220*ex_SCA)^2/(220*ex_SCA))
chisq.test(obs_SCA, p = round(ex_SCA, digits = 6))
chisq.test(obs_SCB, p = round(ex_SCB, digits = 3))
chisq.test(obs_BCA, p = round(ex_BCA, digits = 2))
chisq.test(obs_BCB, p = round(ex_BCB, digits = 4))

#estimate linkage disequilibrium 
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

comparison_BC = (Central_British_Columbia_f-0.01)/exc_BC_f

#after poring over the data there seems to be incredibly strong selection for double heterozygotes(4x what we would expect!) and evidence of selection against double homozygotes in this population. 