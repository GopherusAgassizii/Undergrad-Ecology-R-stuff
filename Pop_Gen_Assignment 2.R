#Part A
#make vector for our SNP alleles
obs_snp1 <- c(91, 119, 35)
obs_snp34 <- c(125, 91, 29)

#make vector to calculate observed frequencies by dividing by the sum
obs_f_snp1 <- obs_snp1/sum(obs_snp1)
obs_f_snp34 <- obs_snp34/sum(obs_snp34)

#make variables for A1, A2, B1, and B2 and calculate their frequencies by adding homozygous with 0.5 * heterozygotes and dividing by the sum(sample size)
p_A1 <- (91 + 0.5*119)/sum(obs_snp1)
p_A2 <- (35 + 0.5*119)/sum(obs_snp1)
q_B1 <- (125 + 0.5*91)/sum(obs_snp34)
q_B2 <- (29 + 0.5*91)/sum(obs_snp34)

#make expected frequencies for each genotype square the homozygotes and 2pq the heteroz
ex_A1A1 <- p_A1^2 
ex_A1A2 <- 2 * p_A1 * p_A2
ex_A2A2 <- p_A2^2 

ex_B1B1 <- q_B1^2 
ex_B1B2 <- 2 * q_B1 * q_B2 
ex_B2B2 <- q_B2^2 

#make a vector that contains our expected frequencies for each genotype
exSNP1 <- c(ex_A1A1, ex_A1A2, ex_A2A2) 
exSNP34 <- c(ex_B1B1, ex_B1B2, ex_B2B2)

excSNP1 <- 245 * exSNP1
excSNP34 <- 245 * exSNP34

#use chiq.test to find the chi square value to test for significance 
chisq.test(obs_snp1, p=round(exSNP1, digits = 3))
chisq.test(obs_snp34, p=round(exSNP34, digits = 4))

#test to make sure it worked by doing chi square by hand the values outputed by test and test_2 should be roughly similar to chiq.test
test <- sum(((excSNP1 - obs_snp1)^2/excSNP1))
test_2 <- sum(((excSNP34 - obs_snp34)^2)/excSNP34)

#Part B

#calculate frequencies of haplotype
obs_hap <- c(32, 28, 24, 39)

freq_A1B1 = 32/sum(obs_hap)
freq_A1B2 = 28/sum(obs_hap)
freq_A2B1 = 24/sum(obs_hap)
freq_A2B2 = 39/sum(obs_hap)

#calculate D
D <- (freq_A1B1*freq_A2B2) - (freq_A1B2*freq_A2B1)

#Part C

#calculate D prime
D_prime = D/0.1867