#make variables for the genotype frequencies for population 1 and 2

MM_pop_1 <- 46
MS_pop_1 <- 40
SS_pop_1 <- 14

Observed_genotype_pop_1 <- c(MM_pop_1, MS_pop_1, SS_pop_1)

MM_pop_2 <- 9
MS_pop_2 <- 35
SS_pop_2 <- 56

observed_genotype_pop_2 <- c(MM_pop_2, MS_pop_2, SS_pop_2)

#make variables for alleles M and S for pop 1 and pop 2

one_M <- (MM_pop_1 + 0.5 * MS_pop_1)/sum(Observed_genotype_pop_1)
one_S <- (SS_pop_1 + 0.5 * MS_pop_1)/sum(Observed_genotype_pop_1)

two_M <- (MM_pop_2 + 0.5 * MS_pop_2)/sum(observed_genotype_pop_2)
two_S <- (SS_pop_2 + 0.5 * MS_pop_2)/sum(observed_genotype_pop_2)

#make variables for expected genotypes by using p^2 + 2pq + q^2 then multiply by the total in the pop

Pop_1_MM <- one_M^2
Pop_1_MS <- 2 * one_M * one_S
Pop_1_SS <- one_S^2

expected_genotype_pop_1 <- sum(Observed_genotype_pop_1) * c(Pop_1_MM, Pop_1_MS, Pop_1_SS)

Pop_2_MM <- two_M^2
Pop_2_MS <- 2 * two_M * two_S
Pop_2_SS <- two_S^2

expected_genotype_pop_2 <- sum(observed_genotype_pop_2) * c(Pop_2_MM, Pop_2_MS, Pop_2_SS)

#Now make expected genotype frequencies for the Pooled population

pooled_observed <- c(observed_genotype_pop_2 + Observed_genotype_pop_1)

M_pooled <- (MM_pop_1 + MM_pop_2 + 0.5 * (MS_pop_1 + MS_pop_2))/sum(pooled_observed)
S_pooled <- (SS_pop_2 + SS_pop_1 + 0.5 * (MS_pop_1 + MS_pop_2))/sum(pooled_observed)

pool_MM <- M_pooled^2
pool_MS <- 2 * M_pooled * S_pooled
pool_SS <- S_pooled^2

pooled_expected <- sum(pooled_observed) * c(pool_MM, pool_MS, pool_SS)

#do chi square using sum((observed - expected)^2/expected) for each pop and pooled pop

chi_square_pop_1 <- sum((Observed_genotype_pop_1 - expected_genotype_pop_1)^2/expected_genotype_pop_1)
chi_square_pop_2 <- sum((observed_genotype_pop_2 - expected_genotype_pop_2)^2/expected_genotype_pop_2)
chi_square_pooled <- sum((pooled_observed - pooled_expected)^2/pooled_expected)
