#create variables for our historic and current heterozygosity
H0_Nor_Cal <- 0.65
H0_Central_Oregon <- 0.59
H0_Alaska <- 0.76

Ht_Nor_Cal <- 0.43
Ht_Central_Oregon <- 0.54
Ht_Alaska <- 0.74

#create a variable for t, the number of generations
t <- (2023-1989)/2

#solve for N
Nor_Cal_N <- -t/(2*log(Ht_Nor_Cal/H0_Nor_Cal))
Oregon_N <- -t/(2*log(Ht_Central_Oregon/H0_Central_Oregon))
Alaska_N <- -t/(2*log(Ht_Alaska/H0_Alaska))
