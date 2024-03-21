#Question 7 part a
#make genotype frequencies and fitness
p = 0.2
q = 0.8
AA = p^2
Aa = 2*p*q
aa = q^2

w11 = 1
w12 = 1
w22 = 0.9

#now we calculate mean population fitness
w = p^2*w11 + 2*p*q*w12 + q^2*w22

#part b
#now we calculate allele and genotype frequencies in the next generation
p_prime <- (p^2*w11 + p*q*w12)/w
q_prime <- (q^2*w22 + p*q*w12)/w

AA1 <- p_prime^2
Aa1 <- q_prime*p_prime*2
aa1 <- q_prime^2

delta_p <- p_prime - p
delta_q <- q_prime - q

#bonus: repeat last steps except A is recessive now 
#make genotype frequencies and fitness
p = 0.2
q = 0.8
AA = p^2
Aa = 2*p*q
aa = q^2

w11 = 1
w12 = 0.9
w22 = 0.9

#now we calculate mean population fitness
w = p^2*w11 + 2*p*q*w12 + q^2*w22

#now we calculate allele and genotype frequencies in the next generation
p_prime <- (p^2*w11 + p*q*w12)/w
q_prime <- (q^2*w22 + p*q*w12)/w

AA1 <- p_prime^2
Aa1 <- q_prime*p_prime*2
aa1 <- q_prime^2

delta_p <- p_prime - p
delta_q <- q_prime - q
