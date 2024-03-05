#make vectors with our population genotypes

PopA <- c(315, 110, 75)
PopB <- c(355, 80, 65)

#calculate allele frequencies within 2 populations using p = (AA + Aa)/(AA + Aa + aa) and q = 1-p

p1 <- (PopA[1] + 0.5*PopA[2])/sum(PopA)
q1 <- 1 - p1

p2 <- (PopB[1] + 0.5*PopB[2])/sum(PopB)
q2 <- 1 - p2

#calculate expected heterozygosity for both populations using He = 2pq

He1 <- 2 * p1 * q1
He2 <- 2 * p2 * q2

#calculate observed heterozygosity for both populations using Ho = Aa/(AA + Aa + aa)

Ho1 <- PopA[2]/sum(PopA)
Ho2 <- PopB[2]/sum(PopB)

#Calculate HI

HI <- (Ho1 * sum(PopA) + Ho2 * sum(PopB))/(sum(PopA) + sum(PopB))

#Calculate HS

HS <- (He1 * sum(PopA) + He2 * sum(PopB))/(sum(PopA) + sum(PopB))

#calculate p and q of the total gene pool if pop 1 and 2 were counted together

p <- ((PopA[1] + PopB[1]) + 0.5 *(PopA[2] + PopB[2]))/(sum(PopA) + sum(PopB))
q <- 1 - p

#Calculate Ht using the p and q we just calculated

Ht <- 2 * p * q

#Calculate Fst using (Ht - Hs)/Ht

Fst <- (Ht - HS)/Ht

#Calculate FIT using (Ht - HI)/Ht

FIT <- (Ht - HI)/Ht

#Calculate FIS using (Hs - HI)/Hs

FIS <- (HS - HI)/HI
