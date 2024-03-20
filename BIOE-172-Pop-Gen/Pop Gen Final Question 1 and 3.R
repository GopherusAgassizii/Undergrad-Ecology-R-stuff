#Question 1 Part A
#create vectors to store the Heterozygosity of each subpopulation in each region
Talus_river <- c(0, 0.338)
Harper_Creek <- c(0.499, 0.334)
Brewster_River <- c(0.531, 0.460, 0.545)

#Create a vector with HR for each region. Talus river, then haper vreek, then Brewester River
HR <- c(0.192, 0.531, 0.627)

#create variable for Ht
Ht <- 0.715

#calculate mean HR and mean HS.Mean HR is a weighted mean. 
Mean_HR <- (HR[1]*2 + HR[2]*2 + HR[3]*3)/7
Mean_HS <- (sum(Talus_river) + sum(Harper_Creek) + sum(Brewster_River))/7

#Calulate FSR, FST, and FRT
FSR <- (Mean_HR - Mean_HS)/Mean_HR
FST <- (Ht - Mean_HS)/Ht
FRT <- (Ht - Mean_HR)/Ht

#Part B
#now lets calculate Nm using wrights island model
Nm <- ((1/FST) - 1)/4

#Question 3 Part A
#we can estimate heterozygosity with theta = 4Neu

#make variables for Ne and u
Ne_1 <- 10021
Ne_2 <- 362

u <- 1.8 * 10^(-7)

#calculate theta
theta_pop_1 <- 4*Ne_1 * u
theta_pop_2 <- 4*Ne_2 * u

#Calculate heterozygosity with (theta/(theta + 1))

H_1 <- theta_pop_1/(theta_pop_1 + 1)
H_2 <- theta_pop_2/(theta_pop_2 + 1)

#Part B
#Lets calculate decay of heterozygosity using Ht = Hoe^(-t/2N)
#we want t so we take natural log of both sides and isolate t

#create variables for Ht
Ht_1 <- 0.5*H_1
Ht_2 <- 0.5*H_2

#calculate t
t_1 <- -2*Ne_1*log(Ht_1/H_1)
t_2 <- -2*Ne_2*log(Ht_2/H_2)
