#we will make a vector for each fish putting theta then T then census size
Bonytail_chub <- c(0.00179, 10, 100)
Humpback_chub <- c(0.00195, 10, 3000)
Razorback_sucker <- c(0.01338, 15, 4000)

#make a variable for mutation rate
u <- 4.0 * 10^-9

#Part a

#estimate the Nef of each species with Ne(f) = theta/(2*T*u)
BC_Nef <- Bonytail_chub[1]/(2*u*Bonytail_chub[2])
HC_Nef <- Humpback_chub[1]/(2*u*Humpback_chub[2])
RS_Nef <- Razorback_sucker[1]/(2*u*Razorback_sucker[2])

#Estimate the Ne of each species by multiplying by 2 since even sex ratios
Ne_BC <- BC_Nef*2
Ne_HC <- HC_Nef*2
Ne_RS <- RS_Nef*2
