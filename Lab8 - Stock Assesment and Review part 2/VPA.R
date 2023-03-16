
#step one
# calculate fdinal population number

require(TropFishR)
data(whiting)

N = rep(0,8) # empty vector of abundance
M = 0.2 # assumed natural mortality. This can be calculated with indirect methods

Fi = rep(0,8) # empty vector of fishing mortality

Fi[8] = 0.2 # Fishing mortality of terminal abundance. This can be assumed or estimated from the catch curve (we estimate Z and then F = Z-M)
#Fi[8] = -log(0.5)

Z = Fi[8]+M
C = whiting$catch[,1]

# step 1 
# calculate final abundance

N[8] = C[8]/((Fi[8]/Z)*(1-exp(-Z))) # this is the final N

# step 2 
# Calculate Fishing mortality previous year

# from the catch equation C = (Fi[7]/(Fi[7]+M)*N*(exp(Fi[7]+M)-1))
# I will write a function that allows me to find the optimal value of Fi

step2fun = function(x, N, C){

	(x/(x+M)*N*(exp(x+M)-1)) - C
}


# I use the uniroot function

Fi[7] = uniroot(step2fun, N = N[8], C = C[7], interval = c(-1,2))$root # uniroot finds the root of the function 
# so the Fi now is 0.2801413

N[7] = N[8]*exp(M+Fi[7])

# now again step 2 for fishing mortality
Fi[6] = uniroot(step2fun, N = N[7], C = C[6], interval = c(-1,1))$root
N[6] = N[7]*exp(M+Fi[6])

Fi[5] = uniroot(step2fun, N = N[6], C = C[5], interval = c(-1,1))$root
N[5] = N[6]*exp(M+Fi[5])

Fi[4] = uniroot(step2fun, N = N[5], C = C[4], interval = c(-1,2))$root
N[4] = N[5]*exp(M+Fi[4])

Fi[3] = uniroot(step2fun, N = N[4], C = C[3], interval = c(-1,2))$root
N[3] = N[4]*exp(M+Fi[3])

# generalizing

for (i in 7:1){

Fi[i] = uniroot(step2fun, N = N[i+1], C = C[i], interval = c(-1,2))$root
N[i] = N[i+1]*exp(M+Fi[i])

}


