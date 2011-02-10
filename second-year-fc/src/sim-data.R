np <- 1000
nh <- 100
ns <- 3
ord <- 2

states.lag <- permutations(ns,ord,0:(ns-1),repeats.allowed=T)
states.lead <- permutations(ns,1,0:(ns-1),repeats.allowed=T)
nfrom <- nrow(states.lag)
nto <- nrow(states.lead)

states <- sample(1:(ns-1),ord,T)

tpm <- matrix(NA,nrow=nfrom,ncol=nto)

for(i in (states+1):(np+nh)){
    
    
    
}
