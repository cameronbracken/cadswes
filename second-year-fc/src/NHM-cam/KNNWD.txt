####KNN WD
k = length(pWD[, 1])
year = sequence[n - 1, q]  #Flow corresponding to the previous year
Flow = obs[year - (obs[1, 1] - 1), 2]
D = abs(pWD[, 2] - Flow)  #calculates difference between selected flow and other observed flows
Delta = cbind(pWD[, 1], D)  #combines difference and corresponding year into one matrix
Delta_sort = cbind(Delta[, 1][order(Delta[, 2])], sort(Delta[, 
    2]))
kmatrix = Delta_sort[1:k, 1:2]
weight = matrix(nrow = k, ncol = 1)  # defines matrix for weights
rnk = rank(kmatrix[, 2])  #ranks distances for purpose of generating weights
for (j in 1:k) {
    weight[j, 1] = 1/(rnk[j])  #fills weighting matrix
}
z = sum(weight)  # sums weights
weights = weight/z
NN = sample(kmatrix[, 1], 1, replace = TRUE, prob = weights)
YR = NN + 1