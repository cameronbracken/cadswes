####KNN DD
k = length(pDD[, 1])
year = sequence[n - 1, q]  #Flow corresponding to current year
Flow = obs[year - (obs[1, 1] - 1), 2]
D = abs(pDD[, 2] - Flow)  #calculates difference between selected flow and other observed flows
Delta = cbind(pDD[, 1], D)  #combines difference and corresponding year into one matrix
Delta_sort = cbind(Delta[, 1][order(Delta[, 2])], sort(Delta[, 
    2]))
kmatrix = Delta_sort[1:k, 1:2]
weight = matrix(nrow = k, ncol = 1)  # defines matrix for weights
rnk = rank(kmatrix[, 2])  #ranks distances for purpose of generating weights
for (j in 1:k) {
    weight[j, 1] = 1/(rnk[j])  #fills weighting matrix
}
z = sum(weight)  # sums weights
weights = weight/z  #divides weights by sum of weights so cumulative probability = 1
NN = sample(kmatrix[, 1], 1, replace = TRUE, prob = weights)
YR = NN + 1