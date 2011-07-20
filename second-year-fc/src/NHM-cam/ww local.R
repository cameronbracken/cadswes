l1 = (t[1]) - hw
h1 = (t[1]) - 1
l2 = (t[1]) + 1
h2 = (t[1]) + hw
wet1 = subset(binaryw[l1:h1, ], binaryw[l1:h1, 2] > 0.5)  #selects the wet years
wet2 = subset(binaryw[l2:h2, ], binaryw[l2:h2, 2] > 0.5)  #selects the wet years
wet = rbind(wet1, wet2)
N = length(wet[, 1])
n=0
pd=0
if (N > 1) {
    WET = subset(wet, wet[, 3] < 2)
    n = length(WET[, 1])
    if (n < 1) 
        {
            P = 0
        }  #if n<1 there exist no events and P=0...skip calc.
    if (n > 0) {
        pn = 0
        MM = matrix(ncol = 1, nrow = n)
        for (e in 1:n) {
            x = ((t[1]) - WET[e, 1])/hw
            K = (3 * hw/(4 * (hw^2) - 1)) * (1 - (x^2))
            MM[e] = K
            pn = K + pn
        }
        pd = 0
        NN = matrix(ncol = 1, nrow = N)
        for (f in 1:N) {
            x = ((t[1]) - wet[f, 1])/hw
            K = (3 * hw/(4 * (hw^2) - 1)) * (1 - (x^2))
            pd = K + pd
            NN[f] = K
        }
        P = ifelse(pd < (-1e-06), pn/pd, 0)
        if (pd > 1e-05) {
            P = pn/pd
        }
    }
}
if (N < 2) {
    P = 0
}
if (n < 1) {
    P = 0
}