l1 = (t[1]) - hd
h1 = (t[1]) - 1
l2 = (t[1]) + 1
h2 = (t[1]) + hd
dry1 = subset(binary1[l1:h1, ], binary1[l1:h1, 2] < 0.5)  #selects the dry years
dry2 = subset(binary1[l2:h2, ], binary1[l2:h2, 2] < 0.5)  #selects the dry years
dry = rbind(dry1, dry2)
N = length(dry[, 1])
if (N > 1) {
    DRY = subset(dry, dry[, 3] < 2)
    n = length(DRY[, 1])
    if (n < 1) 
        {
            P = 0
        }  #if n<1 there exist no events and P=0...skip calc.
    if (n > 0) {
        pn = 0
        MM = matrix(ncol = 1, nrow = n)
        for (e in 1:n) {
            x = ((t[1]) - DRY[e, 1])/hd
            K = (3 * hd/(4 * (hd^2) - 1)) * (1 - (x^2))
            MM[e] = K
            pn = K + pn
        }
        pd = 0
        NN = matrix(ncol = 1, nrow = N)
        for (f in 1:N) {
            x = ((t[1]) - dry[f, 1])/hd
            K = (3 * hd/(4 * (hd^2) - 1)) * (1 - (x^2))
            pd = K + pd
            NN[f] = K
        }
        P = ifelse(pd < (-1e-06), pn/pd, 0)
        if (pd > 1e-05) {
            P = pn/pd
        }
    }
    if (n < 1) {
        P = 0
    }
}
if (N < 2) {
    P = 0
}