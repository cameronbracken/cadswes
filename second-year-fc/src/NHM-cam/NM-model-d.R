#########binary converasion
#rm(list=ls())
#paleo = matrix(scan('Meko.txt'), ncol=2, byrow=T) #read in paleo
#obs = matrix(scan('lf-annual-cy-af.txt'), ncol=2, byrow=T) #read in
#   obs
len = length(obs[, 1])  # length of observed period
plen = length(paleo[, 1])  # length of paleo
med = median(obs[, 2])  # median for wet/dry determination
pnorm = paleo[, 2] - med
binary = matrix(ncol = 2, nrow = plen)  #binary matrix
binary[, 1] = paleo[, 1] - (paleo[1, 1] - 1)  #assigns sequential numbers instead of years
for (g in 1:plen) {
    binary[g, 2] = ifelse(pnorm[g] < 0, 0, 1)  # converts normalized flows to binary (1=wet,0=dry)  # converts normalized to binary
}
drybinary = subset(binary, binary[, 2] < 0.5)  # selects only the dry years from binary
db = length(drybinary[, 2])  # number of dry years (Paleo)
bands = c(2:70)  # band widths to be evaluated
blen = length(bands)  #number of band widths
vals = matrix(nrow = blen, ncol = 2)  #matrix to store LSCV values
vals[1:blen, 1] = bands
fq = matrix(ncol = 1, nrow = blen)
pb <- txtProgressBar(1,blen,style=3)
for (i in 1:blen) {
    setTxtProgressBar(pb,i)
    hd = bands[i]
    LSCV = 0
    oo = hd + 1
    yy = plen - hd
    ddbin = matrix(ncol = 2, nrow = db)  #########calc ww transitions for full length here
    ddbin[, 1] = drybinary[, 1]
    ddbin[1, 2] = 2
    for (dp in 2:db) {
        ddbin[dp, 2] = drybinary[dp, 1] - drybinary[dp - 1, 1]  # difference between year (dp) and (dp-1) to identify if year (dp) is wet wet
        #year (dp) is wet preceded by wet if difference =1
    }
    b3 = matrix(ncol = 1, nrow = plen)
    b3[, 1] = 2
    for (dd in 1:db) {
        p = ddbin[dd, 1]
        b3[p] = ddbin[dd, 2]
    }
    binary1 = cbind(binary, b3)
    avail = binary[(oo):(yy), ]  # based on band width, upper and lower buffers limit the data available
    adrybinary = subset(avail, avail[, 2] < 0.5)
    pp = length(adrybinary[, 1])
    davail = matrix(ncol = 1, nrow = pp)  # single column matrix to identify years with wet wet transition
    davail[1] = 2  # 1st year has no previous year, so no transition
    if (binary[(adrybinary[1, 1] - 1), 2] == 0) {
        davail[1] = 1
    }
    for (dp in 2:pp) {
        davail[dp] = adrybinary[dp, 1] - adrybinary[dp - 1, 1]  # difference between year (dp) and (dp-1) to identify if year (dp) is dry dry
        #year (dp) is dry preceded by dry if difference =1
    }
    ddavail = cbind(adrybinary, davail)
    dryavail = subset(ddavail, ddavail[, 3] < 2)  #years that are ww transition so LSCV will be computed for those years
    b = length(dryavail[, 1])
    jkl = matrix(ncol = 1, nrow = b)
    for (j in 1:(b)) {
        t = dryavail[j, ]
        source("dd local.txt")
        L = (1 - P)^2
        LSCV = LSCV + L
        jkl[j] = LSCV
    }
    val = LSCV/b
    vals[i, 2] = val
}
close(pb)
hd = subset(vals[, 1], vals[, 2] == min(vals[, 2]))
HD = subset(vals, vals[, 2] == min(vals[, 2]))
device = png
ext = "png"
device(paste("plots/Dry-Dry_paleo_prob", ext, sep = "."), width = 500, 
    height = 500)
plot(vals, main = "Dry-Dry Probability", xlab = "h", ylab = "LSCV")
points(HD, pch = 24, col = "red", bg = "red")
dev.off()