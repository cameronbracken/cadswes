#########binary converasion
#rm(list=ls())
#paleo = matrix(scan('inputs/fullpool@gjct.txt'), ncol=2, byrow=T)
#   #read in paleo
#obs = matrix(scan('inputs/WY_09152500(37-97).txt'), ncol=2, byrow=T)
#   #read in obs
len = length(obs[, 1])
plen = length(paleo[, 1])
med = median(obs[, 2])  # median for wet/dry determination
pnorm = paleo[, 2] - med  # paleo normalized by median
binary = matrix(ncol = 2, nrow = plen)  #binary matrix
binary[, 1] = paleo[, 1] - (paleo[1, 1] - 1)  #assigns sequential numbers instead of years
for (g in 1:plen) {
    binary[g, 2] = ifelse(pnorm[g] < 0, 0, 1)  # converts normalized flows to binary (1=wet,0=dry) # converts normalized to binary
}
wetbinary = subset(binary, binary[, 2] > 0.5)  # selects only the wet years from binary
wb = length(wetbinary[, 2])  # number of wet years (Paleo)
bands = c(2:75)  # band widths to be evaluated
blen = length(bands)  #number of band widths
vals = matrix(nrow = blen, ncol = 2)  #matrix to store LSCV values
vals[1:blen, 1] = bands
fq = matrix(ncol = 1, nrow = blen)
pb <- txtProgressBar(1,blen,style=3)
for (i in 1:blen) {
    setTxtProgressBar(pb,i)
    hw = bands[i]
    LSCV = 0
    oo = hw + 1
    yy = plen - hw
    wwbin = matrix(ncol = 2, nrow = wb)  #########calc ww transitions for full length here
    wwbin[, 1] = wetbinary[, 1]
    wwbin[1, 2] = 2
    for (wp in 2:wb) {
        wwbin[wp, 2] = wetbinary[wp, 1] - wetbinary[wp - 1, 1]  # difference between year (wp) and (wp-1) to identify if year (wp) is wet wet
        #year (wp) is wet preceded by wet if difference =1
    }
    b3 = matrix(ncol = 1, nrow = plen)
    b3[, 1] = 2
    for (ww in 1:wb) {
        p = wwbin[ww, 1]
        b3[p] = wwbin[ww, 2]
    }
    binaryw = cbind(binary, b3)
    avail = binary[(oo):(yy), ]  # based on band width, upper and lower buffers limit the data available
    awetbinary = subset(avail, avail[, 2] > 0.5)
    pp = length(awetbinary[, 1])
    wavail = matrix(ncol = 1, nrow = pp)  # single column matrix to identify years with wet wet transition
    wavail[1] = 2  # 1st year has no previous year, so no transition
    if (binary[(awetbinary[1, 1] - 1), 2] == 1) {
        wavail[1] = 1
    }
    for (wp in 2:pp) {
        wavail[wp] = awetbinary[wp, 1] - awetbinary[wp - 1, 1]  # difference between year (wp) and (wp-1) to identify if year (wp) is wetwet
        #year (wp) is wet preceded by wet if difference =1
    }
    wwavail = cbind(awetbinary, wavail)
    wetavail = subset(wwavail, wwavail[, 3] < 2)  #years that are ww transition so LSCV will be computed for those years
    b = length(wetavail[, 1])
    jkl = matrix(ncol = 2, nrow = b)
    for (j in 1:(b)) {
        t = wetavail[j, ]
        source("ww local.txt")
        L = (1 - P)^2
        LSCV = LSCV + L  #sum of all LSCV
        jkl[j, 1] = pd
        jkl[j, 2] = LSCV
    }
    val = LSCV/b
    vals[i, 2] = val
}
close(pb)
hw = subset(vals[, 1], vals[, 2] == min(vals[, 2]))
HW = subset(vals, vals[, 2] == min(vals[, 2]))
device = png
ext = "png"
device(paste("plots/Wet-Wet_paleo_prob", ext, sep = "."), width = 500, 
    height = 500)
plot(vals, main = "Wet Wet Probability", xlab = "h", ylab = "LSCV")
points(HW, pch = 24, col = "red", bg = "red")
dev.off()