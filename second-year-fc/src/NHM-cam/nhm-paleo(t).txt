#rm(list=ls())
l = 105
nsim = 5000

paleo = matrix(scan("Meko.txt"), ncol = 2, byrow = T)  #read in paleo
obs = matrix(scan("lf-annual-cy-af.txt"), ncol = 2, byrow = T)  #read in obs
plen = length(paleo[, 1])

source("NM-model-w.txt")
source("NM-model-d.txt")
cat('Done DW Transitions.\n')
lwb = hw + 1  #lower wet buffer
uwb = plen - hw  #upper wet buffer
ldb = hd + 1  #lower dry buffer
udb = plen - hd  #upper dry buffer
wpaleo = binary[lwb:uwb, ]  #data for which transitions are to be calc.
dpaleo = binary[ldb:udb, ]  #data for which transitions are to be calc.
wlen = length(wpaleo[, 1])
dlen = length(dpaleo[, 1])
wwtp = matrix(ncol = 2, nrow = wlen)  #matrix for wet to wet transition prob.
wwtp[, 1] = wpaleo[, 1]
ddtp = matrix(ncol = 2, nrow = dlen)  #matrix for dry to dry transition prob.
ddtp[, 1] = dpaleo[, 1]
for (i in 1:wlen) {
    t = wpaleo[i, ]
    source("ww local.txt")
    wwtp[i, 2] = P
}
for (i in 1:dlen) {
    t = dpaleo[i, ]
    source("dd local.txt")
    ddtp[i, 2] = P
}
if (hd == hw) {
    trpr = cbind(ddtp, wwtp[, 2])
}  # if band widths are same, probabilities can be merged int 1 matrix
#if not, some data will need to be dropped so they are of the same
#   length
if (hd > hw) {
    delta = hd - hw
    trpr = cbind(ddtp, wwtp[(delta + 1):(wlen - delta), 2])
}
if (hd < hw) {
    delta = hw - hd
    trpr = cbind(ddtp[(delta + 1):(wlen - delta), ], wwtp[, 2])
}
tlen = length(trpr[, 1])
dw = matrix(ncol = 1, nrow = tlen)
for (j in 1:tlen) {
    dw[j] = 1 - trpr[j, 2]
}
wd = matrix(ncol = 1, nrow = tlen)
for (j in 1:tlen) {
    wd[j] = 1 - trpr[j, 3]
}
bin = c(1, 0)
w = length(subset(binary[, 2], binary[, 2] > 0.5))
sprob = c((w/plen), (1 - (w/plen)))
TrPr = cbind(trpr, dw, wd)  # 5 columns (yr,dd,ww,dw,wd)
seqm = matrix(ncol = nsim, nrow = l)
pb <- txtProgressBar(1,nsim,style=3)
for (i in 1:nsim) {
    flush.console()
    setTxtProgressBar(pb,i)
    yr = sample(TrPr[1:(tlen - l), 1], 1, replace = TRUE)  # sampling from 1:(lp-l) prevents selecting a year that would require wrapping
    pos = 1 + yr - TrPr[1, 1]  #matrix position
    block = TrPr[pos:(pos + (l - 1)), ]  #no wrap
    seqm[1, i] = sample(bin, 1, replace = TRUE, prob = sprob)
    for (j in 2:l) {
        if (seqm[j - 1, i] > 0.5) {
            seqm[j, i] = sample(bin, 1, replace = TRUE, prob = c(TrPr[pos + 
                j, 3], TrPr[pos + j, 5]))
        }
        if (seqm[j - 1, i] < 0.5) {
            seqm[j, i] = sample(bin, 1, replace = TRUE, prob = c(TrPr[pos + 
                j, 4], TrPr[pos + j, 2]))
        }
    }
}
close(pb)
###assign flows to binary
#obs= matrix(scan('lf-annual-cy-af.txt'), ncol=2, byrow=TRUE) #read in
#   data
len = length(obs[, 1])
med = median(obs[, 2])  # median for wet/dry determination
wdobs = obs[1:(len - 1), 2] - med
wdobs = cbind(obs[1:(len - 1), 1], wdobs)
wet = subset(wdobs, wdobs[, 2] >= 0)  # wet years and flow normalized my median
dry = subset(wdobs, wdobs[, 2] < 0)  # dry years and flow normalized my median
wl = length(wet[, 1])  #number of wet years in the observed
w = matrix(ncol = 1, nrow = wl)
for (i in 1:wl) {
    w[i, 1] = obs[(wet[i, 1] - (obs[1, 1] - 1)), 2]  # wet flows
}
WETobs = cbind(wet[, 1], w[, 1])  # pairs year and wet flows
dl = length(dry[, 1])  # number of dry years in observed
d = matrix(ncol = 1, nrow = dl)
for (i in 1:dl) {
    d[i, 1] = obs[(dry[i, 1] - (obs[1, 1] - 1)), 2]  # dry flows
}
DRYobs = cbind(dry[, 1], d[, 1])  # pairs year and dry flows
wetwet = matrix(ncol = 1, nrow = wl)
#matrix to record # of years between wet years, if difference =1 this
#   indecates sequential years and thus a wet-wet transition
wetwet[1, ] = 2  # first year in observed can not be wet-wet b/c there is no year to procede it (no wrapping)
for (i in 2:(wl)) {
    wetwet[i, ] = WETobs[i] - WETobs[i - 1]  # difference between year (i) and (i-1) to identify if year (i) is wet wet
    #year (i) is wet followed by wet if difference =1
}
wetwet = cbind(WETobs, wetwet)
WW = subset(wetwet[, 1:2], wetwet[, 3] < 2)  # identifies years that are wet preceded by wet
drydry = matrix(ncol = 1, nrow = dl)
#matrix to record # of years between dry years, if difference=1 this
#   indecates sequential years and thus a dry-dry transition
drydry[1, ] = 2
for (i in 2:(dl)) {
    drydry[i, ] = DRYobs[i, 1] - DRYobs[i - 1, 1]  # difference between year (i) and (i-1) to identify if year (i) is dry dry
    #year (i) is dry followed by dry if difference =1
}
drydry = cbind(DRYobs, drydry)
DD = subset(drydry[, 1:2], drydry[, 3] < 2)  # identifies years that are dry preceded by dry
###w-d and d-w transitions
bin = matrix(ncol = 2, nrow = (len - 1))
bin[, 1] = obs[1:(len - 1), 1]
for (i in 1:(len - 1)) {
    bin[i, 2] = ifelse(wdobs[i, 2] < 0, 0, 1)  #converts wet/dry info to binary (1=wet, 0=dry)
}
binbin = matrix(ncol = 1, nrow = (len - 1))
binbin[1, ] = 0  # no previous year, so can not have a transition classification
for (i in 2:(len - 1)) {
    binbin[i, ] = bin[i, 2] - bin[i - 1, 2]  # difference between binary of year(i) and year (i-1)
}
bin = cbind(bin, binbin)  # new matrix with year, binary and difference
DW = subset(bin[, 1], bin[, 3] > 0)  # finds years that are wet, preceded by dry (difference=1, 0 indicates that transition is ww or dd)
WD = subset(bin[, 1], bin[, 3] < 0)  # finds years that are dry, preceded by wet (difference = -1,)
wetdry = matrix(ncol = 1, nrow = length(WD))
for (i in 1:length(WD)) {
    wetdry[i, 1] = obs[(WD[i] - (obs[1, 1] - 1)), 2]
    # gets corresponding flows for the wet-dry transition years
}
WD = cbind(WD, wetdry)  # combines year and flow into one matrix
drywet = matrix(ncol = 1, nrow = length(DW))
for (i in 1:length(DW)) {
    drywet[i, 1] = obs[(DW[i] - (obs[1, 1] - 1)), 2]
    # gets corresponding flows for the dry-wet transition years
}
DW = cbind(DW, drywet)  # combines year and flow into one matrix
#following 4 calculate matricies with data for 1st year in transition
#   (for WD trans. WD contains dry years pWD contains wet years)
l1 = length(WW[, 1])
pWW = matrix(ncol = 2, nrow = length(WW[, 1]))
pWW[, 1] = (WW[, 1] - 1)
for (i in 1:l1) {
    pWW[i, 2] = obs[(pWW[i, 1] - (obs[1, 1] - 1)), 2]
}
l2 = length(WD[, 1])
pWD = matrix(ncol = 2, nrow = length(WD[, 1]))
pWD[, 1] = (WD[, 1] - 1)
for (i in 1:l2) {
    pWD[i, 2] = obs[(pWD[i, 1] - (obs[1, 1] - 1)), 2]
}
l3 = length(DD[, 1])
pDD = matrix(ncol = 2, nrow = length(DD[, 1]))
pDD[, 1] = (DD[, 1] - 1)
for (i in 1:l3) {
    pDD[i, 2] = obs[(pDD[i, 1] - (obs[1, 1] - 1)), 2]
}
l4 = length(DW[, 1])
pDW = matrix(ncol = 2, nrow = length(DW[, 1]))
pDW[, 1] = (DW[, 1] - 1)
for (i in 1:l4) {
    pDW[i, 2] = obs[(pDW[i, 1] - (obs[1, 1] - 1)), 2]
}
sequence = matrix(ncol = nsim, nrow = l)
pb <- txtProgressBar(1,nsim,style=3)
for (q in 1:nsim) {
    setTxtProgressBar(pb,q)
    if (seqm[1, q] > 0.5) 
        {
            sequence[1, q] = sample(WETobs[, 1], 1, replace = TRUE)
        }  #if first year in sequence is wet, randomly pick a wet year
    if (seqm[1, q] < 0.5) 
        {
            sequence[1, q] = sample(DRYobs[, 1], 1, replace = TRUE)
        }  #if first year in sequence is dry, randomly pick a dry year
    for (n in 2:l) {
        if (seqm[n - 1, q] > 0.5) {
            # if previous year is wet
            if (seqm[n, q] > 0.5) {
                source("KNNWW.txt")
            }
            # current year is wet
            if (seqm[n, q] < 0.5) {
                source("KNNWD.txt")
            }
            # current year is dry
        }
        if (seqm[n - 1, q] < 0.5) {
            # if previous year is dry
            if (seqm[n, q] > 0.5) {
                source("KNNDW.txt")
            }
            # current year is wet
            if (seqm[n, q] < 0.5) {
                source("KNNDD.txt")
            }
            # current year is dry
        }
        sequence[n, q] = YR
    }
}
close(pb)
write(t(sequence), file = "NM_paleo(orig).txt", ncol = nsim)
flows = matrix(ncol = nsim, nrow = l)
for (p in 1:nsim) {
    for (r in 1:l) {
        flows[r, p] = obs[(sequence[r, p] - (obs[1, 1] - 1)), 2]
    }
}
write(flows, file = "NM_paleo_flow(orig).txt", ncol = l)