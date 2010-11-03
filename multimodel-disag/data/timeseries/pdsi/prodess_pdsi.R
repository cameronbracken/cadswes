 
files <- list.files(,'*.dat')
ave <- c(1,9)

for(i in 1:length(files)){
    
    years <- scan(files[i],n=2,quiet=T)
    if(i == 1)
        pdsi.mat.nov <- pdsi.mat.jan <- pdsi.mat.feb <- pdsi.mat.apr <- 
            matrix(NA,nrow=diff(years),ncol=length(files))
        
    data <- as.matrix(read.table(files[i],skip=1,nrows=diff(years)+1)[,-1])
        # even thought the nov and jan predictors should be lagged by 1,
        # put them in the same row, so the correlations are easier 
    for(j in 1:(diff(years))){
        pdsi.mat.nov[j,i] <- data[j,9]
        pdsi.mat.jan[j,i] <- mean(data[j,9:12])
        pdsi.mat.feb[j,i] <- mean(c(data[j,9:12],data[j+1,1]))
        pdsi.mat.apr[j,i] <- mean(data[j+1,1:3])
    }
    
}

pdsi <- cbind(apply(pdsi.mat.nov,1,mean), apply(pdsi.mat.jan,1,mean), 
    apply(pdsi.mat.feb,1,mean), apply(pdsi.mat.apr,1,mean))
    
colnames(pdsi) <- c("pdi_nov","pdi_jan","pdi_feb","pdi_apr")
rownames(pdsi) <- 1:nrow(pdsi)+1948
sink('../../pdsi_crb.txt')
print(pdsi)
sink()