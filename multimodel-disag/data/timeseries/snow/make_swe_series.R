#!/usr/bin/env Rscript
sy <- 1949
snotel <- read.table('snotel_nrcs/snotel.txt')
sy.snotel <- as.numeric(rownames(snotel)[1])
ey.snotel <- max(as.numeric(rownames(snotel)))
snotel.ts <- ts(snotel,sy.snotel)

snotel.recent <- read.table('snotel_recent_nrcs/snotel_recent.txt')
sy.snotel.recent <- as.numeric(rownames(snotel.recent)[1])
ey.snotel.recent <- max(as.numeric(rownames(snotel.recent)))
snotel.ts.recent <- ts(snotel.recent,sy.snotel.recent)

snow_course <- read.table('snow_course_nrcs/snow_course.txt')
sy.snow_course <- as.numeric(rownames(snow_course)[1])
ey.snow_course <- max(as.numeric(rownames(snow_course)))
snow_course.ts <- ts(snow_course,sy.snow_course)

swe <- matrix(NA,nrow=length(sy:ey.snotel.recent),ncol=4)

for( i in 1:4 ){
    
    y <- window(snotel.ts[,i],sy.snotel,ey.snow_course)
    x <- window(snow_course.ts[,i],sy.snotel,ey.snow_course)
    fit <- lm(y~x)
    back.snotel <- ts(predict(fit,window(snow_course.ts[,i],max(sy.snow_course,sy),sy.snotel-1)),sy.snow_course)

    swe[,i] <- c(back.snotel,y, 
        window(snotel.ts[,i],ey.snow_course+1,ey.snotel),
        window(snotel.ts.recent[,i],ey.snotel+1,ey.snotel.recent))
}
rownames(swe) <- sy:ey.snotel.recent
colnames(swe) <- paste('swe',tolower(month.abb[1:4]),sep='_')
swe.ts <- ts(swe,sy)
write.table(swe,file='../../swe_crb.txt',quote=F)
