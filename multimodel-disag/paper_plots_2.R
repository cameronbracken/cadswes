require(tikzDevice)
require(lubridate)
source('lib/RPSS_function.R')
source('lib/libmm.R')

# June flows at different lead times

esp <- read.csv('data/gunn-gj-fc-kaf.csv')
coord <- subset(esp, source == 'COORD')
coord.feb1 <- subset(coord,month(as.Date(as.character(cdate))) == 2)
coord.apr1 <- subset(coord,month(as.Date(as.character(cdate))) == 4)
coord.years <- year(as.Date(coord.feb1$cdate))

site <- c(6,6,6)
mon <- c(3,3,3)

load('diagnostics/output_data/apr_drop-one.Rdata')
mat1 <- t(d$disag[,mon[1],site[1],])*1000
hist1 <- d$hist[,mon[1],site[1]]*1000
rpss1 <- d$stats$rpss[site[1],mon[1]]
mc1 <- d$stats$mc[site[1],mon[1]]
colnames(mat1) <- 1949:2011

load('diagnostics/output_data/feb_drop-one.Rdata')
mat2 <- t(d$disag[,mon[2],site[2],])*1000
hist2 <- d$hist[,mon[2],site[2]]*1000
rpss2 <- d$stats$rpss[site[2],mon[2]]
mc2 <- d$stats$mc[site[2],mon[2]]
colnames(mat2) <- 1949:2011

load('diagnostics/output_data/nov_drop-one.Rdata')
mat3 <- t(d$disag[,mon[3],site[3],])*1000
hist3 <- d$hist[,mon[3],site[3]]*1000
rpss3 <- d$stats$rpss[site[3],mon[3]]
mc3 <- d$stats$mc[site[3],mon[3]]
colnames(mat3) <- 1949:2011



tikz('paper/figures/boxplots-drop-one.tex',width=6.5,height=8.5,pointsize=11)
  layout(cbind(1:3))
  par(mar=c(2.1, 4.1, 3.1, 2.1))
  myboxplot.matrix(mat1,cex=.3,outline=F,ylab='FLow [KAF]')
  lines(hist1,t='b',col='red',cex=.6)
  mtext(paste('(a) RPSS =',round(rpss1,2),'MC =',mc1),3,1)

  myboxplot.matrix(mat2,cex=.3,outline=F,ylab='FLow [KAF]')
  lines(hist2,t='b',col='red',cex=.6)
  mtext(paste('(b) RPSS =',round(rpss2,2),'MC =',mc2),3,1)

  myboxplot.matrix(mat3,cex=.3,outline=F,ylab='FLow [KAF]')
  lines(hist3,t='b',col='red',cex=.6)
  mtext(paste('(c) RPSS =',round(rpss3,2),'MC =',mc3),3,1)
dev.off()


# Seasonal 

site <- c(6,6,6)
mon <- c(3,3,3)

load('diagnostics/output_data/apr_drop-one.Rdata')
mat1 <- t(apply(d$disag[,,site[1],],c(1,3),sum))*1000
hist1 <- hist.seasonal <- apply(d$hist[,,site[1]],1,sum)*1000
rpss1 <- median(RPSS(hist1,mat1))
mc1 <- cor(apply(mat1,2,median)[1:length(hist1)],hist1)
colnames(mat1) <- 1949:2011

load('diagnostics/output_data/feb_drop-one.Rdata')
mat2 <- t(apply(d$disag[,,site[2],],c(1,3),sum))*1000
hist2 <- apply(d$hist[,,site[2]],1,sum)*1000
rpss2 <- median(RPSS(hist2,mat2))
mc2 <- cor(apply(mat2,2,median)[1:length(hist2)],hist2)
colnames(mat2) <- 1949:2011

load('diagnostics/output_data/nov_drop-one.Rdata')
mat3 <- t(apply(d$disag[,,site[3],],c(1,3),sum))*1000
hist3 <- apply(d$hist[,,site[3]],1,sum)*1000
rpss3 <- median(RPSS(hist3,mat3))
mc3 <- cor(apply(mat3,2,median)[1:length(hist3)],hist3)
colnames(mat3) <- 1949:2011


tikz('paper/figures/boxplots-drop-one-seasonal.tex',width=6.5,height=8.5,pointsize=11)
  layout(cbind(1:3))
  ylim <- c(0,4000)
  par(mar=c(2.1, 4.1, 3.1, 2.1))
  myboxplot.matrix(mat1,cex=.3,outline=F,ylim = ylim,ylab='FLow [KAF]')
  lines(hist1,t='b',col='red',cex=.6)
  mtext(paste('(a) RPSS =',sprintf('%4.2f',rpss1),'MC =',sprintf('%4.2f',mc1)),3,1)

  myboxplot.matrix(mat2,cex=.3,outline=F, ylim = ylim,ylab='FLow [KAF]')
  lines(hist2,t='b',col='red',cex=.6)
  mtext(paste('(b) RPSS =',sprintf('%4.2f',rpss2),'MC =',sprintf('%4.2f',mc2)),3,1)

  myboxplot.matrix(mat3,cex=.3,outline=F, ylim = ylim,ylab='FLow [KAF]')
  lines(hist3,t='b',col='red',cex=.6)
  mtext(paste('(c) RPSS =',sprintf('%4.2f',rpss3),'MC =',sprintf('%4.2f',mc3)),3,1)
dev.off()

# retro

site <- c(6,6,6)
mon <- c(3,3,3)

load('diagnostics/output_data/apr_retro.Rdata')
mat1 <- t(apply(d$disag[,,site[1],],c(1,3),sum))*1000
hist1 <- apply(d$hist[,,site[1]],1,sum)*1000
rpss1 <- median(RPSS(hist1,mat1))
mc1 <- cor(apply(mat1,2,median),hist1)
colnames(mat1) <- 1993:2007

load('diagnostics/output_data/feb_retro.Rdata')
mat2 <- t(apply(d$disag[,,site[2],],c(1,3),sum))*1000
hist2 <- apply(d$hist[,,site[2]],1,sum)*1000
rpss2 <- median(RPSS(hist2,mat2))
mc2 <- cor(apply(mat2,2,median),hist2)
colnames(mat2) <- 1993:2007

load('diagnostics/output_data/nov_retro.Rdata')
mat3 <- t(apply(d$disag[,,site[3],],c(1,3),sum))*1000
hist3 <- apply(d$hist[,,site[3]],1,sum)*1000
rpss3 <- median(RPSS(hist3,mat3))
mc3 <- cor(apply(mat3,2,median),hist3)
colnames(mat3) <- 1993:2007


tikz('paper/figures/boxplots-retro.tex',width=6.5,height=8.5,pointsize=11)
  layout(cbind(1:3))
  par(mar=c(2.1, 4.1, 3.1, 2.1))
  ylim <- c(0,3500)
  myboxplot.matrix(mat1,cex=.3,outline=F,ylim=ylim,ylab='FLow [KAF]')
  lines(hist1,t='b',col='red',cex=.6)
  plot.years <- which(rev(coord.years) %in% 1993:2007)
  lines(1:length(plot.years), coord.apr1$e50[plot.years],col='blue')
  lines(1:length(plot.years), coord.apr1$e10[plot.years],col='blue',lty=2)
  lines(1:length(plot.years), coord.apr1$e90[plot.years],col='blue',lty=2)
  mtext(paste('(a) RPSS =',sprintf('%4.2f',rpss1),'MC =',sprintf('%4.2f',mc1)),3,1)

  myboxplot.matrix(mat2,cex=.3,outline=F,ylim=ylim,ylab='FLow [KAF]')
  lines(hist2,t='b',col='red',cex=.6)
  lines(1:length(plot.years), coord.feb1$e50[plot.years],col='blue')
  lines(1:length(plot.years), coord.feb1$e10[plot.years],col='blue',lty=2)
  lines(1:length(plot.years), coord.feb1$e90[plot.years],col='blue',lty=2)
  mtext(paste('(b) RPSS =',sprintf('%4.2f',rpss2),'MC =',sprintf('%4.2f',mc2)),3,1)

  myboxplot.matrix(mat3,cex=.3,outline=F,ylim=ylim,ylab='FLow [KAF]')
  lines(hist3,t='b',col='red',cex=.6)
  mtext(paste('(c) RPSS =',sprintf('%4.2f',rpss3),'MC =',sprintf('%4.2f',mc3)),3,1)
dev.off()
