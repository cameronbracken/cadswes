#(a) Apr1 June flow at Colorado River Near Cameo, CO, 
#(b) Feb1 June San Rafael River Near Green River, UT and 
#(c) Nov1 July Dolores River Near Cisco, UT

c(2, 17, 7)

load('diagnostics/output_data/apr_drop-one.Rdata')
mat1 <- t(d$disag[,3,2,])
hist1 <- d$hist[,3,2]
rpss1 <- d$stats$rpss[2,3]
mc1 <- d$stats$mc[2,3]
colnames(mat1) <- 1949:2011

load('diagnostics/output_data/feb_drop-one.Rdata')
mat2 <- t(d$disag[,3,17,])
hist2 <- d$hist[,3,17]
rpss2 <- d$stats$rpss[17,3]
mc2 <- d$stats$mc[17,3]
colnames(mat2) <- 1949:2011

load('diagnostics/output_data/nov_drop-one.Rdata')
mat3 <- t(d$disag[,4,7,])
hist3 <- d$hist[,4,7]
rpss3 <- d$stats$rpss[7,4]
mc3 <- d$stats$mc[7,4]
colnames(mat3) <- 1949:2011

tikz('paper/figures/boxplots-drop-one.tex',width=7,height=9,pointsize=11)
  layout(cbind(1:3))
  par(mar=c(2.1, 4.1, 3.1, 2.1))
  boxplot(mat1,cex=.3,outline=F)
  lines(hist1,t='b',col='red',cex=.6)
  mtext(paste('(a) RPSS =',round(rpss1,2),'MC =',mc1),3,1)

  boxplot(mat2,cex=.3,outline=F)
  lines(hist2,t='b',col='red',cex=.6)
  mtext(paste('(b) RPSS =',round(rpss2,2),'MC =',mc2),3,1)

  boxplot(mat3,cex=.3,outline=F)
  lines(hist3,t='b',col='red',cex=.6)
  mtext(paste('(c) RPSS =',round(rpss3,2),'MC =',mc3),3,1)
dev.off()




load('diagnostics/output_data/apr_retro.Rdata')
mat1 <- t(d$disag[,3,14,])
hist1 <- d$hist[,3,14]
rpss1 <- d$stats$rpss[14,3]
mc1 <- d$stats$mc[14,3]
colnames(mat1) <- 1993:2007

load('diagnostics/output_data/feb_retro.Rdata')
mat2 <- t(d$disag[,3,14,])
hist2 <- d$hist[,3,14]
rpss2 <- d$stats$rpss[14,3]
mc2 <- d$stats$mc[14,3]
colnames(mat2) <- 1993:2007

load('diagnostics/output_data/nov_retro.Rdata')
mat3 <- t(d$disag[,3,14,])
hist3 <- d$hist[,3,14]
rpss3 <- d$stats$rpss[14,3]
mc3 <- d$stats$mc[14,3]
colnames(mat3) <- 1993:2007


tikz('paper/figures/boxplots-retro.tex',width=7,height=9,pointsize=11)
  layout(cbind(1:3))
  par(mar=c(2.1, 4.1, 3.1, 2.1))
  boxplot(mat1,cex=.3,outline=F)
  lines(hist1,t='b',col='red',cex=.6)
  mtext(paste('(a) RPSS =',round(rpss1,2),'MC =',mc1),3,1)

  boxplot(mat2,cex=.3,outline=F)
  lines(hist2,t='b',col='red',cex=.6)
  mtext(paste('(b) RPSS =',round(rpss2,2),'MC =',mc2),3,1)

  boxplot(mat3,cex=.3,outline=F)
  lines(hist3,t='b',col='red',cex=.6)
  mtext(paste('(c) RPSS =',round(rpss3,2),'MC =',mc3),3,1)
dev.off()