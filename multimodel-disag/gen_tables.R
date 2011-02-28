mon <- c( "apr", "jan", "nov")
type <- "retro"
path <- file.path('diagnostics','output_data/')


for(i in 1:length(mon)){
  
  this.file <- file.path(path,paste(mon[i],'_',type,'.RData',sep=''))
  load(this.file)
  
  rpss <- d$stats$rpss
  mc <- d$stats$mc
  
  if(i ==1){
    
    combo.xtable <- paste.rpss.mc(rpss,mc)
    rownames(combo.xtable) <- 1:20
    
  }else{
    
    combo.xtable <- cbind(combo.xtable,paste.rpss.mc(rpss,mc))
    
  }
  
}

print(xtable(combo.xtable))

paste.rpss.mc <- function(rpss,mc){
  
  x <- rpss
  for(i in 1:ncol(rpss))
    x[,i] <- paste(sprintf('%4.2f',rpss[,i]),' (',sprintf('%4.2f',mc[,i]),')',sep='') 
  x
  
}