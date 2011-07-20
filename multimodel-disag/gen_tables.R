library('xtable')
mon <- c( "apr", "jan", "nov")
type <- "drop-one"
path <- file.path('diagnostics','output_data/')

paste.rpss.mc <- function(rpss,mc){
  
  x <- rpss
  for(i in 1:ncol(rpss)){
    for(j in 1:nrow(rpss)){
      this.rpss <- ifelse(rpss[j,i] < 0, 
          sprintf('%4.2f',rpss[j,i]), 
          sprintf('{\\bf %4.2f}',rpss[j,i]))
      this.mc <- ifelse(mc[j,i] < 0.21, 
          sprintf('%4.2f',mc[j,i]), 
          sprintf('{\\bf %4.2f}',mc[j,i]))
      x[j,i] <- paste(this.rpss,' (',this.mc,')',sep='') 
    }
  }
  x
  
}

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

print(xtable(combo.xtable),sanitize.text.function = function(x) {x})