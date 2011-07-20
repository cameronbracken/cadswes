days_in_months <- function(years,start=1,end=12){
  
  require(lubridate)
  days_in_month <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  days_in_month <- rep(days_in_month,length(years))
  days_in_month[(years-years[1])[leap_year(years)]*12 + 2] <- 29
  days_in_month[start:(length(days_in_month)-12+end)]
  
}


nat_intv_to_mom_tot_sims <- function(disag,agg){
  
  # disag is an array [years, months, sites, sims]
  mom_agg <- array(NA,c(dim(disag)[1:2],length(agg),dim(disag)[4]))
  for(i in 1:length(agg)){
    
    for(j in 1:dim(disag)[4]){
      
      if(length(agg[[i]] > 0)){
        
        mom_agg[,,i,j] <- disag[,,agg[[i]][1],j]
  
        if(length(agg[[i]]) > 1)
          for(k in 2:length(agg[[i]]))
            mom_agg[,,i,j] <- mom_agg[,,i,j] + disag[,,agg[[i]][k],j]
      }
    }
  }
  mom_agg
}


nat_intv_to_mom_tot_obs <- function(disag,agg){
  
  # disag is an array [years, months, sites, sims]
  mom_agg <- array(NA,c(dim(disag)[1:2],length(agg)))
  for(i in 1:length(agg)){
      
    if(length(agg[[i]] > 0)){
      
      mom_agg[,,i] <- disag[,,agg[[i]][1]]

      if(length(agg[[i]]) > 1)
        for(k in 2:length(agg[[i]]))
          mom_agg[,,i] <- mom_agg[,,i] + disag[,,agg[[i]][k]]
    }
  }
  mom_agg
}


cfs_to_acft_per_month <- function(x, years = NULL, months = NULL){
  
  if('ts' %in% class(x)){
    
    years <- unique(floor(time(x)))
    return(x*60*60*24/43560*days_in_months(years,start=start(x)[2],end=end(x)[2]))
    
  }else{
    
    years <- unique(floor(years))
    return(x*60*60*24/43560*days_in_months(years,start=months[1],end=months[length(months)]))
    
  }
  
}


mts.applyfun <- function(mts,fun){
  
  names <- attr(unreg,'dimnames')[[2]]
  for(i in 1:length(names))
    mts[,names[i]] <- fun(mts[,names[i]])
  mts
}