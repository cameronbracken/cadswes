  disagskill=function(evalyes=T,disags,obsdata,l,month,names){
    #this function evauates the skill of each of the disaggregated predictions 
    #Variable list:
    #~inputs~
    #evalyes     =The function will not be carried out if evalyes=F (default evalyes=T)
    #disags      =an array of the actual disaggregated values, four columns for each year 
    #             each column has nsims values
    #obsdata     =The observed seasonal average flow at each site
    #l           =length(years)
    #~outputs~
    #output skills to file disagskill_month.txt
    #~local~
    #cisco...    =hold one column of pred ictions for every year for that site
    #c           =median correlation and median rpss for each site (sites down rows)
    
    if(evalyes){
    
      source('functions/RPSS_function.r')
      
      cisco=matrix(0,nrow=nsims,ncol=l)
      grut=matrix(0,nrow=nsims,ncol=l)
      bluff=matrix(0,nrow=nsims,ncol=l)
      leesferry=matrix(0,nrow=nsims,ncol=l)
      
      for(i in 1:l){
        a=(i-1)*d+1
        cisco[,i]=disags[,a]
        grut[,i]=disags[,a+1]
        bluff[,i]=disags[,a+2]
        leesferry[,i]=disags[,a+3]
      }
      
      #this matrix holds the median correlation and median rpss for each site
      #2 columns for the scores and 4 rows for the sites
      c=matrix(0,ncol=2,nrow=4)
      
      #calcualte the MC and RPSS for each site 
      c[1]=cor(obsdata[,1],apply(cisco,2,median))
      print(paste('Median Correlation ',names[1],': ',round(c[1,1],2),sep=''))
      c[1,2]=RPSS(t(obsdata[,1]),cisco)
      
      c[2]=cor(obsdata[,2],apply(grut,2,median))
      print(paste('Median Correlation ',names[2],': ',round(c[2,1],2),sep=''))
      c[2,2]=RPSS(t(obsdata[,2]),grut)
      
      c[3]=cor(obsdata[,3],apply(bluff,2,median))
      print(paste('Median Correlation ',names[3],': ',round(c[3,1],2),sep=''))
      c[3,2]=RPSS(t(obsdata[,3]),bluff)
      
      c[4]=cor(obsdata[,4],apply(leesferry,2,median))
      print(paste('Median Correlation Lees ',names[4],': ',round(c[4,1],2),sep=''))
      c[4,2]=RPSS(as.vector(obsdata[,4]),leesferry)
      
      #output file is 
      write(t(c),file=paste('disagskill_',names[5],'_',month,'.txt',sep=''))
      print(paste('Skills recorded in disagskill_',names[5],'_',month,'.txt',sep=''))
   } 
}