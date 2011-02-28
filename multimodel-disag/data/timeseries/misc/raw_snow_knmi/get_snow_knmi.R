require(stringr)
require(XML)
sn <- readLines('snow-sites.txt')
path <- file.path('timeseries','raw_snow_knmi')

range <- 1:5
tf <- tempfile()
snow <- list()

for(i in 1:(length(sn)/5)){
    
    this.site <- sn[range]
    this.site <- str_replace_all(this.site,"<br>",'')
    this.name <- tolower(str_replace_all(this.site[2],' ',''))
    this.link <- str_replace_all(str_extract(this.site[4],'\".*\"'),'\"','')
    
    this.datafile <- file.path(path,paste(this.name,'.dat',sep=''))
    
    if(!file.exists(this.datafile)){
        download.file(this.link,tf)
        o <- htmlTreeParse(tf,useInternalNodes = TRUE)

        link.loc <- grep('raw\ data',as.character(xpathApply(o,'//a',xmlValue)))[1]
        this.data.link <- paste("http://climexp.knmi.nl/",xpathApply(o,'//a',xmlAttrs)[[link.loc]],sep='')
    
        download.file(this.data.link,this.datafile)
    }
    
    this.data <- read.table(this.datafile,skip=5)
    names(this.data) <- c('Year',month.abb)
    snow[[this.name]] <- ts(this.data[,2:5],start=this.data[1,1])
    
    range <- range + 5
}

snow.mon <- list()

for(i in 1:length(snow)){
    
    if(i == 1){

        for(j in 1:4)
            snow.mon[[j]] <- snow[[i]][,month.abb[j]]
            
            
       
   }else{
       
       for(j in 1:4)
           snow.mon[[j]] <- cbind(snow.mon[[j]],snow[[i]][,month.abb[j]])
       
   }
    
}

for(i in 1:4)
    attributes(snow.mon[[i]])$dimnames[[2]] <- names(snow)
