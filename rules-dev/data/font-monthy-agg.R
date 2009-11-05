x <- read.table('font-daily-data.txt',header=T)

	#spit out monthly release
out <- "Release"
sm <- x$Month[1]
sy <- x$Year[1]
ey <- x$Year[length(x$Year)]
em <- x$Month[length(x$Month)]

val <- numeric()
y <- sy
m <- sm
i <- 1
while(TRUE){

	val[i] <- mean(x[[out]][x$Year == y & x$Month == m])
	print(x[[out]][x$Year == y & x$Month == m])
	m <- m + 1
	if(m > 12){
		m  <- 1
		y <- y + 1
	}
	i <- i + 1
	if(y == ey) if( m > em) break
}

cat(out,val,sep='\n',file=paste('font-monthy-',out,'.txt',sep=''))