fcontour <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, left=TRUE, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = terrain.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    xaxs = "i", yaxs = "i", las = 1, axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
	
	mar.orig <- (par.orig <- par(c("mar", "las")))$mar
	par(las = las)
	mar <- mar.orig
	on.exit(par(par.orig))
	if (left){
		mar[4]<- 1	
	}
	else{
		mar[2]<- 1
		mar[4]<- 4.1
	}
	par(mar=mar)
	plot.new()
   plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs)
	if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
		stop("no proper 'z' matrix specified")
	if (!is.double(z)) 
	storage.mode(z) <- "double"
	.Internal(filledcontour(as.double(x), as.double(y), z, as.double(levels), 
		col = col))
    if (missing(plot.axes)) {
        if (axes && left) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
        else if(axes){
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 4) 	
        	}
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}

cbar <- function ( nlevels, zlim, rotate=TRUE, levels = pretty(zlim, nlevels), 
    color.palette = terrain.colors, col = color.palette(length(levels) - 1), 
    key.title, key.axes, xaxs = "i", yaxs = "i", las = 1, axes = TRUE) 
{
	mar.orig <- (par.orig <- par(c("mar", "las")))$mar
	par(las = las)
	mar <- mar.orig
	on.exit(par(par.orig))
	plot.new()
	if(rotate){
		mar[4] <- 2
		mar[2] <- 2
		par(mar = mar)
    	plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    	rect(0, levels[-length(levels)], 1, levels[-1], col = col)
    	if (missing(key.axes)) {
       	 if (axes) 
    	        axis(4)
    	}
    	else key.axes
    }
    else {
    	mar[1] <- mar[3]
		mar[3] <- 2
		par(mar = mar)
    	plot.window(xlim = range(levels), ylim = c(0,1), xaxs = "i", 
    	yaxs = "i")
    	rect( levels[-length(levels)], 0, levels[-1], 1, col = col)
    	if (missing(key.axes)) {
       	 if (axes) 
    	        axis(1)
    	}
    	else key.axes
    }
    box()
    if (!missing(key.title)) 
        key.title
}

sizer <- function(rotate=TRUE){
	
	mar <- par("mar")
	
	if(rotate){		
		w <- (3 + mar[2]) * par("csi") * 2.54
	}
	else{
		w <- (3+mar[3]) * par("csi") * 2.54
	}
	return(w)
}