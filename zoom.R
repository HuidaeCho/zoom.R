zoom <- function(plotfunc, extent, final=FALSE) {
	# (C) 2007 GPL by Huidae Cho <https://idea.isnew.info/>
	# Simple R function for interactive zooming/panning
	#
	# Example:
	# data <- runif(100)*10
	# extent <- list(x=c(1, 100), y=c(0, 10))
	# plotfunc <- function(lim) {
	# 	plot(data, xlim=lim$x, ylim=lim$y)
	# 	abline(mean(data), 0, col="red")
	# }
	# zoom(plotfunc, extent)

	if(!final) {
		cat(printf("Zoom in:     Click two corners\n"))
		cat(printf("Zoom out:    Click above plot\n"))
		cat(printf("Prev extent: Click left of plot\n"))
		cat(printf("Next extent: Click right of plot\n"))
		cat(printf("Full extent: Click below plot\n"))
		cat(printf("Pan:         Double click\n"))
		cat(printf("Quit:        Right button\n"))
	}

	lim <- extent
	lim.stack <- c(lim$x, lim$y)
	lim.depth <- 1
	lim.cur <- 1

	repeat{
		plotfunc(lim)

		if(final)
			break

		l <- locator(1)
		if(is.null(l))
			break
		ext <- par()$usr
		if(l$x < ext[1] || l$x > ext[2]) {
			cur <- lim.cur
			lim.cur <- if(l$x < ext[1]) max(lim.cur-1, 1)
				else min(lim.cur+1, lim.depth)
			if(lim.cur != cur)
				lim <- list(x=lim.stack[lim.cur, 1:2],
					y=lim.stack[lim.cur, 3:4])
			next
		}
		if(l$y < ext[3])
			lim <- extent
		else
		if(l$y > ext[4]) {
			cx <- (lim$x[1] + lim$x[2]) / 2
			cy <- (lim$y[1] + lim$y[2]) / 2
			w <- lim$x[2] - lim$x[1]
			h <- lim$y[2] - lim$y[1]
			lim <- list(x=c(cx-w, cx+w), y=c(cy-h, cy+h))
		}else{
			l2 <- locator(1)
			if(is.null(l2))
				break
			if(sum(l$x == l2$x) || sum(l$y == l2$y)) {
				w <- lim$x[2] - lim$x[1]
				h <- lim$y[2] - lim$y[1]
				lim <- list(x=c(l2$x-w/2, l2$x+w/2),
					y=c(l2$y-h/2, l2$y+h/2))
			}else
				lim <- list(x=sort(c(l$x, l2$x)),
					y=sort(c(l$y, l2$y)))
		}
		if(lim.cur < lim.depth) {
			lim.stack <- lim.stack[-((lim.cur+1):lim.depth),]
			lim.depth <- lim.cur
		}
		lim.stack <- rbind(lim.stack, c(lim$x, lim$y))
		lim.depth <- lim.depth + 1
		lim.cur <- lim.cur + 1
	}

	lim
}
