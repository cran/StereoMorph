meanCheckerboardSize <- function(checker.pts, nx, ny, ruler.pts=NULL, ruler.pt.size=NULL, rep=200){

	# EMPTY VECTORS
	square_size_px_all <- rep(NA, 0)
	ruler_size_px_all <- rep(NA, 0)

	# IF ruler.pt.size HAS UNITS, REMOVE AND SAVE UNITS
	ruler_pt_size <- as.numeric(gsub(pattern='([0-9|.])([ ]*)([A-Za-z]*)', replacement='\\1', x=ruler.pt.size))
	unit <- gsub(pattern='([0-9|.])([ ]*)([A-Za-z]*)', replacement='\\3', x=ruler.pt.size)

	for(i in 1:rep){

		# GENERATE RANDOM POINT PAIRS, NO POINTS ARE REPEATED
		p_r <- 1:(nx*ny)
		p1 <- sample(p_r, floor((nx*ny)/2), replace=F)
		p2 <- sample(p_r[-p1], floor((nx*ny)/2), replace=F)
		pairs <- matrix(NA, nrow=floor((nx*ny)/2), ncol=2)
		pairs <- cbind(p1, p2)
	
		# FIND RELATIVE DISTANCE BETWEEN EACH PAIR WITH ADJOINING PAIR DISTANCE BEING ONE
		distance_grid_units <- distanceGridUnits(pairs, nx)
	
		# SCALE ACTUAL DISTANCES IN PIXELS BY RELATIVE UNIT DISTANCES TO GET UNIFORM PIXEL SQUARE SIZE
		square_size_px <- sqrt(rowSums((checker.pts[pairs[, 1], ] - checker.pts[pairs[, 2], ])^2)) / distance_grid_units
	
		# ADD TO VECTORS
		square_size_px_all <- c(square_size_px_all, square_size_px)
		
		# SKIP RULER POINTS IF NULL
		if(is.null(ruler.pts)) next

		# GET DISTANCE BETWEEN RULER POINTS IN REAL-WORLD UNITS
		ruler_size_px <- sqrt(rowSums((ruler.pts[1:(nrow(ruler.pts)-1), ] - ruler.pts[2:nrow(ruler.pts), ])^2))
	
		# ADD TO VECTORS
		ruler_size_px_all <- c(ruler_size_px_all, ruler_size_px)
	}

	# GET MEAN DISTANCES IN PIXELS
	mean_square_size_px <- mean(square_size_px_all)

	# RETURN ONLY MEAN SQUARE SIZE IF RULER POINTS ARE NULL
	if(is.null(ruler.pts)){
		l <- list(
			mean.square.size.px=mean_square_size_px, 
			mean.square.size.rwu=NULL, 
			mean.ruler.size.px=NULL, 
			mean.rwu.per.px=NULL, 
			square.size.px=square_size_px_all, 
			ruler.size.px=NULL,
			unit=NULL
			)
		class(l) <- 'meanCheckerboardSize'
		return(l)
	}

	# GET MEAN DISTANCES IN PIXELS
	mean_ruler_size_px <- mean(ruler_size_px_all)

	# GET MEAN SQUARE SIZE IN REAL-WORLD UNITS
	mean_square_size_rwu <- mean_square_size_px * (ruler_pt_size / mean_ruler_size_px)

	# GET MEAN REAL-WORLD UNITS PER PIXEL
	mean_rwu_per_px <- mean_square_size_rwu / mean_square_size_px

	l <- list(
		mean.square.size.px=mean_square_size_px, 
		mean.square.size.rwu=mean_square_size_rwu, 
		mean.ruler.size.px=mean_ruler_size_px, 
		mean.rwu.per.px=mean_rwu_per_px, 
		square.size.px=square_size_px_all, 
		ruler.size.px=ruler_size_px_all,
		unit=unit
		)
	class(l) <- 'meanCheckerboardSize'
	l
}

summary.meanCheckerboardSize <- function(object, ...){
	r <- ''

	r <- c(r, '\nmeanCheckerboardSize Summary\n')
	r <- c(r, paste0('\tMean square size in pixels: ', format(object$mean.square.size.px), ' px +/- ', format(sd(object$square.size.px))))
	r <- c(r, '\n')
	r <- c(r, paste0('\t\tMin: ', format(min(object$square.size.px)), ' px; Max: ', format(max(object$square.size.px)), ' px'))
	r <- c(r, '\n')
	
	if(is.null(object$ruler.size.px)){
		class(r) <- "summary.meanCheckerboardSize"
		return(r)
	}

	r <- c(r, paste0('\tMean ruler size in pixels: ', format(object$mean.ruler.size.px), ' px +/- ', format(sd(object$ruler.size.px))))
	r <- c(r, '\n')
	r <- c(r, paste0('\t\tMin: ', format(min(object$ruler.size.px)), ' px; Max: ', format(max(object$ruler.size.px)), ' px'))
	r <- c(r, '\n')
	r <- c(r, paste0('\tMean square size in real-world units: ', format(object$mean.square.size.rwu), ' ', object$unit))
	r <- c(r, '\n')
	r <- c(r, paste0('\tMean real-world units per pixel: ', format(object$mean.rwu.per.px), ' ', object$unit))
	r <- c(r, '\n')

	class(r) <- "summary.meanCheckerboardSize"
	r
}

print.summary.meanCheckerboardSize <- function(x, ...) cat(x, sep='')