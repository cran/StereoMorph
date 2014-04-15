resampleGridImagePoints <- function(pts, nx, rx, ry, print.progress = FALSE){

	# FIND SECOND GRID DIMENSION
	ny <- dim(pts)[1]/nx

	# SET INITIAL PARAMETERS
	p_init <- c(pts[1, 1], pts[1, 2], pts[nx, 1], pts[nx, 2], pts[nx*ny, 1], pts[nx*ny, 2], pts[(nx*ny)-nx+1, 1], pts[(nx*ny)-nx+1, 2], 0, 0, 0, 0)

	# FIND BEST FIT PARAMETERS
	nlm_fit <- nlm(imagePlaneGridTransformError, p=p_init, stepmax=100, iterlim=200, nx=nx, ny=ny, grid=pts)
	if(print.progress) cat('Mean fit error:', round(nlm_fit$minimum, 4), 'px')

	errors <- sqrt(rowSums(pts - imagePlaneGridTransform(nlm_fit$estimate, nx, ny))^2)
	if(print.progress) cat('; Max: ', round(max(errors), 4), ' px\n', sep='')

	pts <- imagePlaneGridTransform(nlm_fit$estimate, nx=rx, ny=ry)
	
	list(pts=pts, error=errors)
}