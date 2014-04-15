dltCalibrateCameras <- function(coor.2d, nx, grid.size, c.run = FALSE, reduce.grid.dim = 3, print.progress = FALSE){

	# FIND SECOND GRID DIMENSION
	ny <- dim(coor.2d)[1]/nx

	# REDUCE GRID DIMENSIONS
	if(reduce.grid.dim){

		# CHECK THAT reduce.grid.dim IS GREATER THAN TWO
		if(reduce.grid.dim < 3) stop(paste0("reduce.grid.dim (currently ", reduce.grid.dim, ") must be greater than 2."))
		
		# SET REDUCED GRID DIMENSIONS
		rx <- reduce.grid.dim
		ry <- reduce.grid.dim
		
		# SET REDUCED GRID SIZES
		sx <- ((nx-1)*grid.size) / (rx-1)
		sy <- ((ny-1)*grid.size) / (ry-1)

		# EMPTY REDUCED GRID DIMENSION ARRAY
		coor_2d_red <- array(NA, dim=c(rx*ry, 2, dim(coor.2d)[3], dim(coor.2d)[4]))

		if(print.progress) cat('\nReduce grid point density (', dim(coor.2d)[3]*dim(coor.2d)[4], ' total)\n', sep='')

		for(i in 1:dim(coor.2d)[3]){
			for(j in 1:dim(coor.2d)[4]){
				if(print.progress) cat('\t', (i-1)*dim(coor.2d)[4] + j, ') ', sep='')
				coor_2d_red[, , i, j] <- resampleGridImagePoints(pts=coor.2d[, , i, j], nx=nx, rx=rx, ry=ry, print.progress=print.progress)$pts
			}
		}
	}else{
		
		# MAINTAIN FULL GRID DIMENSIONS
		rx <- nx;ry <- ny;sx <- grid.size;sy <- NULL
		
		# COPY POINTS TO REDUCED 2D COOR ARRAY
		coor_2d_red <- coor.2d
	}	

	# SCALE INITIAL TRANSLATE PARAMETER TO REAL-WORLD UNITS (APPROX HALF THE MAX DIMENSION OF GRID)
	t_init <- (max(nx, ny)*grid.size)/4

	# SET INITIAL TRANSFORM PARAMETERS
	p_init <- matrix(rep(c(0.1, 0.1, 0.1, t_init, t_init, t_init), dim(coor.2d)[3]), nrow=6)

	# SET INITIAL RUNTIME
	run_time_t <- 0

	# SET TOTAL ITERATIONS
	total_iter_t <- 0
	
	# SET FIRST COLUMN TO ZEROS
	p_init[, 1] <- rep(0, 6)

	# GET 2D COORDINATE SUBSET MATRIX FOR TRANSFORMATION OPTIMIZATION
	coor_2d_t <- apply(coor_2d_red, c(2, 4), matrix, byrow=FALSE)

	# SET INITIAL TIME POINT
	ptm <- proc.time()

	# SET INITIAL NUMBER OF ITERATIONS
	#t_iter <<- 0

	# RUN NLM FUNCTION TO FIND TRANSFORMATION PARAMETERS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
	if(print.progress) cat('\nFull Transform RMSE Minimization\nNumber of parameters:', length(c(p_init[, 2:ncol(p_init)])), '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\nRunning minimization...')
	nlm_res_t <- nlm(dltTransformationParameterRMSError, p=c(p_init[, 2:ncol(p_init)]), fscale=1, stepmax=max(nx, ny)*grid.size*10, iterlim=300*(length(c(p_init[, 2:ncol(p_init)]))/6), steptol=1e-9, coor.2d=coor_2d_t[, , 1:2], nx=rx, ny=ry, sx=sx, sy=sy, p.fixed=rep(0, 6))

	# SAVE PROCESSING TIME
	run_time <- proc.time() - ptm

	if(print.progress){cat('\nTermination code:', nlm_res_t$code, '\nIterations:', nlm_res_t$iterations, '\nMinimum:', nlm_res_t$minimum, '\nRun-time:', run_time[1], 'sec\nSum of absolute differences between initial and final parameters:', sum(abs(c(p_init[, 2:ncol(p_init)]) - nlm_res_t$estimate)), '\nEstimate: ');cat(round(nlm_res_t$estimate, 6), sep=', ');cat('\n')}

	# ADD RUN-TIME AND ITERATIONS
	run_time_t <- run_time_t + run_time[1]
	
	# SAVE OPTIMIZED PARAMETERS
	p_init <- matrix(c(rep(0, 6), nlm_res_t$estimate), nrow=6)

	# GET 3D COORDINATES BASED ON OPTIMIZED PARAMETERS
	coor_3d_coeff <- transformPlanarCalibrationCoordinates(tpar=c(p_init), nx=nx, ny=ny, sx=grid.size)

	# GET 2D INPUT COORDINATES
	coor_2d_coeff <- apply(coor.2d, c(2, 4), matrix, byrow=FALSE)

	# GET DLT CALIBRATION COEFFICIENTS FROM OPTIMIZED 3D COORDINATE SUBSET (ALL GRID POINTS - SO RMSE CAN DIFFER FROM NLM MINIMUM)
	dlt_coefficients_t <- dltCoefficients(coor.3d=coor_3d_coeff, coor.2d=coor_2d_coeff)

	if(c.run){

		# GET 2D COORDINATE SUBSET MATRIX FOR COEFFICIENT OPTIMIZATION
		coor_2d_c <- apply(coor_2d_red, c(2, 4), matrix, byrow=FALSE)

		# SET INITIAL TIME POINT AND NUMBER OF ITERATIONS
		ptm <- proc.time();#c_iter <<- 0

		# RUN NLM FUNCTION TO FIND COEFFICIENTS THAT MINIMIZE INTERNAL RMS CALIBRATION ERROR
		if(print.progress) cat('\nCoefficient RMSE Minimization\nNumber of parameters:', length(c(dlt_coefficients_t$cal.coeff)), '\nNumber of points:', rx*ry*dim(coor.2d)[3], '\nRunning minimization...\n')
		nlm_res_c <- nlm(dltCoefficientRMSError, p=c(dlt_coefficients_t$cal.coeff), coor.2d=coor_2d_c)

		if(print.progress) cat('\nTermination code:', nlm_res_c$code, '\nIterations:', nlm_res_c$iterations, '\n')

		# SAVE PROCESSING TIME
		run_time_c <- proc.time() - ptm

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=matrix(nlm_res_c$estimate, nrow=11, ncol=dim(coor.2d)[4]), coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_coefficients_c <- dltCoefficients(coor.3d=dlt_reconstruct$coor.3d, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_c$cal.coeff
		coefficient_rmse <- dlt_coefficients_c$rmse
	}else{

		# USE DLT COEFFICIENTS TO RECONSTRUCT ALL 2D COORDINATES
		dlt_reconstruct <- dltReconstruct(cal.coeff=dlt_coefficients_t$cal.coeff, coor.2d=apply(coor.2d, c(2, 4), matrix, byrow=FALSE))

		mean_reconstruct_rmse <- mean(dlt_reconstruct$rmse)
		coor_3d <- dlt_reconstruct$coor.3d
		cal_coeff <- dlt_coefficients_t$cal.coeff
		coefficient_rmse <- dlt_coefficients_t$rmse
		
		nlm_res_c <- list('estimate'=NA, 'minimum'=NA)
		run_time_c <- NA
	}

	l <- list(
		cal.coeff=cal_coeff, 
		coor.3d=coor_3d, 
		mean.reconstruct.rmse=mean_reconstruct_rmse, 
		coefficient.rmse=coefficient_rmse,
		t.param.final=p_init,
		t.iter=nlm_res_t$iterations,
		t.min=nlm_res_t$minimum,
		t.runtime=run_time_t,
		c.param.init=c(dlt_coefficients_t$cal.coeff),
		c.param.final=nlm_res_c$estimate,
		c.min=nlm_res_c$minimum,
		c.iter=nlm_res_c$iterations,
		c.runtime=run_time_c[1]
		)
	class(l) <- 'dltCalibrateCameras'
	l
}

summary.dltCalibrateCameras <- function(object, ...){
	r <- ''

	r <- c(r, '\ndltCalibrateCameras Summary\n')

	r <- c(r, '\tMinimize RMS Error by transformation\n')
	r <- c(r, '\t\tTotal number of parameters estimated: ', length(object$t.param.final)-6, '\n')
	r <- c(r, '\t\tTotal function calls: ', object$t.iter, '\n')
	r <- c(r, '\t\tFinal Minimum Mean RMS Error: ', round(object$t.min, 3), '\n')
	r <- c(r, '\t\tTotal Run-time: ', round(object$t.runtime, 2), ' sec\n')

	if(!is.null(object$c.iter)){
		r <- c(r, '\tMinimize RMSE by coefficients\n')
		r <- c(r, '\t\tNumber of parameters: ', length(object$c.param.init), '\n')
		r <- c(r, '\t\tSum of absolute differences between initial and final parameters: ', format(sum(abs(object$c.param.init - object$c.param.final))), '\n')
		r <- c(r, '\t\tTotal function calls: ', object$c.iter, '\n')
		r <- c(r, '\t\tFinal Mean RMS Error: ', format(object$c.min), '\n')
		r <- c(r, '\t\tRun-time: ', round(object$c.runtime, 2), ' sec\n')
	}

	r <- c(r, '\n')

	r <- c(r, '\tMean Reconstruct RMSE: ', round(object$mean.reconstruct.rmse, 3), '\n')

	if(length(object$coefficient.rmse) == 1){
		r <- c(r, '\tMean Calibration Coefficient RMS Error: ', round(object$coefficient.rmse, 3), '\n')
	}else{
		r <- c(r, '\tCalibration Coefficient RMS Error:\n')
		for(i in 1:length(object$coefficient.rmse)){
			r <- c(r, '\t\t', i, ': ', round(object$coefficient.rmse[i], 3), '\n')
		}
	}

	r <- c(r, '\t3D Coordinate Matrix dimensions: ', dim(object$coor.3d)[1], ' x ', dim(object$coor.3d)[2], '\n')

	class(r) <- "summary.dltCalibrateCameras"
	r
}

print.summary.dltCalibrateCameras <- function(x, ...) cat(x, sep='')