dltMatchCurvePoints <- function(lm.list, cal.coeff, ref.view='max', window.size=30, min.tangency.angle = 0.1){

	# CHECK IF LIST INPUT IS MORE THAN ONE LANDMARK/CURVE
	if(!is.null(names(lm.list))){

		# EMPTY LISTS FOR STORING OUTPUT VALUES
		epipolar_dist <- list()
		curve_pt_dist <- list()

		for(landmark_name in names(lm.list)){

			# CALL MATCH CURVE POINTS FOR EACH LANDMARK/CURVE SET
			dlt_match_curve_points <- dltMatchCurvePoints(lm.list = lm.list[[landmark_name]], cal.coeff = cal.coeff, ref.view=ref.view, window.size=window.size, min.tangency.angle=min.tangency.angle)

			# SAVE OUTPUT VALUES TO LISTS
			lm.list[[landmark_name]] <- dlt_match_curve_points$match.lm.list
			epipolar_dist[[landmark_name]] <- dlt_match_curve_points$epipolar.dist
			curve_pt_dist[[landmark_name]] <- dlt_match_curve_points$curve.pt.dist
		}
		
		r <- list(match.lm.list=lm.list, epipolar.dist=epipolar_dist, curve.pt.dist=curve_pt_dist)
		class(r) <- 'dltMatchCurvePoints'
		return(r)
	}

	# CHECK THAT THERE ARE AT LEAST TWO CAMERA VIEWS
	if(length(lm.list) < 2){r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)}		

	# CHECK THAT THERE ARE MORE THAN TWO POINTS IN EVERY VIEW
	pt_count <- rep(NA, length(lm.list))
	for(i in 1:length(lm.list)){
		if(is.matrix(lm.list[[i]])){
			pt_count[i] <- nrow(lm.list[[i]])
			if(nrow(lm.list[[i]]) < 3){r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)}
		}else{
			r <- list(match.lm.list=lm.list);class(r) <- 'dltMatchCurvePoints';return(r)
		}
	}

	# FIND MAX OR MIN FOR REFERENCE VIEW IF SPECIFIED
	if(ref.view == 'max') ref.view <- which.max(pt_count)
	if(ref.view == 'min') ref.view <- which.min(pt_count)
	
	# SWITCH COLUMNS IF FIRST VIEW IS NOT REFERENCE VIEW
	if(ref.view == 2){
		lm.list <- list(lm.list[[2]], lm.list[[1]])
		cal.coeff <- cal.coeff[, ncol(cal.coeff):1]
	}

	# HOMLOGOUS CURVE POINT LISTS
	homol_curve_pts_list <- list()
	homol_curve_pts_list[[1]] <- matrix(lm.list[[1]], nrow=nrow(lm.list[[1]]), ncol=ncol(lm.list[[1]]), dimnames=list(rownames(lm.list[[1]], colnames(lm.list[[1]]))))
	homol_curve_pts_list[[2]] <- matrix(NA, nrow=nrow(lm.list[[1]]), ncol=ncol(lm.list[[1]]), dimnames=list(rownames(lm.list[[1]], colnames(lm.list[[1]]))))

	# ASSUME START AND END POINTS ARE HOMOLOGOUS
	homol_curve_pts_list[[2]][1, ] <- lm.list[[2]][1, ]
	homol_curve_pts_list[[2]][nrow(lm.list[[1]]), ] <- lm.list[[2]][nrow(lm.list[[2]]), ]

	# SET WINDOW PARAMETERS
	w_min <- 2
	w_max <- window.size
	
	# INITIAL SLOPES
	self_epipolar_slope <- 0
	curve_slope <- 0

	# EMPTY VECTOR FOR DISTANCE FROM EPIPOLAR POINT TO NEAREST POINT ON CURVE
	curve_pt_dist <- c(0, rep(NA, length=nrow(lm.list[[1]])-2), 0)

	for(i in 2:(nrow(lm.list[[1]])-1)){

		if(!is.null(min.tangency.angle)){
			# FIND SELF EPIPOLAR LINE
			self_epipolar_slope <- abs(dltEpipolarLine(p=lm.list[[1]][i, ], cal.coeff1=cal.coeff, self=TRUE)$m)
	
			# GET INDICES FOR DETERMINING SLOPE
			s_min <- max(1, i - 1)
			s_max <- min(i + 1, nrow(lm.list[[1]]))
	
			# FIND CURVE SLOPE
			if(abs(lm.list[[1]][s_min, 1] - lm.list[[1]][s_max, 1]) > 0){
				curve_slope <- abs((lm.list[[1]][s_min, 2] - lm.list[[1]][s_max, 2]) / (lm.list[[1]][s_min, 1] - lm.list[[1]][s_max, 1]))
			}
			
			# FIND ANGLE BETWEEN SLOPES IN RADIANS
			avectors <- avectors(c(1, curve_slope), c(1, self_epipolar_slope))
			
			if(avectors(c(1, curve_slope), c(1, self_epipolar_slope)) < min.tangency.angle){
				w_max <- min(w_max + 1, nrow(lm.list[[2]]) - 1)
				next
			}
		}

		# FIND NEAREST POINT TO CURVE2 ON EPIPOLAR LINE OF REFERENCE POINT
		pt_match <- dltNearestPointOnEpipolar(p1=lm.list[[1]][i, ], p2=lm.list[[2]][w_min:w_max, ], cal.coeff)
		
		# SAVE DISTANCE FROM POINT ON EPIPOLAR LINE TO NEAREST CURVE POINT
		curve_pt_dist[i] <- pt_match$p2.dist

		# SAVE HOMOLOGOUS POINT ON EPIPOLAR LINE
		homol_curve_pts_list[[2]][i, ] <- pt_match$matching.pt
		
		# IF CURVE POINT IS TAKEN INSTEAD OF EPIPOLAR POINT, SHIFT WINDOW ONE
		if(pt_match$p2.dist == 0){w_add <- 1}else{w_add <- 0}

		# SHIFT WINDOW TO INCLUDE MATCH POINT AND SUBSEQUENT INTERVAL
		w_min <- w_min + pt_match$min.idx + w_add - 1
		w_max <- min(w_min + window.size + w_add, nrow(lm.list[[2]]) - 1)
	}

	# GET EPIPOLAR DISTANCES BETWEEN REFERENCE AND TEST POINTS
	epipolar_dist <- dltEpipolarDistance(p1=homol_curve_pts_list[[1]], p2=homol_curve_pts_list[[2]], cal.coeff, reciprocal=TRUE)

	# SWITCH BACK POINT LISTS IF FIRST VIEW IS NOT REFERENCE VIEW
	if(ref.view == 2) homol_curve_pts_list <- list(homol_curve_pts_list[[2]], homol_curve_pts_list[[1]])

	r <- list(match.lm.list=homol_curve_pts_list, epipolar.dist=epipolar_dist, curve.pt.dist=curve_pt_dist)
	class(r) <- 'dltMatchCurvePoints'

	return(r)
}

summary.dltMatchCurvePoints <- function(object, ...){
	r <- ''
	r <- c(r, '\ndltMatchCurvePoints Summary\n')

	if(!is.null(names(object$match.lm.list))){
		curve_found <- F
		
		for(curve_name in names(object$match.lm.list)){
			if(is.null(object$epipolar.dist[[curve_name]])) next
			r <- c(r, '\tCurve name: ', curve_name, '\n')
			r <- c(r, '\t\tReference point count: ', length(object$epipolar.dist[[curve_name]]), '\n')
			r <- c(r, '\t\tStart/end epipolar distances: ', round(object$epipolar.dist[[curve_name]][1], 2), ' px, ', round(object$epipolar.dist[[curve_name]][length(object$epipolar.dist[[curve_name]])], 2), ' px\n')
			r <- c(r, '\t\tMean epipolar-curve point distance: ', round(mean(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), ' px +/- ', round(sd(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), '\n')
			r <- c(r, '\t\tMax epipolar-curve point distance: ', round(max(object$curve.pt.dist[[curve_name]], na.rm = TRUE), 2), ' px\n')
			curve_found <- T
		}
		if(!curve_found) r <- c(r, '\tNo curves found.\n')
	}else{
		if(length(object$epipolar.dist) == 0){
			r <- c(r, '\tNo curves found.\n')
			class(r) <- "summary.dltMatchCurvePoints"
			return(r)
		}
	
		r <- c(r, '\t\tReference point count: ', length(object$epipolar.dist), '\n')
		r <- c(r, '\t\tStart/end epipolar distances: ', round(object$epipolar.dist[1], 2), ' px, ', round(object$epipolar.dist[length(object$epipolar.dist)], 2), ' px\n')
		r <- c(r, '\t\tMean epipolar-curve point distance: ', round(mean(object$curve.pt.dist, na.rm = TRUE), 2), ' px +/- ', round(sd(object$curve.pt.dist, na.rm = TRUE), 2), '\n')
		r <- c(r, '\t\tMax epipolar-curve point distance: ', round(max(object$curve.pt.dist, na.rm = TRUE), 2), ' px\n')
	}

	class(r) <- "summary.dltMatchCurvePoints"
	r
}

print.summary.dltMatchCurvePoints <- function(x, ...) cat(x, sep='')