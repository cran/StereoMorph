#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]

Rcpp::NumericMatrix equalizeImageHist(Rcpp::NumericMatrix mat) {

	std::map<int, int> counts;
	std::map<int, int> cdf;
	int i, j;
	int nrow = mat.nrow();
	int ncol = mat.ncol();
	int min_cdf = 1;

	// Multiply by 255 and round to create bins
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			mat(j,i) = std::round(mat(j,i) * 255);
		}
	}

	// COUNT INSTANCES OF EACH PIXEL VALUE (FREQUENCY HISTOGRAM)
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			counts[mat(j,i)]++;
		}
	}

	// CUMULATIVE SUM OF FREQUENCY COUNTS
	cdf[0] = counts[0];
	for (i = 1; i <= 255; i++) cdf[i] = counts[i] + cdf[i-1];

	// GET MINIMUM NON-ZERO CUMULATIVE SUM OF FREQUENCIES
	for (i = 0; i <= 255; i++){
		if(cdf[i] > 0){
			min_cdf = cdf[i];
			break;
		}
	}

	// SCALE CUMULATIVE DENSITY FUNCTION VALUES
	for (i = 0; i <= 255; i++){
		if(cdf[i] == 0) continue;
		cdf[i] = round((((double)cdf[i] - min_cdf) / (nrow*ncol - min_cdf)) * 255);
	}

	// CONVERT TO RESCALED VALUES
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			mat(j,i) = cdf[mat(j,i)];
		}
	}

	// DIVIDE BY 255 TO RESTORE ORIGINAL 0->1 VALUES
	for (i = 0; i < ncol; i++) {
		for (j = 0; j < nrow; j++) {
			mat(j,i) = mat(j,i) * 0.00392156862;
		}
	}

	//Rcpp::Rcout << "Print text" << std::endl;

	return mat;
}
