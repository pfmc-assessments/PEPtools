#' Buffer calculation for U.S. West Coast Groundfish Stocks
#' Applies the time-varying sigma approach based on the 2019
#' approach.
#' @param years the years to generate buffer values for
#' @param sigma the sigma value to use (cat 1 = 0.50, cat 2 = 1.0, cat 3 = 2.0)
#' @param pstar managment uncertainty value (i.e., 0.45, 0.40)
#' @example get_buffer(years = 2011:2032, sigma = 1.0, pstar = 0.4)
#' @authorwritten by Chantel Wetzel
get_buffer <- function(years, sigma, pstar){
	y = 1:(length(years) - 1)
	r = 0.075 # the rate of change in sigma approved by the SSC
	# This is the equation for the rate of change in sigma
	sigma_calc =  c(sigma, sigma * (1+(y-1)*r))
	buffer = exp(qnorm(pstar, 0, sigma_calc))
	# Set the buffer for fixed catch years to 1.0
	buffer[1:2] = 1
	out = cbind(years, round(buffer, 3))
	return(out)
}

