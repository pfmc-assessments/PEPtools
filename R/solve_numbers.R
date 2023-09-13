#' Function to solve out numbers of fish in the forecast to equal
#' match management ABC values. If a fleet in your model has removals
#' specified as numbers of fish (1,000s), SS will expect numbers of fish
#' to be used in the forecast file for that fleet is there are pre-specified
#' forecast removals.  This creates challenges when attempting to determine the
#' right number of fish that will meet the harvest specified ABC values. This 
#' function does a bisection approach to iteratively determine the number of fish
#' that will match the fleet specified removals in terms of biomass.
#'
#' @param mod_dir the directory of your model - all runs will be conducted in this folder
#' make sure you are alright if the SS files are changed.
#' @param fore_yrs a vector of forecast years where removals are pre-specified
#' @param fleet_abc a vector of fleet specific abc values for the fleet that is currently
#' in terms of numbers of fish. 
#' @param fleet fleet number within SS3 of the fleet that has removals in terms of numbers of
#' fish.
#' @param thresh Percent difference that controls if the model should be run again. The 
#' default value of 0.01 results in the code identifying a solution as correct that is within
#' 1 percent above or below the input fleet_abc value. The model will only be rerun once. This 
#' threshold may need to be increased if the catch values are very small.
#' @param exe The executable name for Stock Synthesis to be passed to the r4ss::run function.
#' @author Chantel Wetzel
#' @export
#' 
#' @examples
#' \dontrun{
#' solve_numbers(mod_dir = "C:/Models/my_model", 
#'			  fore_yrs = 2021:2022, 
#'			  fleet_abc = c(5.5, 5), 
#'			  fleet = 4)
#'}
#'
solve_numbers <- function(mod_dir, fore_yrs, fleet_abc, fleet = NULL, exe = "ss", thresh = 0.01){
	# Set the wd to run the model
	setwd(mod_dir)

	if(!file.exists("ss.par")){		
		stop("There is no par file in mod_dir. Please run the model.")
	}

	if(length(fore_yrs) != length(fleet_abc)) {
		stop("The length of fore_yrs and fleet_abc need to be the same.")
	}

	yrs = fore_yrs
	abc = fleet_abc

	if(is.null(fleet)){
		dat = r4ss::SS_readdat("data.ss_new")
		fleet = which(dat$fleetinfo$units == 2)

		if(length(fleet) > 1){		
		stop("There appears to be more than one fleet with catches in numbers. \n
			  Function currently only does one fleet at a time.  \n
			  Specify the fleet input in the function to specify which fleet to do.")
		}
	}

	# Turn max phase to 0 and read from par file
	starter = r4ss::SS_readstarter(file.path(mod_dir, "starter.ss"))
	use_par = starter$init_values_src 
	starter$init_values_src = 1
	max_phase = starter$last_estimation_phase
	starter$last_estimation_phase = 0
	r4ss::SS_writestarter(starter, dir = mod_dir, overwrite = TRUE, verbose = FALSE)
	r4ss::run(exe = exe, extras = "-nohess -maxfun 0", skipfinished = FALSE)

	for(i in 1:length(yrs)){
		
	  fore = r4ss::SS_readforecast(file.path(mod_dir, "forecast.ss"), verbose = FALSE)
	  find = which(fore$ForeCatch$Fleet == fleet & fore$ForeCatch$Year == yrs[i])
		fore$ForeCatch[find, "Catch or F"] = abc[i]
		r4ss::SS_writeforecast(fore, dir = mod_dir, overwrite = TRUE)
    r4ss::run(exe = exe, extras = "-nohess -maxfun 0", skipfinished = FALSE)

		rep = r4ss::SS_output(mod_dir, printstats = FALSE, verbose = FALSE, covar = FALSE)
		bio = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(B):_", fleet)]
		num = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(N):_", fleet)]
		wt <- bio/num
		input_num <- abc[i]/wt
			
		fore = r4ss::SS_readforecast(file.path(mod_dir, "forecast.ss"), verbose = FALSE)
		find = which(fore$ForeCatch$Fleet == fleet & fore$ForeCatch$Year == yrs[i])
			
		# Write out the solved input numbers based on the average weight and the abc
		fore$ForeCatch[find, "Catch or F"] = input_num 
		r4ss::SS_writeforecast(fore, dir = mod_dir, overwrite = TRUE)
	  # Rerun the model
		r4ss::run(exe = exe, extras = "-nohess -maxfun 0", skipfinished = FALSE)
	
		rep = r4ss::SS_output(mod_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
		bio = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(B):_", fleet)]

		print(paste0("!!!!!!!!!!!!!!!!! Biomass = ", bio, " ABC = ", abc[i], "!!!!!!!!!!!!!!!!"))
		if(bio/abc[i] < (1 - thresh) | bio/abc[i] > (1 + thres)){
		  bio = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(B):_", fleet)]
		  num = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(N):_", fleet)]
		  wt <- bio/num
		  input_num <- abc[i]/wt
		  
		  fore = r4ss::SS_readforecast(file.path(mod_dir, "forecast.ss"), verbose = FALSE)
		  find = which(fore$ForeCatch$Fleet == fleet & fore$ForeCatch$Year == yrs[i])
		  
		  # Write out the solved input numbers based on the average weight and the abc
		  fore$ForeCatch[find, "Catch or F"] = input_num 
		  r4ss::SS_writeforecast(fore, dir = mod_dir, overwrite = TRUE)
		  # Rerun the model
		  r4ss::run(exe = exe, extras = "-nohess -maxfun 0", skipfinished = FALSE)
		  rep = r4ss::SS_output(mod_dir, covar = FALSE, verbose = FALSE, printstats = FALSE)
		  bio = rep$timeseries[rep$timeseries$Yr == yrs[i], paste0("dead(B):_", fleet)]
		  
		  print(paste0("!!!!!!!!!!!!!!!!! Biomass = ", bio, " ABC = ", abc[i], "!!!!!!!!!!!!!!!!"))
		  
		}
	}

	# Change the max phase back to the original value
	starter = r4ss::SS_readstarter(file.path(mod_dir, "starter.ss"))
	starter$init_values_src = use_par
	starter$last_estimation_phase = max_phase
	r4ss::SS_writestarter(starter, dir = mod_dir, overwrite = TRUE, verbose = FALSE)

}
