#' Buffer calculation for West Coast groundish assessments
#'
#' @details
#' Buffer calculation for U.S. West Coast Groundfish Stocks
#' Applies the time-varying sigma approach based on the 2019
#' approach described in:
#' Wetzel, C.R. and O.S. Hamel, 2023. Applying a probability harvest control rule
#' to account for increased uncertainty in setting precautionary harvest limits
#' from past stock assessments. Fisheries Research 262: 106659.
#'
#' @param years Years to generate buffer values. The first year should align with
#' the year of the assessment. Input should be vector of years to calulculate
#' West Coast groundfish buffers for (e.g., years = 2021:2032).
#' @param sigma The initial sigma value to use based on West Coast groundfish
#' assessment categories (cat 1 = 0.50, cat 2 = 1.0, cat 3 = 2.0). For categories
#' 1 and 2 the sigma will be time-varying.
#' @param pstar The management risk tolerance determined by the PFMC (i.e., 0.45, 0.40).
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `TRUE`.
#'
#' @examples
#' get_buffer(years = 2011:2032, sigma = 1.0, pstar = 0.4)
#' @author written by Chantel Wetzel
#' @export
#'
get_buffer <- function(years, sigma, pstar, verbose = TRUE) {
  y <- 1:(length(years) - 1)
  if (sigma != 2.0) {
    # the rate of change in sigma approved by the SSC in 2019
    r <- 0.075
  } else {
    r <- 0
  }
  if (verbose) {
    cli::cli_inform(
      "The sigma value of {sigma} adjusts yearly by the rate of {r}."
    )
  }

  # This is the equation for the rate of change in sigma
  sigma_calc <- c(sigma, sigma * (1 + (y - 1) * r))
  buffer <- exp(stats::qnorm(pstar, 0, sigma_calc))
  max_buffer <- round(exp(stats::qnorm(pstar, 0, 2.0)), 3)
  # Set the buffer for fixed catch years to 1.0
  buffer[1:2] <- 1
  category_3 <- which(buffer < max_buffer)
  if (length(category_3) > 0) {
    if (sigma != 2) {
      if (verbose) {
        cli::cli_inform(
          "The buffer exceeds that category 3 buffer and is capped at {max_buffer} for
			    {years[category_3]}."
        )
      }
    }
    buffer[category_3] <- max_buffer
  }

  out <- data.frame(
    year = years,
    buffer = round(buffer, 3)
  )

  return(out)
}
