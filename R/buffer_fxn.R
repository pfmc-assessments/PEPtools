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
#'   the year of the assessment. Input should be vector of years to calulculate
#'   West Coast groundfish buffers for (e.g., years = 2021:2032).
#' @param sigma The initial sigma value to use based on West Coast groundfish
#'   assessment categories (cat 1 = 0.50, cat 2 = 1.0, cat 3 = 2.0). For categories
#'   1 and 2 the sigma will be time-varying.
#' @param pstar The management risk tolerance determined by the PFMC (i.e., 0.45, 0.40).
#' @param m A vector of natural mortality value(s) by sex. If added the function will
#'   calculate the geometric mean to determine if m > 0.15 which requires a higher
#'   rate of change in sigma. Default is NULL.
#' @param verbose A logical that specifies if you want to print messages and
#'   warnings to the console. The default is `TRUE`.
#'
#' @examples
#' get_buffer(years = 2025:2036, sigma = 1.0, pstar = 0.4)
#' get_buffer(years = 2025:2036, sigma = 0.5, pstar = 0.45, m = c(0.16, 0.18))
#' \dontrun{
#'   # add buffer to forecast file of SS3 model
#'   inputs <- r4ss::SS_read(mydir)
#'   inputs$fore$Flimitfraction <- -1 # set to -1 to use matrix of time-varying buffers
#'   inputs$fore$Flimitfraction_m <- get_buffer(years = 2025:2036, sigma = 1.0, pstar = 0.4)
#'   r4ss::SS_write(inputs, dir = mydir, overwrite = TRUE)
#' }
#' @author written by Chantel Wetzel
#' @export
#'
get_buffer <- function(years, sigma, pstar, m = NULL, verbose = TRUE) {
  r <- dplyr::case_when(
    sigma == 2.0 ~ 0,
    sigma < 2.0 ~ 0.075
  )

  if (!is.null(m)) {
    geom_m <- round(exp(mean(log(m))), 3)
    if (geom_m > 0.15 & sigma != 2.0) {
      r <- 0.52 * geom_m
    }
    if (verbose) {
      cli::cli_inform(
        "The geometric mean of natural mortality values is {geom_m}."
      )
    }
  } else {
    if (verbose) {
      cli::cli_inform(
        "No natural mortality values provided and is assumed to be =< 0.15."
      )
    }
  }

  if (verbose) {
    cli::cli_inform(
      "The sigma value of {sigma} adjusts yearly by the rate of {r}."
    )
  }

  y <- 1:(length(years) - 1)
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
    sigma_calc[category_3] <- 2.0 # cap sigma to match the capped buffer for those years
  }

  # combine results into a table for output
  out <- data.frame(
    year = years,
    # buffer is typically rounded to 3 digits
    buffer = round(buffer, 3),
    # adding a column of "#" so the sigma column will be ignored
    # by SS3 when reading the forecast file
    hash = "#",
    # rounding sigma to 4 digits because for many stocks the
    # 0.075 adjustment and the default sigma = 0.5 result in values
    # that are exact to 4 digits.
    sigma = round(sigma_calc, 4)
  )

  return(out)
}
