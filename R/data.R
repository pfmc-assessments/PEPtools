#' Port Location Data
#'
#' Latitude and longitude of West Coast ports.
#'
#' @format ## `pacfin_ports_withlatlong`
#' A data frame with 507 rows and 6 columns:
#' \describe{
#'   \item{name, agencydesc}{U.S. West Coast port name.}
#'   \item{pcid}{3 letter port code used in PacFIN}
#'   \item{agid}{Letter indicating the location of port within a state,
#'   where W = Washington, O = Oregon, and C = California.}
#'   \item{latitude, longitude}{Latitude/Longitude of the port in radians.}
#' }
"pacfin_ports_withlatlong"
