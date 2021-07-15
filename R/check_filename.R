#' Check if a file name adheres to the team guidance on file naming
#'
#' Check if a file name adheres to guidance based on advice from
#' years of collaborating with stakeholders and scientists while
#' working with a plethora of softwares.
#' Users and softwares change over time and data often is static;
#' thus, it is imperative that file names are easy to work with and consistent.
#'
#' @section Guidelines:
#' The following guidelines are ordered in terms of importance with the understanding
#' that it will be difficult to adhere to all guidelines at all times.
#' The list is meant to be a rainbows and lollipops ideology; where,
#' it is better to know what everyone wants rather than assume anything.
#' * Underscores as separators
#' * Meta data to include in the name could contain information on
#'   * area
#'   * species
#'   * stakeholder
#' * Dates formatted as yyyymmdd or 20210101 for the first day of January in 2021;
#' consider not including the date if
#'   * older files will not be valid and users would not consider going back in time
#'   * version control will track changes over time
#'   * code would need to change in the future to accommodate a new file name, though
#' this can be overcome with crafty coding
#' * No spaces
#' * Without minimal directory structure, e.g., wcgop_commercial_discard.csv
#' rather than ./WCGOP/Commercial/discard.csv
#' * Camel case only when necessary
#'
#' @param paths A vector of file paths to check.
#'
#' @author Kelli Faye Johnson
#' @export
#' @return A vector of approved file paths and a warning if one or more
#' supplied paths did not pass the checks.
#'
#' @examples
#' check_filename(tempdir())
#'
check_filename <- function(paths) {

  # Check for spaces
  stopifnot(all(!grepl("\\s", paths)))

  return(paths)
}