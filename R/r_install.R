#' Install packages for R
#'
#' Install devtools with all of its dependencies, i.e.,
#' \code{dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")}
#' such that many packages that are needed downstream are installed.
#'
#' @details
#' Install packages from the cloud R-project repository using a single
#' call to install \pkg{devtools}. A single call is facilitated by
#' including \code{"Suggests"} in the dependencies argument.
#' The functionality of \code{r_install} is mainly useful for those
#' that update R on a regular basis. For example, developers of R packages
#' will want to install the development version of R at least monthly.
#'
#' @export
#' @author Kelli Faye Johnson
#' @examples
#' \dontrun{
#' # The following will take at least 15 minutes to run on your machine
#' # if you only have the base R packages.
#' r_install()
#' }
#'
r_install <- function() {
  op <- options()
  on.exit(options(op), add = TRUE)
  options(install.packages.compile.from.source = "always")
  utils::install.packages(
    pkgs = c("devtools"),
    repos = "https://cloud.r-project.org",
    dependencies = c("Depends", "Imports", "LinkingTo", "Suggests"),
    verbose = FALSE, quiet = TRUE
  )
  remotes::install_github("pfmc-assessments/nwfscAgeingError")
  remotes::install_github("pfmc-assessments/nwfscMapping")
  remotes::install_github("pfmc-assessments/nwfscSurvey")
}
