#' Generate Meeting Lead and Scribe one year at a time
#'
#'
#' @param year The year for which to generate the names. Defaults to the
#' current year.
#' @param people A vector of potential leads.
#'
#' @export
#'
#' @examples
#' PEPtools::PEPmeeting()
PEPmeeting <- function(year = format(Sys.time(), "%Y"),
                       people = c(
                         "Aaron", "Brian", "Chantel", "Ian", "Jason",
                         "John", "Kiva", "Kristin", "Vlada"
                       )) {
  set.seed(year)

  # calculate of Wednesdays
  # inspired by:
  # https://stackoverflow.com/questions/5046708/calculate-the-number-of-weekdays-between-2-dates-in-r
  start_date <- lubridate::ymd(paste0(year, "-01-01"))
  # end date assumes no meetings on Christmas Eve or later
  end_date <- lubridate::ymd(paste0(year, "-12-23"))
  my_dates <- seq(from = start_date, to = end_date, by = "days")
  Wednesdays <- my_dates[which(lubridate::wday(my_dates, label = TRUE) == "Wed")]

  # get meeting lead
  results <- data.frame(
    date = Wednesdays,
    lead = sample(people,
      size = length(Wednesdays),
      replace = TRUE
    ),
    scribe = NA
  )
  # get scribe which doesn't match lead
  for (irow in 1:length(Wednesdays)) {
    lead <- results$lead[irow]
    results$scribe[irow] <- sample(setdiff(people, lead), size = 1)
  }

  # print results for copy paste into spreadsheet
  print(results, row.names = FALSE)

  # return the results invisibly
  return(invisible(results))
}
