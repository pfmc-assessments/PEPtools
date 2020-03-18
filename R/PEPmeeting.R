#' Generate Meeting Lead and Snack People
#' 
#' 
#' @param date A numeric value giving the date of the meeting.
#' @param notattending A vector of names of those not attending.
#' @param leads A vector of potential leads.
#' @param snacks A vector of people that can bring snacks but
#' are not leads.
#' 
#' @export
#' 
#' @examples PEPmeeting(20180919, notattending = c("Ian"), snacks = c("Nis", "Owen"))
#' 
PEPmeeting <- function(date, notattending = NA,
  leads = c("Brian", "Chantel", "Jason", "John", "Kelli", "Kristin", "Ian", "Melissa", "Vlada"),
  snacks = c("Josh", "Owen", "Christine", "Kathryn")) {

  # Alternatively, calculate the seed based on N days after today
  #N <- 7
  #(seed <- as.numeric(format(as.Date(Sys.time()) + N, "%Y%m%d")))
  if (!is.numeric(date)) stop("date must be numeric")
  set.seed(date)
  
  if(is.null("notattending")) {
    notattending <- NA
  }
  
  if(all(is.na(match(notattending, leads)))) {
    FRAM.Ass1 <- sort(leads)
  } else {
    out <- match(notattending, leads)
    out <- out[!is.na(out)]
    FRAM.Ass1 <- sort(leads[-out])
  }
  
  lead <- FRAM.Ass1[sample(length(FRAM.Ass1), 1)]  # draw for lead

  # list of potential snack names; 
  # remove lead from list of potential snack names and add any just snack folks
  FRAM.Ass2.All <- c(FRAM.Ass1[!(FRAM.Ass1 %in% lead)], snacks)  
  
  if(all(is.na(match(notattending, FRAM.Ass2.All)))) {
    FRAM.Ass2 <- sort(FRAM.Ass2.All)
  } else {
    out <- match(notattending, FRAM.Ass2.All)
    out <- out[!is.na(out)]
    FRAM.Ass2 <- sort(FRAM.Ass2.All[-out])
  }
  
  snacks <- FRAM.Ass2[sample(length(FRAM.Ass2), 1)]  # draw for snacks

# print results
cat(
  "\nThose not attending:  ", notattending,  
  "\nseed:  ", date, 
  "\nlead:  ", lead, 
  "\nsnacks:", snacks,"\n") # report results

}
