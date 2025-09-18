#' Write a default starter.ss file for PFMC assessments
#'
#' Write a `starter.ss` file for assessments presented to the
#' Pacific Fisheries Management Council (PFMC) using a `starter.ss_new` file
#' from the Stock Synthesis GitHub organization with additional comments.
#'
#' @export
#' @return A `starter.ss` file is saved to the current working directory.
#' @author Vladlena Gertseva, Kelli F. Johnson, Brian J. Langseth,
#'   Kristin Marshall, Kiva L. Oken, Ian G. Taylor, John R. Wallace, and
#'   Sophia Wasserman
#'
#' @examples
#'
pfmc_starter <- function() {
  # Create a local function
  insert_comment <- function(x, comments, pattern) {
    commentsPFMC <- paste("# PFMC:", comments)
    line <- grep(pattern, x)
    stopifnot(length(line) == 1)
    append(
      x = lines,
      values = c(
        "# PFMC: #############################################################",
        commentsPFMC,
        "# PFMC: #############################################################"
      ),
      after = grep(pattern, x) - 1
    )
  }

  # Read in the input file
  lines <- readLines(
    paste0(
      "https://raw.githubusercontent.com/nmfs-stock-synthesis/user-examples/",
      "main/model_files/simple/starter.ss"
    )
  )

  # Manipulate the lines
  lines <- insert_comment(
    x = lines,
    comments = ".[a-z].ss ensures data and control file are first.",
    pattern = "data\\.ss"
  )
  lines[grep("^[a-z]+\\.ss", lines)] <- c(".data.ss", ".control.ss")

  lines <- insert_comment(
    x = lines,
    comments = c(
      "Default max phase is well above typical number of phases,",
      "e.g., 10, to ensure all parameters are estimated."
    ),
    pattern = "phase"
  )
  lines[grep("phase", lines)] <- gsub(
    pattern = "^[0-9]+",
    replacement = 100,
    x = lines[grep("phase", lines)]
  )

  lines <- insert_comment(
    x = lines,
    comments = c(
      "File output is verbose by default, console output is not.",
      "echoinput.sso and parmtrace.sso are useful for debugging.",
      "Turn off output to run models faster."
    ),
    pattern = "display"
  )
  lines[grep("data_boot", lines)] <- gsub(
    pattern = "^[0-9]",
    replacement = 2,
    x = lines[grep("data_boot", lines)]
  )
  lines[grep("echoinput\\.sso", lines)] <- gsub(
    pattern = "^[0-9]",
    replacement = 1,
    x = lines[grep("echoinput\\.sso", lines)]
  )
  lines[grep("ParmTrace", lines)] <- gsub(
    pattern = "^[0-9]",
    replacement = 4,
    x = lines[grep("ParmTrace", lines)]
  )
  lines[grep("m[ai][nx].+yr", lines)] <- gsub(
    pattern = "; #_[0-9]{4}$",
    replacement = "",
    x = mapply(
      FUN = gsub,
      replacement = c(-1, -2),
      x = lines[grep("m[ai][nx].+yr", lines)],
      MoreArgs = list(pattern = "^[0-9]{1,4}")
    )
  )

  lines <- insert_comment(
    x = lines,
    comments = c(
      "Priors, in penalized MLE, and soft bounds, symmetric beta priors,",
      "are turned on by default but see",
      "https://github.com/pfmc-assessments/nwfscDiag/issues/6",
      "for a discussion about the use of priors in likelihood profiles."
    ),
    pattern = "prior"
  )

  lines <- insert_comment(
    x = lines,
    comments = c(
      "Summary biomass equals total biomass to encourage users to,",
      "**CHANGE** the line below to be age at 50 percent maturity."
    ),
    pattern = "summary biomass"
  )
  lines[grep("summary biomass", lines)] <- gsub(
    pattern = "^[0-9]",
    replacement = 0,
    x = lines[grep("summary biomass", lines)]
  )

  lines[grep("Depletion basis|F_reporting_units", lines)] <- gsub(
    pattern = "^[0-9]",
    replacement = 1,
    x = lines[grep("Depletion basis|F_reporting_units", lines)]
  )

  # Write the file out
  writeLines(
    text = lines,
    con = "starter.ss"
  )
  return(invisible())
}
