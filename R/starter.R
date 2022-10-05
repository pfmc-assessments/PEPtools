#' # Purpose
#'
#' The Stock Synthesis `starter.ss` file is used to store input controls and is
#' the first interaction the executable has with a model. This R script can be
#' used to generate a template `starter.ss` for use by scientists producing
#' assessments of stocks managed by the Pacific Fisheries Management Council
#' (PFMC). The **goals** of the template are to
#'
#' 1. Store information regarding decisions made to use one value or another.
#' 1. Standardize the inputs used across the team such that downstream code is
#'    easier to maintain.
#' 1. Provide a known starting place for new hires.

#' # Pull an example file from GitHub
starter <- r4ss::SS_readstarter(
  file = paste0(
    "https://raw.githubusercontent.com/nmfs-stock-synthesis/user-examples/",
    "main/model_files/simple/starter.ss"
  ),
  verbose = FALSE
)
n_inputs <- length(starter)

#' # Set values for PFMC

#' ## File names
#'
#' Filenames start with a full stop to ensure that the files appear at the
#' top of the alphabetized list in file explorer. The file extensions use
#' todo: decide on extension here
#' todo: document decision for extension
#' Initial values come from this control file rather than the par file and
#' all parameters that are turned on prior to phase 1000 are estimated.
starter[["ctlfile"]]
starter[["datfile"]]
starter[["init_values_src"]]
starter[["last_estimation_phase"]] <- 999

#' ## Output
#'
#' The default output to the console and to the disk is very verbose such that
#' users have access to all of the relevant and non-relevant output because
#' disk space is cheap and a file that might not seem relevant today could be
#' tomorrow. Furthermore, files such as echoinput.sso and parmtrace.sso are
#' useful for debugging.
starter[["run_display_detail"]] <- 2
starter[["detailed_age_structure"]] <- 2
starter[["checkup"]] <- 1
starter[["parmtrace"]] <- 4
starter[["cumreport"]] <- 2
starter[["N_bootstraps"]] <- 2

#' Reporting of values is dictated by relative years to allow for changing of
#' the modeled years in the data file without needing to change the input years
#' in this file. Reporting of uncertainty will commence in the first modelled
#' year and end with the last year of projections. For users with extremely long
#' models, it may be in their best interest to change these values and input a
#' vector of years for which uncertainty is reported to speed up their model.
#' This vector should be entered in `[["STD_yr_vec"]]`.
starter[["maxyr_sdreport"]] <- -1
starter[["minyr_sdreport"]] <- -2
starter[["STD_yr_vec"]] <- NULL
starter[["N_STD_yrs"]] <- length(starter[["STD_yr_vec"]])

#' ## Priors
#'
#' todo: decide what to do for priors such that likelihood profiles are correct
#'       unless the profile function does this automatically
starter[["prior_like"]]
starter[["soft_bounds"]]


#' todo: decide categories and values for the following entries
starter[["converge_criterion"]]
starter[["min_age_summary_bio"]]
starter[["depl_basis"]]
starter[["depl_denom_frac"]]
starter[["SPR_basis"]]
starter[["F_report_units"]]
starter[["F_age_range"]]
starter[["F_report_basis"]]
starter[["ALK_tolerance"]]

#' ## Unchanged values

vapply(
  X = c(
    "MCMCburn", "MCMCthin", "MCMC_output_detail",
    "jitter_fraction", "retro_yr", "final", "seed"
  ),
  FUN = function(x) starter[[x]],
  FUN.VALUE = 1
)

#' # Check
#'
#' Check that no new inputs were added to the list to ensure that all names
#' used in this script are up to date with the latest {r4ss} code.

stopifnot(n_inputs == length(starter))
