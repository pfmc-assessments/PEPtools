% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_filename.R
\name{check_filename}
\alias{check_filename}
\title{Check if a file name adheres to the team guidance on file naming}
\usage{
check_filename(paths)
}
\arguments{
\item{paths}{A vector of file paths to check.}
}
\value{
A vector of approved file paths and a warning if one or more
supplied paths did not pass the checks.
}
\description{
Check if a file name adheres to guidance based on advice from
years of collaborating with stakeholders and scientists while
working with a plethora of softwares.
Users and softwares change over time and data often is static;
thus, it is imperative that file names are easy to work with and consistent.
}
\section{Guidelines}{

The following guidelines are ordered in terms of importance with the understanding
that it will be difficult to adhere to all guidelines at all times.
The list is meant to be a rainbows and lollipops ideology; where,
it is better to know what everyone wants rather than assume anything.
\itemize{
\item Underscores as separators
\item Meta data to include in the name could contain information on
\itemize{
\item area
\item species
\item stakeholder
}
\item Dates formatted as yyyymmdd or 20210101 for the first day of January in 2021;
consider not including the date if
\itemize{
\item older files will not be valid and users would not consider going back in time
\item version control will track changes over time
\item code would need to change in the future to accommodate a new file name, though
this can be overcome with crafty coding
}
\item No spaces
\item Without minimal directory structure, e.g., wcgop_commercial_discard.csv
rather than ./WCGOP/Commercial/discard.csv
\item Camel case only when necessary
}
}

\examples{
check_filename(tempdir())

}
\author{
Kelli Faye Johnson
}
