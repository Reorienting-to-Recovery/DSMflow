% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{freeport_flow}
\alias{freeport_flow}
\title{Flow at Freeport}
\format{
A named list where each element contains a matrix with months (1-12) as rows and years (1980-1999) as columns
}
\source{
\itemize{
\item \strong{Data Wrangling:} Sadie Gill  \email{sgill@flowwest.com}
\item \strong{CALSIM Model Output:} Michael Wright \email{mwright@usbr.gov}
}
}
\usage{
freeport_flow
}
\description{
flow in cms at Freeport (CALSIM node C400) from three separate Calsim runs. The components of the list contain output from
four different Calsim runs:
\itemize{
\item biop_2008_2009: output from the 2008-2009 Calsim II BiOp run.
\item biop_itp_2018_2019: output from the 2018-2019 Calsim II BiOp run.
\item calsim_run_of_river: output from the Calsim Run of River run.
\item LTO-12a: output from the LTO-12a Calsim III run.
}
}
\details{
\code{freeport_flow} was calculated using a 'FLOW-CHANNEL' node from CALSIM II.

\href{https://s3-us-west-2.amazonaws.com/cvpiaflow-r-package/BST_CALSIMII_schematic_040110.jpg}{CALSIM II schematic}
}
\keyword{datasets}
