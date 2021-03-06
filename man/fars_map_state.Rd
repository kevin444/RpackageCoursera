% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{State Accident Mapping}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{A state number from the alphabetical list of states.}

\item{year}{The year from which you want to draw data.}
}
\value{
This function, even when successfully run, returns NULL invisibly.
However, it generates a plot in the course of running.
}
\description{
This function takes years and a state number and plots
the accident locations within that state.
}
\examples{
\dontrun{
fars_map_state(1,2015)
fars_map_state(4,2013)
}

}
